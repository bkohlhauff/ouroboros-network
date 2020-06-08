{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

-- 'startProtocols' is using 'HasInitiator' constraint to limit pattern
-- matches.
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Ouroboros.Network.PeerSelection.Governor.PeerStateActions where

import           Control.Exception (Exception (..), SomeException (..), assert)
import           Control.Monad (join)
import           Control.Monad.Class.MonadAsync (MonadAsync)
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime (DiffTime)
import           Control.Monad.Class.MonadSTM.Strict
import           Control.Tracer (Tracer, traceWith)

import           Data.ByteString.Lazy (ByteString)
import           Data.Typeable (Typeable, cast)

import qualified Network.Mux        as Mux
import           Network.Mux.Timeout (TimeoutFn)

import           Ouroboros.Network.ConnectionId
import           Ouroboros.Network.Channel (fromChannel)
import           Ouroboros.Network.Mux
import           Ouroboros.Network.Protocol.Handshake
                   ( HandshakeException
                   , HandshakeClientProtocolError
                   , RefuseReason
                   )
import           Ouroboros.Network.PeerSelection.Governor
                   ( PeerStateActions (..)
                   )
import           Ouroboros.Network.PeerSelection.Types
                   ( PeerStatus (..)
                   )

import           Ouroboros.Network.ConnectionManager.ConnectionHandler
                   ( MuxConnectionManager
                   , MuxPromise (..)
                   )
import           Ouroboros.Network.ConnectionManager.Types


-- | A 'MuxApplicaiton', i.e. a bundle of mini-protocols, can either return an
-- error, if one of them failed (only the first error is recorded), or they all
-- terminated sucessfully.
--
data MuxApplicationResult
    -- | A mini-protocol failed with an exception.
    --
    = MuxApplicationError !MiniProtocolNum !SomeException

    -- | All mini-protocols terminated sucessfuly.
    | MuxApplicationSuccess
  deriving Show

instance Semigroup MuxApplicationResult where
    err@MuxApplicationError{} <> _ = err
    _ <> err@MuxApplicationError{} = err
    MuxApplicationSuccess <> MuxApplicationSuccess = MuxApplicationSuccess



-- | Application Handle which allows to stop or start mux threads.
--
data ApplicationHandle muxMode bytes m a b = ApplicationHandle {
    ahApplication :: [MiniProtocol muxMode bytes m a b],
    ahControlVar  :: StrictTVar m ControlMessage,
    ahAwaitVar    :: StrictTVar m (STM m MuxApplicationResult)
  }

--
-- Useful accessors
--

getControlVar :: TokProtocolTemperature pt
              -> Bundle (ApplicationHandle muxMode bytes m a b)
              -> StrictTVar m ControlMessage
getControlVar tok = ahControlVar . projectBundle tok

getProtocols :: TokProtocolTemperature pt
             -> Bundle (ApplicationHandle muxMode bytes m a b)
             -> [MiniProtocol muxMode bytes m a b]
getProtocols tok bundle = ahApplication (projectBundle tok bundle)

getAwaitVar :: TokProtocolTemperature pt
            -> Bundle (ApplicationHandle muxMode bytes m a b)
            -> StrictTVar m (STM m MuxApplicationResult)
getAwaitVar tok = ahAwaitVar . projectBundle tok


-- | Smart construcotor for 'ApplicationHandle'.
--
mkApplicationHandleBundle
    :: forall (muxMode :: MuxMode) bytes m a b.
       MuxBundle muxMode bytes m a b
    -- ^ mux applicaiton
    -> Bundle (StrictTVar m ControlMessage)
    -- ^ schedule stop var
    -> Bundle (StrictTVar m (STM m MuxApplicationResult))
    -- ^ await for application termination
    -> Bundle (ApplicationHandle muxMode bytes m a b)
mkApplicationHandleBundle muxBundle scheduleStopVarBundle awaitVarBundle =
    Bundle
      (mkApplication TokHot)
      (mkApplication TokWarm)
      (mkApplication TokEstablished)
  where
    mkApplication :: TokProtocolTemperature pt
                  -> WithProtocolTemperature pt (ApplicationHandle muxMode bytes m a b)
    mkApplication tok =
      let app =
            ApplicationHandle {
              ahApplication = projectBundle tok muxBundle,
              ahControlVar  = projectBundle tok scheduleStopVarBundle,
              ahAwaitVar    = projectBundle tok awaitVarBundle
            }
      in case tok of
          TokHot -> WithHot app
          TokWarm -> WithWarm app
          TokEstablished -> WithEstablished app



-- |  Each established connection has access to 'PeerConnectionHandle'.  It
-- allows to promote / demote or close the connection, by having access to
-- 'Mux', three bundles of miniprotocols: for hot, warm and established peers
-- together with their state 'StrictTVar's.
--
data PeerConnectionHandle (muxMode :: MuxMode) peerAddr bytes m a b = PeerConnectionHandle {
    pchConnectionId :: ConnectionId peerAddr,
    pchPeerStatus   :: StrictTVar m  PeerStatus,
    pchMux          :: Mux.Mux muxMode m,
    pchAppHandles   :: Bundle (ApplicationHandle muxMode bytes m a b)
  }

--
-- Exceptions
--

-- | Parent exception of all peer selection action exceptions.
--
data PeerSelectionActionException = forall e. Exception e => PeerSelectionActionException e

instance Show PeerSelectionActionException where
    show (PeerSelectionActionException e) = show e

instance Exception PeerSelectionActionException

peerSelectionActionExceptionToException :: Exception e => e -> SomeException
peerSelectionActionExceptionToException = toException . PeerSelectionActionException

peerSelectionActionExceptionFromException :: Exception e => SomeException -> Maybe e
peerSelectionActionExceptionFromException x = do
    PeerSelectionActionException e <- fromException x
    cast e


data EstablishConnectionException versionNumber
      -- | Mux stopped unexpectedly.
    = EstablishConnectionMuxStoppedUnexpectedly

      -- | Handshake client failed
    | EstablishConnectionClientHandshakeException
        !(HandshakeException (HandshakeClientProtocolError versionNumber))

      -- | Handshake server failed
    | EstablishConnectionServerHandshakeException
        !(HandshakeException (RefuseReason versionNumber))
  deriving Show

instance ( Show versionNumber
         , Typeable versionNumber
         ) => Exception (EstablishConnectionException versionNumber) where
    toException   = peerSelectionActionExceptionToException
    fromException = peerSelectionActionExceptionFromException


data PeerSelectionTimeoutException peerAddr
    = PeerActivationTimeoutException      !(ConnectionId peerAddr)
    | PeerDeactivationTimeoutException    !(ConnectionId peerAddr)
    | PeerCloseConnectionTimeoutException !(ConnectionId peerAddr)
  deriving Show

instance ( Show peerAddr
         , Typeable peerAddr
         ) => Exception (PeerSelectionTimeoutException peerAddr) where
    toException   = peerSelectionActionExceptionToException
    fromException = peerSelectionActionExceptionFromException

--
-- 'PeerStateActionsArguments' and 'peerStateActions'
--


-- | Record of arguments of 'peerSelectionActions'.
--
data PeerStateActionsArguments muxMode socket peerAddr versionNumber m a b =
    PeerStateActionsArguments {

      spsTracer                 :: Tracer m (PeerSelectionActionsTrace peerAddr),

      -- | Peer deactivation timeout: timeouts stopping hot protocols.
      --
      spsDeactivateTimeout      :: DiffTime,

      -- | Timeout on closing connection: timeouts stopping established and warm
      -- peer protocols.
      --
      spsCloseConnectionTimeout :: DiffTime,

      spsConnectionManager      :: MuxConnectionManager muxMode socket peerAddr versionNumber ByteString m a b
    }


peerStateActions
    :: forall (muxMode :: MuxMode) socket peerAddr versionNumber m a b.
       ( MonadAsync         m
       , MonadCatch         m
       , MonadMask          m
       , MonadEvaluate (STM m)
       , HasInitiator muxMode ~ True
       , Typeable versionNumber
       , Show     versionNumber
       , Typeable peerAddr
       , Show     peerAddr
       )
    => TimeoutFn m
    -- ^ timeout function, created by 'withTimeoutSerial'
    -> PeerStateActionsArguments muxMode socket peerAddr versionNumber m a b
    -> PeerStateActions peerAddr (PeerConnectionHandle muxMode peerAddr ByteString m a b) m
peerStateActions timeout
                 PeerStateActionsArguments {
                   spsDeactivateTimeout,
                   spsCloseConnectionTimeout,
                   spsTracer,
                   spsConnectionManager
                 } =
   PeerStateActions {
        establishPeerConnection,
        monitorPeerConnection,
        activatePeerConnection,
        deactivatePeerConnection,
        closePeerConnection
      }
  where
    establishPeerConnection :: peerAddr
                            -> m (PeerConnectionHandle muxMode peerAddr ByteString m a b)
    establishPeerConnection remotePeerAddr =
      bracketOnError
        (newTVarM PeerCold)
        (\peerStatusVar -> atomically $ writeTVar peerStatusVar PeerCold)
        $ \peerStatusVar -> do
          (muxPromise :: MuxPromise muxMode peerAddr versionNumber ByteString m a b)
            <- includeOutboundConnection spsConnectionManager remotePeerAddr
               >>= atomically
          case muxPromise of
            MuxRunning connectionId
                       mux
                       muxBundle
                       scheduleStopVarBundle -> do

              atomically $ do
                writeTVar (projectBundle TokHot         scheduleStopVarBundle) Terminate
                writeTVar (projectBundle TokWarm        scheduleStopVarBundle) Continue
                writeTVar (projectBundle TokEstablished scheduleStopVarBundle) Continue

              awaitVarBundle <- atomically $
                Bundle
                  <$> (WithHot <$> mkAwaitVar)
                  <*> (WithWarm <$> mkAwaitVar)
                  <*> (WithEstablished <$> mkAwaitVar)

              let connHandle =
                    PeerConnectionHandle {
                        pchConnectionId = connectionId,
                        pchPeerStatus   = peerStatusVar,
                        pchMux          = mux,
                        pchAppHandles   = mkApplicationHandleBundle
                                            muxBundle
                                            scheduleStopVarBundle
                                            awaitVarBundle
                      }

              startProtocols TokWarm connHandle
              startProtocols TokEstablished connHandle
              atomically $ writeTVar peerStatusVar PeerWarm
              traceWith spsTracer (PeerStatusChanged
                                    (ColdToWarm
                                      (remoteAddress connectionId)
                                      (Just (localAddress connectionId))))
              pure connHandle

            MuxStopped -> do
              traceWith spsTracer (PeerStatusChangeFailure
                                    (ColdToWarm remotePeerAddr Nothing)
                                    MuxStoppedError)
              throwM
                (EstablishConnectionMuxStoppedUnexpectedly
                  :: EstablishConnectionException versionNumber)

            MuxPromiseHandshakeClientError err -> do
              traceWith spsTracer (PeerStatusChangeFailure
                                    (ColdToWarm remotePeerAddr Nothing)
                                    HandshakeClientError)
              throwM (EstablishConnectionClientHandshakeException err)

            MuxPromiseHandshakeServerError err -> do
              traceWith spsTracer (PeerStatusChangeFailure
                                    (ColdToWarm remotePeerAddr Nothing)
                                    HandshakeServerError)
              throwM (EstablishConnectionServerHandshakeException err)

            MuxPromiseError err -> do
              traceWith spsTracer (PeerStatusChangeFailure
                                    (ColdToWarm remotePeerAddr Nothing)
                                    (MuxException err))
              throwM (peerSelectionActionExceptionToException err)
      where
        mkAwaitVar :: STM m (StrictTVar m (STM m MuxApplicationResult))
        mkAwaitVar = newTVar (pure MuxApplicationSuccess)


    -- 'monitorPeerConnection' is only used against established connections
    monitorPeerConnection :: PeerConnectionHandle muxMode peerAddr ByteString m a b
                          -> STM m PeerStatus
    monitorPeerConnection PeerConnectionHandle { pchPeerStatus } =
      readTVar pchPeerStatus


    -- Take a warm peer and promote it to a hot one.
    activatePeerConnection :: PeerConnectionHandle muxMode peerAddr ByteString m a b
                           -> m ()
    activatePeerConnection
        connHandle@PeerConnectionHandle {
            pchConnectionId,
            pchPeerStatus,
            pchAppHandles } =
      do
        -- quiesce warm peer protocols and set hot ones in 'Continue' mode.
        atomically $ do
          writeTVar (getControlVar TokHot pchAppHandles) Continue
          writeTVar (getControlVar TokWarm pchAppHandles) Quiesce
          e <- readTVar (getControlVar TokEstablished pchAppHandles)
          evaluate $ assert (e == Continue) ()

        -- start hot peer protocols
        startProtocols TokHot connHandle
        atomically $ writeTVar pchPeerStatus PeerHot
        traceWith spsTracer (PeerStatusChanged (WarmToHot pchConnectionId))
      `onException` do
        atomically $ writeTVar pchPeerStatus PeerCold


    -- Take a hot peer and demote it to a warm one.
    deactivatePeerConnection :: PeerConnectionHandle muxMode peerAddr ByteString m a b -> m ()
    deactivatePeerConnection
        PeerConnectionHandle {
            pchConnectionId,
            pchPeerStatus,
            pchMux,
            pchAppHandles
          } =
      do
        atomically $ do
          writeTVar (getControlVar TokHot pchAppHandles) Terminate

          w <- readTVar (getControlVar TokWarm pchAppHandles)
          evaluate $ assert (w == Quiesce) ()
          writeTVar (getControlVar TokWarm pchAppHandles) Continue

          e <- readTVar (getControlVar TokEstablished pchAppHandles)
          evaluate $ assert (e == Continue) ()

        -- Hot protocols should stop within 'spsDeactivateTimeout'.
        res <-
          timeout spsDeactivateTimeout
                  (atomically $ join $ readTVar (getAwaitVar TokHot pchAppHandles))
        case res of
          Nothing -> do
            Mux.stopMux pchMux
            traceWith spsTracer (PeerStatusChangeFailure
                                  (HotToWarm pchConnectionId)
                                  TimeoutError)
            throwM (PeerDeactivationTimeoutException pchConnectionId)

          Just (MuxApplicationError protocolNum e@(SomeException err)) -> do
            traceWith spsTracer (PeerStatusChangeFailure
                                  (HotToWarm pchConnectionId)
                                  (ApplicationError protocolNum e))
            throwM (peerSelectionActionExceptionToException err)

          Just MuxApplicationSuccess -> do
            atomically $ writeTVar pchPeerStatus PeerWarm
            traceWith spsTracer (PeerStatusChanged (HotToWarm pchConnectionId))

        `onException` do
          atomically $ writeTVar pchPeerStatus PeerCold


    closePeerConnection :: PeerConnectionHandle muxMode peerAddr ByteString m a b
                        -> m ()
    closePeerConnection
        PeerConnectionHandle {
            pchConnectionId,
            pchPeerStatus,
            pchMux,
            pchAppHandles
          } =
      do
        atomically $ do
          writeTVar (getControlVar TokWarm pchAppHandles) Terminate
          writeTVar (getControlVar TokEstablished pchAppHandles) Terminate
          -- TODO: double check that there is no HOT -> COLD transition
          h <- readTVar (getControlVar TokHot pchAppHandles)
          evaluate $ assert (h == Terminate) ()

        res <-
          timeout spsCloseConnectionTimeout
                  (atomically $
                    (\a b c -> a <> b <> c)
                      <$> (join $ readTVar (getAwaitVar TokHot pchAppHandles))
                      <*> (join $ readTVar (getAwaitVar TokWarm pchAppHandles))
                      <*> (join $ readTVar (getAwaitVar TokEstablished pchAppHandles)))
        case res of
          Nothing -> do
            Mux.stopMux pchMux
            traceWith spsTracer (PeerStatusChangeFailure
                                  (WarmToCold pchConnectionId)
                                  TimeoutError)

            throwM (PeerCloseConnectionTimeoutException pchConnectionId)

          Just (MuxApplicationError protocolNum e@(SomeException err)) -> do
            traceWith spsTracer (PeerStatusChangeFailure
                                  (WarmToCold pchConnectionId)
                                  (ApplicationError protocolNum e))
            throwM (peerSelectionActionExceptionToException err)

          Just MuxApplicationSuccess ->
            traceWith spsTracer (PeerStatusChanged (WarmToCold pchConnectionId))
      `finally` do
          atomically $ writeTVar pchPeerStatus PeerCold

--
-- Utils
--


-- | Given a singleton 'TokAppKind' and 'PeerConnectionHandle' start the mux
-- protocol bundle indicated by the type of the first argument.
--
startProtocols :: forall (muxMode :: MuxMode) (pt :: ProtocolTemperature) peerAddr m a b.
                  ( MonadAsync m
                  , MonadCatch m
                  , HasInitiator muxMode ~ True
                  )
               => TokProtocolTemperature pt
               -> PeerConnectionHandle muxMode peerAddr ByteString m a b
               -> m ()
startProtocols tok PeerConnectionHandle { pchMux, pchAppHandles } = do
    let ptcls = getProtocols tok pchAppHandles
    as <- traverse runInitiator ptcls
    atomically $ writeTVar (getAwaitVar tok pchAppHandles)
                           (awaitSTM $ zip (miniProtocolNum `map` ptcls) as)
  where
    -- await for all stm transactions to resolve; if any of them fails return
    -- immediatelly.  In this case we will stop mux.
    awaitSTM :: [(MiniProtocolNum, STM m (Either SomeException a))]
             -> STM m MuxApplicationResult
    awaitSTM [] = pure MuxApplicationSuccess
    awaitSTM ((miniProtocolNum, stm) : as) = do
      a <- stm
      case a of
        Right _ -> awaitSTM as
        Left e  -> pure (MuxApplicationError miniProtocolNum e)

    runInitiator :: MiniProtocol muxMode ByteString m a b
                 -> m (STM m (Either SomeException a))
    runInitiator MiniProtocol {
                      miniProtocolNum,
                      miniProtocolRun
                    } =
      case miniProtocolRun of
        InitiatorProtocolOnly initiator ->
            Mux.runMiniProtocol
              pchMux miniProtocolNum
              Mux.InitiatorDirectionOnly
              Mux.StartEagerly
              (runMuxPeer initiator . fromChannel)
        InitiatorAndResponderProtocol initiator _ ->
            Mux.runMiniProtocol
              pchMux miniProtocolNum
              Mux.InitiatorDirection
              Mux.StartEagerly
              (runMuxPeer initiator . fromChannel)

--
-- Trace
--

-- | Type of failure with additional exception context; We don't log handshake
-- errors as this will be done by the handshake tracer.
--
data FailureType =
      HandshakeClientError
    | HandshakeServerError
    | MuxException     !SomeException
    | MuxStoppedError
    | TimeoutError
    | ApplicationError !MiniProtocolNum !SomeException
  deriving Show

-- | All transitions.
--
data PeerStatusChangeType peerAddr =
    -- | During the 'ColdToWarm' transition we have the remote address, and only
    -- if establishing connection (establishing bearer & handhsake negotation)
    -- is successful we have access to full `ConnectionId`.
      ColdToWarm
        !peerAddr         -- ^ remote peer address
        !(Maybe peerAddr) -- ^ local peer address
    | WarmToHot  !(ConnectionId peerAddr)
    | HotToWarm  !(ConnectionId peerAddr)
    | WarmToCold !(ConnectionId peerAddr)
  deriving Show

-- | Traces produced by 'peerSelectionActions'.
--
data PeerSelectionActionsTrace peerAddr =
      PeerStatusChanged       !(PeerStatusChangeType peerAddr)
    | PeerStatusChangeFailure !(PeerStatusChangeType peerAddr) !FailureType
  deriving Show
