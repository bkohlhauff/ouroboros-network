{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Server implementation based on 'ConnectionManager'
--
-- TODO: in the futures this should be moved to `Ouroboros.Network.Server`, but
-- to avoid confusion it will be kept here for now.
--
module Ouroboros.Network.ConnectionManager.Server
  ( ServerArguments (..)
  , run
  -- * Trace
  , ServerTrace (..)
  , AcceptConnectionsPolicyTrace (..)

  -- * Internals
  , peekAlt
  ) where

import           Control.Applicative (Alternative (..))
import           Control.Exception (SomeException)
import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow
import           Control.Monad.Class.MonadTime
import           Control.Monad.Class.MonadTimer
import           Control.Tracer (Tracer, contramap, traceWith)
import           Data.ByteString.Lazy (ByteString)
import           Data.Void (Void)
import           Data.Foldable (traverse_)
import           Data.Functor (void)
import           Data.Sequence.Strict (StrictSeq (..), (|>), (><))
import qualified Data.Sequence.Strict as Seq

import qualified Network.Mux        as Mux

import           Ouroboros.Network.ConnectionManager.Types
import           Ouroboros.Network.ConnectionManager.ConnectionHandler
import           Ouroboros.Network.Mux
import           Ouroboros.Network.Channel (fromChannel)
import           Ouroboros.Network.Server.RateLimiting
import           Ouroboros.Network.Snocket


data ServerArguments (muxMode :: MuxMode) socket peerAddr versionNumber versionDict bytes m a b = ServerArguments {
      serverSocket            :: socket,
      serverSnocket           :: Snocket m socket peerAddr,
      serverTracer            :: Tracer m (ServerTrace peerAddr),
      serverConnectionLimits  :: AcceptedConnectionsLimit,
      serverConnectionManager :: MuxConnectionManager muxMode socket peerAddr
                                                      versionNumber bytes m a b
    }

run :: forall muxMode socket peerAddr versionNumber versionDict m a b.
       ( MonadAsync m
       , MonadCatch m
       , MonadDelay m
       , MonadTime  m
       , Mux.HasResponder muxMode ~ True
       )
    => ServerArguments muxMode socket peerAddr versionNumber versionDict ByteString m a b
    -> m Void
run ServerArguments {
      serverSocket,
      serverSnocket,
      serverTracer,
      serverConnectionLimits,
      serverConnectionManager
    } =
      getLocalAddr serverSnocket serverSocket >>= \localAddr -> do
        traceWith serverTracer (ServerStarted localAddr)
        muxVars <- newTVarM Seq.Empty
        (uncurry (<>)) <$>
          (monitoring muxVars)
          `concurrently`
          (acceptLoop muxVars (accept serverSnocket serverSocket))
      `finally`
        traceWith serverTracer (ServerStopped localAddr)
  where
    -- This is the tricky part of the `monitoring` thread.  We want to return
    -- the 'a' and the list of all other unresolved transations (otherwise we
    -- would leaked memory).  It is implemented in terms of 'Alternative' for
    -- testing purposes.
    peekSTM :: forall x. StrictSeq (STM m x) -> STM m (x, StrictSeq (STM m x))
    peekSTM = peekAlt


    monitoring :: TVar m
                   (StrictSeq
                     (STM m (MuxPromise muxMode peerAddr verionNumber ByteString m a b)))
               -> m Void
    monitoring muxVars = do
      muxPromise <- atomically $ do
        muxs <- readTVar muxVars
        (muxPromise, muxs') <- peekSTM muxs
        writeTVar muxVars muxs'
        pure muxPromise
      case muxPromise of
        MuxRunning _connectionId
                   mux
                   (Bundle
                     (WithHot hotPtls)
                     (WithWarm warmPtls)
                     (WithEstablished establishedPtls))
                 _ -> do
          traverse_ (runResponder mux) hotPtls
          traverse_ (runResponder mux) warmPtls
          traverse_ (runResponder mux) establishedPtls
        _ -> pure ()
      monitoring muxVars


    runResponder :: Mux.Mux muxMode m -> MiniProtocol muxMode ByteString m a b -> m ()
    runResponder mux MiniProtocol {
                        miniProtocolNum,
                        miniProtocolRun
                      } =
        case miniProtocolRun of
          ResponderProtocolOnly responder ->
            void $
              Mux.runMiniProtocol
                mux miniProtocolNum
                Mux.ResponderDirectionOnly
                Mux.StartEagerly
                -- TODO: eliminate 'fromChannel'
                (runMuxPeer responder . fromChannel)
          InitiatorAndResponderProtocol _ responder ->
            void $
              Mux.runMiniProtocol
                mux miniProtocolNum
                Mux.ResponderDirection
                Mux.StartEagerly
                (runMuxPeer responder . fromChannel)


    acceptLoop :: TVar m
                   (StrictSeq
                     (STM m
                       (MuxPromise muxMode peerAddr versionNumber ByteString m a b)))
               -> Accept m SomeException peerAddr socket
               -> m Void
    acceptLoop muxVars acceptOne = do
      runConnectionRateLimits
        (ServerAcceptPolicyTrace `contramap` serverTracer)
        (numberOfConnections serverConnectionManager)
        serverConnectionLimits
      result <- runAccept acceptOne
      case result of
        (AcceptException err, acceptNext) -> do
          traceWith serverTracer (ServerAcceptError err)
          acceptLoop muxVars acceptNext
        (Accepted socket peerAddr, acceptNext) -> do
          traceWith serverTracer (ServerAcceptConnection peerAddr)
          !muxPromise <-
            includeInboundConnection
              serverConnectionManager
              socket peerAddr
          atomically $ modifyTVar muxVars (\as -> as |> muxPromise)
          acceptLoop muxVars acceptNext


--
-- Trace
--

data ServerTrace peerAddr
    = ServerAcceptConnection peerAddr
    | ServerAcceptError SomeException
    | ServerAcceptPolicyTrace AcceptConnectionsPolicyTrace
    | ServerStarted peerAddr
    | ServerStopped peerAddr
  deriving Show

--
-- Internals
--

-- | 'peekAlt' finds first non 'empty' element and returns it together with the
-- sequence of all the other ones (preserving their original order).  Only the
-- returned non-empty element is dropped from the sequence.  It is expressed
-- using 'Alternative' applicative functor, instead of `STM m` for
-- testing purposes.
--
peekAlt :: Alternative m
        => StrictSeq (m a)
        -> m (a, StrictSeq (m a))
peekAlt = go Seq.Empty
  where
    -- in the cons case we either can resolve 'stm', in which case we
    -- return the value together with list of all other transactions, or
    -- (`<|>`) we push it on the `acc` and recrurse.
    go !acc (stm :<| stms) =
      ((\a -> (a, acc >< stms)) <$> stm)
      <|>
      go (acc |> stm) stms
    -- in the 'Empty' case, we just need to 'retry' the trasaction (hence we
    -- use 'empty').
    go _acc Seq.Empty = empty
