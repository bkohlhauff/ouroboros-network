{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.KeepAlive
  ( KeepAliveInterval (..)
  , keepAliveClient
  , keepAliveServer
  ) where

import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadTime (DiffTime)
import           Control.Monad.Class.MonadTimer

import           Ouroboros.Network.Mux (ControlMessage (..), ControlMessageSTM)
import           Ouroboros.Network.Protocol.KeepAlive.Client
import           Ouroboros.Network.Protocol.KeepAlive.Server


newtype KeepAliveInterval = KeepAliveInterval { keepAliveInterval :: DiffTime }


keepAliveClient
    :: forall m.
       ( MonadSTM   m
       , MonadTimer m
       )
    => ControlMessageSTM m
    -> KeepAliveInterval
    -> KeepAliveClient m ()
keepAliveClient controlMessageSTM KeepAliveInterval { keepAliveInterval } =
    SendMsgKeepAlive go
  where
    decisionSTM :: TVar m Bool
                -> STM  m ControlMessage
    decisionSTM delayVar =
      do
       readTVar delayVar >>= fmap (const Continue) . check
      `orElse`
      controlMessageSTM

    go :: m (KeepAliveClient m ())
    go = do
      delayVar <- registerDelay keepAliveInterval
      decision <- atomically (decisionSTM delayVar)
      case decision of
        Terminate -> pure (SendMsgDone (pure ()))
        -- on 'Continue' and 'Quiesce', keep going.
        _         -> pure (SendMsgKeepAlive go)


keepAliveServer
  :: forall m.  Applicative m
  => KeepAliveServer m ()
keepAliveServer = KeepAliveServer {
    recvMsgKeepAlive = pure keepAliveServer,
    recvMsgDone      = pure ()
  }
