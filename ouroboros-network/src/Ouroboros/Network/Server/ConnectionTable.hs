
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ouroboros.Network.Server.ConnectionTable (
      ConnectionTable
    , ConnectionTableRef (..)
    , ValencyCounter

    , newConnectionTable
    , refConnection
    , addConnection
    , removeConnection
    , newValencyCounter
    , addValencyCounter
    , remValencyCounter
    , waitValencyCounter
    , readValencyCounter
    ) where

import           Control.Monad (when)
import           Control.Monad.Class.MonadSTM
--import           Control.Tracer XXX Not Yet
import qualified Data.Map.Strict as M
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Network.Socket as Socket
import           Text.Printf

-- A ConnectionTable represent a set of connections that is shared between
-- servers and subscription workers. It's main purpose is to avoid the creation of duplicate
-- connections (especially connections with identical source address, destination address, source
-- port and destination port which would be rejected by the kernel anyway.).
-- It is only used for bookkeeping, the sockets represented by the connections are not accessable
-- through this structure.
--
data ConnectionTable m = ConnectionTable {
    ctTable     :: TVar m (M.Map Socket.SockAddr (ConnectionTableEntry m))
  , ctLastRefId :: TVar m Int
  }

-- | ValencyCounter represents how many active connections we have towards a given peer.
-- It starts out with a positive value representing a desired number of connections for a specific
-- subscription worker. It can become negative, for example if a peer opens multiple connections
-- to us.
-- The vcId is unique per ConnectionTable and ensures that we won't count the same connection twice.
--
data ValencyCounter m = ValencyCounter {
    vcId  :: Int
  , vcRef :: TVar m Int
  }

-- | Create a new ValencyCounter
newValencyCounter
  :: MonadSTM m
  => ConnectionTable m
  -> Int
  -- ^ Desired valency, that is number of connections a subscription worker will attempt to
  -- maintain.
  -> STM m (ValencyCounter m)
newValencyCounter tbl valency =  do
  lr <- readTVar $ ctLastRefId tbl
  let !lr' = lr + 1
  writeTVar (ctLastRefId tbl) lr'
  v <- newTVar valency
  return $ ValencyCounter lr' v

instance Ord (ValencyCounter m) where
    compare a b = compare (vcId a) (vcId b)

instance Eq (ValencyCounter m) where
    (==) a b = vcId a == vcId b

-- | Returns current ValencyCounter value, represent the number of additional connections that
-- can be created. May be negative.
readValencyCounter :: MonadSTM m => ValencyCounter m -> STM m Int
readValencyCounter vc = readTVar $ vcRef vc

data ConnectionTableEntry m = ConnectionTableEntry {
    -- | Set of ValencyCounter's for subscriptions interested in this peer.
      cteRefs           :: !(Set (ValencyCounter m))
    -- | Set of local SockAddr connected to this peer.
    , cteLocalAddresses :: !(Set Socket.SockAddr)
    }

data ConnectionTableRef =
    ConnectionTableCreate
  -- ^ No connection to peer exists, attempt to create one.
  | ConnectionTableExist
  -- ^ A connection to the peer existed, either from another subscriber or the peer opened one
  -- towards us.
  | ConnectionTableDuplicate
  -- ^ This subscriber already has counted a connection to this peer. It must try another target.
  deriving Show

-- | Add a connection.
addValencyCounter :: MonadSTM m => ValencyCounter m -> STM m ()
addValencyCounter vc = modifyTVar' (vcRef vc) (\r -> r - 1)

-- | Remove a connection.
remValencyCounter :: MonadSTM m => ValencyCounter m -> STM m ()
remValencyCounter vc = modifyTVar' (vcRef vc) (+ 1)

-- | Wait until ValencyCounter becomes positive, used for detecting when
-- we can create new connections.
waitValencyCounter :: MonadSTM m => ValencyCounter m -> STM m ()
waitValencyCounter vc = do
  v <- readTVar $ vcRef vc
  when (v <= 0)
    retry

-- | Create a new ConnectionTable.
newConnectionTable :: MonadSTM m => m (ConnectionTable m)
newConnectionTable =  do
    tbl <- newTVarM M.empty
    li <- newTVarM 0
    return $ ConnectionTable tbl li

-- | Insert a new connection into the ConnectionTable.
addConnection
    :: forall m. (MonadSTM m)
    => ConnectionTable m
    -> Socket.SockAddr
    -> Socket.SockAddr
    -> Maybe (ValencyCounter m)
    -- ^ Optional ValencyCounter, used by subscription worker and set to Nothing when
    -- called by a local server.
    -> STM m ()
addConnection ConnectionTable{..} remoteAddr localAddr ref_m =
    readTVar ctTable >>= M.alterF fn remoteAddr >>= writeTVar ctTable
  where
    fn :: Maybe (ConnectionTableEntry m) -> STM m (Maybe (ConnectionTableEntry m))
    fn Nothing = do
        refs <- case ref_m of
                     Just ref -> do
                         addValencyCounter ref
                         return $ S.singleton ref
                     Nothing -> return S.empty
        return $ Just $ ConnectionTableEntry refs (S.singleton localAddr)
    fn (Just cte) = do
          let refs' = case ref_m of
                           Just ref -> S.insert ref (cteRefs cte)
                           Nothing  -> cteRefs cte
          -- Signal to all parties (dnsSubscriptionWorkers) that are interested in tracking the
          -- number of connections to this particlar peer that we've created a new connection.
          mapM_ addValencyCounter refs'
          return $ Just $ cte {
                cteRefs = refs'
              , cteLocalAddresses = S.insert localAddr (cteLocalAddresses cte)
              }

-- TODO This should use Control.Tracer
_dumpConnectionTable
    :: ConnectionTable IO
    -> IO ()
_dumpConnectionTable ConnectionTable{..} = do
    tbl <- atomically $ readTVar ctTable
    printf "Dumping Table:\n"
    mapM_ dumpTableEntry (M.toList tbl)
  where
    dumpTableEntry :: (Socket.SockAddr, ConnectionTableEntry IO) -> IO ()
    dumpTableEntry (remoteAddr, ce) = do
        refs <- mapM (atomically . readTVar . vcRef) (S.elems $ cteRefs ce)
        let rids = map vcId $ S.elems $ cteRefs ce
            refids = zip rids refs
        printf "Remote Address: %s\nLocal Addresses %s\nReferenses %s\n"
            (show remoteAddr) (show $ cteLocalAddresses ce) (show refids)

-- | Remove a Connection.
removeConnection
    :: forall m. (MonadSTM m)
    => ConnectionTable m
    -> Socket.SockAddr
    -> Socket.SockAddr
    -> m ()
removeConnection ConnectionTable{..} remoteAddr localAddr = atomically $
    readTVar ctTable >>= M.alterF fn remoteAddr >>= writeTVar ctTable
  where
    fn :: Maybe (ConnectionTableEntry m) -> STM m (Maybe (ConnectionTableEntry m))
    fn Nothing = return Nothing -- XXX removing non existent address
    fn (Just ConnectionTableEntry{..}) = do
        mapM_ remValencyCounter cteRefs
        let localAddresses' = S.delete localAddr cteLocalAddresses
        if null localAddresses'
            then return Nothing
            else return $ Just $ ConnectionTableEntry cteRefs localAddresses'

-- | Try to see if it is possible to reference an existing connection rather
-- than creating a new one to the provied peer.
--
refConnection
    :: MonadSTM m
    => ConnectionTable m
    -> Socket.SockAddr
    -> ValencyCounter m
    -> m ConnectionTableRef
refConnection ConnectionTable{..} remoteAddr refVar = atomically $ do
    tbl <- readTVar ctTable
    case M.lookup remoteAddr tbl of
         Nothing -> return ConnectionTableCreate
         Just cte ->
             if S.member refVar $ cteRefs cte
                 then return ConnectionTableDuplicate
                 else do
                     -- TODO We look up remoteAddr twice, is it possible
                     -- to use M.alterF given that we need to be able to return
                     -- ConnectionTableCreate or ConnectionTableExist?
                     let refs' = S.insert refVar (cteRefs cte)
                     mapM_ addValencyCounter $ S.toList refs'

                     writeTVar ctTable $ M.insert remoteAddr
                         (cte { cteRefs = refs'}) tbl
                     return ConnectionTableExist
