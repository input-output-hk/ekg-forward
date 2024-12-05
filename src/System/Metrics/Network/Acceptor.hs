{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module System.Metrics.Network.Acceptor
  ( listenToForwarder
  -- | Export this function for Mux purpose.
  , acceptEKGMetricsInit
  , acceptEKGMetricsResp
  ) where

import           Codec.CBOR.Term (Term)
import qualified Codec.Serialise as CBOR
import           Control.Exception (finally)
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (async, wait)
import           Control.Concurrent.STM.TVar (TVar, readTVarIO)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import           Data.Time.Clock (NominalDiffTime)
import           Data.Void (Void)
import qualified Network.Mux as Mux
import qualified Network.Socket as Socket
import           Ouroboros.Network.Context (MinimalInitiatorContext, ResponderContext)
import           Ouroboros.Network.Driver.Limits (ProtocolTimeLimits)
import           Ouroboros.Network.ErrorPolicy (nullErrorPolicies)
import           Ouroboros.Network.IOManager (withIOManager)
import           Ouroboros.Network.Driver.Simple (runPeer)
import           Ouroboros.Network.Mux (MiniProtocol (..), MiniProtocolCb (..),
                                        MiniProtocolLimits (..),
                                        MiniProtocolNum (..),
                                        OuroborosApplication (..),
                                        RunMiniProtocol (..),
                                        miniProtocolLimits, miniProtocolNum, miniProtocolRun)
import           Ouroboros.Network.Snocket (LocalAddress, MakeBearer, Snocket,
                                            localAddressFromPath, localSnocket, socketSnocket,
                                            makeLocalBearer, makeSocketBearer)
import           Ouroboros.Network.Socket (AcceptedConnectionsLimit (..),
                                           HandshakeCallbacks (..),
                                           SomeResponderApplication (..),
                                           cleanNetworkMutableState, newNetworkMutableState,
                                           nullNetworkServerTracers, withServerNode)
import           Ouroboros.Network.Protocol.Handshake.Codec (noTimeLimitsHandshake,
                                                             timeLimitsHandshake)
import           Ouroboros.Network.Protocol.Handshake.Type (Handshake)
import           Ouroboros.Network.Protocol.Handshake.Unversioned (UnversionedProtocol (..),
                                                                   UnversionedProtocolData (..),
                                                                   unversionedHandshakeCodec,
                                                                   unversionedProtocolDataCodec)
import           Ouroboros.Network.Protocol.Handshake.Version (acceptableVersion, queryVersion, simpleSingletonVersions)
import qualified System.Metrics as EKG

import qualified System.Metrics.Protocol.Acceptor as Acceptor
import qualified System.Metrics.Protocol.Codec as Acceptor
import           System.Metrics.Store.Acceptor (MetricsLocalStore (..), storeMetrics)
import           System.Metrics.ReqResp (Request (..), Response (..))
import           System.Metrics.Configuration (AcceptorConfiguration (..), HowToConnect (..))

listenToForwarder
  :: AcceptorConfiguration
  -> (ResponderContext (Either LocalAddress Socket.SockAddr) -> IO (EKG.Store, TVar MetricsLocalStore))
  -> (ResponderContext (Either LocalAddress Socket.SockAddr) -> IO ())
  -> IO Void
listenToForwarder config mkStores peerErrorHandler = withIOManager $ \iocp -> do
  case forwarderEndpoint config of
    LocalPipe localPipe -> do
      let app = acceptorApp config (mkStores . fmap Left) (peerErrorHandler . fmap Left)
          snocket = localSnocket iocp
          address = localAddressFromPath localPipe
          configureSocket = mempty
      doListenToForwarder snocket makeLocalBearer configureSocket address noTimeLimitsHandshake app
    RemoteSocket host port -> do
      listenAddress:_ <- Socket.getAddrInfo Nothing (Just $ T.unpack host) (Just $ show port)
      let app = acceptorApp config (mkStores . fmap Right) (peerErrorHandler . fmap Right)
          snocket = socketSnocket iocp
          address = Socket.addrAddress listenAddress
          configureSocket fd _addr =
            Socket.setSocketOption fd Socket.ReuseAddr 1
      doListenToForwarder snocket makeSocketBearer configureSocket address timeLimitsHandshake app

doListenToForwarder
  :: Ord addr
  => Snocket IO fd addr
  -> MakeBearer IO fd
  -> (fd -> addr -> IO ()) -- ^ configure socket
  -> addr
  -> ProtocolTimeLimits (Handshake UnversionedProtocol Term)
  -> OuroborosApplication 'Mux.ResponderMode
                          (MinimalInitiatorContext addr)
                          (ResponderContext addr)
                          LBS.ByteString IO Void ()
  -> IO Void
doListenToForwarder snocket makeBearer configureSocket address timeLimits app = do
  networkState <- newNetworkMutableState
  _ <- async $ cleanNetworkMutableState networkState
  withServerNode
    snocket
    makeBearer
    configureSocket
    nullNetworkServerTracers
    networkState
    (AcceptedConnectionsLimit maxBound maxBound 0)
    address
    unversionedHandshakeCodec
    timeLimits
    unversionedProtocolDataCodec
    (HandshakeCallbacks acceptableVersion queryVersion)
    (simpleSingletonVersions
      UnversionedProtocol
      UnversionedProtocolData
      (SomeResponderApplication app))
    nullErrorPolicies
    $ \_ serverAsync -> wait serverAsync -- Block until async exception.

acceptorApp
  :: AcceptorConfiguration
  -> (ResponderContext addr -> IO (EKG.Store, TVar MetricsLocalStore))
  -> (ResponderContext addr -> IO ())
  -> OuroborosApplication 'Mux.ResponderMode
                          (MinimalInitiatorContext addr)
                          (ResponderContext addr)
                          LBS.ByteString IO Void ()
acceptorApp config mkStores peerErrorHandler =
  OuroborosApplication [
    MiniProtocol
      { miniProtocolNum    = MiniProtocolNum 2
      , miniProtocolLimits = MiniProtocolLimits { maximumIngressQueue = maxBound }
      , miniProtocolRun    = acceptEKGMetricsResp config mkStores peerErrorHandler
      }
  ]

acceptEKGMetricsResp
  :: AcceptorConfiguration
  -> (responderCtx -> IO (EKG.Store, TVar MetricsLocalStore))
  -> (responderCtx -> IO ())
  -> RunMiniProtocol 'Mux.ResponderMode initiatorCtx responderCtx LBS.ByteString IO Void ()
acceptEKGMetricsResp config mkStores peerErrorHandler =
  ResponderProtocolOnly $ runPeerWithStores config mkStores peerErrorHandler

acceptEKGMetricsInit
  :: AcceptorConfiguration
  -> (initiatorCtx -> IO (EKG.Store, TVar MetricsLocalStore))
  -> (initiatorCtx -> IO ())
  -> RunMiniProtocol 'Mux.InitiatorMode initiatorCtx responderCtx LBS.ByteString IO () Void
acceptEKGMetricsInit config mkStores peerErrorHandler =
  InitiatorProtocolOnly $ runPeerWithStores config mkStores peerErrorHandler

runPeerWithStores
  :: AcceptorConfiguration
  -> (ctx -> IO (EKG.Store, TVar MetricsLocalStore))
  -> (ctx -> IO ())
  -> MiniProtocolCb ctx LBS.ByteString IO ()
runPeerWithStores config mkStores peerErrorHandler =
  MiniProtocolCb $ \ctx channel -> do
    (ekgStore, metricsStore) <- mkStores ctx
    runPeer
      (acceptorTracer config)
      (Acceptor.codecEKGForward CBOR.encode CBOR.decode
                                CBOR.encode CBOR.decode)
      channel
      (Acceptor.ekgAcceptorPeer $ acceptorActions True config ekgStore metricsStore)
    `finally` peerErrorHandler ctx

acceptorActions
  :: Bool
  -> AcceptorConfiguration
  -> EKG.Store
  -> TVar MetricsLocalStore
  -> Acceptor.EKGAcceptor Request Response IO ()
acceptorActions True config@AcceptorConfiguration{..} ekgStore metricsStore =
  Acceptor.SendMsgReq whatToRequest $ \response -> do
    storeMetrics response ekgStore metricsStore
    threadDelay $ toMicroSecs requestFrequency
    weAreDone <- readTVarIO shouldWeStop
    if weAreDone
      then return $ acceptorActions False config ekgStore metricsStore
      else return $ acceptorActions True  config ekgStore metricsStore
 where
  toMicroSecs :: NominalDiffTime -> Int
  toMicroSecs dt = fromEnum dt `div` 1000000

acceptorActions False _ _ _ =
  Acceptor.SendMsgDone $ return ()
