{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}

module System.Metrics.Network.Forwarder
  ( connectToAcceptor
    -- | Export this function for Mux purpose.
  , forwardEKGMetrics
  , forwardEKGMetricsDummy
  , forwardEKGMetricsResp
  , forwardEKGMetricsRespDummy
  ) where

import           Codec.CBOR.Term (Term)
import qualified Codec.Serialise as CBOR
import           "contra-tracer" Control.Tracer (nullTracer)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import           Data.Void (Void)
import qualified Network.Socket as Socket
import           Ouroboros.Network.Context (MinimalInitiatorContext, ResponderContext)
import           Ouroboros.Network.Driver.Simple (runPeer)
import           Ouroboros.Network.Driver.Limits (ProtocolTimeLimits)
import           Ouroboros.Network.IOManager (withIOManager)
import           Ouroboros.Network.Mux (MiniProtocol (..), MiniProtocolCb (..),
                                        MiniProtocolLimits (..), MiniProtocolNum (..),
                                        MuxMode (..), OuroborosApplication (..),
                                        RunMiniProtocol (..),
                                        miniProtocolLimits, miniProtocolNum, miniProtocolRun)
import           Ouroboros.Network.Protocol.Handshake.Codec (noTimeLimitsHandshake,
                                                             timeLimitsHandshake)
import           Ouroboros.Network.Protocol.Handshake.Type (Handshake)
import           Ouroboros.Network.Protocol.Handshake.Unversioned (UnversionedProtocol (..),
                                                                   UnversionedProtocolData (..),
                                                                   unversionedHandshakeCodec,
                                                                   unversionedProtocolDataCodec)
import           Ouroboros.Network.Protocol.Handshake.Version (acceptableVersion, queryVersion, simpleSingletonVersions)
import           Ouroboros.Network.Snocket (MakeBearer, Snocket,
                                            localAddressFromPath, localSnocket, socketSnocket,
                                            makeLocalBearer, makeSocketBearer)
import           Ouroboros.Network.Socket (HandshakeCallbacks (..), connectToNode, nullNetworkConnectTracers)
import qualified System.Metrics as EKG

import           System.Metrics.Configuration (ForwarderConfiguration (..), HowToConnect (..))
import           System.Metrics.Store.Forwarder (mkResponse, mkResponseDummy)
import qualified System.Metrics.Protocol.Forwarder as Forwarder
import qualified System.Metrics.Protocol.Codec as Forwarder

connectToAcceptor
  :: ForwarderConfiguration
  -> EKG.Store
  -> IO ()
connectToAcceptor config@ForwarderConfiguration{..} ekgStore = withIOManager $ \iocp -> do
  let app = forwarderApp config ekgStore
  case acceptorEndpoint of
    LocalPipe localPipe -> do
      let snocket = localSnocket iocp
          address = localAddressFromPath localPipe
      doConnectToAcceptor snocket makeLocalBearer mempty address noTimeLimitsHandshake app
    RemoteSocket host port -> do
      acceptorAddr:_ <- Socket.getAddrInfo Nothing (Just $ T.unpack host) (Just $ show port)
      let snocket = socketSnocket iocp
          address = Socket.addrAddress acceptorAddr
      doConnectToAcceptor snocket makeSocketBearer mempty address timeLimitsHandshake app

doConnectToAcceptor
  :: Snocket IO fd addr
  -> MakeBearer IO fd
  -> (fd -> IO ()) -- ^ configure socket
  -> addr
  -> ProtocolTimeLimits (Handshake UnversionedProtocol Term)
  -> OuroborosApplication 'InitiatorMode
                          (MinimalInitiatorContext addr)
                          (ResponderContext addr)
                          LBS.ByteString IO () Void
  -> IO ()
doConnectToAcceptor snocket makeBearer configureSocket address timeLimits app =
  connectToNode
    snocket
    makeBearer 
    configureSocket
    unversionedHandshakeCodec
    timeLimits
    unversionedProtocolDataCodec
    nullNetworkConnectTracers
    (HandshakeCallbacks acceptableVersion queryVersion)
    (simpleSingletonVersions
       UnversionedProtocol
       UnversionedProtocolData
       app)
    Nothing
    address

forwarderApp
  :: ForwarderConfiguration
  -> EKG.Store
  -> OuroborosApplication 'InitiatorMode initiatorCtx responderCtx LBS.ByteString IO () Void
forwarderApp config ekgStore =
  OuroborosApplication
    [ MiniProtocol
        { miniProtocolNum    = MiniProtocolNum 2
        , miniProtocolLimits = MiniProtocolLimits { maximumIngressQueue = maxBound }
        , miniProtocolRun    = forwardEKGMetrics config ekgStore
        }
    ]

forwardEKGMetrics
  :: ForwarderConfiguration
  -> EKG.Store
  -> RunMiniProtocol 'InitiatorMode initiatorCtx responderCtx LBS.ByteString IO () Void
forwardEKGMetrics config ekgStore =
  InitiatorProtocolOnly $ MiniProtocolCb $ \_ctx channel ->
    runPeer
      (forwarderTracer config)
      (Forwarder.codecEKGForward CBOR.encode CBOR.decode
                                 CBOR.encode CBOR.decode)
      channel
      (Forwarder.ekgForwarderPeer $ mkResponse config ekgStore)

forwardEKGMetricsResp
  :: ForwarderConfiguration
  -> EKG.Store
  -> RunMiniProtocol 'ResponderMode initiatorCtx responderCtx LBS.ByteString IO Void ()
forwardEKGMetricsResp config ekgStore =
  ResponderProtocolOnly $ MiniProtocolCb $ \_ctx channel ->
    runPeer
      (forwarderTracer config)
      (Forwarder.codecEKGForward CBOR.encode CBOR.decode
                                 CBOR.encode CBOR.decode)
      channel
      (Forwarder.ekgForwarderPeer $ mkResponse config ekgStore)

forwardEKGMetricsDummy
  :: RunMiniProtocol 'InitiatorMode initiatorCtx responderCtx LBS.ByteString IO () Void
forwardEKGMetricsDummy =
  InitiatorProtocolOnly $ MiniProtocolCb $ \_ctx channel ->
    runPeer
      nullTracer
      (Forwarder.codecEKGForward CBOR.encode CBOR.decode
                                 CBOR.encode CBOR.decode)
      channel
      (Forwarder.ekgForwarderPeer mkResponseDummy)

forwardEKGMetricsRespDummy
  :: RunMiniProtocol 'ResponderMode initiatorCtx responderCtx LBS.ByteString IO Void ()
forwardEKGMetricsRespDummy =
  ResponderProtocolOnly $ MiniProtocolCb $ \_ctx channel ->
    runPeer
      nullTracer
      (Forwarder.codecEKGForward CBOR.encode CBOR.decode
                                 CBOR.encode CBOR.decode)
      channel
      (Forwarder.ekgForwarderPeer mkResponseDummy)
