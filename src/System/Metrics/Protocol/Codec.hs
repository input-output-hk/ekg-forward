{-# OPTIONS_GHC -Winaccessible-code #-}
{-# OPTIONS_GHC -Werror #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module System.Metrics.Protocol.Codec (
  codecEKGForward
  ) where

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import           Codec.CBOR.Read (DeserialiseFailure)
import           Control.Monad.Class.MonadST (MonadST)
import qualified Data.ByteString.Lazy as LBS
import           Text.Printf (printf)
import           Network.TypedProtocol.Codec.CBOR (Codec, SomeMessage (..), mkCodecCborLazyBS)
import qualified Network.TypedProtocol.Core as Core

import           System.Metrics.Protocol.Type

codecEKGForward
  :: forall req resp m.
     (MonadST m)
  => (req -> CBOR.Encoding)
  -> (forall s . CBOR.Decoder s req)
  -> (resp -> CBOR.Encoding)
  -> (forall s . CBOR.Decoder s resp)
  -> Codec (EKGForward req resp)
           DeserialiseFailure m LBS.ByteString
codecEKGForward encodeReq  decodeReq
                encodeResp decodeResp =
  mkCodecCborLazyBS encode decode
 where

  -- Encode messages.
  encode
    :: forall (st :: EKGForward req resp) (st' :: EKGForward req resp). ()
    => Core.StateTokenI st
    => Core.ActiveState st
    => Message (EKGForward req resp) st st'
    -> CBOR.Encoding
  encode = mconcat . go (Core.stateToken @st) where

    go :: SEKGForward st -> Message (EKGForward req resp) st st' -> [CBOR.Encoding]
    go SStIdle (MsgReq req) =
      [ CBOR.encodeListLen 2
      , CBOR.encodeWord 0
      , encodeReq req
      ]
    go SStIdle MsgDone =
      [ CBOR.encodeListLen 1
      , CBOR.encodeWord 1
      ]
    go SStBusy (MsgResp resp) =
      [ CBOR.encodeListLen 2
      , CBOR.encodeWord 1
      , encodeResp resp
      ]
    go stateToken@SStDone _ =
      -- absurd: `st' cannot be `Done' while in an `ActiveState st'.
      Core.notActiveState @_ @st stateToken

  -- Decode messages
  decode
    :: forall (st :: EKGForward req resp) s. ()
    => Core.ActiveState st
    => Core.StateToken st
    -> CBOR.Decoder s (SomeMessage st)
  decode stok = do
    len <- CBOR.decodeListLen
    key <- CBOR.decodeWord
    case (key, len, stok) of
      (0, 2, SStIdle) ->
        SomeMessage . MsgReq <$> decodeReq

      (1, 1, SStIdle) ->
        return $ SomeMessage MsgDone

      (1, 2, SStBusy) ->
        SomeMessage . MsgResp <$> decodeResp

      -- Failures per protocol state
      (_, _, SStIdle) ->
        fail (printf "codecEKGForward (%s) unexpected key (%d, %d)" (show stok) key len)
      (_, _, SStBusy) ->
        fail (printf "codecEKGForward (%s) unexpected key (%d, %d)" (show stok) key len)
      _ -> error "missing case"
    -- TODO: Missing cases, or redunant:
    --   (0, p, SStDone) -> (..)
    --   (1, 1, SStDone) -> (..)
    --   (1, 2, SStDone) -> (..)
