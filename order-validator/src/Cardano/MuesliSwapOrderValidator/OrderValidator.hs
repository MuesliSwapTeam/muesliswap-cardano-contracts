{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Cardano.MuesliSwapOrderValidator.OrderValidator
  ( orderSerialised
  , orderSBS
  ) where

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

import           Cardano.Api             hiding ( TxOut
                                                , Value
                                                )
import           Cardano.Api.Shelley     hiding ( TxOut
                                                , Value
                                                )
import           Codec.Serialise         hiding ( encode )
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.ByteString.Short         as SBS
import           GHC.Generics                   ( Generic )
import           Ledger                  hiding ( singleton )
import qualified Ledger.Scripts                as Scripts
import qualified Ledger.Typed.Scripts          as Scripts
                                         hiding ( validatorHash )
import           Ledger.Value                  as Value
import           Plutus.V1.Ledger.Ada           ( lovelaceValueOf )
import qualified Plutus.V1.Ledger.Api          as Plutus
import qualified PlutusTx
import           PlutusTx.Prelude        hiding ( Applicative(..)
                                                , unless
                                                )


data Order = Order
  { oCreator     :: !PubKeyHash
  , oBuyCurrency :: !CurrencySymbol
  , oBuyToken    :: !TokenName
  , oBuyAmount   :: !Integer
  }
  deriving (Generic, ToJSON, FromJSON)

instance Eq Order where
  {-# INLINABLE (==) #-}
  a == b =
    (oCreator a == oCreator b)
      && (oBuyCurrency a == oBuyCurrency b)
      && (oBuyToken a == oBuyToken b)
      && (oBuyAmount a == oBuyAmount b)

PlutusTx.unstableMakeIsData ''Order
PlutusTx.makeLift ''Order

data OrderAction = CancelOrder | FullMatch

PlutusTx.unstableMakeIsData ''OrderAction
PlutusTx.makeLift ''OrderAction

data OrderDatum = OrderDatum
  { odOrder :: !Order
  }

PlutusTx.unstableMakeIsData ''OrderDatum
PlutusTx.makeLift ''OrderDatum

data OrderScript
instance Scripts.ValidatorTypes OrderScript where
  type RedeemerType OrderScript = OrderAction
  type DatumType OrderScript = OrderDatum

{-# INLINABLE mkOrderValidator #-}
mkOrderValidator :: OrderDatum -> OrderAction -> ScriptContext -> Bool
mkOrderValidator od redeemer ctx = case redeemer of
  CancelOrder ->
    traceIfFalse "signature does not match creator in datum" checkSig
  FullMatch ->
    traceIfFalse "expected creator to get all of what she ordered" correctFull
      && traceIfFalse "only matches of pairs of orders allowed" twoParties

 where
  info :: TxInfo
  info = scriptContextTxInfo ctx

  order :: Order
  order = odOrder od

  orderedValue :: Order -> Value
  orderedValue o =
    Value.singleton (oBuyCurrency o) (oBuyToken o) (oBuyAmount o)

  ownInValue :: Value
  ownInValue = case (findOwnInput ctx) of
    Just inInfo -> (txOutValue . txInInfoResolved) inInfo

  resolvePubKeyHash :: Maybe PubKeyHash -> PubKeyHash
  resolvePubKeyHash pkh = case pkh of
    Just h -> h
    _      -> traceError "invalid public key hash"

  getsValue :: PubKeyHash -> Value
  getsValue h = sum
    [ txOutValue o'
    | o' <- txInfoOutputs info
    , resolvePubKeyHash (toPubKeyHash (txOutAddress o')) == h
    ]

  scriptAddress :: Ledger.Address
  scriptAddress = case findOwnInput ctx of
    Just i -> txOutAddress $ txInInfoResolved i

  isOwnScriptInput :: TxInInfo -> Bool
  isOwnScriptInput i =
    let input = txInInfoResolved i
    in  case txOutDatumHash input of
          Nothing -> False
          Just _  -> (txOutAddress input) == scriptAddress

  inputValueFromScript :: Value
  inputValueFromScript = sum
    [ txOutValue $ txInInfoResolved i
    | i <- txInfoInputs info
    , isOwnScriptInput i
    ]

  getsAtLeastValue :: PubKeyHash -> Value -> Bool
  getsAtLeastValue h v = (getsValue h) `geq` v

  correctFull :: Bool
  correctFull =
    getsAtLeastValue (oCreator order)
                     ((orderedValue order) <> (lovelaceValueOf 1500000))
      &&    inputValueFromScript
      `geq` ((orderedValue order) <> ownInValue <> (lovelaceValueOf 1500000))

  checkSig :: Bool
  checkSig = txSignedBy info (oCreator order)

  twoParties :: Bool
  twoParties =
    let xs = [ i | i <- txInfoInputs info, isOwnScriptInput i ]
    in  case xs of
          [_, _] -> True
          _      -> False


-- compilation of the validator
orderTypedValidator :: Scripts.TypedValidator OrderScript
orderTypedValidator = Scripts.mkTypedValidator @OrderScript
    $$(PlutusTx.compile [|| mkOrderValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator

orderValidator :: Validator
orderValidator = Scripts.validatorScript orderTypedValidator

orderAddress :: Ledger.ValidatorHash
orderAddress = Scripts.validatorHash orderValidator


-- serialization
orderScript :: Plutus.Script
orderScript = Plutus.unValidatorScript orderValidator

orderSBS :: SBS.ShortByteString
orderSBS = SBS.toShort . LBS.toStrict $ serialise orderScript

orderSerialised :: PlutusScript PlutusScriptV1
orderSerialised = PlutusScriptSerialised orderSBS
