module InitialConfig.StartProtocol where

import Contract.Prelude

import Contract.Address (getWalletAddresses, ownPaymentPubKeyHash)
import Contract.Config (ConfigParams, testnetNamiConfig)
import Contract.Log (logInfo')
import Contract.Monad (Contract, launchAff_, runContract, liftContractM, liftContractE, liftedM)
import Contract.PlutusData
  ( PlutusData
  , Redeemer(Redeemer)
  , genericToData
  , class ToData
  , class HasPlutusSchema
  , type (:+)
  , type (:=)
  , type (@@)
  , PNil
  , toData
  , S
  , Z
  )
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy(PlutusMintingPolicy), PlutusScript, ApplyArgsError, applyArgs)
import Contract.TextEnvelope
  ( decodeTextEnvelope
  , plutusScriptV1FromEnvelope
  )
import Contract.Transaction
  ( TransactionInput
  , awaitTxConfirmed
  , submitTxFromConstraints
  , TransactionOutputWithRefScript
  )
import Ctl.Internal.Plutus.Types.Transaction (_amount, _output)
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value as Value
import Control.Monad.Error.Class (liftMaybe)
import Data.Array (head, singleton, filter) as Array
import Data.BigInt (fromInt)
import Data.Lens.Getter ((^.))
import Data.Map (toUnfoldable) as Map
import Data.Rational ((%))
import Effect.Exception (error)
import Protocol.UserData (ProtocolConfigParams(..))
import MintingPolicy.NftRedeemer
import MintingPolicy.NftMinting as NFT
import Shared.Helpers as Helpers
import Protocol.Models (PProtocol(..))
import Protocol.Datum

protocolTokenName :: forall (r :: Row Type). Contract r Value.TokenName
protocolTokenName = Helpers.mkTokenName "DonatPoolProtocol"

contract :: ProtocolConfigParams -> Contract () Unit
contract (ProtocolConfigParams { minAmountParam, maxAmountParam, minDurationParam, maxDurationParam, protocolFeeParam }) = do
  logInfo' "Running startDonatPool protocol contract"
  ownPkh <- ownPaymentPubKeyHash >>= liftContractM "Impossible to get own PaymentPubkeyHash"
  logInfo' $ "Own Payment pkh is: " <> show ownPkh
  ownAddress <- liftedM "Failed to get own address" $ Array.head <$> getWalletAddresses
  logInfo' $ "Own address is: " <> show ownAddress
  utxos <- utxosAt ownAddress
  logInfo' $ "UTxOs found on address: " <> show utxos
  oref <-
    liftContractM "Utxo set is empty"
      (fst <$> Array.head (Helpers.filterNonCollateral $ Map.toUnfoldable utxos))
  mp /\ cs <- Helpers.mkCurrencySymbol (NFT.mintingPolicy oref)
  tn <- protocolTokenName
  let
    protocol = PProtocol
      { managerPkh: ownPkh
      , protocolCurrency: cs
      , protocolTokenName: tn
      }
  logInfo' $ "Current protocol: " <> show protocol

  let
    protocolSizeLimits = PPoolSizeLimits
      { minAmount: fromInt minAmountParam
      , maxAmount: fromInt maxAmountParam
      }
  let
    protocolDurationLimits = PDurationLimits
      { minDuration: fromInt minDurationParam
      , maxDuration: fromInt maxDurationParam
      }
  fee <- liftContractM "Zero denominator error" $ Helpers.mkRational protocolFeeParam
  let
    protocolConfig = PProtocolConfig
      { protocolFee: fee
      , poolSizeLimits: protocolSizeLimits
      , durationLimits: protocolDurationLimits
      }
  -- let protocolConstants = PProtocolConstants
  --         { managerPkh: ownPkh
  --         , tokenOriginRef :: TxOutRefCache
  --         , protocolCurrency: : CurrencySymbol
  --         , protocolTokenName :: TokenName
  --         }

  logInfo' "Finished"

