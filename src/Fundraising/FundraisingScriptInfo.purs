module Fundraising.FundraisingScriptInfo where

import Contract.Prelude

import Contract.Address (Address, validatorHashBaseAddress)
import Contract.Config (NetworkId(TestnetId))
import Contract.Monad (Contract, liftContractM)
import Contract.Transaction (TransactionInput, TransactionOutputWithRefScript)
import Contract.Utxos (utxosAt)
import Contract.Value as Value
import Ctl.Internal.Types.Scripts (Validator, ValidatorHash)
import Data.Map (Map)
import Fundraising.Datum (PFundraisingDatum)
import Fundraising.FundraisingScript (fundraisingValidatorScript, getFundraisingValidatorHash)
import Fundraising.Models (Fundraising(..))
import MintingPolicy.VerTokenMinting as VerToken
import Protocol.UserData (ProtocolData, dataToProtocol)
import Shared.Utxo (extractDatumFromUTxO, extractValueFromUTxO, getUtxoByNFT)
import Ext.Contract.Value (mkCurrencySymbol)

makeFundraising :: ProtocolData -> Contract Fundraising
makeFundraising protocolData = do
  protocol <- dataToProtocol protocolData
  _ /\ verTokenCurrency <- mkCurrencySymbol (VerToken.mintingPolicy protocol)
  verTokenName <- VerToken.verTokenName
  pure $ Fundraising
    { protocol: protocol
    , verTokenCurrency: verTokenCurrency
    , verTokenName: verTokenName
    }

newtype FundraisingScriptInfo = FundraisingScriptInfo
  { frValidator :: Validator
  , frValidatorHash :: ValidatorHash
  , frAddress :: Address
  , frUtxos :: Map TransactionInput TransactionOutputWithRefScript
  , frUtxo :: (Tuple TransactionInput TransactionOutputWithRefScript)
  , frDatum :: PFundraisingDatum
  , frValue :: Value.Value
  }

getFundraisingScriptInfo :: Fundraising -> Value.CurrencySymbol -> Value.TokenName -> Contract FundraisingScriptInfo
getFundraisingScriptInfo fr threadTokenCurrency threadTokenName = do
  frValidator <- fundraisingValidatorScript fr
  frValidatorHash <- getFundraisingValidatorHash fr
  frAddress <- liftContractM "Impossible to get Fundraising script address" $ validatorHashBaseAddress TestnetId frValidatorHash
  frUtxos <- utxosAt frAddress
  frUtxo <- getUtxoByNFT "Fundraising" (threadTokenCurrency /\ threadTokenName) frUtxos
  frDatum <- liftContractM "Impossible to get Fundraising Datum" $ extractDatumFromUTxO frUtxo
  let frFunds = extractValueFromUTxO frUtxo
  pure $ FundraisingScriptInfo
    { frValidator: frValidator
    , frValidatorHash: frValidatorHash
    , frAddress: frAddress
    , frUtxos: frUtxos
    , frUtxo: frUtxo
    , frDatum: frDatum
    , frValue: frFunds
    }