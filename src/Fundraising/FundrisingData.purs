module Fundraising.FundrisingScriptInfo where

import Contract.Prelude
import Ctl.Internal.Types.Scripts (Validator, ValidatorHash)

import Contract.Address (Address, validatorHashBaseAddress)
import Contract.Config (NetworkId(TestnetId))
import Contract.Monad (Contract, liftContractM)
import Contract.Transaction (TransactionInput, TransactionOutputWithRefScript)
import Contract.Utxos (utxosAt)
import Contract.Value as Value
import Data.Map (Map)
import Fundraising.Datum (PFundraisingDatum)
import Fundraising.FundraisingScript (fundraisingValidatorScript, getFundraisingValidatorHash)
import Fundraising.Models (Fundraising(..))
import Fundraising.UserData (FundraisingData(..))
import MintingPolicy.VerTokenMinting as VerToken
import Shared.Helpers (extractDatumFromUTxO, extractValueFromUTxO, getUtxoByNFT, mkCurrencySymbol)

makeFundrising :: FundraisingData -> Contract () Fundraising
makeFundrising (FundraisingData fundraisingData) = do
  let protocol = fundraisingData.protocol
  _ /\ verTokenCurrency <- mkCurrencySymbol (VerToken.mintingPolicy protocol)
  verTokenName <- VerToken.verTokenName
  pure $ Fundraising
    { protocol: protocol
    , verTokenCurrency: verTokenCurrency
    , verTokenName: verTokenName
    }

newtype FundrisingScriptInfo = FundrisingScriptInfo
  { frValidator :: Validator
  , frValidatorHash :: ValidatorHash
  , frAddress :: Address
  , frUtxos :: Map TransactionInput TransactionOutputWithRefScript
  , frUtxo :: (Tuple TransactionInput TransactionOutputWithRefScript)
  , frDatum :: PFundraisingDatum
  , frValue :: Value.Value
  }

getFundrisingScriptInfo :: Fundraising -> Value.CurrencySymbol -> Value.TokenName -> Contract () FundrisingScriptInfo
getFundrisingScriptInfo fr threadTokenCurrency threadTokenName = do
  frValidator <- fundraisingValidatorScript fr
  frValidatorHash <- getFundraisingValidatorHash fr
  frAddress <- liftContractM "Impossible to get Fundrising script address" $ validatorHashBaseAddress TestnetId frValidatorHash
  frUtxos <- utxosAt frAddress
  frUtxo <- getUtxoByNFT "Fundraising" (threadTokenCurrency /\ threadTokenName) frUtxos
  frDatum <- liftContractM "Impossible to get Fundraising Datum" $ extractDatumFromUTxO frUtxo
  let frFunds = extractValueFromUTxO frUtxo
  pure $ FundrisingScriptInfo
    { frValidator: frValidator
    , frValidatorHash: frValidatorHash
    , frAddress: frAddress
    , frUtxos: frUtxos
    , frUtxo: frUtxo
    , frDatum: frDatum
    , frValue: frFunds
    }