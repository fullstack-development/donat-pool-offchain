module Shared.ScriptInfo where

import Contract.Prelude

import Contract.Address (getNetworkId, validatorHashBaseAddress)
import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData (class FromData)
import Contract.Transaction (ScriptRef(..), TransactionInput, TransactionOutputWithRefScript, mkTxUnspentOut)
import Contract.TxConstraints (InputWithScriptRef)
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value (CurrencySymbol)
import Contract.Value as Value
import Ctl.Internal.Plutus.Types.Address (Address)
import Ctl.Internal.Types.Scripts (Validator, ValidatorHash)
import Ctl.Internal.Types.TokenName (TokenName)
import Data.Map (Map)
import FeePool.Datum (PFeePoolDatum)
import FeePool.FeePoolScript (feePoolValidatorScript, getFeePoolTokenName, getFeePoolValidatorHash)
import FeePool.Models (mkFeePoolFromProtocol)
import Governance.Datum (GovernanceDatum)
import Governance.GovernanceScript (getGovernanceValidatorHash, governanceTokenName, governanceValidatorScript)
import Proposal.Datum (PProposalDatum)
import Proposal.Model (PProposal)
import Proposal.ProposalScript (getProposalValidatorHash, proposalTokenName, proposalValidatorScript)
import Protocol.Models (Protocol)
import Shared.Utxo (extractDatumFromUTxO, extractValueFromUTxO, getUtxoByNFT, getUtxoByScriptRef)
import StakingPool.Datum (PStakingPoolDatum)
import StakingPool.Models (mkStakingPoolFromProtocol)
import StakingPool.StakingPoolScript (getStakingPoolTokenName, getStakingPoolValidatorHash, stakingPoolValidatorScript)

newtype GetScriptData = GetScriptData
  { getThreadTokenName :: Contract TokenName
  , getValidator :: Contract Validator
  , getValidatoHash :: Contract ValidatorHash
  }

getGovernanceScriptInfo ∷ Protocol → Contract (ScriptInfo GovernanceDatum)
getGovernanceScriptInfo protocol = do
  let
    getScriptData = GetScriptData
      { getThreadTokenName: governanceTokenName
      , getValidator: governanceValidatorScript protocol
      , getValidatoHash: getGovernanceValidatorHash protocol
      }
  getScriptInfo getScriptData (unwrap protocol).protocolCurrency "Governance"

getProposalScriptInfo :: PProposal -> CurrencySymbol -> Contract (ScriptInfo PProposalDatum)
getProposalScriptInfo proposal threadCs = do
  let
    getScriptData = GetScriptData
      { getThreadTokenName: proposalTokenName
      , getValidator: proposalValidatorScript proposal
      , getValidatoHash: getProposalValidatorHash proposal
      }
  getScriptInfo getScriptData threadCs "Proposal"

getFeePoolScriptInfo :: Protocol -> Contract (ScriptInfo PFeePoolDatum)
getFeePoolScriptInfo protocol = do
  feePool <- mkFeePoolFromProtocol protocol
  let
    getScriptData = GetScriptData
      { getThreadTokenName: getFeePoolTokenName
      , getValidator: feePoolValidatorScript feePool
      , getValidatoHash: getFeePoolValidatorHash feePool
      }
  getScriptInfo getScriptData (unwrap protocol).protocolCurrency "FeePool"

getStakingPoolScriptInfo :: Protocol -> Contract (ScriptInfo PStakingPoolDatum)
getStakingPoolScriptInfo protocol = do
  stakingPool <- mkStakingPoolFromProtocol protocol
  let
    getScriptData = GetScriptData
      { getThreadTokenName: getStakingPoolTokenName
      , getValidator: stakingPoolValidatorScript stakingPool
      , getValidatoHash: getStakingPoolValidatorHash stakingPool
      }
  getScriptInfo getScriptData (unwrap protocol).protocolCurrency "StakingPool"

newtype ScriptInfo datum = ScriptInfo
  { tokenName :: TokenName
  , validator :: Validator
  , validatorHash :: ValidatorHash
  , address :: Address
  , utxos :: Map TransactionInput TransactionOutputWithRefScript
  , utxo :: Tuple TransactionInput TransactionOutputWithRefScript
  , datum :: datum
  , value :: Value.Value
  , refScriptUtxo :: Tuple TransactionInput TransactionOutputWithRefScript
  , refScriptInput :: InputWithScriptRef
  }

getScriptInfo
  :: forall (datum :: Type)
   . FromData datum
  => GetScriptData
  -> CurrencySymbol
  -> String
  -> Contract (ScriptInfo datum)
getScriptInfo (GetScriptData getScriptData) cs scriptName = do
  networkId <- getNetworkId
  tn <- getScriptData.getThreadTokenName
  validator <- getScriptData.getValidator
  validatorHash <- getScriptData.getValidatoHash
  address <-
    liftContractM ("Impossible to get " <> scriptName <> " script address") $ validatorHashBaseAddress networkId validatorHash
  utxos <- utxosAt address
  utxo <- getUtxoByNFT scriptName (cs /\ tn) utxos
  datum <- liftContractM ("Impossible to get " <> scriptName <> " Datum") $ extractDatumFromUTxO utxo
  let value = extractValueFromUTxO utxo
  let scriptRef = PlutusScriptRef (unwrap validator)
  refScriptUtxo <- getUtxoByScriptRef scriptName scriptRef utxos
  let refScriptInput = Constraints.RefInput $ mkTxUnspentOut (fst refScriptUtxo) (snd refScriptUtxo)
  pure $ ScriptInfo
    { tokenName: tn
    , validator: validator
    , validatorHash: validatorHash
    , address: address
    , utxos: utxos
    , utxo: utxo
    , datum: datum
    , value: value
    , refScriptUtxo: refScriptUtxo
    , refScriptInput: refScriptInput
    }