module Proposal.ProposalScriptInfo where

import Contract.Prelude

import Contract.Address (getNetworkId, validatorHashBaseAddress)
import Contract.Monad (Contract, liftContractM)
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
import Proposal.Datum (PProposalDatum)
import Proposal.Model (PProposal)
import Proposal.ProposalScript (getProposalValidatorHash, proposalTokenName, proposalValidatorScript)
import Shared.Utxo (extractDatumFromUTxO, extractValueFromUTxO, getUtxoByNFT, getUtxoByScriptRef)

data ProposalScriptInfo = ProposalScriptInfo
  { prTokenName :: TokenName
  , prValidator :: Validator
  , prValidatorHash :: ValidatorHash
  , prAddress :: Address
  , prUtxos :: Map TransactionInput TransactionOutputWithRefScript
  , prUtxo :: Tuple TransactionInput TransactionOutputWithRefScript
  , prDatum :: PProposalDatum
  , prValue :: Value.Value
  , prRefScriptUtxo :: Tuple TransactionInput TransactionOutputWithRefScript
  , prRefScriptInput :: InputWithScriptRef
  }

getProposalScriptInfo ∷ CurrencySymbol ->  PProposal → Contract ProposalScriptInfo
getProposalScriptInfo  proposalCs proposal = do
  networkId <- getNetworkId
  proposalTn <- proposalTokenName
  proposalValidator <- proposalValidatorScript proposal
  proposalValidatorHash <- getProposalValidatorHash proposal
  proposalAddress <-
    liftContractM "Impossible to get Proposal script address" $ validatorHashBaseAddress networkId proposalValidatorHash
  proposalUtxos <- utxosAt proposalAddress
  proposalUtxo <- getUtxoByNFT "Proposal" (proposalCs /\ proposalTn) proposalUtxos
  proposalDatum <- liftContractM "Impossible to get Proposal Datum" $ extractDatumFromUTxO proposalUtxo
  let proposalValue = extractValueFromUTxO proposalUtxo
  let proposalScriptRef = PlutusScriptRef (unwrap proposalValidator)
  proposalRefScriptUtxo <- getUtxoByScriptRef "Proposal" proposalScriptRef proposalUtxos
  let proposalRefScriptInput = Constraints.RefInput $ mkTxUnspentOut (fst proposalRefScriptUtxo) (snd proposalRefScriptUtxo)
  pure $ ProposalScriptInfo
    { prTokenName: proposalTn
    , prValidator: proposalValidator
    , prValidatorHash: proposalValidatorHash
    , prAddress: proposalAddress
    , prUtxos: proposalUtxos
    , prUtxo: proposalUtxo
    , prDatum: proposalDatum
    , prValue: proposalValue
    , prRefScriptUtxo: proposalRefScriptUtxo
    , prRefScriptInput: proposalRefScriptInput
    }