module Info.AllProposals where

import Contract.Prelude

import Contract.Address (getNetworkId, validatorHashBaseAddress, Address)
import Contract.Monad (Contract, liftContractM)
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value (flattenNonAdaAssets)
import Contract.Value as Value
import Ctl.Internal.Cardano.Types.ScriptRef (ScriptRef(..))
import Ctl.Internal.Plutus.Types.Transaction (_amount, _output)
import Ctl.Internal.Plutus.Types.TransactionUnspentOutput (mkTxUnspentOut)
import Ctl.Internal.Types.Interval (POSIXTime)
import Ctl.Internal.Types.Scripts (Validator(..), ValidatorHash)
import Ctl.Internal.Types.TxConstraints (InputWithScriptRef)
import Data.Array as Array
import Data.BigInt (fromInt)
import Data.Lens.Getter ((^.))
import Data.Map as Map
import Ext.Contract.Value (mkCurrencySymbol)
import Ext.Data.Boolean (bigIntToBoolean)
import MintingPolicy.VerTokenMinting as VerToken
import Proposal.Datum (PProposalDatum(..))
import Proposal.Model (mkProposal)
import Proposal.ProposalScript (getProposalValidatorHash, proposalTokenName, proposalValidatorScript, proposalVerTokenName)
import Protocol.Models (Protocol)
import Shared.ScriptInfo (ScriptInfo(..))
import Shared.Utxo (UtxoTuple, UtxosMap, extractDatumFromUTxO, extractValueFromUTxO, filterByToken, getUtxoByScriptRef)

newtype ProposalUtxo = ProposalUtxo
  { datum :: PProposalDatum
  , value :: Value.Value
  , threadCs :: Value.CurrencySymbol
  }

newtype BriefScriptInfo = BriefScriptInfo
  { threadTn :: Value.TokenName
  , verCs :: Value.CurrencySymbol
  , validatorHash :: ValidatorHash
  , validator :: Validator
  , address :: Address
  , utxos :: UtxosMap
  , refScriptUtxo :: UtxoTuple
  , refScriptInput :: InputWithScriptRef
  }

getThreadCsByTn :: UtxoTuple -> Value.TokenName -> Maybe Value.CurrencySymbol
getThreadCsByTn (_ /\ txOutWithRef) tn = do
  let value = (txOutWithRef ^. _output) ^. _amount
  let tokensArray = flattenNonAdaAssets value
  let tokensWithTn = Array.filter haveTn tokensArray
  getCs tokensWithTn
  where
  haveTn (_ /\ tokenName /\ amount) = tokenName == tn && amount == fromInt 1
  getCs arr = case Array.uncons arr of
    Just { head: (cs /\ _ /\ _), tail: [] } -> Just cs
    _ -> Nothing

utxoTupleToProposalUtxo :: Value.TokenName -> UtxoTuple -> Contract ProposalUtxo
utxoTupleToProposalUtxo proposalThreadTn utxo = do
  proposalThreadCs <- liftContractM "Impossible to get threadTokenCs from proposal UTXO" $ getThreadCsByTn utxo proposalThreadTn
  proposalDatum <- liftContractM "Impossible to get Protocol Datum" $ extractDatumFromUTxO utxo
  let proposalValue = extractValueFromUTxO utxo
  pure $ ProposalUtxo
    { datum: proposalDatum
    , value: proposalValue
    , threadCs: proposalThreadCs
    }

mkProposalScriptInfo
  :: BriefScriptInfo
  -> UtxoTuple
  -> Contract (ScriptInfo PProposalDatum)
mkProposalScriptInfo (BriefScriptInfo info) utxoTuple = do
  ProposalUtxo utxo <- utxoTupleToProposalUtxo info.threadTn utxoTuple
  pure $ ScriptInfo
    { tokenName: info.threadTn
    , validator: info.validator
    , validatorHash: info.validatorHash
    , address: info.address
    , utxos: info.utxos
    , utxo: utxoTuple
    , datum: utxo.datum
    , value: utxo.value
    , refScriptUtxo: info.refScriptUtxo
    , refScriptInput: info.refScriptInput
    }

getAllProposalUtxos :: Protocol -> Contract (Array (ScriptInfo PProposalDatum))
getAllProposalUtxos protocol = do
  networkId <- getNetworkId
  _ /\ proposalVerTokenCs <- mkCurrencySymbol (VerToken.mintingPolicy protocol)
  verTn <- proposalVerTokenName
  proposalThreadTn <- proposalTokenName

  let proposal = mkProposal protocol proposalVerTokenCs
  proposalValidatorHash <- getProposalValidatorHash proposal
  proposalValidator@(Validator validator) <- proposalValidatorScript proposal
  proposalAddress <- liftContractM "Impossible to get Proposal script address" $ validatorHashBaseAddress networkId proposalValidatorHash

  allUtxos <- utxosAt proposalAddress
  let scriptRef = PlutusScriptRef validator
  refScriptUtxo <- getUtxoByScriptRef "Proposal" scriptRef allUtxos
  let refScriptInput = Constraints.RefInput $ mkTxUnspentOut (fst refScriptUtxo) (snd refScriptUtxo)

  let
    briefScriptInfo = BriefScriptInfo
      { threadTn: proposalThreadTn
      , verCs: proposalVerTokenCs
      , validatorHash: proposalValidatorHash
      , validator: proposalValidator
      , address: proposalAddress
      , utxos: allUtxos
      , refScriptUtxo: refScriptUtxo
      , refScriptInput: refScriptInput
      }

  let proposalUtxos = filterByToken (proposalVerTokenCs /\ verTn) $ Map.toUnfoldable allUtxos
  proposalScriptInfoList <- traverse (mkProposalScriptInfo briefScriptInfo) proposalUtxos
  pure proposalScriptInfoList

isFinished :: POSIXTime -> ScriptInfo PProposalDatum -> Boolean
isFinished now (ScriptInfo info) =
  let
    PProposalDatum datum = info.datum
    votingIsFinished = datum.deadline < now
    proposalIsNotProcessed = not $ bigIntToBoolean datum.processed
  in
    votingIsFinished && proposalIsNotProcessed

hasReachedQuorum :: ScriptInfo PProposalDatum -> Boolean
hasReachedQuorum (ScriptInfo info) =
  let
    PProposalDatum datum = info.datum
  in
    datum.quorum <= datum.for + datum.against

votedToApply :: ScriptInfo PProposalDatum -> Boolean
votedToApply (ScriptInfo info) =
  let
    PProposalDatum datum = info.datum
  in
    datum.against < datum.for
