module Info.AllProposals where

import Contract.Prelude

import Contract.Address (getNetworkId, validatorHashBaseAddress)
import Contract.Monad (Contract, liftContractM)
import Contract.Utxos (utxosAt)
import Ctl.Internal.Types.Interval (POSIXTime)
import Data.Map as Map
import Ext.Contract.Value (mkCurrencySymbol)
import Ext.Data.Boolean (bigIntToBoolean)
import MintingPolicy.VerTokenMinting as VerToken
import Proposal.Datum (PProposalDatum(..))
import Proposal.Model (mkProposal)
import Proposal.ProposalScript (getProposalValidatorHash, proposalVerTokenName)
import Protocol.Models (Protocol)
import Shared.Utxo (UtxoTuple, extractDatumFromUTxO, filterByToken)

getAllProposalUtxos :: Protocol -> Contract (Array UtxoTuple)
getAllProposalUtxos protocol = do
    networkId <- getNetworkId
    _ /\ proposalVerTokenCs <- mkCurrencySymbol (VerToken.mintingPolicy protocol)
    verTn <- proposalVerTokenName
    let proposal = mkProposal protocol proposalVerTokenCs
    proposalValidatorHash <- getProposalValidatorHash proposal
    proposalAddress <- liftContractM "Impossible to get Proposal script address" $ validatorHashBaseAddress networkId proposalValidatorHash

    allUtxos <- utxosAt proposalAddress
    let proposalUtxos = filterByToken (proposalVerTokenCs /\ verTn) $ Map.toUnfoldable allUtxos
    pure proposalUtxos

isFinished :: POSIXTime ->  UtxoTuple -> Boolean
isFinished now proposalUtxo  =
    let mbDatum = extractDatumFromUTxO proposalUtxo in 
    case mbDatum of
        (Just (PProposalDatum currentDatum)) -> 
            let votingIsFinished = currentDatum.deadline < now
                proposalIsNotProcessed = not $ bigIntToBoolean currentDatum.processed
            in  votingIsFinished && proposalIsNotProcessed
        Nothing -> false

hasReachedQuorum :: UtxoTuple -> Boolean
hasReachedQuorum proposalUtxo = 
    let mbDatum = extractDatumFromUTxO proposalUtxo
    in 
    case mbDatum of
     (Just (PProposalDatum currentDatum)) -> currentDatum.quorum <= currentDatum.for + currentDatum.against
     Nothing -> false

votedToApply :: UtxoTuple -> Boolean
votedToApply proposalUtxo = 
    let mbDatum = extractDatumFromUTxO proposalUtxo
    in 
    case mbDatum of
     (Just (PProposalDatum currentDatum)) -> currentDatum.against < currentDatum.for 
     Nothing -> false
