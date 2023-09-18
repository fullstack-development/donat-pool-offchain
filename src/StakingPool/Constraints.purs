module StakingPool.Constraints where

import Contract.Prelude

import Contract.Credential (Credential(ScriptCredential))
import Contract.Monad (Contract)
import Contract.ScriptLookups as Lookups
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Data.BigInt (fromInt)
import Ext.Contract.Time (Epoch)
import MintingPolicy.NftRedeemer (PNftRedeemer(..))
import Protocol.Models (Protocol(..))
import Shared.MinAda (minAdaValue)
import Shared.ScriptInfo (ScriptInfo(..))
import Shared.Tx (toDatum, toRedeemer)
import StakingPool.Datum (PStakingPoolDatum(..))
import StakingPool.Models (mkStakingPoolFromProtocol)
import StakingPool.Redeemer (PStakingPoolRedeemer(..))
import StakingPool.StakingPoolScript (getStakingPoolTokenName, getStakingPoolValidatorHash)

mkStartConstraints :: Protocol -> Contract (Constraints.TxConstraints Void Void)
mkStartConstraints protocol'@(Protocol protocol) = do
  stakingPoolTn <- getStakingPoolTokenName
  stakingPool <- mkStakingPoolFromProtocol protocol'
  stakingPoolHash <- getStakingPoolValidatorHash stakingPool

  let
    initDatum = PStakingPoolDatum { currentEpoch: fromInt 0 }
    stakingPoolTokenValue = Value.singleton protocol.protocolCurrency stakingPoolTn one
    payment = minAdaValue <> stakingPoolTokenValue

    constraints :: Constraints.TxConstraints Void Void
    constraints =
      Constraints.mustMintValueWithRedeemer
        (toRedeemer $ PMintNft stakingPoolTn)
        stakingPoolTokenValue
        <> Constraints.mustPayToScriptAddress
          stakingPoolHash
          (ScriptCredential stakingPoolHash)
          (toDatum initDatum)
          Constraints.DatumInline
          payment

  pure constraints

mkUpdateEpochConstraints :: ScriptInfo PStakingPoolDatum -> Epoch -> Constraints.TxConstraints Void Void /\ Lookups.ScriptLookups Void
mkUpdateEpochConstraints (ScriptInfo stakingPoolScriptInfo) newEpoch =
  let
    spDatum = toDatum $ PStakingPoolDatum { currentEpoch: newEpoch }
    spRedeemer = toRedeemer POpenNewEpoch
    constraints =
      Constraints.mustSpendScriptOutputUsingScriptRef
        (fst stakingPoolScriptInfo.utxo)
        spRedeemer
        stakingPoolScriptInfo.refScriptInput
        <> Constraints.mustPayToScriptAddress
          stakingPoolScriptInfo.validatorHash
          (ScriptCredential stakingPoolScriptInfo.validatorHash)
          spDatum
          Constraints.DatumInline
          stakingPoolScriptInfo.value
        <> Constraints.mustReferenceOutput (fst stakingPoolScriptInfo.refScriptUtxo)
    lookups = Lookups.unspentOutputs stakingPoolScriptInfo.utxos
  in
    (constraints /\ lookups)
