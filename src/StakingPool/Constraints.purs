module StakingPool.Constraints where

import Contract.Prelude

import Contract.Credential (Credential(ScriptCredential))
import Contract.Monad (Contract)
import Contract.PlutusData (Datum(Datum), Redeemer(Redeemer), toData)
import Contract.TxConstraints as Constraints
import Contract.Value as Value
import Data.BigInt (fromInt)
import MintingPolicy.NftRedeemer (PNftRedeemer(..))
import Protocol.Models (Protocol(..))
import Shared.MinAda (minAdaValue)
import StakingPool.Datum (PStakingPoolDatum(..))
import StakingPool.Models (mkStakingPoolFromProtocol)
import StakingPool.StakingPoolScript (getStakingPoolTokenName, getStakingPoolValidatorHash)

mkStartSystemConstraints :: Protocol -> Contract (Constraints.TxConstraints Void Void)
mkStartSystemConstraints protocol'@(Protocol protocol) = do
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
        (Redeemer $ toData $ PMintNft stakingPoolTn)
        stakingPoolTokenValue
        <> Constraints.mustPayToScriptAddress
          stakingPoolHash
          (ScriptCredential stakingPoolHash)
          (Datum $ toData initDatum)
          Constraints.DatumInline
          payment

  pure constraints
