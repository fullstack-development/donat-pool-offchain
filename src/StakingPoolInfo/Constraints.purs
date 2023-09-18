module StakingPoolInfo.Constraints where

import Contract.Prelude

import Contract.AssocMap as Map
import Contract.Credential (Credential(ScriptCredential))
import Contract.Monad (Contract)
import Contract.TxConstraints as Constraints
import Contract.Scripts (MintingPolicyHash, mintingPolicyHash)
import Contract.Value as Value
import Ext.Contract.Time (Epoch)
import Ext.Contract.Value (mkCurrencySymbol)
import MintingPolicy.VerTokenMinting as VerToken
import MintingPolicy.VerTokenRedeemers (PVerTokenRedeemer(PMintWithStakingPool))
import Protocol.Models (Protocol)
import Protocol.ProtocolScriptInfo (ProtocolScriptInfo(..))
import Shared.MinAda (minAdaValue)
import Shared.Tx (toDatum, toRedeemer)
import StakingPool.Models (mkStakingPoolFromProtocol)
import StakingPoolInfo.Datum (PStakingPoolInfoDatum(..))
import StakingPoolInfo.StakingPoolInfoScript (getStakingPoolInfoValidatorHash)

mkUpdateEpochConstraints :: Protocol -> Epoch -> ProtocolScriptInfo -> Contract (Constraints.TxConstraints Void Void)
mkUpdateEpochConstraints protocol currentEpoch (ProtocolScriptInfo protocolInfo) = do
  spiVerTokenName <- VerToken.stakingPoolInfoVerTokenName
  stakingPool <- mkStakingPoolFromProtocol protocol
  spiHash <- getStakingPoolInfoValidatorHash stakingPool

  verTokenMp /\ verTokenCs <- mkCurrencySymbol (VerToken.mintingPolicy protocol)
  let
    initDatum = toDatum $ PStakingPoolInfoDatum { epoch: currentEpoch, history: Map.empty }
    verTokenValue = Value.singleton verTokenCs spiVerTokenName one
    paymentToSpi = minAdaValue <> verTokenValue

    verTokenPolicyHash :: MintingPolicyHash
    verTokenPolicyHash = mintingPolicyHash verTokenMp

    constraints =
      Constraints.mustMintCurrencyWithRedeemerUsingScriptRef
        verTokenPolicyHash
        (toRedeemer $ PMintWithStakingPool spiVerTokenName)
        spiVerTokenName
        one
        protocolInfo.references.verTokenInput
        <> Constraints.mustPayToScriptAddress
          spiHash
          (ScriptCredential spiHash)
          initDatum
          Constraints.DatumInline
          paymentToSpi

  pure constraints
