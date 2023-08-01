module Test.Plutip.Contracts.UpdateProtocol where

import Prelude

import Contract.Monad (Contract)
import Contract.Test.Plutip (InitialUTxOs, withWallets)
import Contract.Value as Value
import Contract.Wallet (withKeyWallet)
import Control.Monad.Error.Class (try)
import Ctl.Internal.Test.ContractTest (ContractTest)
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Data.BigInt as BigInt
import Data.Tuple.Nested (type (/\), (/\))
import Mote (group, test)
import Protocol.Models (Protocol(..))
import Protocol.StartProtocol as StartProtocol
import Protocol.UpdateProtocol as UpdateProtocol
import Protocol.UserData (ProtocolConfigParams(..), ProtocolData, protocolToData)
import Test.Plutip.Contracts.StartProtocol (startProtocolParams)
import Test.Plutip.Utils (isExpectedError)
import Test.Spec.Assertions (shouldSatisfy)
import Ext.Contract.Value (runMkTokenName)

suite :: TestPlanM ContractTest Unit
suite = do
  group "Update Protocol" do

    test "Should successfully update protocol" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 1_000_000_000
          , BigInt.fromInt 2_000_000_000
          ]
      withWallets distribution \alice -> do
        withKeyWallet alice $ do
          protocol <- StartProtocol.startSystem startProtocolParams
          void $ UpdateProtocol.contract protocol updateProtocolConfig

    test "Should fail if user doesn't have permissions to update Protocol" do
      let
        distribution :: InitialUTxOs /\ InitialUTxOs
        distribution =
          [ BigInt.fromInt 1_000_000_000
          , BigInt.fromInt 2_000_000_000
          ] /\
            [ BigInt.fromInt 30_000_000 ]
      withWallets distribution \(alice /\ bob) -> do
        protocol <- withKeyWallet alice $ StartProtocol.startSystem startProtocolParams
        result <- try $ withKeyWallet bob $ UpdateProtocol.contract protocol updateProtocolConfig
        let errMsg = "Current user doesn't have permissions to update protocol"
        result `shouldSatisfy` (isExpectedError errMsg)

    test "Should fail if Protocol doesn't exist" do
      let
        distribution :: InitialUTxOs
        distribution =
          [ BigInt.fromInt 1_000_000_000
          , BigInt.fromInt 2_000_000_000
          ]
      withWallets distribution \alice -> do
        protocol <- incorrectProtocol
        result <- try $ withKeyWallet alice $ UpdateProtocol.contract protocol updateProtocolConfig
        let errMsg = "Protocol UTxO with given nft not found"
        result `shouldSatisfy` (isExpectedError errMsg)

updateProtocolConfig :: ProtocolConfigParams
updateProtocolConfig =
  ProtocolConfigParams
    { minAmountParam: BigInt.fromInt 2_000_000
    , maxAmountParam: BigInt.fromInt 100_000_000
    , minDurationParam: BigInt.fromInt 5 -- minutes
    , maxDurationParam: BigInt.fromInt 250 -- minutes
    , protocolFeeParam: BigInt.fromInt 3 -- percentage
    }

incorrectProtocol :: Contract ProtocolData
incorrectProtocol = do
  tn <- runMkTokenName "Protocol"
  let protocol = Protocol { protocolCurrency: Value.adaSymbol, protocolTokenName: tn }
  protocolToData protocol
