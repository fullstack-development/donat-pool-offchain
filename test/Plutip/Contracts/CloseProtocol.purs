module Test.Plutip.Contracts.CloseProtocol where

import Prelude

import Contract.Test.Plutip (withStakeKey, withWallets)
import Contract.Wallet (withKeyWallet)
import Control.Monad.Error.Class (try)
import Ctl.Internal.Plutip.Server (PlutipTest)
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Data.Tuple.Nested ((/\))
import Data.BigInt as BigInt
import Mote (group, test)
import Protocol.StartProtocol as StartProtocol
import Protocol.CloseProtocol as CloseProtocol
import Test.Plutip.Common (privateStakeKey)
import Test.Plutip.Contracts.StartProtocol (startProtocolParams)
import Test.Plutip.Contracts.UpdateProtocol (incorrectProtocol)
import Test.Plutip.Utils (isExpectedError)
import Test.Spec.Assertions (shouldSatisfy)

suite :: TestPlanM PlutipTest Unit
suite = do
  group "Close Protocol" do

    test "Should successfully close protocol" do
      let
        aliceUtxos = [ BigInt.fromInt 1_000_000_000, BigInt.fromInt 2_000_000_000 ]
        distribution = withStakeKey privateStakeKey aliceUtxos
      withWallets distribution \alice -> do
        withKeyWallet alice $ do
          protocol <- StartProtocol.contract startProtocolParams
          void $ CloseProtocol.contract protocol

    test "Should fail if user doesn't have permissions to close Protocol" do
      let
        aliceUtxos =
          [ BigInt.fromInt 1_000_000_000
          , BigInt.fromInt 2_000_000_000
          ]
        bobUtxos =
          [ BigInt.fromInt 1_000_000_000
          , BigInt.fromInt 2_000_000_000
          ]
        distribution =
          withStakeKey privateStakeKey aliceUtxos
            /\ withStakeKey privateStakeKey bobUtxos
      withWallets distribution \(alice /\ bob) -> do
        protocol <- withKeyWallet alice $ StartProtocol.contract startProtocolParams
        result <- try $ withKeyWallet bob $ CloseProtocol.contract protocol
        let errMsg = "current user doesn't have permissions to close protocol"
        result `shouldSatisfy` (isExpectedError errMsg)

    test "Should fail if Protocol doesn't exist" do
      let
        aliceUtxos = [ BigInt.fromInt 1_000_000_000, BigInt.fromInt 2_000_000_000 ]
        distribution = withStakeKey privateStakeKey aliceUtxos
      withWallets distribution \alice -> do
        protocol <- incorrectProtocol
        result <- try $ withKeyWallet alice $ CloseProtocol.contract protocol
        let errMsg = "Protocol UTxO with given nft not found"
        result `shouldSatisfy` (isExpectedError errMsg)
