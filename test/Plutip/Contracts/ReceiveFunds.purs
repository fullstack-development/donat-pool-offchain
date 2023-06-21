module Test.Plutip.Contracts.ReceiveFunds where

import Prelude
import Test.Plutip.Contracts.CreateFundraising (mkFundraisingDuration, mkFundraisingParams, createTestFundraising)
import Contract.Test.Plutip (withWallets)
import Contract.Wallet (withKeyWallet)
import Control.Monad.Error.Class (try)
import Ctl.Internal.Test.ContractTest (ContractTest)
import Ctl.Internal.Test.TestPlanM (TestPlanM)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Milliseconds(Milliseconds), delay)
import Effect.Aff.Class (liftAff)
import Fundraising.Donate as Donate
import Fundraising.ReceiveFunds as ReceiveFunds
import Mote (group, test)
import Protocol.StartProtocol as StartProtocol
import Test.Plutip.Contracts.StartProtocol (startProtocolParams)
import Test.Plutip.Utils (isExpectedError)
import Test.Spec.Assertions (shouldSatisfy)
import Test.Plutip.Fixtures (distribution, incorrectFundraisingData, minDurationStartProtocolParams)

suite :: TestPlanM ContractTest Unit
suite = do
  group "Receive funds" do

    test "Should successfully receive funds when fundraising goal is reached" do
      withWallets distribution \(alice /\ bob) -> do
        protocolData <- withKeyWallet alice $ StartProtocol.contract startProtocolParams
        frData <- createTestFundraising bob protocolData (mkFundraisingParams 80 (mkFundraisingDuration 0 0 10))
        withKeyWallet alice $ void $ Donate.contract protocolData frData 80
        withKeyWallet bob $ void $ ReceiveFunds.contract protocolData frData

    test "Should successfully receive funds when time is over" do
      withWallets distribution \(alice /\ bob) -> do
        protocolData <- withKeyWallet alice $ StartProtocol.contract minDurationStartProtocolParams
        frData <- createTestFundraising bob protocolData (mkFundraisingParams 80 (mkFundraisingDuration 0 0 1))
        withKeyWallet alice $ void $ Donate.contract protocolData frData 50
        liftAff $ delay $ Milliseconds 60000.0 -- 1 min
        withKeyWallet bob $ void $ ReceiveFunds.contract protocolData frData

    test "Should fail if fundraising does not exist" do
      withWallets distribution \(alice /\ bob) -> do
        protocolData <- withKeyWallet alice $ StartProtocol.contract startProtocolParams
        frData <- incorrectFundraisingData
        result <- try $ withKeyWallet bob $ void $ ReceiveFunds.contract protocolData frData
        let errMsg = "Fundraising UTxO with given nft not found"
        result `shouldSatisfy` (isExpectedError errMsg)

    test "Should fail if called from foreign wallet" do
      withWallets distribution \(alice /\ bob) -> do
        protocolData <- withKeyWallet alice $ StartProtocol.contract startProtocolParams
        frData <- createTestFundraising bob protocolData (mkFundraisingParams 80 (mkFundraisingDuration 0 0 10))
        withKeyWallet alice $ void $ Donate.contract protocolData frData 80
        result <- try $ withKeyWallet alice $ void $ ReceiveFunds.contract protocolData frData
        let errMsg = "Only fundraising creator can receive funds"
        result `shouldSatisfy` (isExpectedError errMsg)

    test "Should fail if neither fundraising goal reached nor fundraising time is over" do
      withWallets distribution \(alice /\ bob) -> do
        protocolData <- withKeyWallet alice $ StartProtocol.contract startProtocolParams
        frData <- createTestFundraising bob protocolData (mkFundraisingParams 80 (mkFundraisingDuration 0 0 10))
        withKeyWallet alice $ void $ Donate.contract protocolData frData 20
        result <- try $ withKeyWallet bob $ void $ ReceiveFunds.contract protocolData frData
        let errMsg = "Can't receive funds while fundraising is in process"
        result `shouldSatisfy` (isExpectedError errMsg)
