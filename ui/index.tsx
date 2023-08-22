import { useState } from 'react';
import ReactDOM from 'react-dom/client';

const a = await import('@offchain/Scaffold.Main');

// eslint-disable-next-line @typescript-eslint/no-non-null-assertion
const root = ReactDOM.createRoot(document.getElementById('root')!);

const App = () => {

  const protocolData = {
    protocolCurrency: "16e7eb26ee8e6d11da9d9a45ef27243f0529847926619ba18849bc8e",
    protocolTokenName: "DonatPoolProtocol"
  }

  const fundraisingDuration = {
    days: 0,
    hours: 0,
    minutes: 6
  };
  
  const createFundraisingParams = {
    title: 'Donate to feed stray cats',
    amount: 150,
    duration: fundraisingDuration
  };

  const [fundraisingData, setFundraisingData] = useState<{
    frThreadTokenCurrency: any;
    frThreadTokenName: any;
  }>();
 
  const testnetNami = { 
    wallet: "Nami",
    isMainnet: false
  };

  const proposalParams = { 
    minAmount: 50000000
    , maxAmount: 1000000000
    , minDuration: 5
    , maxDuration: 86400
    , protocolFee: 5
    }

  const onCreateFundraisingComplete = createdFundraisingResponse => {
    const frData = {
      frThreadTokenCurrency: createdFundraisingResponse.threadTokenCurrency,
      frThreadTokenName: createdFundraisingResponse.threadTokenName
    };
    
    setFundraisingData(frData);
  };


  const onCreateFundraisingClick = () => {
    a.main.value0.createFundraising(onCreateFundraisingComplete)(console.log)(
      protocolData
    )(testnetNami)(createFundraisingParams)();
  };

  const onDonate = () => {   
    a.main.value0.donate(console.log)(console.log)(protocolData)(testnetNami)(fundraisingData)(100)();
  };

  const onReceiveFunds = () => {
    a.main.value0.receiveFunds(console.log)(console.log)(protocolData)(testnetNami)(fundraisingData)();
  }
  
  const onGetAllFundraising = () => {
    a.main.value0.getAllFundraisings(console.log)(console.log)(protocolData)(testnetNami)();
  };

  const onGetUserRelatedFundraisings = () => {
    a.main.value0.getUserRelatedFundraisings(console.log)(console.log)(protocolData)(testnetNami)();
  };

  const onConnectWallet = () => {
    a.main.value0.connectWallet(console.log)(console.log)(testnetNami)();
  };

  const onGetAppInfo = () => {
    a.main.value0.getAppInfo(console.log)(console.log)(protocolData)(testnetNami)();
  };

  // uncomment if need to mint governance tokens
  // const onMintGovernanceClick = () => {
  //   a.main.value0.mintGovernanceTokens(console.log)(console.log)(testnetNami)();
  // };

  const [proposalCurrency, setProposalCurrency] = useState<{any;}>();

  const onCreateProposalComplete = createdProposalResponse => {
    setProposalCurrency(createdProposalResponse.threadCurrency);
  };

  const onCreateProposal = () => {
    a.main.value0.createProposal(onCreateProposalComplete)(console.log)(protocolData)(proposalParams)(testnetNami)();    
  };

  const onVoteForChangeProtocol = () => {
    const voteData = {
      proposalThreadCurrency: proposalCurrency, 
      amount: 2,
      for: true 
    }
  
    a.main.value0.vote(console.log)(console.log)(protocolData)(voteData)(testnetNami)();    
  };

  return (
    <div>
      <h1>Offchain integration</h1>
      <button onClick={onConnectWallet}>Connect wallet</button>
      <button onClick={onCreateFundraisingClick}>Create fundraising</button>
      <button onClick={onDonate}>Donate 100 Ada</button>
      <button onClick={onReceiveFunds}>Receive funds</button>
      <button onClick={onGetAllFundraising}>Get All Fundraisings</button>
      <button onClick={onGetUserRelatedFundraisings}>Get User related Fundraisings</button>
      <button onClick={onGetAppInfo}>Get app info</button>
      {/* <button onClick={onMintGovernanceClick}>Mint 50000 governance tokens</button> */}
      <button onClick={onCreateProposal}>Propose to change fee</button>
      <button onClick={onVoteForChangeProtocol}>Vote for fee change</button>
    </div>
  );
};

root.render(<App />);
