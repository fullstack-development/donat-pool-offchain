import { useState } from 'react';
import ReactDOM from 'react-dom/client';

const a = await import('@offchain/Scaffold.Main');

// eslint-disable-next-line @typescript-eslint/no-non-null-assertion
const root = ReactDOM.createRoot(document.getElementById('root')!);

const App = () => {

  const protocolData = {
    protocolCurrency: "3fa8e18f6baf0dfc70693a1f9a3c7fabb015c019d615f55707d7643e",
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
 
  const testnetLode = { 
    wallet: "Lode",
    isMainnet: false
  };

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
    )(testnetLode)(createFundraisingParams)();
  };

  const onDonate = () => {   
    a.main.value0.donate(console.log)(console.log)(protocolData)(testnetLode)(fundraisingData)(100)();
  };

  const onReceiveFunds = () => {
    a.main.value0.receiveFunds(console.log)(console.log)(protocolData)(testnetLode)(fundraisingData)();
  }
  
  const onGetAllFundraising = () => {
    a.main.value0.getAllFundraisings(console.log)(console.log)(protocolData)(testnetLode)();
  };

  const onGetUserRelatedFundraisings = () => {
    a.main.value0.getUserRelatedFundraisings(console.log)(console.log)(protocolData)(testnetLode)();
  };

  const onConnectWallet = () => {
    a.main.value0.connectWallet(console.log)(console.log)(testnetLode)();
  };

  const onGetAppInfo = () => {
    a.main.value0.getAppInfo(console.log)(console.log)(protocolData)(testnetLode)();
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
    </div>
  );
};

root.render(<App />);
