import { useState } from 'react';
import ReactDOM from 'react-dom/client';

const a = await import('@offchain/Scaffold.Main');

// eslint-disable-next-line @typescript-eslint/no-non-null-assertion
const root = ReactDOM.createRoot(document.getElementById('root')!);

const App = () => {
  const startProtocolParams = {
    minAmountParam: 50000000,
    maxAmountParam: 1000000000,
    minDurationParam: 100,
    maxDurationParam: 1000,
    protocolFeeParam: 10,
  };
  const updatedParams = {
    minAmountParam: 50000000,
    maxAmountParam: 1000000000,
    minDurationParam: 100,
    maxDurationParam: 1000,
    protocolFeeParam: 9,
  };
  const createFundraisingParams = {
    description: 'Donate to feed stray cats',
    amount: 188,
    duration: 101
  }

  const [protocol, setProtocol] = useState();
  const [fundraising, setFundraising] = useState();

  const onStartProtocolComplete = completedProtocol => {
    setProtocol(completedProtocol);
  };

  const onUpdateProtocolComplete = something => {
    console.log('update success');
  };

  const onCreateFundraisingComplete = createdFundraising => {
    setFundraising(createdFundraising);
  };

  const onStartProtocolClick = () => {
    a.main.value0.startProtocol(onStartProtocolComplete)(console.log)(
      startProtocolParams
    )();
  };
  const onUpdateProtocolClick = () => {
    a.main.value0.updateProtocol(onUpdateProtocolComplete)(console.log)(
      protocol
    )(updatedParams)();
  };
  const onCloseProtocolClick = () => {
    a.main.value0.closeProtocol(protocol)();
  };

  const onCreateFundraisingClick = () => {
    a.main.value0.createFundraising(onCreateFundraisingComplete)(console.log)(
      protocol
    )(createFundraisingParams)();
  };

  const onDonate = () => {
    a.main.value0.donate(fundraising)(100_000_000)();
  };

  return (
    <div>
      <h1>Offchain integration</h1>
      <button onClick={onStartProtocolClick}>Start Protocol</button>
      <button onClick={onUpdateProtocolClick}>Update Protocol</button>
      <button onClick={onCloseProtocolClick}>Close Protocol</button>
      <button onClick={onCreateFundraisingClick}>Create fundraising</button>
      <button onClick={onDonate}>Donate 100 Ada</button>
    </div>
  );
};

root.render(<App />);
