import ReactDOM from 'react-dom/client';

import offchain from '../dist'

const root = ReactDOM.createRoot(document.getElementById('root'))
// offchain.resolve().then(console.log)
console.log(offchain)
const App = () => {
  return (
    <div>
      <h1>Offchain integration</h1>
      {/* <button onClick={()=>{console.log(a.runStartProtocol({"minAmountParam":50000000,"maxAmountParam":1000000000,"minDurationParam":100,"maxDurationParam":1000,"protocolFeeParam":10})().join(console.log)())}}>Connect wallet</button> */}
    </div>
  )
}

root.render(<App/>)
