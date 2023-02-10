import ReactDOM from 'react-dom/client';

// import offchain from '../dist'

const a = await import ('@offchain/Scaffold.Main')

const root = ReactDOM.createRoot(document.getElementById('root'))
const startProtocolParams = {"minAmountParam":50000000,"maxAmountParam":1000000000,"minDurationParam":100,"maxDurationParam":1000,"protocolFeeParam":10}
// offchain.resolve().then(console.log)
// console.log(offchain)

const App = () => {
  return (
    <div>
      <h1>Offchain integration</h1>
      <button onClick={()=>console.log(a)}>Start Protocol</button>
    </div>
  )
}

root.render(<App/>)
