import 'font-awesome/css/font-awesome.min.css';
import './assets/css/app.css';
import CalculatorApp from './pages/CalculatorPage';
import GuidanceApp from './pages/GuidancePage';
import {BrowserRouter as Router, Route, Routes} from 'react-router-dom';

function App() {
  return (
        <Router>
            <Routes>
                <Route exact path='/' element={<CalculatorApp/>} />
                <Route  path='/guidance' element={<GuidanceApp/>} />

            </Routes>  
        </Router>
    )
}

export default App;
