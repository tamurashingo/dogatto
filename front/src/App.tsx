import { Link } from 'react-router-dom';
import Header from './components/Header';
import './App.css';

/**
 * App component.
 *
 * Main application component with header and welcome message.
 */
function App() {
  return (
    <div className="app">
      <Header />
      <main className="main-content">
        <h1>Welcome to Dogatto</h1>
        <p>A simple TODO application</p>
        <div className="actions">
          <Link to="/login" className="button">
            Login
          </Link>
          <Link to="/register" className="button">
            Register
          </Link>
        </div>
      </main>
    </div>
  );
}

export default App;
