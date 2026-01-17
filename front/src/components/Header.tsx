import { useState } from 'react';
import { useNavigate, Link } from 'react-router-dom';
import { useAuth } from '../contexts/AuthContext';
import '../styles/header.css';

/**
 * Header component.
 *
 * Displays navigation header with user information and logout button.
 */
export default function Header(): React.JSX.Element | null {
  const navigate = useNavigate();
  const { user, isAuthenticated, logout } = useAuth();
  const [isLoggingOut, setIsLoggingOut] = useState(false);

  // Don't show header if not authenticated
  if (!isAuthenticated || !user) {
    return null;
  }

  /**
   * Handles logout button click.
   */
  const handleLogout = async () => {
    setIsLoggingOut(true);
    try {
      await logout();
      // Redirect to login page on success
      navigate('/login');
    } catch (error) {
      console.error('Logout failed:', error);
      // Still redirect even if API call fails
      navigate('/login');
    } finally {
      setIsLoggingOut(false);
    }
  };

  return (
    <header className="app-header">
      <div className="header-container">
        <div className="header-left">
          <Link to="/todos" className="app-title">Dogatto</Link>
        </div>

        <div className="header-right">
          <div className="user-info">
            <span className="user-name">{user.name}</span>
          </div>

          <button
            onClick={handleLogout}
            disabled={isLoggingOut}
            className="logout-button"
            type="button"
          >
            {isLoggingOut ? 'Logging out...' : 'Logout'}
          </button>
        </div>
      </div>
    </header>
  );
}
