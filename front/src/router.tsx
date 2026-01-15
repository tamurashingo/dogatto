import { createBrowserRouter } from 'react-router-dom';
import App from './App';
import LoginPage from './pages/LoginPage';

/**
 * Application router configuration.
 * 
 * Defines all routes for the application including authentication and main pages.
 * 
 * @return {Router} Configured browser router instance
 */
export const router = createBrowserRouter([
  {
    path: '/',
    element: <App />,
  },
  {
    path: '/login',
    element: <LoginPage />,
  },
  {
    path: '/register',
    element: <div>Register Page (TODO)</div>,
  },
  {
    path: '/todos',
    element: <div>Todos Page (TODO)</div>,
  },
]);
