import { Navigate } from 'react-router-dom';
import type { ReactNode } from 'react';
import { useAuth } from '../contexts/AuthContext';

/**
 * Protected route props.
 */
interface ProtectedRouteProps {
  children: ReactNode;
}

/**
 * Protected route component.
 *
 * Protects routes that require authentication.
 * Redirects to login page if user is not authenticated.
 * Shows loading state while checking authentication.
 *
 * @param props [ProtectedRouteProps] Component props
 * @return [React.JSX.Element] Protected content or redirect
 */
export default function ProtectedRoute({ children }: ProtectedRouteProps): React.JSX.Element {
  const { isAuthenticated, isLoading } = useAuth();

  // Show loading state while checking authentication
  if (isLoading) {
    return (
      <div className="loading-container">
        <div className="loading-spinner">Loading...</div>
      </div>
    );
  }

  // Redirect to login if not authenticated
  if (!isAuthenticated) {
    return <Navigate to="/login" replace />;
  }

  // Render protected content
  return <>{children}</>;
}
