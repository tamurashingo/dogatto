import { createContext, useContext, useState, useCallback, useEffect } from 'react';
import type { ReactNode } from 'react';
import { authApi } from '../api/auth';
import type { User, LoginRequest } from '../api/auth';
import { ApiError } from '../api/error';

/**
 * Authentication state.
 */
interface AuthState {
  user: User | null;
  isAuthenticated: boolean;
  isLoading: boolean;
  error: string | null;
}

/**
 * Authentication context value.
 */
interface AuthContextValue extends AuthState {
  login: (email: string, password: string) => Promise<void>;
  logout: () => Promise<void>;
  refreshUser: () => Promise<void>;
  clearError: () => void;
}

const AuthContext = createContext<AuthContextValue | undefined>(undefined);

/**
 * Authentication provider props.
 */
interface AuthProviderProps {
  children: ReactNode;
}

/**
 * Authentication provider component.
 *
 * Manages user authentication state and provides auth methods.
 *
 * @param props [AuthProviderProps] Component props
 * @return [React.JSX.Element] Provider component
 */
export function AuthProvider({ children }: AuthProviderProps): React.JSX.Element {
  const [state, setState] = useState<AuthState>({
    user: null,
    isAuthenticated: false,
    isLoading: true,
    error: null,
  });

  /**
   * Loads the current user on app startup.
   */
  useEffect(() => {
    const loadUser = async () => {
      try {
        const user = await authApi.getCurrentUser();
        setState({
          user,
          isAuthenticated: true,
          isLoading: false,
          error: null,
        });
      } catch (error) {
        setState({
          user: null,
          isAuthenticated: false,
          isLoading: false,
          error: null,
        });
      }
    };

    loadUser();
  }, []);

  /**
   * Logs in a user with email and password.
   *
   * @param email [string] User's email
   * @param password [string] User's password
   * @throws [ApiError] When login fails
   */
  const login = useCallback(async (email: string, password: string): Promise<void> => {
    setState((prev) => ({ ...prev, isLoading: true, error: null }));
    try {
      const loginData: LoginRequest = {
        email,
        password,
      };
      
      const user = await authApi.login(loginData);
      setState({
        user,
        isAuthenticated: true,
        isLoading: false,
        error: null,
      });
    } catch (error) {
      const errorMessage = error instanceof ApiError 
        ? error.message 
        : 'Login failed';
      setState({
        user: null,
        isAuthenticated: false,
        isLoading: false,
        error: errorMessage,
      });
      throw error;
    }
  }, []);

  /**
   * Logs out the current user.
   *
   * @throws [ApiError] When logout fails
   */
  const logout = useCallback(async (): Promise<void> => {
    setState((prev) => ({ ...prev, isLoading: true, error: null }));
    try {
      await authApi.logout();
    } catch (error) {
      // Continue with logout even if API call fails
      console.error('Logout API call failed:', error);
    } finally {
      setState({
        user: null,
        isAuthenticated: false,
        isLoading: false,
        error: null,
      });
    }
  }, []);

  /**
   * Refreshes the current user's information.
   *
   * @throws [ApiError] When refresh fails
   */
  const refreshUser = useCallback(async (): Promise<void> => {
    setState((prev) => ({ ...prev, isLoading: true, error: null }));
    try {
      const user = await authApi.getCurrentUser();
      setState({
        user,
        isAuthenticated: true,
        isLoading: false,
        error: null,
      });
    } catch (error) {
      const errorMessage = error instanceof ApiError 
        ? error.message 
        : 'Failed to refresh user';
      setState({
        user: null,
        isAuthenticated: false,
        isLoading: false,
        error: errorMessage,
      });
      throw error;
    }
  }, []);

  /**
   * Clears the error state.
   */
  const clearError = useCallback(() => {
    setState((prev) => ({ ...prev, error: null }));
  }, []);

  const value: AuthContextValue = {
    ...state,
    login,
    logout,
    refreshUser,
    clearError,
  };

  return <AuthContext.Provider value={value}>{children}</AuthContext.Provider>;
}

/**
 * Hook to access authentication context.
 *
 * @return [AuthContextValue] Authentication context value
 * @throws [Error] When used outside of AuthProvider
 */
export function useAuth(): AuthContextValue {
  const context = useContext(AuthContext);
  if (context === undefined) {
    throw new Error('useAuth must be used within an AuthProvider');
  }
  return context;
}
