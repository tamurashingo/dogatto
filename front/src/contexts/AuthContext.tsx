import { createContext, useContext, useState, useCallback } from 'react';
import type { ReactNode } from 'react';
import { apiClient } from '../api/client';
import type { User } from '../types/user';

/**
 * Authentication state.
 */
interface AuthState {
  user: User | null;
  isAuthenticated: boolean;
  isLoading: boolean;
}

/**
 * Authentication context value.
 */
interface AuthContextValue extends AuthState {
  login: (username: string, password: string) => Promise<void>;
  logout: () => Promise<void>;
  refreshUser: () => Promise<void>;
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
  });

  /**
   * Logs in a user with username and password.
   *
   * @param username [string] User's username
   * @param password [string] User's password
   * @throws [ApiError] When login fails
   */
  const login = useCallback(async (username: string, password: string): Promise<void> => {
    setState((prev) => ({ ...prev, isLoading: true }));
    try {
      const response = await apiClient.post<{ user: User; token: string }>('/auth/login', {
        username,
        password,
      });
      
      apiClient.setAuthToken(response.data.token);
      setState({
        user: response.data.user,
        isAuthenticated: true,
        isLoading: false,
      });
    } catch (error) {
      setState({
        user: null,
        isAuthenticated: false,
        isLoading: false,
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
    try {
      await apiClient.post('/auth/logout');
    } finally {
      apiClient.clearAuthToken();
      setState({
        user: null,
        isAuthenticated: false,
        isLoading: false,
      });
    }
  }, []);

  /**
   * Refreshes the current user's information.
   *
   * @throws [ApiError] When refresh fails
   */
  const refreshUser = useCallback(async (): Promise<void> => {
    setState((prev) => ({ ...prev, isLoading: true }));
    try {
      const response = await apiClient.get<{ user: User }>('/auth/me');
      setState({
        user: response.data.user,
        isAuthenticated: true,
        isLoading: false,
      });
    } catch (error) {
      apiClient.clearAuthToken();
      setState({
        user: null,
        isAuthenticated: false,
        isLoading: false,
      });
      throw error;
    }
  }, []);

  const value: AuthContextValue = {
    ...state,
    login,
    logout,
    refreshUser,
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
