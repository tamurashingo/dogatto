import { apiClient } from './client';
import type { ApiResponse } from './client';

/**
 * User data structure.
 */
export interface User {
  id: string;
  name: string;
  email: string;
  createdAt: string;
  updatedAt: string;
}

/**
 * User registration request payload.
 */
export interface RegisterRequest {
  name: string;
  email: string;
  password: string;
}

/**
 * User registration response.
 */
export interface RegisterResponse {
  user: User;
}

/**
 * User login request payload.
 */
export interface LoginRequest {
  email: string;
  password: string;
}

/**
 * User login response.
 */
export interface LoginResponse {
  user: User;
}

/**
 * Current user response.
 */
export interface CurrentUserResponse {
  user: User;
}

/**
 * Authentication API client.
 *
 * Provides methods for user authentication operations.
 */
export const authApi = {
  /**
   * Registers a new user.
   *
   * Creates a new user account with the provided credentials.
   *
   * @param data [RegisterRequest] User registration data
   * @param data.name [string] User's name
   * @param data.email [string] User's email address
   * @param data.password [string] User's password
   * @return [Promise<User>] Registered user data
   * @throws [ApiError] When registration fails (e.g., email already exists)
   */
  async register(data: RegisterRequest): Promise<User> {
    const response: ApiResponse<RegisterResponse> = await apiClient.post<RegisterResponse>(
      '/auth/register',
      data
    );
    return response.data.user;
  },

  /**
   * Logs in a user.
   *
   * Authenticates a user with email and password, creates a session.
   *
   * @param data [LoginRequest] User login credentials
   * @param data.email [string] User's email address
   * @param data.password [string] User's password
   * @return [Promise<User>] Logged in user data
   * @throws [ApiError] When login fails (e.g., invalid credentials)
   */
  async login(data: LoginRequest): Promise<User> {
    const response: ApiResponse<LoginResponse> = await apiClient.post<LoginResponse>(
      '/auth/login',
      data
    );
    return response.data.user;
  },

  /**
   * Logs out the current user.
   *
   * Destroys the current user session.
   *
   * @return [Promise<void>]
   * @throws [ApiError] When logout fails
   */
  async logout(): Promise<void> {
    await apiClient.post<void>('/auth/logout');
  },

  /**
   * Gets the current authenticated user.
   *
   * Retrieves the currently logged-in user's information.
   *
   * @return [Promise<User>] Current user data
   * @throws [ApiError] When user is not authenticated (status 401)
   */
  async getCurrentUser(): Promise<User> {
    const response: ApiResponse<CurrentUserResponse> = await apiClient.get<CurrentUserResponse>(
      '/auth/me'
    );
    return response.data.user;
  },
};
