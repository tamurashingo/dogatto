import { describe, it, expect, vi, beforeEach } from 'vitest';
import { authApi } from './auth';
import { apiClient } from './client';
import { ApiError } from './error';

// Mock the apiClient
vi.mock('./client', () => ({
  apiClient: {
    post: vi.fn(),
    get: vi.fn(),
  },
}));

describe('authApi', () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  describe('register', () => {
    it('should register a new user successfully', async () => {
      // Arrange
      const mockUser = {
        id: '01HXZ2M3N4P5Q6R7S8T9V0W1X2',
        name: 'Test User',
        email: 'test@example.com',
        createdAt: '2026-01-15T00:00:00Z',
        updatedAt: '2026-01-15T00:00:00Z',
      };
      const registerRequest = {
        name: 'Test User',
        email: 'test@example.com',
        password: 'password123',
      };

      vi.mocked(apiClient.post).mockResolvedValue({
        data: { status: 'success', data: { user: mockUser } },
        status: 201,
      });

      // Act
      const result = await authApi.register(registerRequest);

      // Assert
      expect(apiClient.post).toHaveBeenCalledWith('/api/v1/auth/register', registerRequest);
      expect(result).toEqual(mockUser);
    });

    it('should throw ApiError when registration fails', async () => {
      // Arrange
      const registerRequest = {
        name: 'Test User',
        email: 'existing@example.com',
        password: 'password123',
      };
      const apiError = new ApiError('Email already exists', 409, 'EMAIL_EXISTS');

      vi.mocked(apiClient.post).mockRejectedValue(apiError);

      // Act & Assert
      await expect(authApi.register(registerRequest)).rejects.toThrow(ApiError);
      await expect(authApi.register(registerRequest)).rejects.toThrow('Email already exists');
    });
  });

  describe('login', () => {
    it('should login user successfully', async () => {
      // Arrange
      const mockUser = {
        id: '01HXZ2M3N4P5Q6R7S8T9V0W1X2',
        name: 'Test User',
        email: 'test@example.com',
        createdAt: '2026-01-15T00:00:00Z',
        updatedAt: '2026-01-15T00:00:00Z',
      };
      const loginRequest = {
        email: 'test@example.com',
        password: 'password123',
      };

      vi.mocked(apiClient.post).mockResolvedValue({
        data: { status: 'success', data: { user: mockUser } },
        status: 200,
      });

      // Act
      const result = await authApi.login(loginRequest);

      // Assert
      expect(apiClient.post).toHaveBeenCalledWith('/api/v1/auth/login', loginRequest);
      expect(result).toEqual(mockUser);
    });

    it('should throw ApiError when login fails with invalid credentials', async () => {
      // Arrange
      const loginRequest = {
        email: 'test@example.com',
        password: 'wrongpassword',
      };
      const apiError = new ApiError('Invalid credentials', 401, 'INVALID_CREDENTIALS');

      vi.mocked(apiClient.post).mockRejectedValue(apiError);

      // Act & Assert
      await expect(authApi.login(loginRequest)).rejects.toThrow(ApiError);
      await expect(authApi.login(loginRequest)).rejects.toThrow('Invalid credentials');
    });
  });

  describe('logout', () => {
    it('should logout user successfully', async () => {
      // Arrange
      vi.mocked(apiClient.post).mockResolvedValue({
        data: undefined,
        status: 200,
      });

      // Act
      await authApi.logout();

      // Assert
      expect(apiClient.post).toHaveBeenCalledWith('/api/v1/auth/logout');
    });

    it('should throw ApiError when logout fails', async () => {
      // Arrange
      const apiError = new ApiError('Logout failed', 500, 'SERVER_ERROR');

      vi.mocked(apiClient.post).mockRejectedValue(apiError);

      // Act & Assert
      await expect(authApi.logout()).rejects.toThrow(ApiError);
    });
  });

  describe('getCurrentUser', () => {
    it('should get current user successfully', async () => {
      // Arrange
      const mockUser = {
        id: '01HXZ2M3N4P5Q6R7S8T9V0W1X2',
        name: 'Test User',
        email: 'test@example.com',
        createdAt: '2026-01-15T00:00:00Z',
        updatedAt: '2026-01-15T00:00:00Z',
      };

      vi.mocked(apiClient.get).mockResolvedValue({
        data: { status: 'success', data: { user: mockUser } },
        status: 200,
      });

      // Act
      const result = await authApi.getCurrentUser();

      // Assert
      expect(apiClient.get).toHaveBeenCalledWith('/api/v1/auth/me');
      expect(result).toEqual(mockUser);
    });

    it('should throw ApiError when user is not authenticated', async () => {
      // Arrange
      const apiError = new ApiError('Unauthorized', 401, 'UNAUTHORIZED');

      vi.mocked(apiClient.get).mockRejectedValue(apiError);

      // Act & Assert
      await expect(authApi.getCurrentUser()).rejects.toThrow(ApiError);
      await expect(authApi.getCurrentUser()).rejects.toThrow('Unauthorized');
    });
  });
});
