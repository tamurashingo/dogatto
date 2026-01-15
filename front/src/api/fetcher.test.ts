import { describe, it, expect, vi, beforeEach } from 'vitest';
import { fetcher, postFetcher, putFetcher, deleteFetcher } from './fetcher';
import { apiClient } from './client';
import { ApiError } from './error';

// Mock the apiClient
vi.mock('./client', () => ({
  apiClient: {
    get: vi.fn(),
    post: vi.fn(),
    put: vi.fn(),
    delete: vi.fn(),
  },
}));

describe('fetcher', () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  describe('fetcher (GET)', () => {
    it('should fetch data successfully', async () => {
      // Arrange
      const mockData = { id: 1, name: 'Test' };
      vi.mocked(apiClient.get).mockResolvedValue({
        data: mockData,
        status: 200,
      });

      // Act
      const result = await fetcher('/test');

      // Assert
      expect(apiClient.get).toHaveBeenCalledWith('/test');
      expect(result).toEqual(mockData);
    });

    it('should throw ApiError when fetch fails', async () => {
      // Arrange
      const apiError = new ApiError('Not found', 404, 'NOT_FOUND');
      vi.mocked(apiClient.get).mockRejectedValue(apiError);

      // Act & Assert
      await expect(fetcher('/test')).rejects.toThrow(ApiError);
      await expect(fetcher('/test')).rejects.toThrow('Not found');
    });

    it('should return correct type', async () => {
      // Arrange
      interface TestData {
        id: number;
        name: string;
      }
      const mockData: TestData = { id: 1, name: 'Test' };
      vi.mocked(apiClient.get).mockResolvedValue({
        data: mockData,
        status: 200,
      });

      // Act
      const result = await fetcher<TestData>('/test');

      // Assert
      expect(result).toEqual(mockData);
      expect(result.id).toBe(1);
      expect(result.name).toBe('Test');
    });
  });

  describe('postFetcher', () => {
    it('should post data successfully', async () => {
      // Arrange
      const requestBody = { name: 'New Item' };
      const mockResponse = { id: 1, name: 'New Item' };
      vi.mocked(apiClient.post).mockResolvedValue({
        data: mockResponse,
        status: 201,
      });

      // Act
      const result = await postFetcher('/test', requestBody);

      // Assert
      expect(apiClient.post).toHaveBeenCalledWith('/test', requestBody);
      expect(result).toEqual(mockResponse);
    });

    it('should throw ApiError when post fails', async () => {
      // Arrange
      const requestBody = { name: 'New Item' };
      const apiError = new ApiError('Validation failed', 422, 'VALIDATION_ERROR');
      vi.mocked(apiClient.post).mockRejectedValue(apiError);

      // Act & Assert
      await expect(postFetcher('/test', requestBody)).rejects.toThrow(ApiError);
      await expect(postFetcher('/test', requestBody)).rejects.toThrow('Validation failed');
    });

    it('should handle empty body', async () => {
      // Arrange
      const mockResponse = { success: true };
      vi.mocked(apiClient.post).mockResolvedValue({
        data: mockResponse,
        status: 200,
      });

      // Act
      const result = await postFetcher('/test', {});

      // Assert
      expect(apiClient.post).toHaveBeenCalledWith('/test', {});
      expect(result).toEqual(mockResponse);
    });
  });

  describe('putFetcher', () => {
    it('should put data successfully', async () => {
      // Arrange
      const requestBody = { name: 'Updated Item' };
      const mockResponse = { id: 1, name: 'Updated Item' };
      vi.mocked(apiClient.put).mockResolvedValue({
        data: mockResponse,
        status: 200,
      });

      // Act
      const result = await putFetcher('/test/1', requestBody);

      // Assert
      expect(apiClient.put).toHaveBeenCalledWith('/test/1', requestBody);
      expect(result).toEqual(mockResponse);
    });

    it('should throw ApiError when put fails', async () => {
      // Arrange
      const requestBody = { name: 'Updated Item' };
      const apiError = new ApiError('Not found', 404, 'NOT_FOUND');
      vi.mocked(apiClient.put).mockRejectedValue(apiError);

      // Act & Assert
      await expect(putFetcher('/test/1', requestBody)).rejects.toThrow(ApiError);
      await expect(putFetcher('/test/1', requestBody)).rejects.toThrow('Not found');
    });
  });

  describe('deleteFetcher', () => {
    it('should delete data successfully', async () => {
      // Arrange
      const mockResponse = { success: true };
      vi.mocked(apiClient.delete).mockResolvedValue({
        data: mockResponse,
        status: 200,
      });

      // Act
      const result = await deleteFetcher('/test/1');

      // Assert
      expect(apiClient.delete).toHaveBeenCalledWith('/test/1');
      expect(result).toEqual(mockResponse);
    });

    it('should throw ApiError when delete fails', async () => {
      // Arrange
      const apiError = new ApiError('Not found', 404, 'NOT_FOUND');
      vi.mocked(apiClient.delete).mockRejectedValue(apiError);

      // Act & Assert
      await expect(deleteFetcher('/test/1')).rejects.toThrow(ApiError);
      await expect(deleteFetcher('/test/1')).rejects.toThrow('Not found');
    });

    it('should handle 204 No Content response', async () => {
      // Arrange
      vi.mocked(apiClient.delete).mockResolvedValue({
        data: undefined as any,
        status: 204,
      });

      // Act
      const result = await deleteFetcher('/test/1');

      // Assert
      expect(apiClient.delete).toHaveBeenCalledWith('/test/1');
      expect(result).toBeUndefined();
    });
  });

  describe('type safety', () => {
    it('should work with generic types', async () => {
      // Arrange
      interface User {
        id: string;
        email: string;
      }
      const mockUser: User = { id: '1', email: 'test@example.com' };
      vi.mocked(apiClient.get).mockResolvedValue({
        data: mockUser,
        status: 200,
      });

      // Act
      const result = await fetcher<User>('/users/1');

      // Assert
      expect(result.id).toBe('1');
      expect(result.email).toBe('test@example.com');
    });
  });
});
