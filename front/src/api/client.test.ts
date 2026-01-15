import { describe, it, expect, vi, beforeEach, afterEach } from 'vitest';
import { ApiClient } from './client';
import { ApiError } from './error';

// Mock global fetch
global.fetch = vi.fn();

describe('ApiClient', () => {
  let client: ApiClient;

  beforeEach(() => {
    client = new ApiClient('/api/v1');
    vi.clearAllMocks();
  });

  afterEach(() => {
    vi.restoreAllMocks();
  });

  describe('constructor', () => {
    it('should create client with default base URL', () => {
      // Arrange & Act
      const defaultClient = new ApiClient();

      // Assert
      expect(defaultClient).toBeInstanceOf(ApiClient);
    });

    it('should create client with custom base URL', () => {
      // Arrange & Act
      const customClient = new ApiClient('/custom/api');

      // Assert
      expect(customClient).toBeInstanceOf(ApiClient);
    });
  });

  describe('setAuthToken', () => {
    it('should set authorization header', async () => {
      // Arrange
      const mockResponse = {
        ok: true,
        status: 200,
        headers: new Headers({ 'content-type': 'application/json' }),
        json: async () => ({ data: 'test' }),
      };
      vi.mocked(fetch).mockResolvedValue(mockResponse as Response);

      // Act
      client.setAuthToken('test-token');
      await client.get('/test');

      // Assert
      expect(fetch).toHaveBeenCalledWith(
        expect.any(String),
        expect.objectContaining({
          headers: expect.objectContaining({
            'Authorization': 'Bearer test-token',
          }),
        })
      );
    });
  });

  describe('clearAuthToken', () => {
    it('should remove authorization header', async () => {
      // Arrange
      const mockResponse = {
        ok: true,
        status: 200,
        headers: new Headers({ 'content-type': 'application/json' }),
        json: async () => ({ data: 'test' }),
      };
      vi.mocked(fetch).mockResolvedValue(mockResponse as Response);

      client.setAuthToken('test-token');

      // Act
      client.clearAuthToken();
      await client.get('/test');

      // Assert
      expect(fetch).toHaveBeenCalledWith(
        expect.any(String),
        expect.objectContaining({
          headers: expect.not.objectContaining({
            'Authorization': expect.any(String),
          }),
        })
      );
    });
  });

  describe('get', () => {
    it('should make GET request successfully', async () => {
      // Arrange
      const mockData = { id: 1, name: 'Test' };
      const mockResponse = {
        ok: true,
        status: 200,
        headers: new Headers({ 'content-type': 'application/json' }),
        json: async () => mockData,
      };
      vi.mocked(fetch).mockResolvedValue(mockResponse as Response);

      // Act
      const result = await client.get('/test');

      // Assert
      expect(fetch).toHaveBeenCalledWith(
        expect.stringContaining('/test'),
        expect.objectContaining({
          method: 'GET',
        })
      );
      expect(result.data).toEqual(mockData);
      expect(result.status).toBe(200);
    });

    it('should make GET request with query parameters', async () => {
      // Arrange
      const mockResponse = {
        ok: true,
        status: 200,
        headers: new Headers({ 'content-type': 'application/json' }),
        json: async () => ({ data: 'test' }),
      };
      vi.mocked(fetch).mockResolvedValue(mockResponse as Response);

      // Act
      await client.get('/test', { page: '1', limit: '10' });

      // Assert
      expect(fetch).toHaveBeenCalledWith(
        expect.stringContaining('page=1'),
        expect.any(Object)
      );
      expect(fetch).toHaveBeenCalledWith(
        expect.stringContaining('limit=10'),
        expect.any(Object)
      );
    });
  });

  describe('post', () => {
    it('should make POST request successfully', async () => {
      // Arrange
      const requestBody = { name: 'Test' };
      const mockResponse = {
        ok: true,
        status: 201,
        headers: new Headers({ 'content-type': 'application/json' }),
        json: async () => ({ id: 1, ...requestBody }),
      };
      vi.mocked(fetch).mockResolvedValue(mockResponse as Response);

      // Act
      const result = await client.post('/test', requestBody);

      // Assert
      expect(fetch).toHaveBeenCalledWith(
        expect.any(String),
        expect.objectContaining({
          method: 'POST',
          body: JSON.stringify(requestBody),
        })
      );
      expect(result.status).toBe(201);
    });

    it('should make POST request without body', async () => {
      // Arrange
      const mockResponse = {
        ok: true,
        status: 200,
        headers: new Headers({ 'content-type': 'application/json' }),
        json: async () => ({ success: true }),
      };
      vi.mocked(fetch).mockResolvedValue(mockResponse as Response);

      // Act
      await client.post('/test');

      // Assert
      expect(fetch).toHaveBeenCalledWith(
        expect.any(String),
        expect.objectContaining({
          method: 'POST',
          body: undefined,
        })
      );
    });
  });

  describe('put', () => {
    it('should make PUT request successfully', async () => {
      // Arrange
      const requestBody = { name: 'Updated' };
      const mockResponse = {
        ok: true,
        status: 200,
        headers: new Headers({ 'content-type': 'application/json' }),
        json: async () => ({ id: 1, ...requestBody }),
      };
      vi.mocked(fetch).mockResolvedValue(mockResponse as Response);

      // Act
      await client.put('/test/1', requestBody);

      // Assert
      expect(fetch).toHaveBeenCalledWith(
        expect.any(String),
        expect.objectContaining({
          method: 'PUT',
          body: JSON.stringify(requestBody),
        })
      );
    });
  });

  describe('patch', () => {
    it('should make PATCH request successfully', async () => {
      // Arrange
      const requestBody = { name: 'Patched' };
      const mockResponse = {
        ok: true,
        status: 200,
        headers: new Headers({ 'content-type': 'application/json' }),
        json: async () => ({ id: 1, ...requestBody }),
      };
      vi.mocked(fetch).mockResolvedValue(mockResponse as Response);

      // Act
      await client.patch('/test/1', requestBody);

      // Assert
      expect(fetch).toHaveBeenCalledWith(
        expect.any(String),
        expect.objectContaining({
          method: 'PATCH',
          body: JSON.stringify(requestBody),
        })
      );
    });
  });

  describe('delete', () => {
    it('should make DELETE request successfully', async () => {
      // Arrange
      const mockResponse = {
        ok: true,
        status: 204,
        headers: new Headers({ 'content-type': 'application/json' }),
        json: async () => ({}),
      };
      vi.mocked(fetch).mockResolvedValue(mockResponse as Response);

      // Act
      await client.delete('/test/1');

      // Assert
      expect(fetch).toHaveBeenCalledWith(
        expect.any(String),
        expect.objectContaining({
          method: 'DELETE',
        })
      );
    });
  });

  describe('error handling', () => {
    it('should throw ApiError for HTTP error with JSON response', async () => {
      // Arrange
      const errorResponse = {
        error: {
          message: 'Not found',
          code: 'NOT_FOUND',
        },
      };
      const mockResponse = {
        ok: false,
        status: 404,
        statusText: 'Not Found',
        headers: new Headers({ 'content-type': 'application/json' }),
        json: async () => errorResponse,
      };
      vi.mocked(fetch).mockResolvedValue(mockResponse as Response);

      // Act & Assert
      await expect(client.get('/test')).rejects.toThrow(ApiError);
      await expect(client.get('/test')).rejects.toThrow('Not found');
    });

    it('should throw ApiError for HTTP error without JSON response', async () => {
      // Arrange
      const mockResponse = {
        ok: false,
        status: 500,
        statusText: 'Internal Server Error',
        headers: new Headers({ 'content-type': 'text/plain' }),
        text: async () => 'Error',
      };
      vi.mocked(fetch).mockResolvedValue(mockResponse as Response);

      // Act & Assert
      await expect(client.get('/test')).rejects.toThrow(ApiError);
      await expect(client.get('/test')).rejects.toThrow('Internal Server Error');
    });

    it('should throw ApiError for network error', async () => {
      // Arrange
      vi.mocked(fetch).mockRejectedValue(new Error('Network error'));

      // Act & Assert
      await expect(client.get('/test')).rejects.toThrow(ApiError);
      await expect(client.get('/test')).rejects.toThrow('Network error');
    });

    it('should throw ApiError with NETWORK_ERROR code for network failures', async () => {
      // Arrange
      vi.mocked(fetch).mockRejectedValue(new Error('Failed to fetch'));

      // Act & Assert
      try {
        await client.get('/test');
      } catch (error) {
        expect(error).toBeInstanceOf(ApiError);
        expect((error as ApiError).code).toBe('NETWORK_ERROR');
      }
    });
  });

  describe('credentials', () => {
    it('should include credentials with same-origin', async () => {
      // Arrange
      const mockResponse = {
        ok: true,
        status: 200,
        headers: new Headers({ 'content-type': 'application/json' }),
        json: async () => ({ data: 'test' }),
      };
      vi.mocked(fetch).mockResolvedValue(mockResponse as Response);

      // Act
      await client.get('/test');

      // Assert
      expect(fetch).toHaveBeenCalledWith(
        expect.any(String),
        expect.objectContaining({
          credentials: 'same-origin',
        })
      );
    });
  });
});
