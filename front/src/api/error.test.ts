import { describe, it, expect } from 'vitest';
import { ApiError } from './error';

describe('ApiError', () => {
  describe('constructor', () => {
    it('should create ApiError with all parameters', () => {
      // Arrange & Act
      const error = new ApiError(
        'Test error message',
        404,
        'NOT_FOUND',
        { field: 'email' }
      );

      // Assert
      expect(error).toBeInstanceOf(Error);
      expect(error).toBeInstanceOf(ApiError);
      expect(error.name).toBe('ApiError');
      expect(error.message).toBe('Test error message');
      expect(error.status).toBe(404);
      expect(error.code).toBe('NOT_FOUND');
      expect(error.details).toEqual({ field: 'email' });
    });

    it('should create ApiError with default code when not provided', () => {
      // Arrange & Act
      const error = new ApiError('Test error', 500);

      // Assert
      expect(error.code).toBe('UNKNOWN_ERROR');
      expect(error.details).toBeUndefined();
    });

    it('should maintain proper stack trace', () => {
      // Arrange & Act
      const error = new ApiError('Test error', 500);

      // Assert
      expect(error.stack).toBeDefined();
      expect(error.stack).toContain('ApiError');
    });
  });

  describe('isAuthError', () => {
    it('should return true for status 401', () => {
      // Arrange
      const error = new ApiError('Unauthorized', 401);

      // Act & Assert
      expect(error.isAuthError()).toBe(true);
    });

    it('should return false for non-401 status', () => {
      // Arrange
      const error = new ApiError('Not Found', 404);

      // Act & Assert
      expect(error.isAuthError()).toBe(false);
    });
  });

  describe('isForbiddenError', () => {
    it('should return true for status 403', () => {
      // Arrange
      const error = new ApiError('Forbidden', 403);

      // Act & Assert
      expect(error.isForbiddenError()).toBe(true);
    });

    it('should return false for non-403 status', () => {
      // Arrange
      const error = new ApiError('Unauthorized', 401);

      // Act & Assert
      expect(error.isForbiddenError()).toBe(false);
    });
  });

  describe('isNotFoundError', () => {
    it('should return true for status 404', () => {
      // Arrange
      const error = new ApiError('Not Found', 404);

      // Act & Assert
      expect(error.isNotFoundError()).toBe(true);
    });

    it('should return false for non-404 status', () => {
      // Arrange
      const error = new ApiError('Server Error', 500);

      // Act & Assert
      expect(error.isNotFoundError()).toBe(false);
    });
  });

  describe('isValidationError', () => {
    it('should return true for status 422', () => {
      // Arrange
      const error = new ApiError('Validation Error', 422);

      // Act & Assert
      expect(error.isValidationError()).toBe(true);
    });

    it('should return false for non-422 status', () => {
      // Arrange
      const error = new ApiError('Bad Request', 400);

      // Act & Assert
      expect(error.isValidationError()).toBe(false);
    });
  });

  describe('isServerError', () => {
    it('should return true for status 500', () => {
      // Arrange
      const error = new ApiError('Internal Server Error', 500);

      // Act & Assert
      expect(error.isServerError()).toBe(true);
    });

    it('should return true for status >= 500', () => {
      // Arrange
      const error503 = new ApiError('Service Unavailable', 503);
      const error502 = new ApiError('Bad Gateway', 502);

      // Act & Assert
      expect(error503.isServerError()).toBe(true);
      expect(error502.isServerError()).toBe(true);
    });

    it('should return false for status < 500', () => {
      // Arrange
      const error = new ApiError('Not Found', 404);

      // Act & Assert
      expect(error.isServerError()).toBe(false);
    });
  });

  describe('error categorization', () => {
    it('should correctly categorize different error types', () => {
      // Arrange
      const errors = [
        { error: new ApiError('Unauthorized', 401), expected: 'auth' },
        { error: new ApiError('Forbidden', 403), expected: 'forbidden' },
        { error: new ApiError('Not Found', 404), expected: 'notfound' },
        { error: new ApiError('Validation Failed', 422), expected: 'validation' },
        { error: new ApiError('Server Error', 500), expected: 'server' },
      ];

      // Act & Assert
      errors.forEach(({ error, expected }) => {
        switch (expected) {
          case 'auth':
            expect(error.isAuthError()).toBe(true);
            expect(error.isForbiddenError()).toBe(false);
            break;
          case 'forbidden':
            expect(error.isForbiddenError()).toBe(true);
            expect(error.isAuthError()).toBe(false);
            break;
          case 'notfound':
            expect(error.isNotFoundError()).toBe(true);
            expect(error.isServerError()).toBe(false);
            break;
          case 'validation':
            expect(error.isValidationError()).toBe(true);
            expect(error.isServerError()).toBe(false);
            break;
          case 'server':
            expect(error.isServerError()).toBe(true);
            expect(error.isValidationError()).toBe(false);
            break;
        }
      });
    });
  });
});
