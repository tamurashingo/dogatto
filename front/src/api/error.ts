/**
 * API error class.
 *
 * Represents an error response from the backend API.
 */
export class ApiError extends Error {
  public readonly status: number;
  public readonly code: string;
  public readonly details?: unknown;

  /**
   * Creates a new ApiError instance.
   *
   * @param message [string] Error message
   * @param status [number] HTTP status code
   * @param code [string] Application-specific error code
   * @param details [unknown] Additional error details
   */
  constructor(
    message: string,
    status: number,
    code: string = 'UNKNOWN_ERROR',
    details?: unknown
  ) {
    super(message);
    this.name = 'ApiError';
    this.status = status;
    this.code = code;
    this.details = details;

    // Maintains proper stack trace for where error was thrown
    if ((Error as any).captureStackTrace) {
      (Error as any).captureStackTrace(this, ApiError);
    }
  }

  /**
   * Checks if the error is an authentication error.
   *
   * @return [boolean] True if status is 401
   */
  isAuthError(): boolean {
    return this.status === 401;
  }

  /**
   * Checks if the error is a forbidden error.
   *
   * @return [boolean] True if status is 403
   */
  isForbiddenError(): boolean {
    return this.status === 403;
  }

  /**
   * Checks if the error is a not found error.
   *
   * @return [boolean] True if status is 404
   */
  isNotFoundError(): boolean {
    return this.status === 404;
  }

  /**
   * Checks if the error is a validation error.
   *
   * @return [boolean] True if status is 422
   */
  isValidationError(): boolean {
    return this.status === 422;
  }

  /**
   * Checks if the error is a server error.
   *
   * @return [boolean] True if status is 500 or above
   */
  isServerError(): boolean {
    return this.status >= 500;
  }
}
