import { ApiError } from './error';

/**
 * API response wrapper.
 */
export interface ApiResponse<T> {
  data: T;
  status: number;
}

/**
 * API error response format.
 */
interface ErrorResponse {
  error: {
    message: string;
    code?: string;
    details?: unknown;
  };
}

/**
 * Base API client class.
 *
 * Provides common functionality for making HTTP requests to the backend API.
 */
export class ApiClient {
  private baseUrl: string;
  private defaultHeaders: HeadersInit;

  /**
   * Creates a new ApiClient instance.
   *
   * @param baseUrl [string] Base URL for the API (default: '/api/v1')
   */
  constructor(baseUrl: string = '/api/v1') {
    this.baseUrl = baseUrl;
    this.defaultHeaders = {
      'Content-Type': 'application/json',
    };
  }

  /**
   * Sets the authorization token for all requests.
   *
   * @param token [string] JWT or session token
   */
  setAuthToken(token: string): void {
    this.defaultHeaders = {
      ...this.defaultHeaders,
      Authorization: `Bearer ${token}`,
    };
  }

  /**
   * Clears the authorization token.
   */
  clearAuthToken(): void {
    const { Authorization, ...rest } = this.defaultHeaders as Record<string, string>;
    this.defaultHeaders = rest;
  }

  /**
   * Makes a GET request.
   *
   * @param path [string] API endpoint path
   * @param params [Record<string, string>] Query parameters (optional)
   * @return [Promise<ApiResponse<T>>] Response data
   * @throws [ApiError] When request fails
   */
  async get<T>(path: string, params?: Record<string, string>): Promise<ApiResponse<T>> {
    const url = this.buildUrl(path, params);
    return this.request<T>(url, { method: 'GET' });
  }

  /**
   * Makes a POST request.
   *
   * @param path [string] API endpoint path
   * @param body [unknown] Request body (optional)
   * @return [Promise<ApiResponse<T>>] Response data
   * @throws [ApiError] When request fails
   */
  async post<T>(path: string, body?: unknown): Promise<ApiResponse<T>> {
    const url = this.buildUrl(path);
    return this.request<T>(url, {
      method: 'POST',
      body: body ? JSON.stringify(body) : undefined,
    });
  }

  /**
   * Makes a PUT request.
   *
   * @param path [string] API endpoint path
   * @param body [unknown] Request body (optional)
   * @return [Promise<ApiResponse<T>>] Response data
   * @throws [ApiError] When request fails
   */
  async put<T>(path: string, body?: unknown): Promise<ApiResponse<T>> {
    const url = this.buildUrl(path);
    return this.request<T>(url, {
      method: 'PUT',
      body: body ? JSON.stringify(body) : undefined,
    });
  }

  /**
   * Makes a PATCH request.
   *
   * @param path [string] API endpoint path
   * @param body [unknown] Request body (optional)
   * @return [Promise<ApiResponse<T>>] Response data
   * @throws [ApiError] When request fails
   */
  async patch<T>(path: string, body?: unknown): Promise<ApiResponse<T>> {
    const url = this.buildUrl(path);
    return this.request<T>(url, {
      method: 'PATCH',
      body: body ? JSON.stringify(body) : undefined,
    });
  }

  /**
   * Makes a DELETE request.
   *
   * @param path [string] API endpoint path
   * @return [Promise<ApiResponse<T>>] Response data
   * @throws [ApiError] When request fails
   */
  async delete<T>(path: string): Promise<ApiResponse<T>> {
    const url = this.buildUrl(path);
    return this.request<T>(url, { method: 'DELETE' });
  }

  /**
   * Builds full URL with query parameters.
   *
   * @param path [string] API endpoint path
   * @param params [Record<string, string>] Query parameters (optional)
   * @return [string] Full URL
   */
  private buildUrl(path: string, params?: Record<string, string>): string {
    const url = new URL(path, window.location.origin + this.baseUrl);
    if (params) {
      Object.entries(params).forEach(([key, value]) => {
        url.searchParams.append(key, value);
      });
    }
    return url.toString();
  }

  /**
   * Makes an HTTP request and handles errors.
   *
   * @param url [string] Full URL
   * @param options [RequestInit] Fetch options
   * @return [Promise<ApiResponse<T>>] Response data
   * @throws [ApiError] When request fails
   */
  private async request<T>(url: string, options: RequestInit): Promise<ApiResponse<T>> {
    try {
      const response = await fetch(url, {
        ...options,
        headers: {
          ...this.defaultHeaders,
          ...options.headers,
        },
        credentials: 'same-origin',
      });

      const contentType = response.headers.get('content-type');
      const isJson = contentType?.includes('application/json');

      if (!response.ok) {
        if (isJson) {
          const errorData = (await response.json()) as ErrorResponse;
          throw new ApiError(
            errorData.error.message,
            response.status,
            errorData.error.code,
            errorData.error.details
          );
        } else {
          throw new ApiError(
            response.statusText || 'Request failed',
            response.status
          );
        }
      }

      const data = isJson ? await response.json() : await response.text();
      return {
        data: data as T,
        status: response.status,
      };
    } catch (error) {
      if (error instanceof ApiError) {
        throw error;
      }
      throw new ApiError(
        error instanceof Error ? error.message : 'Network error',
        0,
        'NETWORK_ERROR'
      );
    }
  }
}

export const apiClient = new ApiClient();
