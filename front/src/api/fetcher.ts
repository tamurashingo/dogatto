import { apiClient } from './client';
import type { ApiResponse } from './client';

/**
 * Fetcher wrapper function for data fetching libraries (e.g., SWR, React Query).
 *
 * @param url [string] API endpoint path
 * @return [Promise<T>] Response data
 * @throws [ApiError] When request fails
 */
export async function fetcher<T>(url: string): Promise<T> {
  const response: ApiResponse<T> = await apiClient.get<T>(url);
  return response.data;
}

/**
 * Fetcher with body for POST requests.
 *
 * @param url [string] API endpoint path
 * @param body [unknown] Request body
 * @return [Promise<T>] Response data
 * @throws [ApiError] When request fails
 */
export async function postFetcher<T>(url: string, body: unknown): Promise<T> {
  const response: ApiResponse<T> = await apiClient.post<T>(url, body);
  return response.data;
}

/**
 * Fetcher with body for PUT requests.
 *
 * @param url [string] API endpoint path
 * @param body [unknown] Request body
 * @return [Promise<T>] Response data
 * @throws [ApiError] When request fails
 */
export async function putFetcher<T>(url: string, body: unknown): Promise<T> {
  const response: ApiResponse<T> = await apiClient.put<T>(url, body);
  return response.data;
}

/**
 * Fetcher for DELETE requests.
 *
 * @param url [string] API endpoint path
 * @return [Promise<T>] Response data
 * @throws [ApiError] When request fails
 */
export async function deleteFetcher<T>(url: string): Promise<T> {
  const response: ApiResponse<T> = await apiClient.delete<T>(url);
  return response.data;
}
