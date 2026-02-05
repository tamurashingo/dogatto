import { apiClient } from './client';
import type { ApiResponse } from './client';

/**
 * Label data structure.
 */
export interface Label {
  id: number;
  ulid: string;
  name: string;
  description: string | null;
  tagCount: number;
  todoCount: number;
  createdAt: number;
  updatedAt: number;
}

/**
 * Label statistics.
 */
export interface LabelStats {
  totalLabels: number;
  usedLabels: number;
  unusedLabels: number;
}

/**
 * Label creation request payload.
 */
export interface CreateLabelRequest {
  name: string;
  description?: string;
  tagUlids: string[];
}

/**
 * Label update request payload.
 */
export interface UpdateLabelRequest {
  name?: string;
  description?: string;
  tagUlids?: string[];
}

/**
 * Label search parameters.
 */
export interface LabelSearchParams {
  page?: number;
  per_page?: number;
  sort?: 'name' | 'tag_count' | 'todo_count' | 'updated_at';
  order?: 'asc' | 'desc';
  filter?: 'all' | 'used' | 'unused';
  search_mode?: 'label_name' | 'tag_name';
  q?: string;
}

/**
 * Backend response wrapper.
 */
interface BackendResponse<T> {
  status: string;
  data: T;
}

/**
 * Label list response.
 */
interface LabelListResponse {
  labels: Label[];
  stats: LabelStats;
}

/**
 * Single label response.
 */
interface LabelResponse {
  label: Label;
}

/**
 * TODO count estimation response.
 */
interface EstimateResponse {
  count: number;
}

/**
 * Label API client.
 *
 * Provides methods for label CRUD operations and TODO count estimation.
 */
export const labelsApi = {
  /**
   * Gets all labels for the authenticated user with optional filtering.
   *
   * Retrieves a list of labels with support for pagination, search, and filtering.
   *
   * @param params [LabelSearchParams] Search and filter parameters (optional)
   * @param params.page [number] Page number (default: 1)
   * @param params.per_page [number] Items per page (default: 20, max: 100)
   * @param params.sort [string] Sort field (name, tag_count, todo_count, updated_at)
   * @param params.order [string] Sort order (asc, desc)
   * @param params.filter [string] Filter type (all, used, unused)
   * @param params.search_mode [string] Search mode (label_name, tag_name)
   * @param params.q [string] Search query
   * @return [Promise<{labels: Label[], stats: LabelStats}>] Labels and statistics
   * @throws [ApiError] When request fails
   */
  async getLabels(params?: LabelSearchParams): Promise<{ labels: Label[]; stats: LabelStats }> {
    const queryParams = new URLSearchParams();
    if (params) {
      if (params.page) queryParams.append('page', params.page.toString());
      if (params.per_page) queryParams.append('per_page', params.per_page.toString());
      if (params.sort) queryParams.append('sort', params.sort);
      if (params.order) queryParams.append('order', params.order);
      if (params.filter) queryParams.append('filter', params.filter);
      if (params.search_mode) queryParams.append('search_mode', params.search_mode);
      if (params.q) queryParams.append('q', params.q);
    }
    
    const url = `/api/v1/labels${queryParams.toString() ? `?${queryParams.toString()}` : ''}`;
    const response: ApiResponse<BackendResponse<LabelListResponse>> = 
      await apiClient.get<BackendResponse<LabelListResponse>>(url);
    return (response.data as any).data;
  },

  /**
   * Gets a specific label by ULID.
   *
   * Retrieves detailed information for a single label.
   * Only returns labels owned by the authenticated user.
   *
   * @param ulid [string] Label ULID
   * @return [Promise<Label>] Label data
   * @throws [ApiError] When label not found or access denied
   */
  async getLabelByUlid(ulid: string): Promise<Label> {
    const response: ApiResponse<BackendResponse<LabelResponse>> = 
      await apiClient.get<BackendResponse<LabelResponse>>(`/api/v1/labels/${ulid}`);
    return (response.data as any).data.label;
  },

  /**
   * Creates a new label.
   *
   * Creates a new label for the authenticated user.
   * At least one tag is required.
   *
   * @param data [CreateLabelRequest] Label creation data
   * @param data.name [string] Label name (required, 1-100 characters)
   * @param data.description [string] Label description (optional, max 1000 characters)
   * @param data.tagUlids [string[]] Tag ULIDs (required, at least 1)
   * @return [Promise<Label>] Created label data
   * @throws [ApiError] When creation fails (e.g., validation error, duplicate name)
   */
  async createLabel(data: CreateLabelRequest): Promise<Label> {
    const response: ApiResponse<BackendResponse<LabelResponse>> = 
      await apiClient.post<BackendResponse<LabelResponse>>('/api/v1/labels', data);
    return (response.data as any).data.label;
  },

  /**
   * Updates an existing label.
   *
   * Updates a label's information.
   * Only updates labels owned by the authenticated user.
   *
   * @param ulid [string] Label ULID
   * @param data [UpdateLabelRequest] Label update data
   * @param data.name [string] Updated name (optional, 1-100 characters)
   * @param data.description [string] Updated description (optional, max 1000 characters)
   * @param data.tagUlids [string[]] Updated tag ULIDs (optional, at least 1 if provided)
   * @return [Promise<Label>] Updated label data
   * @throws [ApiError] When update fails or access denied
   */
  async updateLabel(ulid: string, data: UpdateLabelRequest): Promise<Label> {
    const response: ApiResponse<BackendResponse<LabelResponse>> = 
      await apiClient.put<BackendResponse<LabelResponse>>(`/api/v1/labels/${ulid}`, data);
    return (response.data as any).data.label;
  },

  /**
   * Deletes a label.
   *
   * Permanently deletes a label.
   * Only deletes labels owned by the authenticated user.
   * Associated label_tags records are deleted automatically (CASCADE).
   *
   * @param ulid [string] Label ULID
   * @return [Promise<void>] No return value
   * @throws [ApiError] When deletion fails or access denied
   */
  async deleteLabel(ulid: string): Promise<void> {
    await apiClient.delete(`/api/v1/labels/${ulid}`);
  },

  /**
   * Estimates TODO count for given tag ULIDs (AND condition).
   *
   * Counts TODOs that have ALL of the specified tags.
   * Used for preview when creating/editing labels.
   *
   * @param tagUlids [string[]] Tag ULIDs to check (at least 1 required)
   * @return [Promise<number>] Estimated TODO count
   * @throws [ApiError] When request fails
   */
  async estimateTodoCount(tagUlids: string[]): Promise<number> {
    const queryParams = new URLSearchParams();
    queryParams.append('tag_ulids', tagUlids.join(','));
    
    const response: ApiResponse<BackendResponse<EstimateResponse>> = 
      await apiClient.get<BackendResponse<EstimateResponse>>(
        `/api/v1/labels/estimate-todo-count?${queryParams.toString()}`
      );
    return (response.data as any).data.count;
  },
};
