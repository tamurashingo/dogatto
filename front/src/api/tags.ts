import { apiClient } from './client';
import type { ApiResponse } from './client';

/**
 * Tag data structure.
 */
export interface Tag {
  id: number;
  ulid: string;
  name: string;
  color: string;
  createdAt: number;
  updatedAt: number;
}

/**
 * Tag creation request payload.
 */
export interface CreateTagRequest {
  name: string;
  color?: string;
}

/**
 * Tag update request payload.
 */
export interface UpdateTagRequest {
  name?: string;
  color?: string;
}

/**
 * Backend response wrapper.
 */
interface BackendResponse<T> {
  status: string;
  data: T;
}

/**
 * Tag list response.
 */
interface TagListResponse {
  tags: Tag[];
}

/**
 * Single tag response.
 */
interface TagResponse {
  tag: Tag;
}

/**
 * Tag statistics response.
 */
export interface TagStatistics {
  total: number;
  active: number;
  completed: number;
}

/**
 * Tag with statistics response.
 */
interface TagWithStatisticsResponse {
  tag: Tag;
  statistics: TagStatistics;
}

/**
 * Tag API client.
 *
 * Provides methods for tag CRUD operations.
 */
export const tagsApi = {
  /**
   * Gets all tags for the authenticated user.
   *
   * Retrieves a list of all tags belonging to the current user.
   *
   * @return [Promise<Tag[]>] List of tags
   * @throws [ApiError] When request fails
   */
  async getTags(): Promise<Tag[]> {
    const response: ApiResponse<BackendResponse<TagListResponse>> = 
      await apiClient.get<BackendResponse<TagListResponse>>('/api/v1/tags');
    return (response.data as any).data.tags;
  },

  /**
   * Gets a specific tag by ULID with statistics.
   *
   * Retrieves detailed information for a single tag including usage statistics.
   * Only returns tags owned by the authenticated user.
   *
   * @param ulid [string] Tag ULID
   * @return [Promise<{tag: Tag, statistics: TagStatistics}>] Tag data with statistics
   * @throws [ApiError] When tag not found or access denied
   */
  async getTagByUlid(ulid: string): Promise<{ tag: Tag; statistics: TagStatistics }> {
    const response: ApiResponse<BackendResponse<TagWithStatisticsResponse>> = 
      await apiClient.get<BackendResponse<TagWithStatisticsResponse>>(`/api/v1/tags/${ulid}`);
    return (response.data as any).data;
  },

  /**
   * Creates a new tag.
   *
   * Creates a new tag for the authenticated user.
   *
   * @param data [CreateTagRequest] Tag creation data
   * @param data.name [string] Tag name (required)
   * @param data.color [string] Tag color in hex format (optional, default: #3B82F6)
   * @return [Promise<Tag>] Created tag data
   * @throws [ApiError] When creation fails (e.g., validation error)
   */
  async createTag(data: CreateTagRequest): Promise<Tag> {
    const response: ApiResponse<BackendResponse<TagResponse>> = 
      await apiClient.post<BackendResponse<TagResponse>>('/api/v1/tags', data);
    return (response.data as any).data.tag;
  },

  /**
   * Updates an existing tag.
   *
   * Updates a tag's information.
   * Only updates tags owned by the authenticated user.
   *
   * @param ulid [string] Tag ULID
   * @param data [UpdateTagRequest] Tag update data
   * @param data.name [string] Updated name (optional)
   * @param data.color [string] Updated color in hex format (optional)
   * @return [Promise<Tag>] Updated tag data
   * @throws [ApiError] When update fails or access denied
   */
  async updateTag(ulid: string, data: UpdateTagRequest): Promise<Tag> {
    const response: ApiResponse<BackendResponse<TagResponse>> = 
      await apiClient.put<BackendResponse<TagResponse>>(`/api/v1/tags/${ulid}`, data);
    return (response.data as any).data.tag;
  },

  /**
   * Deletes a tag.
   *
   * Permanently deletes a tag.
   * Only deletes tags owned by the authenticated user.
   *
   * @param ulid [string] Tag ULID
   * @return [Promise<void>] No return value
   * @throws [ApiError] When deletion fails or access denied
   */
  async deleteTag(ulid: string): Promise<void> {
    await apiClient.delete(`/api/v1/tags/${ulid}`);
  },
};
