import { apiClient } from './client';
import type { ApiResponse } from './client';
import type { Tag } from './tags';

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
 * Tag assignment request payload.
 */
export interface AssignTagsRequest {
  tagUlids: string[];
}

/**
 * TODO-Tag API client.
 *
 * Provides methods for managing tags on TODOs.
 */
export const todoTagsApi = {
  /**
   * Gets all tags assigned to a TODO.
   *
   * Retrieves a list of tags associated with a specific TODO.
   *
   * @param todoUlid [string] TODO ULID
   * @return [Promise<Tag[]>] List of tags
   * @throws [ApiError] When request fails or access denied
   */
  async getTagsForTodo(todoUlid: string): Promise<Tag[]> {
    const response: ApiResponse<BackendResponse<TagListResponse>> = 
      await apiClient.get<BackendResponse<TagListResponse>>(`/api/v1/todos/${todoUlid}/tags`);
    return (response.data as any).data.tags;
  },

  /**
   * Assigns tags to a TODO.
   *
   * Replaces all existing tags with the provided list of tag ULIDs.
   * Maximum 10 tags per TODO.
   *
   * @param todoUlid [string] TODO ULID
   * @param tagUlids [string[]] Array of tag ULIDs to assign
   * @return [Promise<Tag[]>] Updated list of tags
   * @throws [ApiError] When assignment fails or access denied
   */
  async assignTagsToTodo(todoUlid: string, tagUlids: string[]): Promise<Tag[]> {
    const response: ApiResponse<BackendResponse<TagListResponse>> = 
      await apiClient.put<BackendResponse<TagListResponse>>(
        `/api/v1/todos/${todoUlid}/tags`,
        { tagUlids }
      );
    return (response.data as any).data.tags;
  },

  /**
   * Removes a specific tag from a TODO.
   *
   * Removes the association between a TODO and a tag.
   *
   * @param todoUlid [string] TODO ULID
   * @param tagUlid [string] Tag ULID to remove
   * @return [Promise<void>] No return value
   * @throws [ApiError] When removal fails or access denied
   */
  async removeTagFromTodo(todoUlid: string, tagUlid: string): Promise<void> {
    await apiClient.delete(`/api/v1/todos/${todoUlid}/tags/${tagUlid}`);
  },
};
