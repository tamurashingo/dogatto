import { describe, it, expect, vi, beforeEach } from 'vitest';
import { todoTagsApi } from './todoTags';
import type { Tag } from './tags';
import * as client from './client';

vi.mock('./client');

describe('todoTagsApi', () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  describe('getTagsForTodo', () => {
    it('should fetch tags for a todo', async () => {
      const mockTags: Tag[] = [
        {
          id: 1,
          ulid: '01HXXX1',
          name: 'Work',
          color: '#FF5733',
          createdAt: 1234567890,
          updatedAt: 1234567890,
        },
        {
          id: 2,
          ulid: '01HXXX2',
          name: 'Urgent',
          color: '#FF0000',
          createdAt: 1234567891,
          updatedAt: 1234567891,
        },
      ];

      vi.spyOn(client.apiClient, 'get').mockResolvedValue({
        data: {
          status: 'success',
          data: { tags: mockTags },
        },
        status: 200,
      });

      const result = await todoTagsApi.getTagsForTodo('01TODO1');

      expect(client.apiClient.get).toHaveBeenCalledWith('/api/v1/todos/01TODO1/tags');
      expect(result).toEqual(mockTags);
    });
  });

  describe('assignTagsToTodo', () => {
    it('should assign tags to a todo', async () => {
      const tagUlids = ['01HXXX1', '01HXXX2'];
      const mockTags: Tag[] = [
        {
          id: 1,
          ulid: '01HXXX1',
          name: 'Work',
          color: '#FF5733',
          createdAt: 1234567890,
          updatedAt: 1234567890,
        },
        {
          id: 2,
          ulid: '01HXXX2',
          name: 'Urgent',
          color: '#FF0000',
          createdAt: 1234567891,
          updatedAt: 1234567891,
        },
      ];

      vi.spyOn(client.apiClient, 'put').mockResolvedValue({
        data: {
          status: 'success',
          data: { tags: mockTags },
        },
        status: 200,
      });

      const result = await todoTagsApi.assignTagsToTodo('01TODO1', tagUlids);

      expect(client.apiClient.put).toHaveBeenCalledWith(
        '/api/v1/todos/01TODO1/tags',
        { tagUlids }
      );
      expect(result).toEqual(mockTags);
    });

    it('should replace existing tags', async () => {
      const newTagUlids = ['01HXXX3'];
      const mockTags: Tag[] = [
        {
          id: 3,
          ulid: '01HXXX3',
          name: 'Personal',
          color: '#33FF57',
          createdAt: 1234567892,
          updatedAt: 1234567892,
        },
      ];

      vi.spyOn(client.apiClient, 'put').mockResolvedValue({
        data: {
          status: 'success',
          data: { tags: mockTags },
        },
        status: 200,
      });

      const result = await todoTagsApi.assignTagsToTodo('01TODO1', newTagUlids);

      expect(result).toHaveLength(1);
      expect(result[0].ulid).toBe('01HXXX3');
    });

    it('should clear all tags when empty array provided', async () => {
      vi.spyOn(client.apiClient, 'put').mockResolvedValue({
        data: {
          status: 'success',
          data: { tags: [] },
        },
        status: 200,
      });

      const result = await todoTagsApi.assignTagsToTodo('01TODO1', []);

      expect(client.apiClient.put).toHaveBeenCalledWith(
        '/api/v1/todos/01TODO1/tags',
        { tagUlids: [] }
      );
      expect(result).toEqual([]);
    });
  });

  describe('removeTagFromTodo', () => {
    it('should remove a tag from a todo', async () => {
      vi.spyOn(client.apiClient, 'delete').mockResolvedValue({
        data: {
          status: 'success',
          message: 'Tag removed successfully',
        },
        status: 200,
      });

      await todoTagsApi.removeTagFromTodo('01TODO1', '01HXXX1');

      expect(client.apiClient.delete).toHaveBeenCalledWith('/api/v1/todos/01TODO1/tags/01HXXX1');
    });
  });
});
