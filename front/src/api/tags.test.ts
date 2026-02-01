import { describe, it, expect, vi, beforeEach } from 'vitest';
import { tagsApi } from './tags';
import type { Tag, CreateTagRequest, UpdateTagRequest, TagStatistics } from './tags';
import * as client from './client';

vi.mock('./client');

describe('tagsApi', () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  describe('getTags', () => {
    it('should fetch all tags', async () => {
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
          name: 'Personal',
          color: '#33FF57',
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
        statusText: 'OK',
        headers: {},
      });

      const result = await tagsApi.getTags();

      expect(client.apiClient.get).toHaveBeenCalledWith('/api/v1/tags');
      expect(result).toEqual(mockTags);
    });
  });

  describe('getTagByUlid', () => {
    it('should fetch a specific tag with statistics', async () => {
      const mockTag: Tag = {
        id: 1,
        ulid: '01HXXX1',
        name: 'Work',
        color: '#FF5733',
        createdAt: 1234567890,
        updatedAt: 1234567890,
      };

      const mockStatistics: TagStatistics = {
        total: 10,
        active: 7,
        completed: 3,
      };

      vi.spyOn(client.apiClient, 'get').mockResolvedValue({
        data: {
          status: 'success',
          data: {
            tag: mockTag,
            statistics: mockStatistics,
          },
        },
        status: 200,
        statusText: 'OK',
        headers: {},
      });

      const result = await tagsApi.getTagByUlid('01HXXX1');

      expect(client.apiClient.get).toHaveBeenCalledWith('/api/v1/tags/01HXXX1');
      expect(result.tag).toEqual(mockTag);
      expect(result.statistics).toEqual(mockStatistics);
    });
  });

  describe('createTag', () => {
    it('should create a new tag', async () => {
      const newTagData: CreateTagRequest = {
        name: 'Urgent',
        color: '#FF0000',
      };

      const createdTag: Tag = {
        id: 3,
        ulid: '01HXXX3',
        name: 'Urgent',
        color: '#FF0000',
        createdAt: 1234567892,
        updatedAt: 1234567892,
      };

      vi.spyOn(client.apiClient, 'post').mockResolvedValue({
        data: {
          status: 'success',
          data: { tag: createdTag },
        },
        status: 201,
        statusText: 'Created',
        headers: {},
      });

      const result = await tagsApi.createTag(newTagData);

      expect(client.apiClient.post).toHaveBeenCalledWith('/api/v1/tags', newTagData);
      expect(result).toEqual(createdTag);
    });

    it('should create a tag with default color', async () => {
      const newTagData: CreateTagRequest = {
        name: 'NoColor',
      };

      const createdTag: Tag = {
        id: 4,
        ulid: '01HXXX4',
        name: 'NoColor',
        color: '#3B82F6',
        createdAt: 1234567893,
        updatedAt: 1234567893,
      };

      vi.spyOn(client.apiClient, 'post').mockResolvedValue({
        data: {
          status: 'success',
          data: { tag: createdTag },
        },
        status: 201,
        statusText: 'Created',
        headers: {},
      });

      const result = await tagsApi.createTag(newTagData);

      expect(client.apiClient.post).toHaveBeenCalledWith('/api/v1/tags', newTagData);
      expect(result.color).toBe('#3B82F6');
    });
  });

  describe('updateTag', () => {
    it('should update a tag', async () => {
      const updateData: UpdateTagRequest = {
        name: 'Updated Work',
        color: '#0000FF',
      };

      const updatedTag: Tag = {
        id: 1,
        ulid: '01HXXX1',
        name: 'Updated Work',
        color: '#0000FF',
        createdAt: 1234567890,
        updatedAt: 1234567900,
      };

      vi.spyOn(client.apiClient, 'put').mockResolvedValue({
        data: {
          status: 'success',
          data: { tag: updatedTag },
        },
        status: 200,
        statusText: 'OK',
        headers: {},
      });

      const result = await tagsApi.updateTag('01HXXX1', updateData);

      expect(client.apiClient.put).toHaveBeenCalledWith('/api/v1/tags/01HXXX1', updateData);
      expect(result).toEqual(updatedTag);
    });

    it('should update only tag name', async () => {
      const updateData: UpdateTagRequest = {
        name: 'New Name',
      };

      const updatedTag: Tag = {
        id: 1,
        ulid: '01HXXX1',
        name: 'New Name',
        color: '#FF5733',
        createdAt: 1234567890,
        updatedAt: 1234567900,
      };

      vi.spyOn(client.apiClient, 'put').mockResolvedValue({
        data: {
          status: 'success',
          data: { tag: updatedTag },
        },
        status: 200,
        statusText: 'OK',
        headers: {},
      });

      const result = await tagsApi.updateTag('01HXXX1', updateData);

      expect(result.name).toBe('New Name');
      expect(result.color).toBe('#FF5733');
    });
  });

  describe('deleteTag', () => {
    it('should delete a tag', async () => {
      vi.spyOn(client.apiClient, 'delete').mockResolvedValue({
        data: {
          status: 'success',
          message: 'Tag deleted successfully',
        },
        status: 200,
        statusText: 'OK',
        headers: {},
      });

      await tagsApi.deleteTag('01HXXX1');

      expect(client.apiClient.delete).toHaveBeenCalledWith('/api/v1/tags/01HXXX1');
    });
  });
});
