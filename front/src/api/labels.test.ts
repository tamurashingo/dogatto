import { describe, it, expect, vi, beforeEach } from 'vitest';
import { labelsApi } from './labels';
import type { Label, CreateLabelRequest, UpdateLabelRequest, LabelStats } from './labels';
import type { Tag } from './tags';
import * as client from './client';

vi.mock('./client');

describe('labelsApi', () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  describe('getLabels', () => {
    it('should fetch all labels with stats', async () => {
      const mockTag: Tag = {
        id: 1,
        ulid: '01HXXX1',
        name: 'Work',
        color: '#FF5733',
        createdAt: 1234567890,
        updatedAt: 1234567890,
      };

      const mockLabels: Label[] = [
        {
          id: 1,
          ulid: '01HLABEL1',
          name: 'Urgent Work',
          description: 'High priority work items',
          tagCount: 2,
          todoCount: 5,
          tags: [mockTag],
          createdAt: 1234567890,
          updatedAt: 1234567890,
        },
        {
          id: 2,
          ulid: '01HLABEL2',
          name: 'Personal Projects',
          description: null,
          tagCount: 1,
          todoCount: 3,
          createdAt: 1234567891,
          updatedAt: 1234567891,
        },
      ];

      const mockStats: LabelStats = {
        totalLabels: 10,
        usedLabels: 8,
        unusedLabels: 2,
      };

      vi.spyOn(client.apiClient, 'get').mockResolvedValue({
        data: {
          status: 'success',
          data: {
            labels: mockLabels,
            stats: mockStats,
          },
        },
        status: 200,
      });

      const result = await labelsApi.getLabels();

      expect(client.apiClient.get).toHaveBeenCalledWith('/api/v1/labels');
      expect(result.labels).toEqual(mockLabels);
      expect(result.stats).toEqual(mockStats);
    });

    it('should fetch labels with search parameters', async () => {
      const mockLabels: Label[] = [
        {
          id: 1,
          ulid: '01HLABEL1',
          name: 'Work',
          description: null,
          tagCount: 2,
          todoCount: 5,
          createdAt: 1234567890,
          updatedAt: 1234567890,
        },
      ];

      const mockStats: LabelStats = {
        totalLabels: 1,
        usedLabels: 1,
        unusedLabels: 0,
      };

      vi.spyOn(client.apiClient, 'get').mockResolvedValue({
        data: {
          status: 'success',
          data: {
            labels: mockLabels,
            stats: mockStats,
          },
        },
        status: 200,
      });

      const result = await labelsApi.getLabels({
        page: 2,
        per_page: 10,
        sort: 'name',
        order: 'asc',
        filter: 'used',
        search_mode: 'label_name',
        q: 'work',
      });

      expect(client.apiClient.get).toHaveBeenCalledWith(
        '/api/v1/labels?page=2&per_page=10&sort=name&order=asc&filter=used&search_mode=label_name&q=work'
      );
      expect(result.labels).toEqual(mockLabels);
      expect(result.stats).toEqual(mockStats);
    });

    it('should handle empty parameters', async () => {
      const mockLabels: Label[] = [];
      const mockStats: LabelStats = {
        totalLabels: 0,
        usedLabels: 0,
        unusedLabels: 0,
      };

      vi.spyOn(client.apiClient, 'get').mockResolvedValue({
        data: {
          status: 'success',
          data: {
            labels: mockLabels,
            stats: mockStats,
          },
        },
        status: 200,
      });

      const result = await labelsApi.getLabels({});

      expect(client.apiClient.get).toHaveBeenCalledWith('/api/v1/labels');
      expect(result.labels).toEqual([]);
    });
  });

  describe('getLabelByUlid', () => {
    it('should fetch a specific label with tags', async () => {
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

      const mockLabel: Label = {
        id: 1,
        ulid: '01HLABEL1',
        name: 'Urgent Work',
        description: 'High priority work items',
        tagCount: 2,
        todoCount: 5,
        tags: mockTags,
        createdAt: 1234567890,
        updatedAt: 1234567890,
      };

      vi.spyOn(client.apiClient, 'get').mockResolvedValue({
        data: {
          status: 'success',
          data: {
            label: mockLabel,
          },
        },
        status: 200,
      });

      const result = await labelsApi.getLabelByUlid('01HLABEL1');

      expect(client.apiClient.get).toHaveBeenCalledWith('/api/v1/labels/01HLABEL1');
      expect(result).toEqual(mockLabel);
      expect(result.tags).toHaveLength(2);
    });
  });

  describe('createLabel', () => {
    it('should create a new label', async () => {
      const newLabelData: CreateLabelRequest = {
        name: 'New Project',
        description: 'A new project label',
        tagUlids: ['01HXXX1', '01HXXX2'],
      };

      const createdLabel: Label = {
        id: 3,
        ulid: '01HLABEL3',
        name: 'New Project',
        description: 'A new project label',
        tagCount: 2,
        todoCount: 0,
        createdAt: 1234567892,
        updatedAt: 1234567892,
      };

      vi.spyOn(client.apiClient, 'post').mockResolvedValue({
        data: {
          status: 'success',
          data: { label: createdLabel },
        },
        status: 201,
      });

      const result = await labelsApi.createLabel(newLabelData);

      expect(client.apiClient.post).toHaveBeenCalledWith('/api/v1/labels', newLabelData);
      expect(result).toEqual(createdLabel);
    });

    it('should create a label without description', async () => {
      const newLabelData: CreateLabelRequest = {
        name: 'Simple Label',
        tagUlids: ['01HXXX1'],
      };

      const createdLabel: Label = {
        id: 4,
        ulid: '01HLABEL4',
        name: 'Simple Label',
        description: null,
        tagCount: 1,
        todoCount: 0,
        createdAt: 1234567893,
        updatedAt: 1234567893,
      };

      vi.spyOn(client.apiClient, 'post').mockResolvedValue({
        data: {
          status: 'success',
          data: { label: createdLabel },
        },
        status: 201,
      });

      const result = await labelsApi.createLabel(newLabelData);

      expect(result.description).toBeNull();
      expect(result.tagCount).toBe(1);
    });
  });

  describe('updateLabel', () => {
    it('should update a label with all fields', async () => {
      const updateData: UpdateLabelRequest = {
        name: 'Updated Label',
        description: 'Updated description',
        tagUlids: ['01HXXX1', '01HXXX2', '01HXXX3'],
      };

      const updatedLabel: Label = {
        id: 1,
        ulid: '01HLABEL1',
        name: 'Updated Label',
        description: 'Updated description',
        tagCount: 3,
        todoCount: 7,
        createdAt: 1234567890,
        updatedAt: 1234567900,
      };

      vi.spyOn(client.apiClient, 'put').mockResolvedValue({
        data: {
          status: 'success',
          data: { label: updatedLabel },
        },
        status: 200,
      });

      const result = await labelsApi.updateLabel('01HLABEL1', updateData);

      expect(client.apiClient.put).toHaveBeenCalledWith('/api/v1/labels/01HLABEL1', updateData);
      expect(result).toEqual(updatedLabel);
    });

    it('should update only label name', async () => {
      const updateData: UpdateLabelRequest = {
        name: 'New Name',
      };

      const updatedLabel: Label = {
        id: 1,
        ulid: '01HLABEL1',
        name: 'New Name',
        description: 'Original description',
        tagCount: 2,
        todoCount: 5,
        createdAt: 1234567890,
        updatedAt: 1234567900,
      };

      vi.spyOn(client.apiClient, 'put').mockResolvedValue({
        data: {
          status: 'success',
          data: { label: updatedLabel },
        },
        status: 200,
      });

      const result = await labelsApi.updateLabel('01HLABEL1', updateData);

      expect(result.name).toBe('New Name');
      expect(result.description).toBe('Original description');
    });

    it('should update only tags', async () => {
      const updateData: UpdateLabelRequest = {
        tagUlids: ['01HXXX1'],
      };

      const updatedLabel: Label = {
        id: 1,
        ulid: '01HLABEL1',
        name: 'Original Name',
        description: 'Original description',
        tagCount: 1,
        todoCount: 3,
        createdAt: 1234567890,
        updatedAt: 1234567900,
      };

      vi.spyOn(client.apiClient, 'put').mockResolvedValue({
        data: {
          status: 'success',
          data: { label: updatedLabel },
        },
        status: 200,
      });

      const result = await labelsApi.updateLabel('01HLABEL1', updateData);

      expect(result.tagCount).toBe(1);
      expect(result.todoCount).toBe(3);
    });
  });

  describe('deleteLabel', () => {
    it('should delete a label', async () => {
      vi.spyOn(client.apiClient, 'delete').mockResolvedValue({
        data: {
          status: 'success',
          message: 'Label deleted successfully',
        },
        status: 200,
      });

      await labelsApi.deleteLabel('01HLABEL1');

      expect(client.apiClient.delete).toHaveBeenCalledWith('/api/v1/labels/01HLABEL1');
    });
  });

  describe('estimateTodoCount', () => {
    it('should estimate TODO count for given tag ULIDs', async () => {
      vi.spyOn(client.apiClient, 'get').mockResolvedValue({
        data: {
          status: 'success',
          data: { count: 12 },
        },
        status: 200,
      });

      const result = await labelsApi.estimateTodoCount(['01HXXX1', '01HXXX2']);

      expect(client.apiClient.get).toHaveBeenCalledWith(
        '/api/v1/labels/estimate-todo-count?tag_ulids=01HXXX1%2C01HXXX2'
      );
      expect(result).toBe(12);
    });

    it('should handle single tag', async () => {
      vi.spyOn(client.apiClient, 'get').mockResolvedValue({
        data: {
          status: 'success',
          data: { count: 5 },
        },
        status: 200,
      });

      const result = await labelsApi.estimateTodoCount(['01HXXX1']);

      expect(client.apiClient.get).toHaveBeenCalledWith(
        '/api/v1/labels/estimate-todo-count?tag_ulids=01HXXX1'
      );
      expect(result).toBe(5);
    });

    it('should handle zero count', async () => {
      vi.spyOn(client.apiClient, 'get').mockResolvedValue({
        data: {
          status: 'success',
          data: { count: 0 },
        },
        status: 200,
      });

      const result = await labelsApi.estimateTodoCount(['01HXXX1', '01HXXX2', '01HXXX3']);

      expect(result).toBe(0);
    });
  });
});
