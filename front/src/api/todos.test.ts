import { describe, it, expect, vi, beforeEach } from 'vitest';
import { todosApi } from './todos';
import { apiClient } from './client';
import { ApiError } from './error';
import type { Todo } from './todos';

// Mock the apiClient
vi.mock('./client', () => ({
  apiClient: {
    get: vi.fn(),
    post: vi.fn(),
    put: vi.fn(),
    delete: vi.fn(),
  },
}));

describe('todosApi', () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  const mockTodo: Todo = {
    id: 1,
    ulid: '01HXZ2M3N4P5Q6R7S8T9V0W1X2',
    ownerId: 100,
    title: 'Test Todo',
    content: 'Test content',
    dueDate: 1705276800,
    status: 'pending',
    completedAt: null,
    createdAt: 1705190400,
    updatedAt: 1705190400,
  };

  describe('getTodos', () => {
    it('should get all todos successfully', async () => {
      // Arrange
      const mockTodos = [mockTodo];
      vi.mocked(apiClient.get).mockResolvedValue({
        data: { status: 'success', data: { todos: mockTodos } },
        status: 200,
      });

      // Act
      const result = await todosApi.getTodos();

      // Assert
      expect(apiClient.get).toHaveBeenCalledWith('/api/v1/todos');
      expect(result).toEqual(mockTodos);
    });

    it('should throw ApiError when request fails', async () => {
      // Arrange
      const apiError = new ApiError('Authentication required', 401);
      vi.mocked(apiClient.get).mockRejectedValue(apiError);

      // Act & Assert
      await expect(todosApi.getTodos()).rejects.toThrow(ApiError);
      await expect(todosApi.getTodos()).rejects.toThrow('Authentication required');
    });
  });

  describe('getTodoByUlid', () => {
    it('should get a specific todo successfully', async () => {
      // Arrange
      vi.mocked(apiClient.get).mockResolvedValue({
        data: { status: 'success', data: { todo: mockTodo } },
        status: 200,
      });

      // Act
      const result = await todosApi.getTodoByUlid("test-ulid");

      // Assert
      expect(apiClient.get).toHaveBeenCalledWith('/api/v1/todos/1');
      expect(result).toEqual(mockTodo);
    });

    it('should throw ApiError when todo not found', async () => {
      // Arrange
      const apiError = new ApiError('TODO not found', 404);
      vi.mocked(apiClient.get).mockRejectedValue(apiError);

      // Act & Assert
      await expect(todosApi.getTodoByUlid("invalid-ulid")).rejects.toThrow(ApiError);
      await expect(todosApi.getTodoByUlid("invalid-ulid")).rejects.toThrow('TODO not found');
    });

    it('should throw ApiError when access denied', async () => {
      // Arrange
      const apiError = new ApiError('Access denied', 403);
      vi.mocked(apiClient.get).mockRejectedValue(apiError);

      // Act & Assert
      await expect(todosApi.getTodoByUlid("test-ulid")).rejects.toThrow(ApiError);
      await expect(todosApi.getTodoByUlid("test-ulid")).rejects.toThrow('Access denied');
    });
  });

  describe('createTodo', () => {
    it('should create a todo successfully', async () => {
      // Arrange
      const createRequest = {
        title: 'New Todo',
        content: 'New content',
        dueDate: 1705276800,
      };
      vi.mocked(apiClient.post).mockResolvedValue({
        data: { status: 'success', data: { todo: mockTodo } },
        status: 201,
      });

      // Act
      const result = await todosApi.createTodo(createRequest);

      // Assert
      expect(apiClient.post).toHaveBeenCalledWith('/api/v1/todos', createRequest);
      expect(result).toEqual(mockTodo);
    });

    it('should create a todo with minimal data', async () => {
      // Arrange
      const createRequest = {
        title: 'Minimal Todo',
      };
      vi.mocked(apiClient.post).mockResolvedValue({
        data: { status: 'success', data: { todo: mockTodo } },
        status: 201,
      });

      // Act
      const result = await todosApi.createTodo(createRequest);

      // Assert
      expect(apiClient.post).toHaveBeenCalledWith('/api/v1/todos', createRequest);
      expect(result).toEqual(mockTodo);
    });

    it('should throw ApiError when title is missing', async () => {
      // Arrange
      const apiError = new ApiError('Title is required', 400);
      vi.mocked(apiClient.post).mockRejectedValue(apiError);

      // Act & Assert
      await expect(todosApi.createTodo({ title: '' })).rejects.toThrow(ApiError);
      await expect(todosApi.createTodo({ title: '' })).rejects.toThrow('Title is required');
    });
  });

  describe('updateTodo', () => {
    it('should update a todo successfully', async () => {
      // Arrange
      const updateRequest = {
        title: 'Updated Title',
        content: 'Updated content',
      };
      const updatedTodo = { ...mockTodo, ...updateRequest };
      vi.mocked(apiClient.put).mockResolvedValue({
        data: { status: 'success', data: { todo: updatedTodo } },
        status: 200,
      });

      // Act
      const result = await todosApi.updateTodo("test-ulid", updateRequest);

      // Assert
      expect(apiClient.put).toHaveBeenCalledWith('/api/v1/todos/1', updateRequest);
      expect(result).toEqual(updatedTodo);
    });

    it('should update partial todo fields', async () => {
      // Arrange
      const updateRequest = {
        title: 'Updated Title Only',
      };
      vi.mocked(apiClient.put).mockResolvedValue({
        data: { status: 'success', data: { todo: mockTodo } },
        status: 200,
      });

      // Act
      const result = await todosApi.updateTodo("test-ulid", updateRequest);

      // Assert
      expect(apiClient.put).toHaveBeenCalledWith('/api/v1/todos/1', updateRequest);
      expect(result).toEqual(mockTodo);
    });

    it('should throw ApiError when todo not found', async () => {
      // Arrange
      const apiError = new ApiError('TODO not found', 404);
      vi.mocked(apiClient.put).mockRejectedValue(apiError);

      // Act & Assert
      await expect(todosApi.updateTodo("invalid-ulid", { title: 'Updated' })).rejects.toThrow(ApiError);
    });

    it('should throw ApiError when access denied', async () => {
      // Arrange
      const apiError = new ApiError('Access denied', 403);
      vi.mocked(apiClient.put).mockRejectedValue(apiError);

      // Act & Assert
      await expect(todosApi.updateTodo("test-ulid", { title: 'Updated' })).rejects.toThrow(ApiError);
      await expect(todosApi.updateTodo("test-ulid", { title: 'Updated' })).rejects.toThrow('Access denied');
    });
  });

  describe('deleteTodo', () => {
    it('should delete a todo successfully', async () => {
      // Arrange
      vi.mocked(apiClient.delete).mockResolvedValue({
        data: { status: 'success', message: 'TODO deleted successfully' },
        status: 200,
      });

      // Act
      await todosApi.deleteTodo("test-ulid");

      // Assert
      expect(apiClient.delete).toHaveBeenCalledWith('/api/v1/todos/1');
    });

    it('should throw ApiError when todo not found', async () => {
      // Arrange
      const apiError = new ApiError('TODO not found', 404);
      vi.mocked(apiClient.delete).mockRejectedValue(apiError);

      // Act & Assert
      await expect(todosApi.deleteTodo("invalid-ulid")).rejects.toThrow(ApiError);
      await expect(todosApi.deleteTodo("invalid-ulid")).rejects.toThrow('TODO not found');
    });

    it('should throw ApiError when access denied', async () => {
      // Arrange
      const apiError = new ApiError('Access denied', 403);
      vi.mocked(apiClient.delete).mockRejectedValue(apiError);

      // Act & Assert
      await expect(todosApi.deleteTodo("test-ulid")).rejects.toThrow(ApiError);
      await expect(todosApi.deleteTodo("test-ulid")).rejects.toThrow('Access denied');
    });
  });

  describe('toggleTodoComplete', () => {
    it('should toggle todo completion status successfully', async () => {
      // Arrange
      const completedTodo = { ...mockTodo, status: 'completed', completedAt: 1705276800 };
      vi.mocked(apiClient.put).mockResolvedValue({
        data: { status: 'success', data: { todo: completedTodo } },
        status: 200,
      });

      // Act
      const result = await todosApi.toggleTodoComplete("test-ulid");

      // Assert
      expect(apiClient.put).toHaveBeenCalledWith('/api/v1/todos/1/complete');
      expect(result).toEqual(completedTodo);
      expect(result.status).toBe('completed');
    });

    it('should throw ApiError when todo not found', async () => {
      // Arrange
      const apiError = new ApiError('TODO not found', 404);
      vi.mocked(apiClient.put).mockRejectedValue(apiError);

      // Act & Assert
      await expect(todosApi.toggleTodoComplete("invalid-ulid")).rejects.toThrow(ApiError);
      await expect(todosApi.toggleTodoComplete("invalid-ulid")).rejects.toThrow('TODO not found');
    });

    it('should throw ApiError when access denied', async () => {
      // Arrange
      const apiError = new ApiError('Access denied', 403);
      vi.mocked(apiClient.put).mockRejectedValue(apiError);

      // Act & Assert
      await expect(todosApi.toggleTodoComplete("test-ulid")).rejects.toThrow(ApiError);
      await expect(todosApi.toggleTodoComplete("test-ulid")).rejects.toThrow('Access denied');
    });
  });
});
