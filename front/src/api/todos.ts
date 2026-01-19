import { apiClient } from './client';
import type { ApiResponse } from './client';

/**
 * TODO data structure.
 */
export interface Todo {
  id: number;
  ulid: string;
  ownerId: number;
  title: string;
  content: string | null;
  dueDate: number | null;
  status: string;
  completedAt: number | null;
  createdAt: number;
  updatedAt: number;
}

/**
 * TODO creation request payload.
 */
export interface CreateTodoRequest {
  title: string;
  content?: string;
  dueDate?: number;
}

/**
 * TODO update request payload.
 */
export interface UpdateTodoRequest {
  title?: string;
  content?: string;
  dueDate?: number;
}

/**
 * Backend response wrapper.
 */
interface BackendResponse<T> {
  status: string;
  data: T;
}

/**
 * TODO list response.
 */
interface TodoListResponse {
  todos: Todo[];
}

/**
 * Single TODO response.
 */
interface TodoResponse {
  todo: Todo;
}

/**
 * TODO API client.
 *
 * Provides methods for TODO CRUD operations.
 */
export const todosApi = {
  /**
   * Gets all todos for the authenticated user.
   *
   * Retrieves a list of all todos belonging to the current user.
   *
   * @return [Promise<Todo[]>] List of todos
   * @throws [ApiError] When request fails
   */
  async getTodos(): Promise<Todo[]> {
    const response: ApiResponse<BackendResponse<TodoListResponse>> = 
      await apiClient.get<BackendResponse<TodoListResponse>>('/api/v1/todos');
    return (response.data as any).data.todos;
  },

  /**
   * Gets a specific todo by ULID.
   *
   * Retrieves detailed information for a single todo.
   * Only returns todos owned by the authenticated user.
   *
   * @param ulid [string] TODO ULID
   * @return [Promise<Todo>] TODO data
   * @throws [ApiError] When todo not found or access denied
   */
  async getTodoByUlid(ulid: string): Promise<Todo> {
    const response: ApiResponse<BackendResponse<TodoResponse>> = 
      await apiClient.get<BackendResponse<TodoResponse>>(`/api/v1/todos/${ulid}`);
    return (response.data as any).data.todo;
  },

  /**
   * Creates a new todo.
   *
   * Creates a new todo for the authenticated user.
   *
   * @param data [CreateTodoRequest] TODO creation data
   * @param data.title [string] TODO title (required)
   * @param data.content [string] TODO description (optional)
   * @param data.dueDate [number] Due date as Unix timestamp (optional)
   * @return [Promise<Todo>] Created todo data
   * @throws [ApiError] When creation fails (e.g., validation error)
   */
  async createTodo(data: CreateTodoRequest): Promise<Todo> {
    const response: ApiResponse<BackendResponse<TodoResponse>> = 
      await apiClient.post<BackendResponse<TodoResponse>>('/api/v1/todos', data);
    return (response.data as any).data.todo;
  },

  /**
   * Updates an existing todo.
   *
   * Updates a todo's information.
   * Only updates todos owned by the authenticated user.
   *
   * @param ulid [string] TODO ULID
   * @param data [UpdateTodoRequest] TODO update data
   * @param data.title [string] Updated title (optional)
   * @param data.content [string] Updated description (optional)
   * @param data.dueDate [number] Updated due date as Unix timestamp (optional)
   * @return [Promise<Todo>] Updated todo data
   * @throws [ApiError] When update fails or access denied
   */
  async updateTodo(ulid: string, data: UpdateTodoRequest): Promise<Todo> {
    const response: ApiResponse<BackendResponse<TodoResponse>> = 
      await apiClient.put<BackendResponse<TodoResponse>>(`/api/v1/todos/${ulid}`, data);
    return (response.data as any).data.todo;
  },

  /**
   * Deletes a todo.
   *
   * Permanently deletes a todo.
   * Only deletes todos owned by the authenticated user.
   *
   * @param ulid [string] TODO ULID
   * @return [Promise<void>] No return value
   * @throws [ApiError] When deletion fails or access denied
   */
  async deleteTodo(ulid: string): Promise<void> {
    await apiClient.delete(`/api/v1/todos/${ulid}`);
  },

  /**
   * Toggles todo completion status.
   *
   * Toggles between 'pending' and 'completed' status.
   * Only affects todos owned by the authenticated user.
   *
   * @param ulid [string] TODO ULID
   * @return [Promise<Todo>] Updated todo data
   * @throws [ApiError] When toggle fails or access denied
   */
  async toggleTodoComplete(ulid: string): Promise<Todo> {
    const response: ApiResponse<BackendResponse<TodoResponse>> = 
      await apiClient.put<BackendResponse<TodoResponse>>(`/api/v1/todos/${ulid}/complete`);
    return (response.data as any).data.todo;
  },
};
