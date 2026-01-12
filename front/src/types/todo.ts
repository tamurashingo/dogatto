/**
 * Todo type definition.
 *
 * Represents a todo item in the system.
 */
export interface Todo {
  id: string;
  userId: string;
  title: string;
  description: string | null;
  status: TodoStatus;
  dueDate: string | null;
  createdAt: string;
  updatedAt: string;
  tags?: Tag[];
}

/**
 * Todo status type.
 */
export type TodoStatus = 'pending' | 'in_progress' | 'completed' | 'archived';

/**
 * Todo creation request payload.
 */
export interface CreateTodoRequest {
  title: string;
  description?: string;
  status?: TodoStatus;
  dueDate?: string;
  tagIds?: string[];
}

/**
 * Todo update request payload.
 */
export interface UpdateTodoRequest {
  title?: string;
  description?: string;
  status?: TodoStatus;
  dueDate?: string;
  tagIds?: string[];
}

/**
 * Tag type reference (to avoid circular dependency).
 */
interface Tag {
  id: string;
  name: string;
}
