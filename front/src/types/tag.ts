/**
 * Tag type definition.
 *
 * Represents a tag that can be attached to todos.
 */
export interface Tag {
  id: string;
  userId: string;
  name: string;
  color: string | null;
  createdAt: string;
  updatedAt: string;
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
