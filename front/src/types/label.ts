/**
 * Label type definition.
 *
 * Represents a label that groups multiple tags together.
 */
export interface Label {
  id: string;
  userId: string;
  name: string;
  description: string | null;
  createdAt: string;
  updatedAt: string;
  tags?: Tag[];
}

/**
 * Label creation request payload.
 */
export interface CreateLabelRequest {
  name: string;
  description?: string;
  tagIds?: string[];
}

/**
 * Label update request payload.
 */
export interface UpdateLabelRequest {
  name?: string;
  description?: string;
  tagIds?: string[];
}

/**
 * Tag type reference (to avoid circular dependency).
 */
interface Tag {
  id: string;
  name: string;
}
