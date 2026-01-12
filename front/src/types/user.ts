/**
 * User type definition.
 *
 * Represents a user entity in the system.
 */
export interface User {
  id: string;
  username: string;
  email: string;
  createdAt: string;
  updatedAt: string;
}

/**
 * User creation request payload.
 */
export interface CreateUserRequest {
  username: string;
  email: string;
  password: string;
}

/**
 * User update request payload.
 */
export interface UpdateUserRequest {
  username?: string;
  email?: string;
  password?: string;
}
