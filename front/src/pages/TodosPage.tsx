import { useState, useEffect } from 'react';
import { Link } from 'react-router-dom';
import Header from '../components/Header';
import { todosApi } from '../api/todos';
import type { Todo } from '../api/todos';
import { ApiError } from '../api/error';
import '../styles/todos.css';

/**
 * Todos page component.
 *
 * Displays list of todos with create, edit, delete, and toggle complete functionality.
 */
export default function TodosPage(): React.JSX.Element {
  const [todos, setTodos] = useState<Todo[]>([]);
  const [isLoading, setIsLoading] = useState(true);
  const [error, setError] = useState('');

  /**
   * Fetches todos from API.
   */
  const fetchTodos = async () => {
    try {
      setIsLoading(true);
      setError('');
      const data = await todosApi.getTodos();
      setTodos(data);
    } catch (err) {
      if (err instanceof ApiError) {
        setError(err.message);
      } else {
        setError('Failed to load todos');
      }
    } finally {
      setIsLoading(false);
    }
  };

  /**
   * Toggles todo completion status.
   *
   * @param ulid [string] TODO ULID
   */
  const handleToggleComplete = async (ulid: string) => {
    try {
      const updatedTodo = await todosApi.toggleTodoComplete(ulid);
      setTodos(todos.map(todo => todo.ulid === ulid ? updatedTodo : todo));
    } catch (err) {
      if (err instanceof ApiError) {
        setError(err.message);
      } else {
        setError('Failed to update todo');
      }
    }
  };

  /**
   * Deletes a todo.
   *
   * @param ulid [string] TODO ULID
   */
  const handleDelete = async (ulid: string) => {
    if (!window.confirm('Are you sure you want to delete this todo?')) {
      return;
    }

    try {
      await todosApi.deleteTodo(ulid);
      setTodos(todos.filter(todo => todo.ulid !== ulid));
    } catch (err) {
      if (err instanceof ApiError) {
        setError(err.message);
      } else {
        setError('Failed to delete todo');
      }
    }
  };

  /**
   * Formats Unix timestamp to date string.
   *
   * @param timestamp [number | null] Unix timestamp
   * @return [string] Formatted date string
   */
  const formatDate = (timestamp: number | null): string => {
    if (!timestamp) return '';
    const date = new Date(timestamp * 1000);
    return date.toLocaleDateString();
  };

  /**
   * Checks if todo is overdue.
   *
   * @param dueDate [number | null] Due date as Unix timestamp
   * @param status [string] TODO status
   * @return [boolean] True if overdue
   */
  const isOverdue = (dueDate: number | null, status: string): boolean => {
    if (!dueDate || status === 'completed') return false;
    const now = Math.floor(Date.now() / 1000);
    return dueDate < now;
  };

  useEffect(() => {
    fetchTodos();
  }, []);

  return (
    <div className="todos-page">
      <Header />
      <main className="main-content">
        <div className="todos-header">
          <h1>My TODOs</h1>
          <Link to="/todos/new" className="btn-create">
            Create TODO
          </Link>
        </div>

        {error && (
          <div className="error-message">
            {error}
          </div>
        )}

        {isLoading ? (
          <div className="loading">Loading todos...</div>
        ) : todos.length === 0 ? (
          <div className="empty-state">
            <p>No todos yet. Create your first todo to get started!</p>
            <Link to="/todos/new" className="btn-create-large">
              Create Your First TODO
            </Link>
          </div>
        ) : (
          <div className="todos-grid">
            {todos.map(todo => (
              <div
                key={todo.ulid}
                className={`todo-card ${todo.status === 'completed' ? 'completed' : ''} ${isOverdue(todo.dueDate, todo.status) ? 'overdue' : ''}`}
              >
                <div className="todo-header">
                  <input
                    type="checkbox"
                    checked={todo.status === 'completed'}
                    onChange={() => handleToggleComplete(todo.ulid)}
                    className="todo-checkbox"
                  />
                  <h3 className="todo-title">{todo.title}</h3>
                </div>

                {todo.content && (
                  <p className="todo-content">{todo.content}</p>
                )}

                {todo.dueDate && (
                  <div className="todo-due-date">
                    Due: {formatDate(todo.dueDate)}
                  </div>
                )}

                <div className="todo-actions">
                  <Link to={`/todos/${todo.ulid}`} className="btn-view">
                    View
                  </Link>
                  <Link 
                    to={`/todos/${todo.ulid}/edit`}
                    state={{ from: 'list' }}
                    className="btn-edit"
                  >
                    Edit
                  </Link>
                  <button
                    onClick={() => handleDelete(todo.ulid)}
                    className="btn-delete"
                    type="button"
                  >
                    Delete
                  </button>
                </div>
              </div>
            ))}
          </div>
        )}
      </main>
    </div>
  );
}
