import { useState, useEffect } from 'react';
import { useNavigate, useParams, Link } from 'react-router-dom';
import Header from '../components/Header';
import { todosApi } from '../api/todos';
import type { Todo } from '../api/todos';
import { ApiError } from '../api/error';
import '../styles/todo-detail.css';

/**
 * TODO detail page component.
 *
 * Displays detailed information about a single todo.
 */
export default function TodoDetailPage(): React.JSX.Element {
  const navigate = useNavigate();
  const { ulid } = useParams<{ ulid: string }>();
  const [todo, setTodo] = useState<Todo | null>(null);
  const [isLoading, setIsLoading] = useState(true);
  const [error, setError] = useState('');

  /**
   * Formats Unix timestamp to readable date and time string.
   *
   * @param timestamp [number | null] Unix timestamp
   * @return [string] Formatted date/time string
   */
  const formatDateTime = (timestamp: number | null): string => {
    if (!timestamp) return 'N/A';
    const date = new Date(timestamp * 1000);
    return date.toLocaleString();
  };

  /**
   * Formats Unix timestamp to date string.
   *
   * @param timestamp [number | null] Unix timestamp
   * @return [string] Formatted date string
   */
  const formatDate = (timestamp: number | null): string => {
    if (!timestamp) return 'N/A';
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

  /**
   * Fetches todo data.
   */
  useEffect(() => {
    const fetchTodo = async () => {
      if (!ulid) {
        setError('Invalid TODO ULID');
        setIsLoading(false);
        return;
      }

      try {
        const data = await todosApi.getTodoByUlid(ulid);
        setTodo(data);
      } catch (err) {
        if (err instanceof ApiError) {
          setError(err.message);
        } else {
          setError('Failed to load todo');
        }
      } finally {
        setIsLoading(false);
      }
    };

    fetchTodo();
  }, [ulid]);

  /**
   * Toggles todo completion status.
   */
  const handleToggleComplete = async () => {
    if (!todo) return;

    try {
      const updatedTodo = await todosApi.toggleTodoComplete(todo.ulid);
      setTodo(updatedTodo);
    } catch (err) {
      if (err instanceof ApiError) {
        setError(err.message);
      } else {
        setError('Failed to update todo');
      }
    }
  };

  /**
   * Deletes the todo and navigates back to list.
   */
  const handleDelete = async () => {
    if (!todo || !window.confirm('Are you sure you want to delete this todo?')) {
      return;
    }

    try {
      await todosApi.deleteTodo(todo.ulid);
      navigate('/todos');
    } catch (err) {
      if (err instanceof ApiError) {
        setError(err.message);
      } else {
        setError('Failed to delete todo');
      }
    }
  };

  if (isLoading) {
    return (
      <div className="todo-detail-page">
        <Header />
        <main className="detail-content">
          <div className="loading">Loading todo...</div>
        </main>
      </div>
    );
  }

  if (error || !todo) {
    return (
      <div className="todo-detail-page">
        <Header />
        <main className="detail-content">
          <div className="error-message">
            {error || 'Todo not found'}
          </div>
          <Link to="/todos" className="btn-back">
            Back to List
          </Link>
        </main>
      </div>
    );
  }

  return (
    <div className="todo-detail-page">
      <Header />
      <main className="detail-content">
        <div className="detail-container">
          <div className="detail-header">
            <Link to="/todos" className="btn-back-inline">
              ← Back to List
            </Link>
          </div>

          <div className={`todo-detail ${todo.status === 'completed' ? 'completed' : ''} ${isOverdue(todo.dueDate, todo.status) ? 'overdue' : ''}`}>
            <div className="status-badge">
              <span className={`badge ${todo.status}`}>
                {todo.status === 'completed' ? 'Completed' : 'Pending'}
              </span>
              {isOverdue(todo.dueDate, todo.status) && (
                <span className="badge overdue-badge">Overdue</span>
              )}
            </div>

            <h1 className="todo-title">{todo.title}</h1>

            {todo.content && (
              <div className="todo-section">
                <h2>Description</h2>
                <p className="todo-content">{todo.content}</p>
              </div>
            )}

            <div className="todo-section">
              <h2>Details</h2>
              <div className="detail-grid">
                <div className="detail-item">
                  <span className="detail-label">Due Date:</span>
                  <span className="detail-value">{formatDate(todo.dueDate)}</span>
                </div>
                <div className="detail-item">
                  <span className="detail-label">Status:</span>
                  <span className="detail-value">{todo.status}</span>
                </div>
                <div className="detail-item">
                  <span className="detail-label">Created:</span>
                  <span className="detail-value">{formatDateTime(todo.createdAt)}</span>
                </div>
                <div className="detail-item">
                  <span className="detail-label">Updated:</span>
                  <span className="detail-value">{formatDateTime(todo.updatedAt)}</span>
                </div>
                {todo.completedAt && (
                  <div className="detail-item">
                    <span className="detail-label">Completed:</span>
                    <span className="detail-value">{formatDateTime(todo.completedAt)}</span>
                  </div>
                )}
              </div>
            </div>

            <div className="action-buttons">
              <button
                onClick={handleToggleComplete}
                className="btn-toggle"
                type="button"
              >
                {todo.status === 'completed' ? 'Mark as Pending' : 'Mark as Completed'}
              </button>

              <Link to={`/todos/${todo.ulid}/edit`} className="btn-edit-large">
                Edit
              </Link>

              <button
                onClick={handleDelete}
                className="btn-delete-large"
                type="button"
              >
                Delete
              </button>
            </div>
          </div>
        </div>
      </main>
    </div>
  );
}
