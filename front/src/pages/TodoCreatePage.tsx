import { useState } from 'react';
import type { FormEvent } from 'react';
import { useNavigate, Link } from 'react-router-dom';
import Header from '../components/Header';
import { todosApi } from '../api/todos';
import { ApiError } from '../api/error';
import '../styles/todo-form.css';

/**
 * TODO create page component.
 *
 * Provides interface for creating new todos.
 */
export default function TodoCreatePage(): React.JSX.Element {
  const navigate = useNavigate();
  const [title, setTitle] = useState('');
  const [content, setContent] = useState('');
  const [dueDate, setDueDate] = useState('');
  const [error, setError] = useState('');
  const [isLoading, setIsLoading] = useState(false);

  /**
   * Validates the todo form.
   *
   * @return [boolean] True if form is valid
   */
  const validateForm = (): boolean => {
    if (!title.trim()) {
      setError('Title is required');
      return false;
    }

    if (title.length > 255) {
      setError('Title must be 255 characters or less');
      return false;
    }

    return true;
  };

  /**
   * Handles form submission.
   *
   * @param e [FormEvent] Form event
   */
  const handleSubmit = async (e: FormEvent) => {
    e.preventDefault();
    setError('');

    if (!validateForm()) {
      return;
    }

    setIsLoading(true);

    try {
      const dueDateTimestamp = dueDate 
        ? Math.floor(new Date(dueDate).getTime() / 1000)
        : undefined;

      await todosApi.createTodo({
        title: title.trim(),
        content: content.trim() || undefined,
        dueDate: dueDateTimestamp,
      });

      navigate('/todos');
    } catch (err) {
      if (err instanceof ApiError) {
        setError(err.message);
      } else {
        setError('Failed to create todo. Please try again.');
      }
    } finally {
      setIsLoading(false);
    }
  };

  return (
    <div className="todo-form-page">
      <Header />
      <main className="form-content">
        <div className="form-container">
          <h1>Create New TODO</h1>

          {error && (
            <div className="error-message">
              {error}
            </div>
          )}

          <form onSubmit={handleSubmit} className="todo-form">
            <div className="form-group">
              <label htmlFor="title">
                Title <span className="required">*</span>
              </label>
              <input
                id="title"
                type="text"
                value={title}
                onChange={(e) => setTitle(e.target.value)}
                placeholder="Enter todo title"
                disabled={isLoading}
                maxLength={255}
                required
              />
              <span className="char-count">{title.length}/255</span>
            </div>

            <div className="form-group">
              <label htmlFor="content">
                Description
              </label>
              <textarea
                id="content"
                value={content}
                onChange={(e) => setContent(e.target.value)}
                placeholder="Enter todo description (optional)"
                disabled={isLoading}
                rows={5}
              />
            </div>

            <div className="form-group">
              <label htmlFor="dueDate">
                Due Date
              </label>
              <input
                id="dueDate"
                type="date"
                value={dueDate}
                onChange={(e) => setDueDate(e.target.value)}
                disabled={isLoading}
              />
            </div>

            <div className="form-actions">
              <button
                type="submit"
                disabled={isLoading}
                className="btn-submit"
              >
                {isLoading ? 'Creating...' : 'Create TODO'}
              </button>

              <Link to="/todos" className="btn-cancel">
                Cancel
              </Link>
            </div>
          </form>
        </div>
      </main>
    </div>
  );
}
