import { useState, useEffect } from 'react';
import type { FormEvent } from 'react';
import { useNavigate, useParams, Link } from 'react-router-dom';
import Header from '../components/Header';
import { todosApi } from '../api/todos';
import { ApiError } from '../api/error';
import '../styles/todo-form.css';

/**
 * TODO edit page component.
 *
 * Provides interface for editing existing todos.
 */
export default function TodoEditPage(): React.JSX.Element {
  const navigate = useNavigate();
  const { id } = useParams<{ id: string }>();
  const [title, setTitle] = useState('');
  const [content, setContent] = useState('');
  const [dueDate, setDueDate] = useState('');
  const [error, setError] = useState('');
  const [isLoading, setIsLoading] = useState(false);
  const [isFetching, setIsFetching] = useState(true);

  /**
   * Fetches todo data for editing.
   */
  useEffect(() => {
    const fetchTodo = async () => {
      if (!id) {
        setError('Invalid TODO ID');
        setIsFetching(false);
        return;
      }

      try {
        const todo = await todosApi.getTodoById(parseInt(id, 10));
        setTitle(todo.title);
        setContent(todo.content || '');
        
        if (todo.dueDate) {
          const date = new Date(todo.dueDate * 1000);
          setDueDate(date.toISOString().split('T')[0]);
        }
      } catch (err) {
        if (err instanceof ApiError) {
          setError(err.message);
        } else {
          setError('Failed to load todo');
        }
      } finally {
        setIsFetching(false);
      }
    };

    fetchTodo();
  }, [id]);

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

    if (!validateForm() || !id) {
      return;
    }

    setIsLoading(true);

    try {
      const dueDateTimestamp = dueDate 
        ? Math.floor(new Date(dueDate).getTime() / 1000)
        : undefined;

      await todosApi.updateTodo(parseInt(id, 10), {
        title: title.trim(),
        content: content.trim() || undefined,
        dueDate: dueDateTimestamp,
      });

      navigate('/todos');
    } catch (err) {
      if (err instanceof ApiError) {
        setError(err.message);
      } else {
        setError('Failed to update todo. Please try again.');
      }
    } finally {
      setIsLoading(false);
    }
  };

  if (isFetching) {
    return (
      <div className="todo-form-page">
        <Header />
        <main className="form-content">
          <div className="form-container">
            <div className="loading">Loading todo...</div>
          </div>
        </main>
      </div>
    );
  }

  return (
    <div className="todo-form-page">
      <Header />
      <main className="form-content">
        <div className="form-container">
          <h1>Edit TODO</h1>

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
                {isLoading ? 'Updating...' : 'Update TODO'}
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
