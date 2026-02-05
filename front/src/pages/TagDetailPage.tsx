import { useState, useEffect } from 'react';
import { useParams, useNavigate } from 'react-router-dom';
import Header from '../components/Header';
import TagFormModal from '../components/TagFormModal';
import { tagsApi } from '../api/tags';
import { todosApi } from '../api/todos';
import type { Tag, TagStatistics, UpdateTagRequest } from '../api/tags';
import type { Todo } from '../api/todos';
import { ApiError } from '../api/error';
import '../styles/tag-detail.css';

/**
 * Tag detail page component.
 *
 * Displays tag information, statistics, and related TODOs.
 */
export default function TagDetailPage(): React.JSX.Element {
  const { ulid } = useParams<{ ulid: string }>();
  const navigate = useNavigate();
  const [tag, setTag] = useState<Tag | null>(null);
  const [statistics, setStatistics] = useState<TagStatistics | null>(null);
  const [todos, setTodos] = useState<Todo[]>([]);
  const [isLoading, setIsLoading] = useState(true);
  const [error, setError] = useState('');
  const [isEditModalOpen, setIsEditModalOpen] = useState(false);

  /**
   * Fetches tag details and related data.
   */
  const fetchTagDetails = async () => {
    if (!ulid) return;

    try {
      setIsLoading(true);
      setError('');
      
      const { tag: tagData, statistics: stats } = await tagsApi.getTagByUlid(ulid);
      setTag(tagData);
      setStatistics(stats);

      const todosData = await todosApi.getTodos({ tags: [ulid] });
      setTodos(todosData);
    } catch (err) {
      if (err instanceof ApiError) {
        setError(err.message);
      } else {
        setError('Failed to load tag details');
      }
    } finally {
      setIsLoading(false);
    }
  };

  /**
   * Handles tag update.
   *
   * @param data [UpdateTagRequest] Tag update data
   */
  const handleUpdate = async (data: UpdateTagRequest) => {
    if (!tag) return;

    try {
      const updatedTag = await tagsApi.updateTag(tag.ulid, data);
      setTag(updatedTag);
    } catch (err) {
      if (err instanceof ApiError) {
        setError(err.message);
      } else {
        setError('Failed to update tag');
      }
      throw err;
    }
  };

  /**
   * Handles tag deletion.
   */
  const handleDelete = async () => {
    if (!tag) return;

    if (!window.confirm('Are you sure you want to delete this tag? This will remove it from all TODOs.')) {
      return;
    }

    try {
      await tagsApi.deleteTag(tag.ulid);
      navigate('/tags');
    } catch (err) {
      if (err instanceof ApiError) {
        setError(err.message);
      } else {
        setError('Failed to delete tag');
      }
    }
  };

  /**
   * Handles TODO completion toggle.
   *
   * @param todoUlid [string] TODO ULID
   */
  const handleToggleComplete = async (todoUlid: string) => {
    try {
      const updatedTodo = await todosApi.toggleTodoComplete(todoUlid);
      setTodos(todos.map(todo => todo.ulid === todoUlid ? updatedTodo : todo));
    } catch (err) {
      if (err instanceof ApiError) {
        setError(err.message);
      } else {
        setError('Failed to update todo');
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

  useEffect(() => {
    fetchTagDetails();
  }, [ulid]);

  if (isLoading) {
    return (
      <div className="tag-detail-page">
        <Header />
        <main className="main-content">
          <div className="loading">Loading tag details...</div>
        </main>
      </div>
    );
  }

  if (!tag) {
    return (
      <div className="tag-detail-page">
        <Header />
        <main className="main-content">
          <div className="error-message">Tag not found</div>
        </main>
      </div>
    );
  }

  return (
    <div className="tag-detail-page">
      <Header />
      <main className="main-content">
        <div className="tag-detail-header">
          <button onClick={() => navigate('/tags')} className="btn-back">
            ← Back to Tags
          </button>
        </div>

        {error && (
          <div className="error-message">
            {error}
          </div>
        )}

        <div className="tag-info-card">
          <div className="tag-info-header">
            <div className="tag-title">
              <div className="tag-color-indicator-large" style={{ backgroundColor: tag.color }} />
              <h1>{tag.name}</h1>
            </div>
            <div className="tag-actions">
              <button
                onClick={() => setIsEditModalOpen(true)}
                className="btn-edit"
                type="button"
              >
                Edit
              </button>
              <button
                onClick={handleDelete}
                className="btn-delete"
                type="button"
              >
                Delete
              </button>
            </div>
          </div>

          {statistics && (
            <div className="tag-statistics">
              <div className="stat-item">
                <span className="stat-label">Total TODOs</span>
                <span className="stat-value">{statistics.total}</span>
              </div>
              <div className="stat-item">
                <span className="stat-label">Active</span>
                <span className="stat-value stat-active">{statistics.active}</span>
              </div>
              <div className="stat-item">
                <span className="stat-label">Completed</span>
                <span className="stat-value stat-completed">{statistics.completed}</span>
              </div>
            </div>
          )}
        </div>

        <div className="related-todos-section">
          <h2>Related TODOs</h2>
          
          {todos.length === 0 ? (
            <div className="empty-state">
              <p>No TODOs with this tag yet.</p>
            </div>
          ) : (
            <div className="todos-list">
              {todos.map(todo => (
                <div
                  key={todo.ulid}
                  className={`todo-item ${todo.status === 'completed' ? 'completed' : ''}`}
                >
                  <div className="todo-item-header">
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

                  <div className="todo-tags">
                    {todo.tags.map(todoTag => (
                      <span
                        key={todoTag.ulid}
                        className="tag-badge"
                        style={{ backgroundColor: todoTag.color }}
                      >
                        {todoTag.name}
                      </span>
                    ))}
                  </div>

                  <div className="todo-item-actions">
                    <button
                      onClick={() => navigate(`/todos/${todo.ulid}`)}
                      className="btn-view-todo"
                      type="button"
                    >
                      View Details
                    </button>
                  </div>
                </div>
              ))}
            </div>
          )}
        </div>
      </main>

      <TagFormModal
        isOpen={isEditModalOpen}
        mode="edit"
        tag={tag}
        onClose={() => setIsEditModalOpen(false)}
        onSubmit={handleUpdate}
      />
    </div>
  );
}
