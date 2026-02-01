import { useState, useEffect } from 'react';
import { Link } from 'react-router-dom';
import Header from '../components/Header';
import TagCard from '../components/TagCard';
import { tagsApi } from '../api/tags';
import type { Tag } from '../api/tags';
import { ApiError } from '../api/error';
import '../styles/tags.css';

/**
 * Tags page component.
 *
 * Displays list of tags with create, edit, and delete functionality.
 */
export default function TagsPage(): React.JSX.Element {
  const [tags, setTags] = useState<Tag[]>([]);
  const [isLoading, setIsLoading] = useState(true);
  const [error, setError] = useState('');

  /**
   * Fetches tags from API.
   */
  const fetchTags = async () => {
    try {
      setIsLoading(true);
      setError('');
      const data = await tagsApi.getTags();
      setTags(data);
    } catch (err) {
      if (err instanceof ApiError) {
        setError(err.message);
      } else {
        setError('Failed to load tags');
      }
    } finally {
      setIsLoading(false);
    }
  };

  /**
   * Deletes a tag.
   *
   * @param ulid [string] Tag ULID
   */
  const handleDelete = async (ulid: string) => {
    if (!window.confirm('Are you sure you want to delete this tag? This will remove it from all TODOs.')) {
      return;
    }

    try {
      await tagsApi.deleteTag(ulid);
      setTags(tags.filter(tag => tag.ulid !== ulid));
    } catch (err) {
      if (err instanceof ApiError) {
        setError(err.message);
      } else {
        setError('Failed to delete tag');
      }
    }
  };

  useEffect(() => {
    fetchTags();
  }, []);

  return (
    <div className="tags-page">
      <Header />
      <main className="main-content">
        <div className="tags-header">
          <h1>Tags</h1>
          <Link to="/tags/new" className="btn-create">
            Create Tag
          </Link>
        </div>

        {error && (
          <div className="error-message">
            {error}
          </div>
        )}

        {isLoading ? (
          <div className="loading">Loading tags...</div>
        ) : tags.length === 0 ? (
          <div className="empty-state">
            <p>No tags yet. Create your first tag to organize your TODOs!</p>
            <Link to="/tags/new" className="btn-create-large">
              Create Your First Tag
            </Link>
          </div>
        ) : (
          <div className="tags-grid">
            {tags.map(tag => (
              <TagCard
                key={tag.ulid}
                tag={tag}
                onDelete={() => handleDelete(tag.ulid)}
              />
            ))}
          </div>
        )}
      </main>
    </div>
  );
}
