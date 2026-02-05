import { useState, useEffect } from 'react';
import Header from '../components/Header';
import TagCard from '../components/TagCard';
import TagFormModal from '../components/TagFormModal';
import { tagsApi } from '../api/tags';
import type { Tag, CreateTagRequest, UpdateTagRequest } from '../api/tags';
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
  const [isModalOpen, setIsModalOpen] = useState(false);
  const [editingTag, setEditingTag] = useState<Tag | undefined>(undefined);

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
   * Handles tag creation.
   *
   * @param data [CreateTagRequest] Tag data
   */
  const handleCreate = async (data: CreateTagRequest | UpdateTagRequest) => {
    try {
      const newTag = await tagsApi.createTag(data as CreateTagRequest);
      setTags([...tags, newTag]);
    } catch (err) {
      if (err instanceof ApiError) {
        setError(err.message);
      } else {
        setError('Failed to create tag');
      }
      throw err;
    }
  };

  /**
   * Handles tag update.
   *
   * @param data [UpdateTagRequest] Tag data
   */
  const handleUpdate = async (data: CreateTagRequest | UpdateTagRequest) => {
    if (!editingTag) return;

    try {
      const updatedTag = await tagsApi.updateTag(editingTag.ulid, data);
      setTags(tags.map(tag => tag.ulid === editingTag.ulid ? updatedTag : tag));
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
   * Opens edit modal.
   *
   * @param tag [Tag] Tag to edit
   */
  const handleEdit = (tag: Tag) => {
    setEditingTag(tag);
    setIsModalOpen(true);
  };

  /**
   * Closes modal.
   */
  const handleCloseModal = () => {
    setIsModalOpen(false);
    setEditingTag(undefined);
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
          <button onClick={() => setIsModalOpen(true)} className="btn-create">
            Create Tag
          </button>
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
            <button onClick={() => setIsModalOpen(true)} className="btn-create-large">
              Create Your First Tag
            </button>
          </div>
        ) : (
          <div className="tags-grid">
            {tags.map(tag => (
              <TagCard
                key={tag.ulid}
                tag={tag}
                onEdit={() => handleEdit(tag)}
                onDelete={() => handleDelete(tag.ulid)}
              />
            ))}
          </div>
        )}
      </main>

      <TagFormModal
        isOpen={isModalOpen}
        mode={editingTag ? 'edit' : 'create'}
        tag={editingTag}
        onClose={handleCloseModal}
        onSubmit={editingTag ? handleUpdate : handleCreate}
      />
    </div>
  );
}
