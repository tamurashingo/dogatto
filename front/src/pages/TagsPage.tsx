import { useState, useEffect } from 'react';
import Header from '../components/Header';
import TagCard from '../components/TagCard';
import TagFormModal from '../components/TagFormModal';
import TagMergeModal from '../components/TagMergeModal';
import { tagsApi } from '../api/tags';
import type { Tag, CreateTagRequest, UpdateTagRequest } from '../api/tags';
import { ApiError } from '../api/error';
import '../styles/tags.css';

/**
 * Tags page component.
 *
 * Displays list of tags with create, edit, delete, and merge functionality.
 */
export default function TagsPage(): React.JSX.Element {
  const [tags, setTags] = useState<Tag[]>([]);
  const [isLoading, setIsLoading] = useState(true);
  const [error, setError] = useState('');
  const [isModalOpen, setIsModalOpen] = useState(false);
  const [isMergeModalOpen, setIsMergeModalOpen] = useState(false);
  const [editingTag, setEditingTag] = useState<Tag | undefined>(undefined);
  const [selectedTags, setSelectedTags] = useState<Tag[]>([]);
  const [isSelectionMode, setIsSelectionMode] = useState(false);

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

  /**
   * Toggles selection mode for merging.
   */
  const handleToggleSelectionMode = () => {
    setIsSelectionMode(!isSelectionMode);
    setSelectedTags([]);
  };

  /**
   * Toggles tag selection.
   *
   * @param tag [Tag] Tag to toggle
   */
  const handleToggleTagSelection = (tag: Tag) => {
    if (selectedTags.find(t => t.ulid === tag.ulid)) {
      setSelectedTags(selectedTags.filter(t => t.ulid !== tag.ulid));
    } else {
      setSelectedTags([...selectedTags, tag]);
    }
  };

  /**
   * Opens merge modal.
   */
  const handleOpenMergeModal = () => {
    if (selectedTags.length < 1) {
      setError('Please select at least 1 tag to merge');
      return;
    }
    setIsMergeModalOpen(true);
  };

  /**
   * Handles successful merge.
   */
  const handleMergeSuccess = () => {
    setIsSelectionMode(false);
    setSelectedTags([]);
    fetchTags();
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
          <div className="tags-actions">
            {isSelectionMode && (
              <>
                <button
                  onClick={handleOpenMergeModal}
                  className="btn-merge"
                  disabled={selectedTags.length < 1}
                >
                  Merge {selectedTags.length > 0 ? `(${selectedTags.length})` : ''}
                </button>
                <button onClick={handleToggleSelectionMode} className="btn-cancel">
                  Cancel
                </button>
              </>
            )}
            {!isSelectionMode && (
              <>
                <button onClick={handleToggleSelectionMode} className="btn-select">
                  Select to Merge
                </button>
                <button onClick={() => setIsModalOpen(true)} className="btn-create">
                  Create Tag
                </button>
              </>
            )}
          </div>
        </div>

        {isSelectionMode && (
          <div className="selection-info">
            <p>Select tags to merge. You can merge multiple tags into one existing tag or create a new tag.</p>
          </div>
        )}

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
                isSelectionMode={isSelectionMode}
                isSelected={selectedTags.some(t => t.ulid === tag.ulid)}
                onToggleSelect={() => handleToggleTagSelection(tag)}
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

      <TagMergeModal
        isOpen={isMergeModalOpen}
        onClose={() => setIsMergeModalOpen(false)}
        onSuccess={handleMergeSuccess}
        selectedTags={selectedTags}
      />
    </div>
  );
}
