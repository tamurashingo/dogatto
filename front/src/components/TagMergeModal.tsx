import { useState, useEffect } from 'react';
import { tagsApi } from '../api/tags';
import type { Tag, MergeToExistingRequest, MergeToNewRequest } from '../api/tags';
import { ApiError } from '../api/error';
import '../styles/tag-merge-modal.css';

/**
 * Tag merge modal props.
 */
interface TagMergeModalProps {
  isOpen: boolean;
  onClose: () => void;
  onSuccess: () => void;
  selectedTags: Tag[];
}

/**
 * Tag merge mode type.
 */
type MergeMode = 'existing' | 'new';

/**
 * Tag merge modal component.
 *
 * Modal for merging tags to existing tag or creating a new tag.
 */
export default function TagMergeModal({
  isOpen,
  onClose,
  onSuccess,
  selectedTags,
}: TagMergeModalProps): React.JSX.Element | null {
  const [mergeMode, setMergeMode] = useState<MergeMode>('existing');
  const [availableTags, setAvailableTags] = useState<Tag[]>([]);
  const [targetTagUlid, setTargetTagUlid] = useState('');
  const [newTagName, setNewTagName] = useState('');
  const [newTagColor, setNewTagColor] = useState('#3B82F6');
  const [isSubmitting, setIsSubmitting] = useState(false);
  const [error, setError] = useState('');

  /**
   * Fetches available tags excluding selected tags.
   */
  useEffect(() => {
    if (isOpen && mergeMode === 'existing') {
      fetchAvailableTags();
    }
  }, [isOpen, mergeMode, selectedTags]);

  /**
   * Fetches tags from API and filters out selected tags.
   */
  const fetchAvailableTags = async () => {
    try {
      const tags = await tagsApi.getTags();
      const selectedUlids = selectedTags.map(t => t.ulid);
      const filtered = tags.filter(t => !selectedUlids.includes(t.ulid));
      setAvailableTags(filtered);
    } catch (err) {
      if (err instanceof ApiError) {
        setError(err.message);
      } else {
        setError('Failed to load tags');
      }
    }
  };

  /**
   * Handles merge submission.
   */
  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    setError('');
    setIsSubmitting(true);

    try {
      const sourceUlids = selectedTags.map(t => t.ulid);

      if (mergeMode === 'existing') {
        if (!targetTagUlid) {
          setError('Please select a target tag');
          setIsSubmitting(false);
          return;
        }

        const request: MergeToExistingRequest = {
          source_ulids: sourceUlids,
          target_ulid: targetTagUlid,
        };

        await tagsApi.mergeToExisting(request);
      } else {
        if (!newTagName.trim()) {
          setError('Please enter a tag name');
          setIsSubmitting(false);
          return;
        }

        const request: MergeToNewRequest = {
          source_ulids: sourceUlids,
          new_tag: {
            name: newTagName.trim(),
            color: newTagColor,
          },
        };

        await tagsApi.mergeToNew(request);
      }

      onSuccess();
      handleClose();
    } catch (err) {
      if (err instanceof ApiError) {
        setError(err.message);
      } else {
        setError('Failed to merge tags');
      }
    } finally {
      setIsSubmitting(false);
    }
  };

  /**
   * Handles modal close.
   */
  const handleClose = () => {
    setMergeMode('existing');
    setTargetTagUlid('');
    setNewTagName('');
    setNewTagColor('#3B82F6');
    setError('');
    onClose();
  };

  if (!isOpen) return null;

  return (
    <div className="modal-overlay" onClick={handleClose}>
      <div className="modal-content tag-merge-modal" onClick={(e) => e.stopPropagation()}>
        <div className="modal-header">
          <h2>Merge Tags</h2>
          <button type="button" className="modal-close" onClick={handleClose}>
            ×
          </button>
        </div>

        <div className="modal-body">
          <div className="merge-info">
            <h3>Tags to merge:</h3>
            <div className="selected-tags-list">
              {selectedTags.map(tag => (
                <span key={tag.ulid} className="tag-badge" style={{ backgroundColor: tag.color }}>
                  {tag.name}
                </span>
              ))}
            </div>
          </div>

          <form onSubmit={handleSubmit}>
            <div className="form-group">
              <label>Merge mode:</label>
              <div className="merge-mode-selector">
                <button
                  type="button"
                  className={`mode-button ${mergeMode === 'existing' ? 'active' : ''}`}
                  onClick={() => setMergeMode('existing')}
                >
                  Merge to existing tag
                </button>
                <button
                  type="button"
                  className={`mode-button ${mergeMode === 'new' ? 'active' : ''}`}
                  onClick={() => setMergeMode('new')}
                >
                  Create new tag
                </button>
              </div>
            </div>

            {mergeMode === 'existing' ? (
              <div className="form-group">
                <label htmlFor="target-tag">Target tag:</label>
                <select
                  id="target-tag"
                  value={targetTagUlid}
                  onChange={(e) => setTargetTagUlid(e.target.value)}
                  required
                >
                  <option value="">-- Select a tag --</option>
                  {availableTags.map(tag => (
                    <option key={tag.ulid} value={tag.ulid}>
                      {tag.name}
                    </option>
                  ))}
                </select>
              </div>
            ) : (
              <>
                <div className="form-group">
                  <label htmlFor="new-tag-name">New tag name:</label>
                  <input
                    type="text"
                    id="new-tag-name"
                    value={newTagName}
                    onChange={(e) => setNewTagName(e.target.value)}
                    placeholder="Enter tag name"
                    required
                  />
                </div>

                <div className="form-group">
                  <label htmlFor="new-tag-color">Color:</label>
                  <input
                    type="color"
                    id="new-tag-color"
                    value={newTagColor}
                    onChange={(e) => setNewTagColor(e.target.value)}
                  />
                </div>
              </>
            )}

            {error && <div className="error-message">{error}</div>}

            <div className="modal-actions">
              <button
                type="button"
                className="btn-cancel"
                onClick={handleClose}
                disabled={isSubmitting}
              >
                Cancel
              </button>
              <button
                type="submit"
                className="btn-submit"
                disabled={isSubmitting}
              >
                {isSubmitting ? 'Merging...' : 'Merge Tags'}
              </button>
            </div>
          </form>
        </div>
      </div>
    </div>
  );
}
