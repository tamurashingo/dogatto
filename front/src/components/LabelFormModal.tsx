import { useState, useEffect } from 'react';
import type { Label, CreateLabelRequest, UpdateLabelRequest } from '../api/labels';
import { tagsApi } from '../api/tags';
import { labelsApi } from '../api/labels';
import type { Tag } from '../api/tags';

/**
 * Label form modal props.
 */
interface LabelFormModalProps {
  isOpen: boolean;
  mode: 'create' | 'edit';
  label?: Label;
  onClose: () => void;
  onSubmit: (data: CreateLabelRequest | UpdateLabelRequest) => Promise<void>;
}

/**
 * Label form modal component.
 *
 * Modal dialog for creating and editing labels.
 */
export default function LabelFormModal({ isOpen, mode, label, onClose, onSubmit }: LabelFormModalProps): React.JSX.Element | null {
  const [name, setName] = useState('');
  const [description, setDescription] = useState('');
  const [selectedTagUlids, setSelectedTagUlids] = useState<string[]>([]);
  const [availableTags, setAvailableTags] = useState<Tag[]>([]);
  const [estimatedTodoCount, setEstimatedTodoCount] = useState<number | null>(null);
  const [isEstimating, setIsEstimating] = useState(false);
  const [isSubmitting, setIsSubmitting] = useState(false);
  const [error, setError] = useState('');

  /**
   * Fetches available tags.
   */
  const fetchTags = async () => {
    try {
      const tags = await tagsApi.getTags();
      setAvailableTags(tags);
    } catch (err) {
      setError('Failed to load tags');
    }
  };

  /**
   * Estimates TODO count for selected tags.
   */
  const estimateTodoCount = async () => {
    if (selectedTagUlids.length === 0) {
      setEstimatedTodoCount(null);
      return;
    }

    try {
      setIsEstimating(true);
      const count = await labelsApi.estimateTodoCount(selectedTagUlids);
      setEstimatedTodoCount(count);
    } catch (err) {
      console.error('Failed to estimate TODO count:', err);
    } finally {
      setIsEstimating(false);
    }
  };

  /**
   * Handles tag selection toggle.
   *
   * @param ulid [string] Tag ULID
   */
  const handleTagToggle = (ulid: string) => {
    setSelectedTagUlids(prev => 
      prev.includes(ulid)
        ? prev.filter(id => id !== ulid)
        : [...prev, ulid]
    );
  };

  /**
   * Handles form submission.
   *
   * @param e [React.FormEvent] Form event
   */
  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    setError('');

    if (!name.trim()) {
      setError('Label name is required');
      return;
    }

    if (selectedTagUlids.length === 0) {
      setError('At least one tag is required');
      return;
    }

    try {
      setIsSubmitting(true);
      const data = {
        name: name.trim(),
        description: description.trim() || undefined,
        tagUlids: selectedTagUlids,
      };

      await onSubmit(data);
      handleClose();
    } catch (err) {
      // Error handled by parent
    } finally {
      setIsSubmitting(false);
    }
  };

  /**
   * Closes modal and resets form.
   */
  const handleClose = () => {
    setName('');
    setDescription('');
    setSelectedTagUlids([]);
    setEstimatedTodoCount(null);
    setError('');
    onClose();
  };

  // Load form data when editing
  useEffect(() => {
    if (isOpen && mode === 'edit' && label) {
      setName(label.name);
      setDescription(label.description || '');
      // Note: We don't have tag ULIDs in label object, would need to fetch them
      // For now, reset to empty
      setSelectedTagUlids([]);
    } else if (isOpen && mode === 'create') {
      setName('');
      setDescription('');
      setSelectedTagUlids([]);
    }
  }, [isOpen, mode, label]);

  // Fetch tags when modal opens
  useEffect(() => {
    if (isOpen) {
      fetchTags();
    }
  }, [isOpen]);

  // Estimate TODO count when tags change
  useEffect(() => {
    if (isOpen && selectedTagUlids.length > 0) {
      estimateTodoCount();
    } else {
      setEstimatedTodoCount(null);
    }
  }, [selectedTagUlids, isOpen]);

  if (!isOpen) {
    return null;
  }

  return (
    <div className="modal-overlay" onClick={handleClose}>
      <div className="modal-content" onClick={(e) => e.stopPropagation()}>
        <div className="modal-header">
          <h2>{mode === 'create' ? 'Create Label' : 'Edit Label'}</h2>
          <button onClick={handleClose} className="modal-close" type="button">×</button>
        </div>

        <form onSubmit={handleSubmit} className="label-form">
          <div className="form-group">
            <label htmlFor="label-name">Label Name *</label>
            <input
              id="label-name"
              type="text"
              value={name}
              onChange={(e) => setName(e.target.value)}
              placeholder="Enter label name"
              maxLength={100}
              required
              disabled={isSubmitting}
            />
          </div>

          <div className="form-group">
            <label htmlFor="label-description">Description</label>
            <textarea
              id="label-description"
              value={description}
              onChange={(e) => setDescription(e.target.value)}
              placeholder="Enter label description (optional)"
              maxLength={1000}
              rows={3}
              disabled={isSubmitting}
            />
          </div>

          <div className="form-group">
            <label>Select Tags * (at least 1 required)</label>
            <div className="tags-selection">
              {availableTags.length === 0 ? (
                <p>No tags available. Please create tags first.</p>
              ) : (
                availableTags.map(tag => (
                  <label key={tag.ulid} className="tag-checkbox">
                    <input
                      type="checkbox"
                      checked={selectedTagUlids.includes(tag.ulid)}
                      onChange={() => handleTagToggle(tag.ulid)}
                      disabled={isSubmitting}
                    />
                    <span className="tag-color-dot" style={{ backgroundColor: tag.color }} />
                    <span>{tag.name}</span>
                  </label>
                ))
              )}
            </div>
          </div>

          {selectedTagUlids.length > 0 && (
            <div className="form-group todo-estimate">
              <label>Estimated TODOs matching ALL selected tags:</label>
              <div className="estimate-value">
                {isEstimating ? (
                  <span>Calculating...</span>
                ) : (
                  <span className="estimate-count">{estimatedTodoCount ?? 0}</span>
                )}
              </div>
            </div>
          )}

          {error && (
            <div className="form-error">{error}</div>
          )}

          <div className="form-actions">
            <button
              type="button"
              onClick={handleClose}
              className="btn-cancel"
              disabled={isSubmitting}
            >
              Cancel
            </button>
            <button
              type="submit"
              className="btn-submit"
              disabled={isSubmitting || selectedTagUlids.length === 0}
            >
              {isSubmitting ? 'Saving...' : mode === 'create' ? 'Create' : 'Update'}
            </button>
          </div>
        </form>
      </div>
    </div>
  );
}
