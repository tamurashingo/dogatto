import { useState, useEffect } from 'react';
import { HexColorPicker } from 'react-colorful';
import type { Tag, CreateTagRequest, UpdateTagRequest } from '../api/tags';
import '../styles/tag-form-modal.css';

/**
 * Tag form modal props.
 */
interface TagFormModalProps {
  isOpen: boolean;
  mode: 'create' | 'edit';
  tag?: Tag;
  onClose: () => void;
  onSubmit: (data: CreateTagRequest | UpdateTagRequest) => Promise<void>;
}

/**
 * Default color palette for quick selection.
 */
const DEFAULT_COLORS = [
  '#EF4444', // Red
  '#F97316', // Orange
  '#F59E0B', // Amber
  '#EAB308', // Yellow
  '#84CC16', // Lime
  '#22C55E', // Green
  '#10B981', // Emerald
  '#14B8A6', // Teal
  '#06B6D4', // Cyan
  '#0EA5E9', // Sky
  '#3B82F6', // Blue
  '#6366F1', // Indigo
  '#8B5CF6', // Violet
  '#A855F7', // Purple
  '#D946EF', // Fuchsia
  '#EC4899', // Pink
];

/**
 * Tag form modal component.
 *
 * Provides a modal dialog for creating or editing tags with color picker.
 */
export default function TagFormModal({
  isOpen,
  mode,
  tag,
  onClose,
  onSubmit,
}: TagFormModalProps): React.JSX.Element | null {
  const [name, setName] = useState('');
  const [color, setColor] = useState('#3B82F6');
  const [errors, setErrors] = useState<{ name?: string; color?: string }>({});
  const [isSubmitting, setIsSubmitting] = useState(false);
  const [showColorPicker, setShowColorPicker] = useState(false);

  /**
   * Initialize form when tag changes.
   */
  useEffect(() => {
    if (mode === 'edit' && tag) {
      setName(tag.name);
      setColor(tag.color);
    } else {
      setName('');
      setColor('#3B82F6');
    }
    setErrors({});
  }, [mode, tag]);

  /**
   * Validates form fields.
   *
   * @return [boolean] True if valid
   */
  const validate = (): boolean => {
    const newErrors: { name?: string; color?: string } = {};

    if (!name.trim()) {
      newErrors.name = 'Tag name is required';
    } else if (name.length > 50) {
      newErrors.name = 'Tag name must be 50 characters or less';
    }

    if (!/^#[0-9A-F]{6}$/i.test(color)) {
      newErrors.color = 'Invalid color format';
    }

    setErrors(newErrors);
    return Object.keys(newErrors).length === 0;
  };

  /**
   * Handles form submission.
   */
  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();

    if (!validate()) {
      return;
    }

    setIsSubmitting(true);
    try {
      await onSubmit({ name: name.trim(), color });
      handleClose();
    } catch (error) {
      console.error('Failed to save tag:', error);
    } finally {
      setIsSubmitting(false);
    }
  };

  /**
   * Handles modal close.
   */
  const handleClose = () => {
    setName('');
    setColor('#3B82F6');
    setErrors({});
    setShowColorPicker(false);
    onClose();
  };

  /**
   * Handles color selection from palette.
   */
  const handleColorSelect = (selectedColor: string) => {
    setColor(selectedColor);
    setShowColorPicker(false);
  };

  if (!isOpen) {
    return null;
  }

  return (
    <div className="modal-overlay" onClick={handleClose}>
      <div className="modal-content" onClick={(e) => e.stopPropagation()}>
        <div className="modal-header">
          <h2>{mode === 'create' ? 'Create Tag' : 'Edit Tag'}</h2>
          <button
            type="button"
            className="modal-close"
            onClick={handleClose}
            aria-label="Close"
          >
            ×
          </button>
        </div>

        <form onSubmit={handleSubmit}>
          <div className="form-group">
            <label htmlFor="tag-name">
              Tag Name <span className="required">*</span>
            </label>
            <input
              id="tag-name"
              type="text"
              value={name}
              onChange={(e) => setName(e.target.value)}
              placeholder="Enter tag name"
              maxLength={50}
              className={errors.name ? 'input-error' : ''}
            />
            {errors.name && <span className="error-text">{errors.name}</span>}
            <span className="char-count">{name.length}/50</span>
          </div>

          <div className="form-group">
            <label>
              Color <span className="required">*</span>
            </label>
            
            <div className="color-preview-container">
              <div
                className="color-preview"
                style={{ backgroundColor: color }}
                onClick={() => setShowColorPicker(!showColorPicker)}
              />
              <input
                type="text"
                value={color}
                onChange={(e) => setColor(e.target.value.toUpperCase())}
                placeholder="#3B82F6"
                maxLength={7}
                className={errors.color ? 'input-error' : ''}
              />
            </div>
            {errors.color && <span className="error-text">{errors.color}</span>}

            {showColorPicker && (
              <div className="color-picker-container">
                <HexColorPicker color={color} onChange={setColor} />
              </div>
            )}

            <div className="color-palette">
              {DEFAULT_COLORS.map((paletteColor) => (
                <button
                  key={paletteColor}
                  type="button"
                  className={`color-swatch ${color === paletteColor ? 'selected' : ''}`}
                  style={{ backgroundColor: paletteColor }}
                  onClick={() => handleColorSelect(paletteColor)}
                  aria-label={`Select color ${paletteColor}`}
                />
              ))}
            </div>
          </div>

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
              {isSubmitting ? 'Saving...' : mode === 'create' ? 'Create' : 'Save'}
            </button>
          </div>
        </form>
      </div>
    </div>
  );
}
