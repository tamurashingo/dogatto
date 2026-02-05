import { useNavigate } from 'react-router-dom';
import type { Tag } from '../api/tags';
import '../styles/tag-badge.css';

/**
 * Tag badge component props.
 */
interface TagBadgeProps {
  tag: Tag;
  clickable?: boolean;
  onRemove?: () => void;
  size?: 'small' | 'medium';
}

/**
 * Tag badge component.
 *
 * Displays a colored tag badge with optional click and remove functionality.
 */
export default function TagBadge({
  tag,
  clickable = false,
  onRemove,
  size = 'medium',
}: TagBadgeProps): React.JSX.Element {
  const navigate = useNavigate();

  /**
   * Handles badge click to navigate to tag detail or filter.
   */
  const handleClick = () => {
    if (clickable) {
      navigate(`/tags/${tag.ulid}`);
    }
  };

  /**
   * Handles remove button click.
   *
   * @param e [React.MouseEvent] Click event
   */
  const handleRemove = (e: React.MouseEvent) => {
    e.stopPropagation();
    if (onRemove) {
      onRemove();
    }
  };

  const badgeClasses = [
    'tag-badge',
    size === 'small' ? 'tag-badge-small' : 'tag-badge-medium',
    clickable ? 'tag-badge-clickable' : '',
  ].filter(Boolean).join(' ');

  return (
    <span
      className={badgeClasses}
      style={{ backgroundColor: tag.color }}
      onClick={handleClick}
      role={clickable ? 'button' : undefined}
      tabIndex={clickable ? 0 : undefined}
      onKeyDown={clickable ? (e) => e.key === 'Enter' && handleClick() : undefined}
    >
      <span className="tag-badge-name">{tag.name}</span>
      {onRemove && (
        <button
          type="button"
          className="tag-badge-remove"
          onClick={handleRemove}
          aria-label={`Remove ${tag.name}`}
        >
          ×
        </button>
      )}
    </span>
  );
}
