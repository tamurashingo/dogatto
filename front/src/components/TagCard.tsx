import { Link } from 'react-router-dom';
import type { Tag } from '../api/tags';

/**
 * Tag card component props.
 */
interface TagCardProps {
  tag: Tag;
  onEdit: () => void;
  onDelete: () => void;
}

/**
 * Tag card component.
 *
 * Displays a tag with its color, name, and action buttons.
 */
export default function TagCard({ tag, onEdit, onDelete }: TagCardProps): React.JSX.Element {
  return (
    <div className="tag-card">
      <div className="tag-card-header">
        <div className="tag-color-indicator" style={{ backgroundColor: tag.color }} />
        <h3 className="tag-name">{tag.name}</h3>
      </div>

      <div className="tag-card-actions">
        <Link to={`/tags/${tag.ulid}`} className="btn-view">
          View
        </Link>
        <button 
          onClick={onEdit}
          className="btn-edit"
          type="button"
        >
          Edit
        </button>
        <button
          onClick={onDelete}
          className="btn-delete"
          type="button"
        >
          Delete
        </button>
      </div>
    </div>
  );
}
