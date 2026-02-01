import { Link } from 'react-router-dom';
import type { Tag } from '../api/tags';

/**
 * Tag card component props.
 */
interface TagCardProps {
  tag: Tag;
  onDelete: () => void;
}

/**
 * Tag card component.
 *
 * Displays a tag with its color, name, and action buttons.
 */
export default function TagCard({ tag, onDelete }: TagCardProps): React.JSX.Element {
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
        <Link 
          to={`/tags/${tag.ulid}/edit`}
          state={{ from: 'list' }}
          className="btn-edit"
        >
          Edit
        </Link>
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
