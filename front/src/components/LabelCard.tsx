import type { Label } from '../api/labels';
import TagBadge from './TagBadge';

/**
 * Label card component props.
 */
interface LabelCardProps {
  label: Label;
  onClick: () => void;
  onEdit: () => void;
  onDelete: () => void;
}

/**
 * Label card component.
 *
 * Displays a label with its name, description, tags, TODO count, and action buttons.
 */
export default function LabelCard({ label, onClick, onEdit, onDelete }: LabelCardProps): React.JSX.Element {
  const displayTags = label.tags?.slice(0, 5) || [];
  const remainingCount = (label.tags?.length || 0) - displayTags.length;

  return (
    <div className="label-card" onClick={onClick}>
      <div className="label-card-header">
        <h3 className="label-name">{label.name}</h3>
      </div>

      {label.description && (
        <p className="label-description">{label.description}</p>
      )}

      {label.tags && label.tags.length > 0 && (
        <div className="label-tags">
          {displayTags.map(tag => (
            <TagBadge key={tag.ulid} tag={tag} />
          ))}
          {remainingCount > 0 && (
            <span className="tag-badge tag-more">+{remainingCount}</span>
          )}
        </div>
      )}

      <div className="label-stats">
        <div className="label-stat-item">
          <span className="label-stat-label">Tags:</span>
          <span className="label-stat-value">{label.tagCount}</span>
        </div>
        <div className="label-stat-item">
          <span className="label-stat-label">TODOs:</span>
          <span className="label-stat-value">{label.todoCount}</span>
        </div>
      </div>

      <div className="label-card-actions" onClick={(e) => e.stopPropagation()}>
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
