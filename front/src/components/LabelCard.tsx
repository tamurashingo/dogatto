import type { Label } from '../api/labels';

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
 * Displays a label with its name, description, tag count, TODO count, and action buttons.
 */
export default function LabelCard({ label, onClick, onEdit, onDelete }: LabelCardProps): React.JSX.Element {
  return (
    <div className="label-card" onClick={onClick}>
      <div className="label-card-header">
        <h3 className="label-name">{label.name}</h3>
      </div>

      {label.description && (
        <p className="label-description">{label.description}</p>
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
