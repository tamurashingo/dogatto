import type { Label } from '../api/labels';

/**
 * Label delete confirmation dialog props.
 */
interface LabelDeleteConfirmProps {
  isOpen: boolean;
  label: Label;
  onConfirm: () => void;
  onCancel: () => void;
}

/**
 * Label delete confirmation dialog component.
 *
 * Shows warning and impact information before deleting a label.
 */
export default function LabelDeleteConfirm({ isOpen, label, onConfirm, onCancel }: LabelDeleteConfirmProps): React.JSX.Element | null {
  if (!isOpen) {
    return null;
  }

  return (
    <div className="modal-overlay" onClick={onCancel}>
      <div className="modal-content delete-confirm" onClick={(e) => e.stopPropagation()}>
        <div className="modal-header">
          <h2>Delete Label</h2>
          <button onClick={onCancel} className="modal-close" type="button">×</button>
        </div>

        <div className="delete-confirm-body">
          <div className="warning-icon">⚠️</div>
          <p className="warning-message">
            Are you sure you want to delete the label <strong>"{label.name}"</strong>?
          </p>

          <div className="impact-info">
            <h3>Impact:</h3>
            <ul>
              <li>This label groups <strong>{label.tagCount}</strong> tag(s)</li>
              <li>Currently matches <strong>{label.todoCount}</strong> TODO(s)</li>
              <li>The label will be permanently deleted</li>
              <li>TODOs and tags will not be affected</li>
            </ul>
          </div>

          {label.todoCount > 0 && (
            <div className="warning-note">
              <strong>Note:</strong> This label is currently being used to filter TODOs. 
              Deleting it will remove this filtering option.
            </div>
          )}
        </div>

        <div className="form-actions">
          <button
            type="button"
            onClick={onCancel}
            className="btn-cancel"
          >
            Cancel
          </button>
          <button
            type="button"
            onClick={onConfirm}
            className="btn-delete"
          >
            Delete Label
          </button>
        </div>
      </div>
    </div>
  );
}
