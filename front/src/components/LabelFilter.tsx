import { useState, useEffect } from 'react';
import { labelsApi } from '../api/labels';
import type { Label } from '../api/labels';

/**
 * Props for LabelFilter component.
 */
interface LabelFilterProps {
  selectedLabelUlid: string | null;
  onChange: (labelUlid: string | null) => void;
}

/**
 * Label filter component.
 *
 * Provides dropdown to filter TODOs by label (AND condition on label's tags).
 *
 * @param props [LabelFilterProps] Component props
 * @param props.selectedLabelUlid [string | null] Currently selected label ULID
 * @param props.onChange [function] Callback when selection changes
 * @return [React.JSX.Element] Label filter component
 */
export default function LabelFilter({ selectedLabelUlid, onChange }: LabelFilterProps): React.JSX.Element {
  const [labels, setLabels] = useState<Label[]>([]);
  const [isLoading, setIsLoading] = useState(true);

  /**
   * Fetches labels from API.
   */
  useEffect(() => {
    const fetchLabels = async () => {
      try {
        setIsLoading(true);
        const response = await labelsApi.getLabels({ filter: 'used' });
        setLabels(response.labels);
      } catch (err) {
        console.error('Failed to load labels:', err);
      } finally {
        setIsLoading(false);
      }
    };

    fetchLabels();
  }, []);

  /**
   * Handles label selection change.
   *
   * @param event [React.ChangeEvent<HTMLSelectElement>] Change event
   */
  const handleChange = (event: React.ChangeEvent<HTMLSelectElement>) => {
    const value = event.target.value;
    onChange(value === '' ? null : value);
  };

  return (
    <div className="label-filter">
      <label htmlFor="label-filter-select" className="label-filter-label">
        Filter by Label:
      </label>
      <select
        id="label-filter-select"
        className="label-filter-select"
        value={selectedLabelUlid || ''}
        onChange={handleChange}
        disabled={isLoading}
      >
        <option value="">All Labels</option>
        {labels.map(label => (
          <option key={label.ulid} value={label.ulid}>
            {label.name} ({label.todoCount})
          </option>
        ))}
      </select>
    </div>
  );
}
