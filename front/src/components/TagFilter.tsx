import { useState, useEffect } from 'react';
import { tagsApi } from '../api/tags';
import type { Tag } from '../api/tags';
import { ApiError } from '../api/error';
import '../styles/tag-filter.css';

/**
 * Tag filter component props.
 */
interface TagFilterProps {
  selectedTagUlids: string[];
  includeUntagged: boolean;
  onChange: (tagUlids: string[], untagged: boolean) => void;
}

/**
 * Tag filter component.
 *
 * Allows users to filter TODOs by tags with multiple selection support.
 */
export default function TagFilter({
  selectedTagUlids,
  includeUntagged,
  onChange,
}: TagFilterProps): React.JSX.Element {
  const [tags, setTags] = useState<Tag[]>([]);
  const [isLoading, setIsLoading] = useState(true);
  const [error, setError] = useState('');
  const [isOpen, setIsOpen] = useState(false);

  /**
   * Fetches all available tags.
   */
  useEffect(() => {
    const fetchTags = async () => {
      try {
        setIsLoading(true);
        setError('');
        const data = await tagsApi.getTags();
        setTags(data);
      } catch (err) {
        if (err instanceof ApiError) {
          setError(err.message);
        } else {
          setError('Failed to load tags');
        }
      } finally {
        setIsLoading(false);
      }
    };

    fetchTags();
  }, []);

  /**
   * Handles tag selection toggle.
   *
   * @param ulid [string] Tag ULID
   */
  const handleTagToggle = (ulid: string) => {
    const newSelection = selectedTagUlids.includes(ulid)
      ? selectedTagUlids.filter(id => id !== ulid)
      : [...selectedTagUlids, ulid];
    onChange(newSelection, includeUntagged);
  };

  /**
   * Handles untagged option toggle.
   */
  const handleUntaggedToggle = () => {
    onChange(selectedTagUlids, !includeUntagged);
  };

  /**
   * Clears all filters.
   */
  const handleClear = () => {
    onChange([], false);
  };

  const hasActiveFilters = selectedTagUlids.length > 0 || includeUntagged;

  return (
    <div className="tag-filter">
      <div className="filter-header">
        <button
          type="button"
          className="filter-toggle"
          onClick={() => setIsOpen(!isOpen)}
        >
          <span className="filter-icon">🏷️</span>
          <span className="filter-label">Filter by Tags</span>
          {hasActiveFilters && (
            <span className="filter-count">
              {selectedTagUlids.length + (includeUntagged ? 1 : 0)}
            </span>
          )}
          <span className={`dropdown-arrow ${isOpen ? 'open' : ''}`}>▼</span>
        </button>

        {hasActiveFilters && (
          <button
            type="button"
            className="filter-clear"
            onClick={handleClear}
          >
            Clear
          </button>
        )}
      </div>

      {isOpen && (
        <div className="filter-dropdown">
          {error && (
            <div className="filter-error">{error}</div>
          )}

          {isLoading ? (
            <div className="filter-loading">Loading tags...</div>
          ) : (
            <>
              <div className="filter-option">
                <label className="filter-checkbox">
                  <input
                    type="checkbox"
                    checked={includeUntagged}
                    onChange={handleUntaggedToggle}
                  />
                  <span className="checkbox-label">
                    <span className="untagged-icon">📝</span>
                    Untagged TODOs
                  </span>
                </label>
              </div>

              {tags.length > 0 && (
                <>
                  <div className="filter-divider" />
                  <div className="filter-options-list">
                    {tags.map(tag => (
                      <div key={tag.ulid} className="filter-option">
                        <label className="filter-checkbox">
                          <input
                            type="checkbox"
                            checked={selectedTagUlids.includes(tag.ulid)}
                            onChange={() => handleTagToggle(tag.ulid)}
                          />
                          <span className="checkbox-label">
                            <span
                              className="tag-color-indicator"
                              style={{ backgroundColor: tag.color }}
                            />
                            {tag.name}
                          </span>
                        </label>
                      </div>
                    ))}
                  </div>
                </>
              )}

              {tags.length === 0 && !isLoading && (
                <div className="filter-empty">
                  No tags available. Create tags to filter TODOs.
                </div>
              )}
            </>
          )}
        </div>
      )}

      {hasActiveFilters && (
        <div className="active-filters">
          <span className="active-filters-label">Active filters:</span>
          <div className="active-filters-list">
            {includeUntagged && (
              <span className="active-filter-badge untagged">
                <span className="badge-text">Untagged</span>
                <button
                  type="button"
                  onClick={handleUntaggedToggle}
                  className="badge-remove"
                  aria-label="Remove untagged filter"
                >
                  ×
                </button>
              </span>
            )}
            {selectedTagUlids.map(ulid => {
              const tag = tags.find(t => t.ulid === ulid);
              if (!tag) return null;
              return (
                <span
                  key={ulid}
                  className="active-filter-badge"
                  style={{ backgroundColor: tag.color }}
                >
                  <span className="badge-text">{tag.name}</span>
                  <button
                    type="button"
                    onClick={() => handleTagToggle(ulid)}
                    className="badge-remove"
                    aria-label={`Remove ${tag.name} filter`}
                  >
                    ×
                  </button>
                </span>
              );
            })}
          </div>
        </div>
      )}
    </div>
  );
}
