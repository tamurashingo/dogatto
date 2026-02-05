import { useState, useEffect, useRef } from 'react';
import { tagsApi } from '../api/tags';
import type { Tag } from '../api/tags';
import { ApiError } from '../api/error';
import '../styles/tag-selector.css';

/**
 * Tag selector component props.
 */
interface TagSelectorProps {
  selectedTags: Tag[];
  onChange: (tags: Tag[]) => void;
  maxTags?: number;
}

/**
 * Tag selector component.
 *
 * Multi-select dropdown for selecting tags with search functionality.
 */
export default function TagSelector({
  selectedTags,
  onChange,
  maxTags = 10,
}: TagSelectorProps): React.JSX.Element {
  const [allTags, setAllTags] = useState<Tag[]>([]);
  const [isOpen, setIsOpen] = useState(false);
  const [searchQuery, setSearchQuery] = useState('');
  const [error, setError] = useState('');
  const dropdownRef = useRef<HTMLDivElement>(null);

  /**
   * Fetches all available tags.
   */
  const fetchTags = async () => {
    try {
      const tags = await tagsApi.getTags();
      setAllTags(tags);
    } catch (err) {
      if (err instanceof ApiError) {
        setError(err.message);
      } else {
        setError('Failed to load tags');
      }
    }
  };

  /**
   * Filters tags based on search query.
   *
   * @return [Tag[]] Filtered tags
   */
  const getFilteredTags = (): Tag[] => {
    const query = searchQuery.toLowerCase().trim();
    if (!query) {
      return allTags.filter(
        tag => !selectedTags.some(selected => selected.ulid === tag.ulid)
      );
    }

    return allTags.filter(
      tag =>
        tag.name.toLowerCase().includes(query) &&
        !selectedTags.some(selected => selected.ulid === tag.ulid)
    );
  };

  /**
   * Handles tag selection.
   *
   * @param tag [Tag] Tag to select
   */
  const handleSelectTag = (tag: Tag) => {
    if (selectedTags.length >= maxTags) {
      return;
    }

    onChange([...selectedTags, tag]);
    setSearchQuery('');
    setIsOpen(false);
  };

  /**
   * Handles tag removal.
   *
   * @param tagUlid [string] Tag ULID to remove
   */
  const handleRemoveTag = (tagUlid: string) => {
    onChange(selectedTags.filter(tag => tag.ulid !== tagUlid));
  };

  /**
   * Handles click outside to close dropdown.
   */
  const handleClickOutside = (event: MouseEvent) => {
    if (dropdownRef.current && !dropdownRef.current.contains(event.target as Node)) {
      setIsOpen(false);
    }
  };

  useEffect(() => {
    fetchTags();
  }, []);

  useEffect(() => {
    document.addEventListener('mousedown', handleClickOutside);
    return () => {
      document.removeEventListener('mousedown', handleClickOutside);
    };
  }, []);

  const filteredTags = getFilteredTags();
  const isMaxReached = selectedTags.length >= maxTags;

  return (
    <div className="tag-selector" ref={dropdownRef}>
      <label className="tag-selector-label">
        Tags
        <span className="tag-count">
          ({selectedTags.length}/{maxTags})
        </span>
      </label>

      {error && (
        <div className="tag-selector-error">
          {error}
        </div>
      )}

      {/* Selected Tags Display */}
      {selectedTags.length > 0 && (
        <div className="selected-tags">
          {selectedTags.map(tag => (
            <div
              key={tag.ulid}
              className="selected-tag-badge"
              style={{ backgroundColor: tag.color }}
            >
              <span className="tag-name">{tag.name}</span>
              <button
                type="button"
                className="tag-remove-btn"
                onClick={() => handleRemoveTag(tag.ulid)}
                aria-label={`Remove ${tag.name}`}
              >
                ×
              </button>
            </div>
          ))}
        </div>
      )}

      {/* Tag Selector Input */}
      <div className="tag-selector-input-container">
        <input
          type="text"
          className="tag-selector-input"
          placeholder={isMaxReached ? 'Maximum tags reached' : 'Search and select tags...'}
          value={searchQuery}
          onChange={(e) => setSearchQuery(e.target.value)}
          onFocus={() => !isMaxReached && setIsOpen(true)}
          disabled={isMaxReached}
        />
        {isMaxReached && (
          <div className="max-tags-warning">
            Maximum of {maxTags} tags allowed
          </div>
        )}
      </div>

      {/* Dropdown List */}
      {isOpen && !isMaxReached && (
        <div className="tag-selector-dropdown">
          {filteredTags.length === 0 ? (
            <div className="dropdown-empty">
              {searchQuery ? 'No tags found' : 'No more tags available'}
            </div>
          ) : (
            <ul className="tag-options-list">
              {filteredTags.map(tag => (
                <li key={tag.ulid}>
                  <button
                    type="button"
                    className="tag-option"
                    onClick={() => handleSelectTag(tag)}
                  >
                    <div
                      className="tag-color-indicator"
                      style={{ backgroundColor: tag.color }}
                    />
                    <span className="tag-option-name">{tag.name}</span>
                  </button>
                </li>
              ))}
            </ul>
          )}
        </div>
      )}
    </div>
  );
}
