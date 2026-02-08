import { useState, useEffect } from 'react';
import { useNavigate } from 'react-router-dom';
import Header from '../components/Header';
import LabelCard from '../components/LabelCard';
import LabelFormModal from '../components/LabelFormModal';
import LabelDeleteConfirm from '../components/LabelDeleteConfirm';
import { labelsApi } from '../api/labels';
import type { Label, LabelStats, CreateLabelRequest, UpdateLabelRequest, LabelSearchParams } from '../api/labels';
import { ApiError } from '../api/error';
import '../styles/labels.css';

/**
 * Labels page component.
 *
 * Displays list of labels with create, edit, delete, search, and filter functionality.
 */
export default function LabelsPage(): React.JSX.Element {
  const navigate = useNavigate();
  const [labels, setLabels] = useState<Label[]>([]);
  const [stats, setStats] = useState<LabelStats>({ totalLabels: 0, usedLabels: 0, unusedLabels: 0 });
  const [isLoading, setIsLoading] = useState(true);
  const [error, setError] = useState('');
  const [isModalOpen, setIsModalOpen] = useState(false);
  const [editingLabel, setEditingLabel] = useState<Label | undefined>(undefined);
  const [deletingLabel, setDeletingLabel] = useState<Label | undefined>(undefined);
  
  // Search and filter state
  const [searchMode, setSearchMode] = useState<'label_name' | 'tag_name'>('label_name');
  const [searchQuery, setSearchQuery] = useState('');
  const [filter, setFilter] = useState<'all' | 'used' | 'unused'>('all');
  const [sortBy, setSortBy] = useState<'name' | 'tag_count' | 'todo_count' | 'updated_at'>('name');
  const [sortOrder, setSortOrder] = useState<'asc' | 'desc'>('asc');

  /**
   * Fetches labels from API with current search/filter parameters.
   */
  const fetchLabels = async () => {
    try {
      setIsLoading(true);
      setError('');
      
      const params: LabelSearchParams = {
        sort: sortBy,
        order: sortOrder,
        filter,
      };
      
      if (searchQuery.trim()) {
        params.search_mode = searchMode;
        params.q = searchQuery.trim();
      }
      
      const data = await labelsApi.getLabels(params);
      setLabels(data.labels);
      setStats(data.stats);
    } catch (err) {
      if (err instanceof ApiError) {
        setError(err.message);
      } else {
        setError('Failed to load labels');
      }
    } finally {
      setIsLoading(false);
    }
  };

  /**
   * Handles label creation.
   *
   * @param data [CreateLabelRequest] Label data
   */
  const handleCreate = async (data: CreateLabelRequest | UpdateLabelRequest) => {
    try {
      await labelsApi.createLabel(data as CreateLabelRequest);
      await fetchLabels(); // Refresh list
    } catch (err) {
      if (err instanceof ApiError) {
        setError(err.message);
      } else {
        setError('Failed to create label');
      }
      throw err;
    }
  };

  /**
   * Handles label update.
   *
   * @param data [UpdateLabelRequest] Label data
   */
  const handleUpdate = async (data: CreateLabelRequest | UpdateLabelRequest) => {
    if (!editingLabel) return;

    try {
      // Fetch full label details first if tags are not loaded
      let labelWithTags = editingLabel;
      if (!editingLabel.tags) {
        labelWithTags = await labelsApi.getLabelByUlid(editingLabel.ulid);
        setEditingLabel(labelWithTags);
      }

      await labelsApi.updateLabel(labelWithTags.ulid, data as UpdateLabelRequest);
      await fetchLabels(); // Refresh list
    } catch (err) {
      if (err instanceof ApiError) {
        setError(err.message);
      } else {
        setError('Failed to update label');
      }
      throw err;
    }
  };

  /**
   * Opens edit modal.
   *
   * @param label [Label] Label to edit
   */
  const handleEdit = async (label: Label) => {
    // Fetch full label details including tags
    try {
      const labelWithTags = await labelsApi.getLabelByUlid(label.ulid);
      setEditingLabel(labelWithTags);
      setIsModalOpen(true);
    } catch (err) {
      if (err instanceof ApiError) {
        setError(err.message);
      } else {
        setError('Failed to load label details');
      }
    }
  };

  /**
   * Closes modal.
   */
  const handleCloseModal = () => {
    setIsModalOpen(false);
    setEditingLabel(undefined);
  };

  /**
   * Opens delete confirmation dialog.
   *
   * @param label [Label] Label to delete
   */
  const handleDeleteClick = (label: Label) => {
    setDeletingLabel(label);
  };

  /**
   * Confirms and executes label deletion.
   */
  const handleConfirmDelete = async () => {
    if (!deletingLabel) return;

    try {
      await labelsApi.deleteLabel(deletingLabel.ulid);
      setDeletingLabel(undefined);
      await fetchLabels(); // Refresh list
    } catch (err) {
      if (err instanceof ApiError) {
        setError(err.message);
      } else {
        setError('Failed to delete label');
      }
      setDeletingLabel(undefined);
    }
  };

  /**
   * Cancels label deletion.
   */
  const handleCancelDelete = () => {
    setDeletingLabel(undefined);
  };

  /**
   * Handles label click to view details/apply filter.
   *
   * @param label [Label] Clicked label
   */
  const handleLabelClick = (label: Label) => {
    // Navigate to TODO list with label filter
    navigate(`/todos?label=${label.ulid}`);
  };

  /**
   * Handles search form submit.
   *
   * @param e [React.FormEvent] Form event
   */
  const handleSearch = (e: React.FormEvent) => {
    e.preventDefault();
    fetchLabels();
  };

  /**
   * Clears search query.
   */
  const handleClearSearch = () => {
    setSearchQuery('');
  };

  /**
   * Toggles sort order.
   */
  const toggleSortOrder = () => {
    setSortOrder(sortOrder === 'asc' ? 'desc' : 'asc');
  };

  // Fetch labels on mount and when search/filter params change
  useEffect(() => {
    fetchLabels();
  }, [filter, sortBy, sortOrder]);

  return (
    <div className="labels-page">
      <Header />
      <main className="main-content">
        <div className="labels-header">
          <h1>Labels</h1>
          <button onClick={() => setIsModalOpen(true)} className="btn-create">
            Create Label
          </button>
        </div>

        {/* Statistics */}
        <div className="label-stats">
          <div className="stat-item">
            <span className="stat-label">Total Labels:</span>
            <span className="stat-value">{stats.totalLabels}</span>
          </div>
          <div className="stat-item">
            <span className="stat-label">Used:</span>
            <span className="stat-value">{stats.usedLabels}</span>
          </div>
          <div className="stat-item">
            <span className="stat-label">Unused:</span>
            <span className="stat-value">{stats.unusedLabels}</span>
          </div>
        </div>

        {/* Search and Filter */}
        <div className="labels-controls">
          <form onSubmit={handleSearch} className="search-form">
            <select
              value={searchMode}
              onChange={(e) => setSearchMode(e.target.value as 'label_name' | 'tag_name')}
              className="search-mode-select"
            >
              <option value="label_name">Search by Label Name</option>
              <option value="tag_name">Search by Tag Name</option>
            </select>
            <input
              type="text"
              value={searchQuery}
              onChange={(e) => setSearchQuery(e.target.value)}
              placeholder={searchMode === 'label_name' ? 'Search labels...' : 'Search by tag name...'}
              className="search-input"
            />
            <button type="submit" className="btn-search">Search</button>
            {searchQuery && (
              <button type="button" onClick={handleClearSearch} className="btn-clear">Clear</button>
            )}
          </form>

          <div className="filter-controls">
            <label>Filter:</label>
            <select value={filter} onChange={(e) => setFilter(e.target.value as 'all' | 'used' | 'unused')}>
              <option value="all">All Labels</option>
              <option value="used">Used Labels</option>
              <option value="unused">Unused Labels</option>
            </select>

            <label>Sort by:</label>
            <select value={sortBy} onChange={(e) => setSortBy(e.target.value as any)}>
              <option value="name">Name</option>
              <option value="tag_count">Tag Count</option>
              <option value="todo_count">TODO Count</option>
              <option value="updated_at">Last Updated</option>
            </select>

            <button onClick={toggleSortOrder} className="btn-sort-toggle">
              {sortOrder === 'asc' ? '↑' : '↓'}
            </button>
          </div>
        </div>

        {error && (
          <div className="error-message">
            {error}
          </div>
        )}

        {isLoading ? (
          <div className="loading">Loading labels...</div>
        ) : labels.length === 0 ? (
          <div className="empty-state">
            {searchQuery ? (
              <p>No labels found matching "{searchQuery}"</p>
            ) : (
              <>
                <p>No labels yet. Create your first label to group TODOs by tags!</p>
                <button onClick={() => setIsModalOpen(true)} className="btn-create-large">
                  Create Your First Label
                </button>
              </>
            )}
          </div>
        ) : (
          <div className="labels-grid">
            {labels.map(label => (
              <LabelCard
                key={label.ulid}
                label={label}
                onClick={() => handleLabelClick(label)}
                onEdit={() => handleEdit(label)}
                onDelete={() => handleDeleteClick(label)}
              />
            ))}
          </div>
        )}
      </main>

      <LabelFormModal
        isOpen={isModalOpen}
        mode={editingLabel ? 'edit' : 'create'}
        label={editingLabel}
        onClose={handleCloseModal}
        onSubmit={editingLabel ? handleUpdate : handleCreate}
      />

      {deletingLabel && (
        <LabelDeleteConfirm
          isOpen={true}
          label={deletingLabel}
          onConfirm={handleConfirmDelete}
          onCancel={handleCancelDelete}
        />
      )}
    </div>
  );
}
