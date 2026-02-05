; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto/models/label
  (:use #:cl)
  (:import-from #:clails/model
                #:<base-model>
                #:defmodel
                #:make-record
                #:ref
                #:ref-error
                #:has-error-p
                #:save
                #:destroy
                #:execute-query
                #:query
                #:validate)
  (:import-from #:dogatto/utils/ulid
                #:generate-ulid)
  (:export #:<label>
           #:create-label
           #:find-label-by-id
           #:find-label-by-ulid
           #:find-labels-by-owner
           #:update-label
           #:delete-label
           #:check-label-name-uniqueness))

(in-package #:dogatto/models/label)

(defmodel <label> (<base-model>)
  (:table "labels"
   :relations ((:has-many "dogatto/models/label-tag::<label-tag>"
                :as :label-tags
                :foreign-key :label-id))))

(defun validate-label-name (label)
  "Validate label name.

   Checks if name is present and within length limits.

   @param label [<label>] Label instance
   "
  (let ((name (ref label :name)))
    (when (or (null name) (string= (string-trim '(#\Space #\Tab) name) ""))
      (setf (ref-error label :name) "Label name is required"))
    
    (when (and name (> (length name) 100))
      (setf (ref-error label :name) "Label name must be 100 characters or less"))))

(defun validate-label-description (label)
  "Validate label description.

   Checks if description is within length limits.

   @param label [<label>] Label instance
   "
  (let ((description (ref label :description)))
    (when (and description (> (length description) 1000))
      (setf (ref-error label :description) "Label description must be 1000 characters or less"))))

(defmethod validate ((label <label>))
  "Validate label data before saving.

   Checks:
   - name is required and not empty
   - name is 100 characters or less
   - description is 1000 characters or less (if provided)
   - owner-id is required
   "
  (validate-label-name label)
  (validate-label-description label)
  
  (when (or (null (ref label :owner-id))
            (not (integerp (ref label :owner-id))))
    (setf (ref-error label :owner-id) "Owner ID is required"))
  
  (when (or (null (ref label :ulid))
            (string= (ref label :ulid) ""))
    (setf (ref-error label :ulid) "ULID is required")))

(defun check-label-name-uniqueness (owner-id name &optional exclude-ulid)
  "Check if label name is unique for the user.

   Label names are case-insensitive unique per user.
   All queries filter by owner-id at SQL level.

   @param owner-id [integer] Owner user ID
   @param name [string] Label name to check
   @param exclude-ulid [string] ULID to exclude from check (for updates)
   @return [boolean] T if name is unique, NIL if duplicate exists
   "
  (let* ((trimmed-name (string-trim '(#\Space #\Tab) name))
         (existing (if exclude-ulid
                       (execute-query
                        (query <label>
                               :as :label
                               :where (:and (:= (:label :owner-id) :owner-id)
                                            (:= (:lower (:label :name)) :name)
                                            (:/= (:label :ulid) :exclude-ulid)))
                        (list :owner-id owner-id
                              :name (string-downcase trimmed-name)
                              :exclude-ulid exclude-ulid))
                       (execute-query
                        (query <label>
                               :as :label
                               :where (:and (:= (:label :owner-id) :owner-id)
                                            (:= (:lower (:label :name)) :name)))
                        (list :owner-id owner-id
                              :name (string-downcase trimmed-name))))))
    (null existing)))

(defun create-label (owner-id name description tag-ulids)
  "Create a new label.

   Creates a label with the specified attributes and saves it to the database.
   Tag associations are created separately using label-tag model.
   All operations filter by owner-id at SQL level.

   @param owner-id [integer] ID of the label owner (user)
   @param name [string] Label name (required, 1-100 characters)
   @param description [string] Label description (optional, max 1000 characters)
   @param tag-ulids [list] List of tag ULIDs to associate with this label
   @return [<label>] Created label instance
   @return [nil] If validation fails
   @condition error If label name already exists for user
   "
  (let* ((trimmed-name (string-trim '(#\Space #\Tab) name))
         (label (make-record '<label>
                             :ulid (generate-ulid)
                             :owner-id owner-id
                             :name trimmed-name
                             :description description
                             :merged-to-ulid nil)))
    
    ;; Check tag-ulids is not empty
    (when (or (null tag-ulids) (zerop (length tag-ulids)))
      (error "At least one tag is required"))
    
    ;; Check uniqueness (filtered by owner-id at SQL level)
    (unless (check-label-name-uniqueness owner-id trimmed-name)
      (error "Label name already exists"))
    
    ;; Save will call validate method
    (unless (save label)
      (return-from create-label nil))
    
    label))

(defun find-label-by-id (id owner-id)
  "Find a label by its internal ID.

   Query filters by owner-id at SQL level for security.

   @param id [integer] Label ID
   @param owner-id [integer] Owner user ID
   @return [<label>] Label instance
   @return [nil] If label not found or not owned by user
   "
  (first (execute-query
          (query <label>
                 :as :label
                 :where (:and (:= (:label :id) :id)
                              (:= (:label :owner-id) :owner-id)))
          (list :id id
                :owner-id owner-id))))

(defun find-label-by-ulid (ulid owner-id)
  "Find a label by its ULID.

   Query filters by owner-id at SQL level for security.

   @param ulid [string] Label ULID
   @param owner-id [integer] Owner user ID
   @return [<label>] Label instance
   @return [nil] If label not found or not owned by user
   "
  (first (execute-query
          (query <label>
                 :as :label
                 :where (:and (:= (:label :ulid) :ulid)
                              (:= (:label :owner-id) :owner-id)))
          (list :ulid ulid
                :owner-id owner-id))))

(defun find-labels-by-owner (owner-id &key page per-page sort order filter search-mode q)
  "Find labels belonging to a user with optional filtering and pagination.

   All queries filter by owner-id at SQL level for security.

   @param owner-id [integer] User ID
   @param page [integer] Page number (default 1)
   @param per-page [integer] Items per page (default 20, max 100)
   @param sort [keyword] Sort field (:name, :tag-count, :todo-count, :updated-at)
   @param order [keyword] Sort order (:asc, :desc)
   @param filter [keyword] Filter type (:all, :used, :unused)
   @param search-mode [keyword] Search mode (:label-name, :tag-name)
   @param q [string] Search query
   @return [list] List of label instances
   "
  (let* ((page (or page 1))
         (per-page (min (or per-page 20) 100))
         (offset (* (1- page) per-page))
         (sort-field (case sort
                       (:tag-count :tag-count)
                       (:todo-count :todo-count)
                       (:updated-at :updated-at)
                       (t :name)))
         (sort-order (if (eq order :desc) :desc :asc)))
    
    ;; Basic query filtered by owner-id
    ;; Complex filtering (tag search, todo count) will be implemented in controller
    (execute-query
     (query <label>
            :as :label
            :where (:= (:label :owner-id) :owner-id)
            :order-by ((:label sort-field sort-order))
            :limit per-page
            :offset offset)
     (list :owner-id owner-id))))

(defun update-label (ulid owner-id &key name description tag-ulids)
  "Update label attributes.

   Updates the specified attributes and saves changes to the database.
   Tag associations must be updated separately using label-tag model.
   Query filters by owner-id at SQL level for security.

   @param ulid [string] Label ULID
   @param owner-id [integer] Owner user ID
   @param name [string] New label name (optional)
   @param description [string] New label description (optional)
   @param tag-ulids [list] New list of tag ULIDs (optional)
   @return [<label>] Updated label instance
   @return [nil] If label not found or not owned by user or validation fails
   @condition error If new name already exists for user
   "
  (let ((label (find-label-by-ulid ulid owner-id)))
    (unless label
      (return-from update-label nil))
    
    (when name
      (let ((trimmed-name (string-trim '(#\Space #\Tab) name)))
        ;; Uniqueness check filtered by owner-id at SQL level
        (unless (check-label-name-uniqueness owner-id trimmed-name ulid)
          (error "Label name already exists"))
        
        (setf (ref label :name) trimmed-name)))
    
    (when description
      (setf (ref label :description) description))
    
    ;; Check tag-ulids if provided
    (when tag-ulids
      (when (or (null tag-ulids) (zerop (length tag-ulids)))
        (error "At least one tag is required")))
    
    (setf (ref label :updated-at) (get-universal-time))
    
    ;; Save will call validate method
    (unless (save label)
      (return-from update-label nil))
    
    label))

(defun delete-label (ulid owner-id)
  "Delete a label.

   Deletes the label from the database. Associated label_tags records
   will be deleted automatically by CASCADE constraint.
   Query filters by owner-id at SQL level for security.

   @param ulid [string] Label ULID
   @param owner-id [integer] Owner user ID
   @return [boolean] T if successful
   @return [nil] If label not found or not owned by user
   "
  (let ((label (find-label-by-ulid ulid owner-id)))
    (unless label
      (return-from delete-label nil))
    (destroy label)
    t))
