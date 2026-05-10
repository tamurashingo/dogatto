; -*- mode: lisp -*-
(in-package #:cl-user)
(defpackage #:dogatto/controllers/labels-controller
  (:use #:cl
        #:clails/controller/base-controller)
  (:import-from #:dogatto/services/label-service
                #:list-labels
                #:get-label
                #:create-new-label
                #:update-existing-label
                #:delete-existing-label
                #:estimate-todo-count)
  (:import-from #:dogatto/helpers/auth-helper
                #:get-authenticated-user)
  (:import-from #:dogatto/helpers/json-converters
                #:label-to-json
                #:tag-to-json)
  (:import-from #:dogatto/utils/request
                #:read-body-as-string)
  (:import-from #:clails/model
                #:ref)
  (:export #:<labels-list-controller>
           #:<label-item-controller>
           #:<label-estimate-controller>))

(in-package #:dogatto/controllers/labels-controller)

(defclass <labels-list-controller> (<rest-controller>)
  ()
  (:documentation "Controller for labels collection (GET /labels, POST /labels)"))

(defclass <label-item-controller> (<rest-controller>)
  ()
  (:documentation "Controller for single label item (GET /labels/:ulid, PUT /labels/:ulid, DELETE /labels/:ulid)"))

(defclass <label-estimate-controller> (<rest-controller>)
  ()
  (:documentation "Controller for TODO count estimation (GET /labels/estimate-todo-count)"))

;; GET /api/v1/labels
(defmethod do-get ((controller <labels-list-controller>))
  "Get all labels for the authenticated user with optional filtering."
  (let ((user (get-authenticated-user (env controller))))
    (unless user
      (setf (slot-value controller 'clails/controller/base-controller:code) 401)
      (return-from do-get
        (set-response controller
                     `(("status" . "error")
                       ("message" . "Authentication required")))))

    (let* ((query-params (getf (env controller) :query-string))
           (params (when query-params
                     (quri:url-decode-params query-params)))
           (page (parse-integer (or (cdr (assoc "page" params :test #'string=)) "1")
                                :junk-allowed t))
           (per-page (parse-integer (or (cdr (assoc "per_page" params :test #'string=)) "20")
                                    :junk-allowed t))
           (sort (cdr (assoc "sort" params :test #'string=)))
           (order (cdr (assoc "order" params :test #'string=)))
           (filter (cdr (assoc "filter" params :test #'string=)))
           (search-mode (cdr (assoc "search_mode" params :test #'string=)))
           (q (cdr (assoc "q" params :test #'string=)))
           (result (list-labels (ref user :id)
                                :page page
                                :per-page per-page
                                :sort sort
                                :order order
                                :filter filter
                                :search-mode search-mode
                                :q q)))

      (if (getf result :success)
          (let* ((labels-data (getf result :labels))
                 (stats (getf result :stats))
                 (labels-json (mapcar #'(lambda (entry)
                                          (label-to-json (getf entry :label)
                                                         (getf entry :tag-count)
                                                         (getf entry :todo-count)))
                                      labels-data)))
            (setf (slot-value controller 'clails/controller/base-controller:code) 200)
            (set-response controller
                         `(("status" . "success")
                           ("data" . (("labels" . ,labels-json)
                                     ("stats" . (("totalLabels" . ,(getf stats :total-labels))
                                                ("usedLabels" . ,(getf stats :used-labels))
                                                ("unusedLabels" . ,(getf stats :unused-labels)))))))))
          (progn
            (setf (slot-value controller 'clails/controller/base-controller:code) 500)
            (set-response controller
                         `(("status" . "error")
                           ("message" . ,(car (getf result :errors))))))))))

;; POST /api/v1/labels
(defmethod do-post ((controller <labels-list-controller>))
  "Create a new label."
  (let ((user (get-authenticated-user (env controller))))
    (unless user
      (setf (slot-value controller 'clails/controller/base-controller:code) 401)
      (return-from do-post
        (set-response controller
                     `(("status" . "error")
                       ("message" . "Authentication required")))))

    (let* ((body (read-body-as-string (getf (env controller) :raw-body)))
           (json-data (jonathan:parse body :as :alist))
           (name (cdr (assoc "name" json-data :test #'string=)))
           (description (cdr (assoc "description" json-data :test #'string=)))
           (tag-ulids (cdr (assoc "tagUlids" json-data :test #'string=)))
           (result (create-new-label (ref user :id) name description tag-ulids)))

      (if (getf result :success)
          (let ((label-json (label-to-json (getf result :label)
                                           (getf result :tag-count)
                                           (getf result :todo-count))))
            (setf (slot-value controller 'clails/controller/base-controller:code) 201)
            (set-response controller
                         `(("status" . "success")
                           ("data" . (("label" . ,label-json))))))
          (progn
            (setf (slot-value controller 'clails/controller/base-controller:code) 400)
            (set-response controller
                         `(("status" . "error")
                           ("message" . ,(car (getf result :errors))))))))))

;; GET /api/v1/labels/:ulid
(defmethod do-get ((controller <label-item-controller>))
  "Get a single label by ULID."
  (let ((user (get-authenticated-user (env controller))))
    (unless user
      (setf (slot-value controller 'clails/controller/base-controller:code) 401)
      (return-from do-get
        (set-response controller
                     `(("status" . "error")
                       ("message" . "Authentication required")))))

    (let* ((ulid (param controller "ulid"))
           (result (get-label ulid (ref user :id))))

      (if (getf result :success)
          (let ((label-json (label-to-json (getf result :label)
                                           (getf result :tag-count)
                                           (getf result :todo-count)
                                           (getf result :tags))))
            (setf (slot-value controller 'clails/controller/base-controller:code) 200)
            (set-response controller
                         `(("status" . "success")
                           ("data" . (("label" . ,label-json))))))
          (progn
            (setf (slot-value controller 'clails/controller/base-controller:code) 404)
            (set-response controller
                         `(("status" . "error")
                           ("message" . ,(car (getf result :errors))))))))))

;; PUT /api/v1/labels/:ulid
(defmethod do-put ((controller <label-item-controller>))
  "Update a label."
  (let ((user (get-authenticated-user (env controller))))
    (unless user
      (setf (slot-value controller 'clails/controller/base-controller:code) 401)
      (return-from do-put
        (set-response controller
                     `(("status" . "error")
                       ("message" . "Authentication required")))))

    (let* ((ulid (param controller "ulid"))
           (body (read-body-as-string (getf (env controller) :raw-body)))
           (json-data (jonathan:parse body :as :alist))
           (name (cdr (assoc "name" json-data :test #'string=)))
           (description (cdr (assoc "description" json-data :test #'string=)))
           (tag-ulids (cdr (assoc "tagUlids" json-data :test #'string=)))
           (result (update-existing-label ulid (ref user :id)
                                          :name name
                                          :description description
                                          :tag-ulids tag-ulids)))

      (if (getf result :success)
          (let ((label-json (label-to-json (getf result :label)
                                           (getf result :tag-count)
                                           (getf result :todo-count))))
            (setf (slot-value controller 'clails/controller/base-controller:code) 200)
            (set-response controller
                         `(("status" . "success")
                           ("data" . (("label" . ,label-json))))))
          (progn
            (setf (slot-value controller 'clails/controller/base-controller:code) 400)
            (set-response controller
                         `(("status" . "error")
                           ("message" . ,(car (getf result :errors))))))))))

;; DELETE /api/v1/labels/:ulid
(defmethod do-delete ((controller <label-item-controller>))
  "Delete a label."
  (let ((user (get-authenticated-user (env controller))))
    (unless user
      (setf (slot-value controller 'clails/controller/base-controller:code) 401)
      (return-from do-delete
        (set-response controller
                     `(("status" . "error")
                       ("message" . "Authentication required")))))

    (let* ((ulid (param controller "ulid"))
           (result (delete-existing-label ulid (ref user :id))))

      (if (getf result :success)
          (progn
            (setf (slot-value controller 'clails/controller/base-controller:code) 204)
            (set-response controller nil))
          (progn
            (setf (slot-value controller 'clails/controller/base-controller:code) 404)
            (set-response controller
                         `(("status" . "error")
                           ("message" . ,(car (getf result :errors))))))))))

;; GET /api/v1/labels/estimate-todo-count
(defmethod do-get ((controller <label-estimate-controller>))
  "Estimate TODO count for given tag ULIDs."
  (let ((user (get-authenticated-user (env controller))))
    (unless user
      (setf (slot-value controller 'clails/controller/base-controller:code) 401)
      (return-from do-get
        (set-response controller
                     `(("status" . "error")
                       ("message" . "Authentication required")))))

    (let* ((query-params (getf (env controller) :query-string))
           (params (when query-params
                     (quri:url-decode-params query-params)))
           (tag-ulids-str (cdr (assoc "tag_ulids" params :test #'string=)))
           (tag-ulids (when tag-ulids-str
                        (cl-ppcre:split "," tag-ulids-str)))
           (result (estimate-todo-count (ref user :id) tag-ulids)))

      (if (getf result :success)
          (progn
            (setf (slot-value controller 'clails/controller/base-controller:code) 200)
            (set-response controller
                         `(("status" . "success")
                           ("data" . (("count" . ,(getf result :count)))))))
          (progn
            (setf (slot-value controller 'clails/controller/base-controller:code) 400)
            (set-response controller
                         `(("status" . "error")
                           ("message" . ,(car (getf result :errors))))))))))
