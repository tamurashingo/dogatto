(in-package #:cl-user)
(defpackage #:dogatto/views/pages/show/package
  (:use #:cl)
  (:import-from #:clails/view/view-helper
                #:*view-context*
                #:view)
  (:import-from #:dogatto/helpers/asset-helper
                #:asset-path))

(in-package #:dogatto/views/pages/show/package)
