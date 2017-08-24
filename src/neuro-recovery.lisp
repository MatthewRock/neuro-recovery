;;;; neuro-recovery.lisp

(in-package #:neuro-recovery)

;;; "neuro-recovery" goes here. Hacks and glory await!

(defparameter *neuroshima-elx-base-address*
  (puri:uri "http://neuroshima.elx.pl"))
(defparameter *default-category-name* "Generic site")
(defparameter *default-title* "<no title>")
(defparameter *data-path* (merge-pathnames
                           #P"Programming/Lisp/neuro-recovery/data/"
                           (user-homedir-pathname)))

(-> get-text-from-node (html5-parser::element &key (default-value string)) string)
(defun get-text-from-node (node &key (default-value ""))
  "Return text from node which has a text-node child"
  (let* ((child (html5-parser:node-first-child node))
         (text (when child (html5-parser:node-value child))))
    (or text default-value)))

(-> get-page ((or string puri::uri)) html5-parser::document)
(defun get-page (uri)
  "Return parsed contents of page."
  (html5-parser:parse-html5
   (drakma:http-request uri)))

(-> get-page-category (html5-parser::document) string)
(defun get-page-category (document)
  "Return the category that page resides in, or default category name."
  (let* ((maybe-category-list
          (css-selectors:query "h3" document))
         (n-titles (length maybe-category-list))
         (category (when (= 1 n-titles)
                     (get-text-from-node
                      (car maybe-category-list)
                      :default-value *default-category-name*))))
    (or category *default-category-name*)))

(-> get-page-link-nodes (html5-parser::document) list)
(defun get-page-link-nodes (document)
  "Return links to content of the page."
  (css-selectors:query ".a_tyt" document))

(-> get-link-title (html5-parser::element) string)
(defun get-link-title (link-node)
  "Return link's title or default title."
  (get-text-from-node link-node :default-value *default-title*))

(-> get-element-date (html5-parser::element) string)
(defun get-element-date (element)
  ;; Date always starts with "dodano: " followed by the date itself...
  (subseq (html5-parser:element-attribute element "title") 8))

(-> get-page-links (html5-parser::document) list)
(defun get-page-links (document)
  "Get links to other pages of the site(second, third, ...)"
  (mapcar (alexandria:rcurry #'html5-parser:element-attribute "href")
          (css-selectors:query ".text > center > a" document)))

(-> get-all-available-pages () list)
(defun get-all-available-pages ()
  "Query the site for the available pages with articles. Return list of lists of pages. One list means one category."
  (log4cl:log-info "Getting the list of available pages...")
  (loop for i from 0 below 100
     for ending = (format nil "/articles.php?topic=~D" i)
     for document = (html5-parser:parse-html5
                     (drakma:http-request
                      (format nil "http://neuroshima.elx.pl~A" ending)))
     for elements = (css-selectors:query
                     ".a_tyt"
                     document)
     when elements collecting
       (mapcar (alexandria:rcurry #'puri:merge-uris "http://neuroshima.elx.pl")
               (cons ending
                     (mapcar (alexandria:curry #'concatenate 'string "/articles.php")
                             (get-page-links document))))))

(-> get-page-uri-from-link-node (html5-parser::element &optional puri::uri) puri::uri)
(defun get-page-uri-from-link-node (link-node &optional (root *neuroshima-elx-base-address*))
  "Return URI to the page leading from the A element"
  (puri::merge-uris (html5-parser:element-attribute link-node "href") root))

(-> harvest-pages (list) null)
(defun harvest-pages (page-uris)
  "Harvest all pages from the list of uris."
  (log4cl:log-info "Harvesting pages from list ~A..." page-uris)
  (pmap nil #'harvest-page page-uris))

(-> harvest-page (puri::uri &optional pathname) t)
(defun harvest-page (page-uri &optional (path-beginning *data-path*))
  "Harvest all the articles found at the PAGE and save it at folder PATH-BEGINNING."
  (let* ((document (get-page page-uri))
         (category (get-page-category document))
         (links (get-page-link-nodes document))
         (file-folder (merge-pathnames
                       (pathname (format nil "~A/" (escape-string category)))
                       path-beginning)))
    (log4cl:log-info "Harvesting category ~A..." category)
    (ensure-directories-exist file-folder)
    (pmap nil (alexandria:rcurry #'harvest-text-page file-folder) links)))

(-> harvest-text-page (html5-parser::element string pathname) t)
(defun harvest-text-page (link-node &optional (path-beginning *data-path*))
  "Harvest contents of the article and store at folder PATH-BEGINNING"
  (let* ((title (get-link-title link-node))
         (creation-date (get-element-date link-node))
         (text-page-uri (get-page-uri-from-link-node link-node))
         (file-path (merge-pathnames (create-filename-from-string title) path-beginning)))
    (log4cl:log-info "Harvesting article ~A..." title)
    (with-open-file (out file-path :direction :output :if-exists :supersede :external-format :utf-8)
      (print-header title creation-date text-page-uri out)
      (html5-parser:element-map-children
       (alexandria:rcurry #'parse-node out)
       (css-selectors:query1 ".text" (get-page text-page-uri))))))

(-> harvest-all-pages () null)
(defun harvest-all-pages ()
  "Harvest all available pages."
  (log4cl:log-info "Starting the harvest...")
  ;; If you wish to use lparallel's pmap, comment out the next line and uncomment the one with kernel.
  (setf (fdefinition 'pmap) #'map)
  ;(setf lparallel:*kernel* (lparallel:make-kernel 8))
  (pmap nil #'harvest-pages (get-all-available-pages)))

(-> parse-node (html5-parser::node stream) null)
(defun parse-node (node out)
  "Parse node and print it to OUT stream."
  (cond
    ((typep node 'html5-parser::text-node)
     (write-to-out-wrapping-lines (html5-parser:node-value node) out))
    ((typep node 'html5-parser::element)
     (let ((node-name (html5-parser::node-name node)))
       (cond
         ((string= node-name "br")
          (format out "~%"))
         ((string= node-name "i")
          (format out "/")
          (html5-parser:element-map-children (alexandria:rcurry #'parse-node out) node)
          (format out "/"))
         ((string= node-name "b")
          (format out "*")
          (html5-parser:element-map-children (alexandria:rcurry #'parse-node out) node)
          (format out "*"))
         ((or (string= node-name "a") (string= node-name "img"))
          (format out "[[~A][~A]]"
                  (html5-parser:element-attribute node "href")
                  "link"))
         (t (html5-parser:element-map-children (alexandria:rcurry #'parse-node out) node)))))))

(-> print-header (string string puri::uri stream) null)
(defun print-header (title creation-date uri out)
  "Print information about the article to OUT stream using the passed arguments."
  (format out "Artykuł ~S dodany ~A. Oryginalny URI: ~A.~%~%~%"
          title creation-date uri))

(-> create-filename-from-string (string) pathname)
(defun create-filename-from-string (str)
  "Escape string and return it as a pathname."
  (pathname (format nil "~A.org" (escape-string str))))

(-> escape-string (string) string)
(defun escape-string (str)
  "Escape string from hazardous characters unsuitable for pathname."
  (let ((pairs-to-replace'(("/" ".") (" " "_") ("\\?" "") ("!" "") ("\\." "")
                           ("\\-" "_") ("," "")
                           ("\\(" "") ("\\)" "") ("\\[" "") ("\\]" ""))))
    (replace-pairs pairs-to-replace str)))

(-> replace-pairs (list string) string)
(defun replace-pairs (pairs string)
  "Replace pairs (regex replacement) from pairs in string."
  (loop for (regex replacement) in pairs
     for result = (cl-ppcre:regex-replace-all regex string replacement)
     then (cl-ppcre:regex-replace-all regex result replacement)
     finally (return result)))


(-> write-to-out-wrapping-lines (string stream &optional integer) null)
(defun write-to-out-wrapping-lines (text out &optional (line-size 80))
  (loop
     with buffer = (- line-size (floor line-size 10))
     with guard = nil
     for c across text
     for i from 0 do
       (when (= i buffer)
         (setf guard t))
       (if (= i line-size)
           (progn
             (format out "-~%~A" c)
             (setf i 0
                   guard nil))
           (if (and guard (member c '(#\Space #\Tab #\Newline) :test #'char=))
               (progn
                 (format out "~%")
                 (setf i 0
                       guard nil))
               (princ c out)))))
