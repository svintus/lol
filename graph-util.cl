

;; DOT graphing routines


(defun dot-name (exp)
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))


(defparameter *max-label-length* 30)

;; Convert an expression into a DOT-compatible label
;; exp -> str -> str[30] | s:str | s != "-"
(defun dot-label (exp)
  (if exp
    (let ((s (write-to-string exp :pretty nil)))
      (if (> (length s) *max-label-length*)
          (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
          s))
    ""))

;; Convert a list of nodes into DOT format
;; [node->label] -> str([DOT(node->label)])
(defun nodes->dot (nodes)
  (mapc (lambda (node)
        (fresh-line)
        (princ (dot-name (car node)))
        (princ "[label=\"")
        (princ (dot-label node))
        (princ "\";"))
   nodes))


;; Convert a list of edges into DOT format
(defun edges->dot (edges)
  (mapc (lambda (node)
          (mapc (lambda (edge)
                  (fresh-line)
                  (princ (dot-name (car node)))
                  (princ "->")
                  (princ (dot-name (car edge)))
                  (princ "[label=\"")
                  (princ (dot-label (cdr edge)))
                  (princ "\"];"))
                (cdr node)))
        edges))


;; Generate a DOT graph from a list of edges and a list of nodes
(defun graph-dot (nodes edges)
  (princ "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}"))


;; DOT->.png conversion routines


(defun dot->png (fname thunk)
  (with-open-file (*standard-output*
                    fname
                    :direction :output
                    :if-exists :supersede)
    (funcall thunk))

  (ext:shell (concatenate 'string "dot -Tpng -O " fname)))


(defun graph->png (fname nodes edges)
     (dot->png fname
               (lambda ()
                 (graph-dot nodes edges))))


;; Convert a list on undirectional edges to DOT format
;; The duplicate egde elimination via maplist is notably elegant
(defun uedges->dot (edges)
  (maplist (lambda (lst)
             (mapc (lambda (edge)
                     (unless (assoc (car edge) (cdr lst))
                       (fresh-line)
                         (princ (dot-name (caar lst)))
                         (princ "--")
                         (princ (dot-name (car edge)))
                         (princ "[label=\"")
                         (princ (dot-label (cdr edge)))
                         (princ "\"];")))
                   (cdar lst)))
           edges))


(defun ugraph->dot (nodes edges)
  (princ "graph{")
  (nodes->dot nodes)
  (uedges->dot edges)
  (princ "}"))


(defun ugraph->png (fname nodes edges)
  (dot->png fname
            (lambda ()
              (ugraph->dot nodes edges))))


           
