;; Author: Losich Alexey (some.any.key@gmail.com), 2017

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(defmacro switch (inValue &rest clauses)
  `(cond ,@(mapcar (lambda (c) 
                     (destructuring-bind (matchValue &rest actions) c
                       `((eq ,matchValue ,inValue)
                         ,@actions)))
                   clauses)))

(defun test-indent (text function)
  (with-temp-buffer
    (kotlin-mode)
    (insert text)
    (let ((indent-line-function function))
      (indent-region (point-min) (point-max)))

    (buffer-substring-no-properties (point-min) (point-max))))


(defmacro search-skipper (skip-unsearchable-zone &rest actions)
  (let ((result (gensym)))
    `(progn
       ,skip-unsearchable-zone
       (loop for ,result = (progn ,@actions) while ,skip-unsearchable-zone finally return ,result))))

(defun go-to-comment-start ()
  (let ((comment-beginning (comment-beginning)))
    (when comment-beginning
      (goto-char comment-beginning)
      t)))

(defun go-to-comment-end ()
  (when (save-excursion
          (comment-beginning))
    (goto-char 
     (save-excursion 
       (or (+ 1 (line-end-position))
           ;; TODO: use comment from syntax table
           (search-forward-regexp "*/" nil t))))))

(defmacro go-skipping-comments (direction &rest actions)
  (when (null direction)
    (error "direction is nil"))
  (let ((go-out-of-comment (gensym))
        (walker (switch direction 
                        (:backward
                         'go-to-comment-start)
                        (:forward
                         'go-to-comment-end))))
    (when (null walker)
      (error (concat "waler is nil, direction is: " (pp-to-string direction))))
    `(labels ((,go-out-of-comment ()
                                  (,walker)))

       (search-skipper
        (,go-out-of-comment)
        ,@actions))))

(defun go-out-of-string (direction)
  (cl-labels ((is-in-string (pt) 
                            (eq (get-text-property pt 'face) 'font-lock-string-face))
              (need-go-back ()
                            (and (is-in-string (point))
                                 (is-in-string (- (point) 1))
                                 (not (eq (point) (point-min))))))
    (let ((walker (switch direction
                          (:backward
                           'backward-char)
                          
                          (:forward)
                          'forward-char)))
    (prog1 (need-go-back)
      (while (need-go-back)
        (funcall walker))))))


(defmacro go-skip-comment-string (direction &rest actions)
  `(go-skipping-comments ,direction
    (search-skipper 
     (go-out-of-string ,direction)
     ,@actions)))

(defun find-closest (open-re close-re)
  (save-excursion
    (cl-labels ((find-back (re) 
                           (save-excursion
                             (go-skip-comment-string :backward
                              (search-backward-regexp re nil t)))))
      (let ((closest-open (find-back open-re))
            (closest-close (find-back close-re)))

        (cond ((and (null closest-open)
                    (null closest-close))
               nil)

              ((null closest-open)
               (list :close closest-close))

              ((null closest-close)
               (list :open closest-open))
              
              ((> closest-open closest-close)
               (list :open closest-open))

              ((< closest-open closest-close)
               (list :close closest-close)))))))






(defun find-closest-unclosed (open-re close-re)
  (macrolet ((with-search ((type position) &rest body)
                          `(destructuring-bind 
                               (&optional ,type position)
                               (find-closest open-re close-re)
                               (when ,type
                                 ,@body))))
    (cl-labels ((skip-sexp-back
                 ()
                 (with-search (type position)



                              (cond ((eq type :open)
                                     (goto-char position))

                                    ((eq type :close)
                                     (goto-char position)
                                     (skip-sexp-back)
                                     (skip-sexp-back)))))
                (search-step 
                 ()
                 (with-search (type position)



                              (cond ((eq type :open)
                                     position)
                                    
                                    ((eq type :close)
                                     (goto-char position)
                                     (skip-sexp-back)
                                     (search-step))))))
      (save-excursion
        (search-step)))))


(defun find-base-positions ()
  (cl-labels ((find-base (type open close)
                         (let ((base (find-closest-unclosed open close)))
                           (when base 
                             (list type base))))
              (find-next-base ()
                              (first 
                               (sort*
                               (delete-if 
                                'null
                                (list (find-base :block "{" "}")
                                      (find-base :args "(" ")")
                                      (find-base :args "<" ">")))
                               '>
                               :key 'second)))
              (go-next-base ()
                            (let ((b (find-next-base)))
                              (when b
                                (goto-char (second b))
                                b))))
    (save-excursion 
      (loop for base = (go-next-base) while base collect base))))

(setq kotlin-mode-line-continuation-symbols '("\\+" "-" "=" "\\." "\\?\\." ":"))

(setq kotlin-block-indent-depth 4)
(setq kotlin-continuation-indent-depth 4)


(defun kotlin-mode--previous-indent ()
  (save-excursion 
  (block 'searching
    (let ((indent 0))
      (loop for base in (find-base-positions)
            do (destructuring-bind (type position) base
                 (cond ((eq type :args)
                        (save-excursion 
                          (goto-char position)
                          (cl-return-from 'searching (+ 1 indent (- position (line-beginning-position))))))

                       ((eq type :block)
                        (setq indent (+ indent kotlin-block-indent-depth))))))
      indent))))

(defun line-begins-with (regexp)
  ;; TODO: take string/comment into consideration
  (save-excursion 
    (beginning-of-line)
    (looking-at (concat "[ \t]*" regexp))))

(defun kotlin-mode--this-line-indent-change ()
  (save-excursion
    (cond ((line-begins-with "}")
           (- kotlin-block-indent-depth))
          ((find-if 'line-begins-with
                    kotlin-mode-line-continuation-symbols)
           (+ kotlin-continuation-indent-depth))
        (0))))

(defun kotlin-mode--elk-indent-line ()
  "Indent current line as kotlin code"
  (interactive)
  (let ((line-position (- (point) (line-beginning-position)))
        (line-indentation (current-indentation)))
    (beginning-of-line)
    (if (bobp) ; 1.)
        (progn
          (indent-line-to 0))
      (progn (indent-line-to (+ (kotlin-mode--previous-indent)

                                (kotlin-mode--this-line-indent-change)))
             (forward-char (- line-position line-indentation))))))

;; (test-indent
;; "class Camera360Model
;; {
;;     public enum class CameraConnectionResult
;;     {
;;         val e = some
;; // more comment (
;; // more comment <
;; // more comment {
;;             .call(some,
;;                   arg,
;;                   object : Callback<T,
;; E>(args,
;;                                     /* comment */ moreArgs) { // comment
;;                       fun someCall(arg:Arg,
;;                                    more:More?) : Fun
;;                       {
;;                       /* comment */    someAction
;;                           otherAction
;;                       }
;;                   },
;;                   other)
;;     }
;; }"
;; 'kotlin-mode--elk-indent-line)


;; ))(
