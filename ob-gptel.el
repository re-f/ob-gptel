;;; ob-gptel.el --- Org-babel backend for GPTel AI interactions -*- lexical-binding: t -*-

;; Copyright (C) 2025 John Wiegley

;; Author: John Wiegley
;; Keywords: org, babel, ai, gptel
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (org "9.0") (gptel "0.9.8.5"))

;;; Commentary:

;; This package provides an Org-babel backend for GPTel, allowing
;; AI interactions directly within Org mode source blocks.
;;
;; Usage:
;;   #+begin_src gptel :model gpt-4 :temperature 0.7
;;   What is the capital of France?
;;   #+end_src

;;; Code:

(require 'ob)
(require 'gptel)

(defvar org-babel-default-header-args:gptel
  '((:results . "replace")
    (:exports . "both")
    (:model . nil)
    (:temperature . nil)
    (:max-tokens . nil)
    (:system . nil)
    (:backend . nil)
    (:dry-run . nil)
    (:preset . nil)
    (:context . nil)
    (:prompt . nil))
  "Default header arguments for gptel source blocks.")

(defun ob-gptel-find-prompt (prompt &optional system-message)
  "Given a PROMPT identifier, find the block/result pair it names.
The result is a directive in the format of `gptel-directives', which
includes the SYSTEM-MESSAGE, the block as a message in the USER role,
and the result in the ASSISTANT role."
  (let ((directives (list system-message)))
    (let ((block (org-babel-find-named-block prompt)))
      (when block
        (save-excursion
          (goto-char block)
          (let ((info (and block
                           (save-excursion
                             (goto-char block)
                             (org-babel-get-src-block-info)))))
            (when info
              (nconc directives (list (and info (nth 1 info))))
              (let ((result (org-babel-where-is-src-block-result nil info)))
                (when result
                  (goto-char result)
                  (nconc directives (list (org-babel-read-result))))))))))
    directives))

(defun ob-gptel--add-context (context)
  "Call `gptel--transform-add-context' with the given CONTEXT."
  `(lambda (callback fsm)
     (setq-local gptel-context--alist
                 ,(if (stringp context)
                      (list (list context))
                    (mapcar #'list context)))
     (gptel--transform-add-context callback fsm)))

(defmacro ob-gptel--with-preset (name &rest body)
  "Run BODY with gptel preset NAME applied.
This macro can be used to create `gptel-request' command with settings
from a gptel preset applied.  NAME is the preset name, typically a
symbol."
  (declare (indent 1))
  `(let ((name ,name))
     (cl-progv (and name (gptel--preset-syms (gptel-get-preset name)))
         nil
       (if name (gptel--apply-preset name))
       ,@body)))

(defun org-babel-execute:gptel (body params)
  "Execute a gptel source block with BODY and PARAMS.
This function sends the BODY text to GPTel and returns the response."
  (let* ((model (cdr (assoc :model params)))
         (temperature (cdr (assoc :temperature params)))
         (max-tokens (cdr (assoc :max-tokens params)))
         (system-message (cdr (assoc :system params)))
         (backend-name (cdr (assoc :backend params)))
         (prompt (cdr (assoc :prompt params)))
         (preset (cdr (assoc :preset params)))
         (context (cdr (assoc :context params)))
         (dry-run (cdr (assoc :dry-run params)))
         (buffer (current-buffer))
         (dry-run (and dry-run (not (member dry-run '("no" "nil" "false")))))
         (ob-gptel--uuid (concat "<gptel_thinking_" (org-id-uuid) ">"))
         (fsm
          (ob-gptel--with-preset (and preset (intern-soft preset))
            (let ((gptel-model
                   (if model
                       (if (symbolp model) model (intern model))
                     gptel-model))
                  (gptel-temperature
                   (if temperature
                       (string-to-number temperature)
                     gptel-temperature))
                  (gptel-max-tokens
                   (if max-tokens
                       (string-to-number max-tokens)
                     gptel-max-tokens))
                  (gptel--system-message
                   (or system-message
                       gptel--system-message))
                  (gptel-backend
                   (if backend-name
                       (let ((backend (gptel-get-backend backend-name)))
                         (if backend
                             (setq-local gptel-backend backend)
                           gptel-backend))
                     gptel-backend)))
              (gptel-request
                  body
                :callback
                #'(lambda (response info)
                    (when (stringp response)
                      (with-current-buffer buffer
                        (save-excursion
                          (save-restriction
                            (widen)
                            (goto-char (point-min))
                            (when (search-forward ob-gptel--uuid nil t)
                              (replace-match (string-trim response) nil t)))))))
                :buffer (current-buffer)
                :transforms (list #'gptel--transform-apply-preset
                                  (ob-gptel--add-context context))
                :system (and prompt
                             (with-current-buffer buffer
                               (ob-gptel-find-prompt prompt system-message)))
                :dry-run dry-run
                :stream nil)))))
    (if dry-run
        (thread-first
          fsm
          (gptel-fsm-info)
          (plist-get :data)
          (pp-to-string))
      ob-gptel--uuid)))

(defun org-babel-prep-session:gptel (session params)
  "Prepare SESSION according to PARAMS.
GPTel blocks don't use sessions, so this is a no-op."
  session)

(defun ob-gptel-var-to-gptel (var)
  "Convert an elisp VAR into a string for GPTel."
  (format "%S" var))

(defun org-babel-variable-assignments:gptel (params)
  "Return list of GPTel statements assigning variables from PARAMS."
  (mapcar
   (lambda (pair)
     (format "%s = %s"
             (car pair)
             (ob-gptel-var-to-gptel (cdr pair))))
   (org-babel--get-vars params)))

(with-eval-after-load 'org-src
  (add-to-list 'org-src-lang-modes '("gptel" . text)))

(provide 'ob-gptel)

;;; ob-gptel.el ends here
