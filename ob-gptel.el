;;; ob-gptel.el --- Org-babel backend for GPTel AI interactions -*- lexical-binding: t -*-

;; Copyright (C) 2025 John Wiegley

;; Author: John Wiegley
;; Keywords: org, babel, ai, gptel
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (org "9.0") (gptel "0.5"))

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
    (:stream . nil)
    (:backend . nil))
  "Default header arguments for gptel source blocks.")

(defun org-babel-execute:gptel (body params)
  "Execute a gptel source block with BODY and PARAMS.
This function sends the BODY text to GPTel and returns the response."
  (let* ((model (cdr (assoc :model params)))
         (temperature (cdr (assoc :temperature params)))
         (max-tokens (cdr (assoc :max-tokens params)))
         (system-message (cdr (assoc :system params)))
         (stream (cdr (assoc :stream params)))
         (backend-name (cdr (assoc :backend params)))
         (original-model gptel-model)
         (original-temperature gptel-temperature)
         (original-max-tokens gptel-max-tokens)
         (original-system gptel--system-message)
         (original-stream gptel-stream)
         (original-backend gptel-backend)
         (buffer (current-buffer))
         result)

    (with-temp-buffer
      ;; Temporarily set gptel parameters if specified
      (when model
        (setq-local gptel-model (if (symbolp model) model (intern model))))
      (when temperature
        (setq-local gptel-temperature (string-to-number temperature)))
      (when max-tokens
        (setq-local gptel-max-tokens (string-to-number max-tokens)))
      (when system-message
        (setq-local gptel--system-message system-message))
      (when stream
        (setq-local gptel-stream (not (member stream '("no" "nil" "false")))))
      (when backend-name
        (let ((backend (gptel-get-backend backend-name)))
          (when backend
            (setq-local gptel-backend backend))))
      (let ((ob-gptel--uuid (concat "<gptel_thinking_" (uuidgen-1) ">")))
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
                        (replace-match (string-trim response) nil t)))))
                (funcall komplete response)))
          :buffer (current-buffer)
          :stream nil)
        ob-gptel--uuid))))

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

(provide 'ob-gptel)

;;; ob-gptel.el ends here
