;;; test-conversion.el --- Test markdown to org conversion -*- lexical-binding: t -*-

;; Test the gptel--convert-markdown->org function

(require 'ob-gptel)
(require 'gptel-org)

(defun test-markdown-to-org ()
  "Test the markdown to org conversion function."
  (let ((test-cases
         '(;; Headers
           ("# Header 1\n## Header 2\n### Header 3"
            "* Header 1\n** Header 2\n*** Header 3")
           
           ;; Bold and italic
           ("**bold text**"
            "*bold text*")
           ("*italic text*"
            "/italic text/")
           ("_italic underscore_"
            "/italic underscore/")
           
           ;; Inline code
           ("`inline code`"
            "~inline code~")
           
           ;; Code blocks
           ("```python\ndef hello():\n    print(\"Hello\")\n```"
            "#+begin_src python\ndef hello():\n    print(\"Hello\")\n#+end_src")
           
           ;; Links
           ("[Link text](https://example.com)"
            "[[https://example.com][Link text]]")
           
           ;; Combined elements
           ("# Title\n\nSome **bold** and *italic* text with `code`.\n\n```python\nprint(\"test\")\n```"
            "* Title\n\nSome *bold* and /italic/ text with ~code~.\n\n#+begin_src python\nprint(\"test\")\n#+end_src"))))
    
    (dolist (test test-cases)
      (let* ((input (car test))
             (expected (cadr test))
             (result (gptel--convert-markdown->org input)))
        (if (string= result expected)
            (message "✓ Test passed: %s" (substring input 0 (min 30 (length input))))
          (message "✗ Test failed:\n  Input: %s\n  Expected: %s\n  Got: %s"
                   input expected result))))))

(test-markdown-to-org)

;;; test-conversion.el ends here