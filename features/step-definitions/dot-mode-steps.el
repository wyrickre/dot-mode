;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(Given "^I load \"\\([^\"]+\\)\"$"
  (lambda (library)
    (require (intern library))))

(And "^I activate undo-tree$"
  (lambda ()
    (undo-tree-mode 1)))

(And "^I bind M-x to smex$"
  (lambda ()
    (global-set-key (kbd "M-x") 'smex)))

;; NOTE Don't have a generalised version for see vs only see because espuds
;; already has a (Then "^I should see$") step
(Then "^I should only see\\(?: \"\\(.+\\)\"\\|:\\)$"
  "Asserts that the current buffer just has some text."
  (lambda (expected)
    (let ((actual (buffer-string))
          (message "Expected\n%s\nto match:\n%s"))
      (cl-assert (string= expected actual) nil message expected actual))))

(Given "^I bind \"\\([^\"]+\\)\" to \"\\([^\"]+\\)\"$"
  (lambda (command key)
    (global-set-key (kbd key) (intern command))))
