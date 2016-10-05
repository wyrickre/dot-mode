(require 'f)

(defvar dot-mode-support-path
  (f-dirname load-file-name))

(defvar dot-mode-features-path
  (f-parent dot-mode-support-path))

(defvar dot-mode-root-path
  (f-parent dot-mode-features-path))

(add-to-list 'load-path dot-mode-root-path)

(require 'dot-mode)
(require 'espuds)
(require 'ert)

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
