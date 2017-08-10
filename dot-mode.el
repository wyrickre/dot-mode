;;; dot-mode.el --- minor mode to repeat typing or commands

;;; Copyright (C) 1995 James Gillespie
;;; Copyright (C) 2000 Robert Wyrick (rob@wyrick.org)

;; Author: Robert Wyrick <rob@wyrick.org>
;; Maintainer: Robert Wyrick <rob@wyrick.org>
;; Keywords: convenience
;; Version: 1.13
;; URL: https://github.com/wyrickre/dot-mode
;; Package-Requires: ((emacs "24.3"))

;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; A copy of the GNU General Public License can be obtained from
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA
;;; 02139, USA.

;;; Commentary:

;;
;; Purpose of this package: minor mode to repeat typing or commands
;;
;; Installation instructions
;;
;; Install this file somewhere in your load path, byte-compile it and
;; add one of the following to your .emacs file (remove the comment
;; delimiters ;-)
;;
;; If you only want dot-mode to activate when you press "C-.", add the
;; the following to your .emacs:
;;
;;     (autoload 'dot-mode "dot-mode" nil t) ; vi `.' command emulation
;;     (global-set-key [(control ?.)] (lambda () (interactive) (dot-mode 1)
;;                                       (message "Dot mode activated.")))
;;
;; If you want dot-mode all the time (like me), add the following to
;; your .emacs:
;;
;;     (require 'dot-mode)
;;     (add-hook 'find-file-hooks 'dot-mode-on)
;;
;; You may still want to use the global-set-key above.. especially if you
;; use the *scratch* buffer.
;;
;; To toggle dot mode on or off type `M-x dot-mode'
;;
;; There are only two variables that allow you to modify how dot-mode
;; behaves:
;;           dot-mode-ignore-undo
;;           dot-mode-global-mode
;;
;; dot-mode-ignore-undo - defaults to t.  When nil, it will record keystrokes
;;     that generate an undo just like any other keystroke that changed the
;;     buffer.  I personally find that annoying, but if you want dot-mode to
;;     always remember your undo's:
;;         (setq dot-mode-ignore-undo nil)
;;     Besides, you can always use dot-mode-override to record an undo when
;;     you need to (or even M-x undo).
;;
;; dot-mode-global-mode - defaults to t.  When t, dot-mode only has one
;;     keyboard command buffer.  That means you can make a change in one
;;     buffer, switch buffers, then repeat the change.  When set to nil,
;;     each buffer gets its own command buffer.  That means that after
;;     making a change in a buffer, if you switch buffers, that change
;;     cannot repeated.  If you switch back to the first buffer, your
;;     change can then be repeated again.  This has a nasty side effect
;;     if your change yanks from the kill-ring (You could end up
;;     yanking text you killed in a different buffer).
;;     If you want to set this to nil, you should do so before dot-mode
;;     is activated on any buffers.  Otherwise, you may end up with some
;;     buffers having a local command buffer and others using the global
;;     one.
;;
;; Usage instructions:
;;
;; `C-.'    is bound to dot-mode-execute, which executes the buffer of
;;          stored commands as a keyboard macro.
;;
;; `C-M-.'  is bound to dot-mode-override, which will cause dot-mode
;;          to remember the next keystroke regardless of whether it
;;          changes the buffer and regardless of the value of the
;;          dot-mode-ignore-undo variable.
;;
;; `C-c-.'  is bound to dot-mode-copy-to-last-kbd-macro, which will
;;          copy the current dot mode keyboard macro to the last-kbd-macro
;;          variable.  It can then be executed via call-last-kbd-macro
;;          (normally bound to `C-x-e'), named via name-last-kbd-macro,
;;          and then inserted into your .emacs via insert-kbd-macro.
;;
;; Known bugs:
;;
;; none
;;

;;; COMMENTARY
;;;
;;; This mode is written to address one argument in the emacs vs. vi
;;; jihad :-)  It emulates the vi `redo' command, repeating the
;;; immediately preceding sequence of commands.  This is done by
;;; recording input commands which change the buffer, i.e. not motion
;;; commands.

;;; DESIGN
;;;
;;; The heart of this minor mode is a state machine.  The function
;;; dot-mode-after-change is called from after-change-functions and
;;; sets a variable (is there one already?  I couldn't find it) which
;;; is examined by dot-mode-loop, called from from post-command-hook.
;;; This variable, dot-mode-changed, is used in conjunction with
;;; dot-mode-state to move to the next state in the state machine.
;;; The state machine is hard coded into dot-mode-loop in the
;;; interests of speed; it uses two normal states (idle and store)
;;; and two corresponding override states which allow the user to
;;; forcibly store commands which do not change the buffer.
;;;
;;; TODO
;;; * Explore using recent-keys for this functionality

;;; Code:

(defconst dot-mode-version "1.13"
  "Report bugs to: Robert Wyrick <rob@wyrick.org>")

;;; CHANGE HISTORY
;;;
;;; 1.1
;;; Wrote dot-mode.el
;;;
;;; 1.2
;;; At the suggestion of Scott Evans <gse@ocsystems.com>, added
;;; 'dot-mode-override' to allow the user to force dot mode to store a
;;; motion command
;;;
;;; 1.3
;;; Changed dot-mode-loop to use a state machine instead of several
;;; booleans
;;;
;;; 1.4
;;; Hard coded the state machine into dot-mode-loop in the hope of
;;; speeding it up
;;;
;;; 1.5
;;; Ported to GNU Emacs - nearly: the keymap doesn't seem to install
;;; correctly.
;;;
;;; 1.6
;;; Rob Wyrick (that's me) took over maintenance of the package from
;;; Jim Gillespie.
;;;
;;; In some versions of Emacs, (this-command-keys) returns a empty
;;; vector by the time it is called from the 'post-command-hook.
;;; So, I split the functionality... now dot-mode-command-keys
;;; stores (this-command-keys) output in a temp variable to be used
;;; by dot-mode-loop and dot-mode-command-keys is called from the
;;; pre-command-hook.  Also re/ported to XEmacs/GNU Emacs.  It works
;;; on both now.  dot-mode-command-keys could have been put on the
;;; after-change-functions hook, but I've begun preliminary work to
;;; capture what's going on in the minibuffer and I'm certain I need
;;; it where it is.
;;;
;;; 1.7
;;; Added my first attempt to capture what the user is doing with
;;; execute-extended-command (M-x).  It even works if the executed
;;; command prompts the user.
;;; Also added some error recovery if the user interrupts or there is
;;; an error during execution of the stored macro.
;;;
;;; 1.8
;;; Second attempt to capture what the user is doing with
;;; execute-extended-command (M-x).  The previous version didn't work
;;; in XEmacs.  This version works in both XEmacs and GNUEmacs.
;;;
;;; 1.9
;;; Third attempt to capture what the user is doing with
;;; execute-extended-command (M-x).  Wow was I making things hard.
;;; It's cost me a lot of version numbers in a short amount of time,
;;; so we won't discuss my previous attempts. *grin*  My second attempt
;;; worked just fine, but it was more complicated and maybe not as
;;; portable to older version of X/GNU Emacs.
;;; Other things:
;;;   - Yet another restructuring of the code.  By doing so,
;;;     quoted-insert (C-q) is properly stored by dot-mode.
;;;     (quoted-insert has been broken since ver 1.6)
;;;   - Deleted an extraneous state and the "extended-state" added
;;;     in ver 1.8.  We're down to just two normal states and two
;;;     override states.
;;;   - Added dot-mode-ignore-undo and dot-mode-global-mode variables
;;;     as well as the new function dot-mode-copy-to-last-kbd-macro.
;;;
;;; 1.10
;;; Fixed a bug where the META key wasn't properly recorded on GNU
;;; Emacs.  Actually, if you used ESC for META (like me), everything
;;; worked fine.  But using ALT for META was broken.
;;; Now I'm using this-command-keys-vector when I can.
;;; I also added the dot-mode-event-to-string function to make the
;;; output a little prettier.
;;; Thanks to Scott Evans <gse@antisleep.com> for reporting the bug!
;;;
;;; 1.11
;;; Fixed a bug where dot-mode would give an error if you used
;;; dot-mode-override to record a <right> and then tried to call
;;; dot-mode-execute.  The bug was in dot-mode-event-to-string
;;; Thanks to Scott Evans <gse@antisleep.com> for reporting the bug!
;;;
;;; 1.12
;;; Make calls to make-local-hook optional for Emacs 24 compatibility.
;;; Use kmacro-display for displaying the macro string.
;;;
;;; 1.13
;;; Misc updates to follow elisp progression and add tests.
;;; Remove XEmacs compatibility.

(require 'kmacro)

(defvar dot-mode-global-mode t
  "Should dot-mode share its command buffer between buffers?")

(defvar dot-mode-ignore-undo t
  "Should dot-mode ignore undo?")

(defvar dot-mode-changed nil
  "Did last command change buffer?")

(defvar dot-mode-cmd-buffer nil
  "Saved commands.")

(defvar dot-mode-cmd-keys nil
  "Saved keys.")

(defvar dot-mode-state 0
  "Current state of dot mode.
0 - Initial (no changes)
1 - Recording buffer changes
2 - Override from state 0
3 - Override from state 1")

(defvar dot-mode-minibuffer-input nil
  "Global buffer to capture minibuffer input")

(defvar dot-mode-verbose t
  "Message the user every time a repeat happens")

;; n.b. This is a little tricky ... when the prefix-argument is changed it
;; doesn't leave much of a trace. It resets `this-command' and
;; `real-this-command' to the previous ones.
;; Hence the best way (that I know of) to tell whether the last command was
;; changing the prefix is by adding a hook into
;; `prefix-command-preserve-state-hook'.
(defvar dot-mode-prefix-arg nil
  "Marker variable to show the prefix argument has been changed.")

(defvar dot-mode-argument-buffer nil
  "Global buffer to store current digit argument.")

(defun dot-mode-buffer-to-string ()
  "Return the macro buffer as a string."
  (kmacro-display dot-mode-cmd-buffer))

(defun dot-mode-minibuffer-exit ()
  "Catch minibuffer exit"
  ;; I'd really like to check `this-command' to see if it's `exit-minibuffer'
  ;; and remove this function from the `minibuffer-exit-hook' if it is.
  ;; Unfortunately, if an extended command asks for 2 or more arguments,
  ;; the first arg would be the only one to get recorded since `exit-minibuffer'
  ;; is called between each argument.
  (push (minibuffer-contents) dot-mode-minibuffer-input))

(defun dot-mode-after-change (start end prevlen)
  "Dot mode's `after-change-functions' hook"
  ;; By the time we get here, `dot-mode-pre-hook' has already setup
  ;; `dot-mode-cmd-keys.'  It'll be a `vector', `t', or `nil'.
  (cond ((vectorp dot-mode-cmd-keys)
         ;; We just did `execute-extended-command' or an override.
         ;; If we're in override, the keys have already been read and
         ;; `dot-mode-changed' is `t'
         (unless dot-mode-changed
           (remove-hook 'minibuffer-exit-hook 'dot-mode-minibuffer-exit)
           (unless (null dot-mode-minibuffer-input)
             ;; The first item in this list is what was in the minibuffer
             ;; after choosing the command from either
             ;; `execute-extended-command' or `smex'.
             ;; This may very well not be the name of the command, so we
             ;; replace it with the head of the list
             ;; `extended-command-history'.
             (setq dot-mode-cmd-keys
                   (vconcat dot-mode-cmd-keys
                            (mapconcat
                             #'identity
                             (cons (car extended-command-history)
                                   (cdr (nreverse dot-mode-minibuffer-input)))
                             "\r"))))))
        ;; Normal mode
        (dot-mode-cmd-keys
         (setq dot-mode-cmd-keys (this-command-keys-vector))))
  ;; Else, do nothing `dot-mode-cmd-keys' will remain `nil'.
  ;; (Only happens on `ignore-undo')
  (when dot-mode-cmd-keys
    (setq dot-mode-changed t)))

(defun dot-mode-pre-hook ()
  "Dot mode's `pre-command-hook'"

  ;; remove hook (should already be removed... but double check)
  ;; The only time this will ever do any good is if you did a
  ;; quit out of the minibuffer.  In that case, the hook will
  ;; still be there.  It won't really hurt anything, it will just
  ;; continue to record everything you do in the minibuffer
  ;; regardless of whether or not it is an `execute-extended-command'.
  ;; And the `dot-mode-minibuffer-input' buffer could get quite large.
  (remove-hook 'minibuffer-exit-hook 'dot-mode-minibuffer-exit)

  (cond
   ;; Is this an `execute-extended-command' or `smex'?
   ((member this-command '(execute-extended-command smex))
    (setq dot-mode-minibuffer-input nil
          ;; Must get this (M-x) now!  It's gone later.
          dot-mode-cmd-keys         (this-command-keys-vector)
          ;; ignore an override
          dot-mode-changed          nil)
    ;; Must be a global hook
    (add-hook 'minibuffer-exit-hook 'dot-mode-minibuffer-exit))
   (dot-mode-changed ;; on override, `dot-mode-changed' is t
    ;; Always read the keys here on override _UNLESS_ it's a `quoted-insert'.
    ;; This is to make sure we capture keys that don't change the buffer.
    ;; On `quoted-insert', all we get here is , but in `dot-mode-after-change',
    ;; we get  plus the following key (and we're guaranteed to change the
    ;; buffer)
    (setq dot-mode-cmd-keys (or (eq this-command 'quoted-insert)
                                (this-command-keys-vector))))
   ;; Should we ignore this key sequence? (is it an undo?)
   ((and dot-mode-ignore-undo
         (member this-command '(advertised-undo undo undo-tree-undo undo-tree-redo)))
    (setq dot-mode-cmd-keys nil))
   ;; signal to read later (in `dot-mode-after-change')
   (t (setq dot-mode-cmd-keys t))))

;; (defun dot-mode--state-name ()
;;   (nth dot-mode-state '("Initial (no changes)"
;;                         "Recording buffer changes"
;;                         "Override from recording"
;;                         "Override from initial")))

(defun dot-mode-prefix-command-hook () (setq dot-mode-prefix-arg t))
(defun dot-mode-loop ()
  "The heart of dot mode."
  ;; (message "in:\tstate: \"%s\"\n\tcommand: \"%S\""
  ;;          (dot-mode--state-name) this-command)
  ;; (message "in: cmd-buffer is '%s'" (dot-mode-buffer-to-string))

  ;; Record all digit-argument and universal-argument functions
  (cond (dot-mode-prefix-arg
         ;; Keep this keypress around, and don't change the current state
         (setq dot-mode-prefix-arg nil
               dot-mode-argument-buffer (vconcat dot-mode-argument-buffer (this-command-keys-vector))))
        ((= dot-mode-state 0)           ; idle
         (if dot-mode-changed
             (setq dot-mode-state       1
                   dot-mode-changed     nil
                   dot-mode-cmd-buffer  (vconcat dot-mode-argument-buffer dot-mode-cmd-keys)))
         (setq dot-mode-argument-buffer nil))
        ((= dot-mode-state 1)           ; recording
         (if dot-mode-changed
             (setq dot-mode-changed     nil
                   dot-mode-cmd-buffer  (vconcat dot-mode-cmd-buffer dot-mode-argument-buffer dot-mode-cmd-keys))
           (setq dot-mode-state 0))
         (setq dot-mode-argument-buffer nil))
        (t ; = 2 or 3                   ; override
         (setq dot-mode-state       (- dot-mode-state 2)
               dot-mode-changed     t)))
  ;; (message "out: state is \"%s\"" (dot-mode--state-name))
  ;; (message "out: cmd-buffer is '%s'" (dot-mode-buffer-to-string))
  )

(defun dot-mode-remove-hooks ()
  (remove-hook 'pre-command-hook 'dot-mode-pre-hook t)
  (remove-hook 'post-command-hook 'dot-mode-loop t)
  (remove-hook 'after-change-functions 'dot-mode-after-change t)
  (remove-hook 'prefix-command-preserve-state-hook 'dot-mode-prefix-command-hook t))

(defun dot-mode-add-hooks ()
  (add-hook 'pre-command-hook 'dot-mode-pre-hook nil t)
  (add-hook 'post-command-hook 'dot-mode-loop nil t)
  (add-hook 'after-change-functions 'dot-mode-after-change nil t)
  (add-hook 'prefix-command-preserve-state-hook 'dot-mode-prefix-command-hook nil t))

;;;###autoload
(defun dot-mode-copy-to-last-kbd-macro ()
  "Copy the current `dot-mode' command buffer to the `last-kbd-macro' variable.
Then it can be called with `call-last-kbd-macro', named with
`name-last-kbd-macro', or even saved for later use with
`name-last-kbd-macro'"
  (interactive)
  (if (null dot-mode-cmd-buffer)
      (message "Nothing to copy.")
    (setq last-kbd-macro dot-mode-cmd-buffer)
    (message "Copied.")))

;;;###autoload
(defun dot-mode-execute ()
  "Execute stored commands."
  (interactive)
  ;; Don't want execution to kick off infinite recursion
  (if (null dot-mode-cmd-buffer)
      (message "Nothing to repeat")
    (dot-mode-remove-hooks)
    ;; Do the business
    (when dot-mode-verbose
      (message "Repeating \"%s\"" (dot-mode-buffer-to-string)))
    (condition-case nil
        (execute-kbd-macro dot-mode-cmd-buffer)
      ((error quit exit)
       (setq dot-mode-cmd-buffer nil
             dot-mode-state      0)
       (message "Dot mode reset")))
    (if (and (not (null dot-mode-cmd-buffer))
             dot-mode-verbose)
        ;; I message before AND after a macro execution.
        ;; This way you'll know if your macro somehow
        ;; hangs during execution.
        (message "Repeated \"%s\"" (dot-mode-buffer-to-string)))
    ;; Put the hooks back
    (dot-mode-add-hooks)))

;;;###autoload
(defun dot-mode-override ()
  "Unconditionally store next keystroke."
  (interactive)
  (setq dot-mode-state (+ dot-mode-state 2))
  ;; If dot-mode-argument-buffer is non nil then we were in the middle (or at
  ;; the end of a argument chain). In that case we take care to not break it.
  ;; If it is `nil', then `universal-argument--mode' was not previously active,
  ;; and we don't activate it in order to avoid changing behaviour.
  ;; n.b. We're checking `dot-mode-argument-buffer' as a variable that happens
  ;; to be `nil' when we weren't in an argument chain, it's *not* the "thing
  ;; we're interested in".
  (when dot-mode-argument-buffer
    ;; The docstring of `prefix-command-update' says we need to call it whenever
    ;; we change the "prefix command state".
      (progn(prefix-command-update)
            (setq prefix-arg current-prefix-arg)
            (universal-argument--mode)))
  (message "dot-mode will remember the next keystroke..."))

;;;###autoload
(define-minor-mode dot-mode
  "Dot mode mimics the `.' function in vi, repeating sequences of
commands and/or typing delimited by motion events.  Use `C-.'
rather than just `.'."  nil " Dot"
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-.")   'dot-mode-execute)
    (define-key map (kbd "C-M-.") 'dot-mode-override)
    (define-key map (kbd "C-c .") 'dot-mode-copy-to-last-kbd-macro)
    map)
  (if (not dot-mode)
      (dot-mode-remove-hooks)
    (dot-mode-add-hooks)
    (if dot-mode-global-mode
        (progn
          (kill-local-variable 'dot-mode-cmd-buffer)
          (kill-local-variable 'dot-mode-cmd-keys)
          (kill-local-variable 'dot-mode-state)
          (kill-local-variable 'dot-mode-changed)
          (kill-local-variable 'dot-mode-prefix-arg)
          (kill-local-variable 'dot-mode-argument-buffer))
      ;; ELSE
      (make-local-variable 'dot-mode-cmd-buffer)
      (make-local-variable 'dot-mode-cmd-keys)
      (make-local-variable 'dot-mode-state)
      (make-local-variable 'dot-mode-changed)
      (make-local-variable 'dot-mode-prefix-arg)
      (make-local-variable 'dot-mode-argument-buffer)
      (setq dot-mode-state        0
            dot-mode-changed      nil
            dot-mode-cmd-buffer   nil
            dot-mode-cmd-keys     nil
            dot-mode-prefix-arg   nil
            dot-mode-argument-buffer nil))))

;;;###autoload
(defun dot-mode-on ()
  "Turn on dot-mode."
  (interactive)
  ;; Ignore internal buffers -- this stops modifications in the echo area being
  ;; recorded as a macro that gets used elsewhere.
  (unless (or (eq ?\  (aref (buffer-name) 0))
              ;; Also ignore the *Messages* buffer -- when `dot-mode' is enabled
              ;; here some recursion happens due to the `after-change-functions'
              ;; in that buffer getting called.
              (eq (current-buffer) (messages-buffer))
              ;; I suspect all minibuffers will have a space at the start of
              ;; their buffer name, and hence I won't need this check.
              ;; Unfortunately I can't find any documentation
              ;; disproving/confirming this, so we include this check.
              (minibufferp))
    (dot-mode 1)))

;;;###autoload
(defalias 'turn-on-dot-mode 'dot-mode-on)
;;;###autoload
(define-global-minor-mode global-dot-mode dot-mode dot-mode-on)

(provide 'dot-mode)

;;; dot-mode.el ends here
