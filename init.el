;; -*- Mode:emacs-lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      emacs
;; @author    Mitch Richling <https://www.mitchr.me>
;; @brief     My Emacs dot file.@EOL
;; @std       Emacs Lisp unix windows osx
;; @copyright
;;  @parblock
;;  Copyright (c) 1989-2017, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
;;
;;  Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
;;
;;  1. Redistributions of source code must retain the above copyright notice, this list of conditions, and the following disclaimer.
;;
;;  2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions, and the following disclaimer in the documentation
;;     and/or other materials provided with the distribution.
;;
;;  3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software
;;     without specific prior written permission.
;;
;;  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
;;  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
;;  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;  LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
;;  DAMAGE.
;;  @endparblock
;; @warning   Very specific to my needs -- a bug for everyone else and a feature for me.@EOL@EOL
;; @warning   Tries to load mjr-dark-theme.el in your .emacs.d direcory.@EOL@EOL
;; @warning   You will need to fix the stuff under "Manual-Meta-Config".@EOL@EOL
;; @filedetails
;;
;; This config makes use of some external tools without which some functionality will be missing:
;;
;;     * MJR-thingy-lookeruper
;;       - MJR-home-bin/browser
;;       - /usr/bin/ldapsearch
;;       - /usr/bin/getent
;;       - /usr/local/bin/de
;;       - /home/sysadmin/bin/pde
;;       - /usr/bin/finger
;;       - /usr/bin/dig
;;       - /usr/bin/nslookup
;;     * MJR-view-file-or-url-at-point
;;       - MJR-home-bin/mjrpdfview
;;     * MJR-latexit
;;       - MJR-home-bin/latexit.rb
;;     * MJR-insert-from-web
;;       - MJR-home-bin/curl
;;     * MJR-term
;;       - MJR-home-bin/s
;;       - MJR-home-bin/sn
;;       - MJR-home-bin/t
;;       - MJR-home-bin/tn
;;       - MJR-home-bin/td
;;       - /bin/bash
;;       - /bin/zsh
;;     * Several paths are checked for Macaulay, Maxima, and common lisp.
;;
;; I keep common stuff in a "core" directory I take with me.  Some stuff this config looks for:
;;     * MJR-home-cor/codeBits/  -- A directory of source code templates MJR-prepend-header
;;     * MJR-home-cor/codeBits/cheaderSTD.txt  -- Used by MJR-fix-c-includes
;;     * MJR-home-cor/elisp      -- A directory containing various bits of elisp
;;        * MJR-home-cor/elisp/git/
;;        * MJR-home-cor/elisp/gap/
;;        * MJR-home-cor/elisp/octave/
;;        * MJR-home-cor/elisp/processing/
;;     * MJR-home-cor/texinputs   Used for bookmarks  (TeX input files and templtes)
;;     * MJR-home-cor/org-mode    Used for bookmarks  (org-mode input files and templtes)
;;     * MJR-home-cor/lispy       Used for bookmarks  (production copy of *mjrcalc*)
;;     * MJR-home-cor/yasnippets  Used for yasnippets
;;
;; Stuff to play with later:
;;  * htmlfontify.el - (v23.2)
;;  * bubbles - like SameGame.
;;  * display-time-world
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "MJR: INIT: STAGE: Start Customizing Emacs....")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun MJR-quiet-message (&rest rest)
  "Log a message to the *Messages* buffer, but do not display the message"
  (let ((inhibit-message 't))
    (apply #'message rest)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: STAGE: Pre-Customizing Emacs (performance tweaks)....")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq gc-cons-threshold 50000000)
(add-hook 'emacs-startup-hook (lambda ()
                                (message "MJR: POST-INIT: HOOK: emacs-startup-hook")
                                (setq gc-cons-threshold 800000)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: STAGE: Manual-Meta-Config...")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set to "auto-config" and the values will be set to a best guess, or hard-wire the value to something here.
(defvar MJR-expert-mode "auto-config")
(defvar MJR-pookie-mode "auto-config")
(defvar MJR-uname       "auto-config")
(defvar MJR-home        "auto-config")
(defvar MJR-home-bin    "auto-config")
(defvar MJR-home-cor    "auto-config")
(defvar MJR-home-dot    "auto-config")
(defvar MJR-location    "auto-config")
(defvar MJR-platform    "auto-config")

(MJR-quiet-message "MJR: INIT: STAGE: Manual-Meta-Config: MJR-expert-mode: %s" MJR-expert-mode)
(MJR-quiet-message "MJR: INIT: STAGE: Manual-Meta-Config: MJR-pookie-mode: %s" MJR-pookie-mode)
(MJR-quiet-message "MJR: INIT: STAGE: Manual-Meta-Config: MJR-uname:       %s" MJR-uname)
(MJR-quiet-message "MJR: INIT: STAGE: Manual-Meta-Config: MJR-home:        %s" MJR-home)
(MJR-quiet-message "MJR: INIT: STAGE: Manual-Meta-Config: MJR-home-bin:    %s" MJR-home-bin)
(MJR-quiet-message "MJR: INIT: STAGE: Manual-Meta-Config: MJR-home-cor:    %s" MJR-home-cor)
(MJR-quiet-message "MJR: INIT: STAGE: Manual-Meta-Config: MJR-home-dot:    %s" MJR-home-dot)
(MJR-quiet-message "MJR: INIT: STAGE: Manual-Meta-Config: LOCATION:        %s" MJR-location)
(MJR-quiet-message "MJR: INIT: STAGE: Manual-Meta-Config: PLATFORM:        %s" MJR-platform)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: STAGE: Auto-Meta-Config...")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl-flet ((set-if-auto-config (var val)
                           (if (string-equal (symbol-value var) "auto-config")
                               (set var val))))
  ;; If we have a recognized login-name, then auto-config MJR-expert-mode, MJR-pookie-mode, & MJR-uname
  (let ((urln (user-real-login-name)))
    (cond ((find urln '("richmit"
                        "a0864027"
                        "mjr")      :test #'string=) (progn (set-if-auto-config 'MJR-uname      "richmit")))
          ((find urln '("jrichli"
                        "swift")    :test #'string=) (progn (set-if-auto-config 'MJR-uname      "jrichli")
                                                            (set-if-auto-config 'MJR-expert-mode nil)
                                                            (set-if-auto-config 'MJR-pookie-mode 't))))

    (set-if-auto-config 'MJR-uname       urln)
    (set-if-auto-config 'MJR-expert-mode 't)
    (set-if-auto-config 'MJR-pookie-mode nil))

  ;; Check some hardwired fixed paths first, then use the magical ~ path if we fail.
  (set-if-auto-config 'MJR-home (or (let ((tmp (expand-file-name "~")))
                                      (and tmp (file-exists-p tmp) tmp))
                                    (find-if #'file-exists-p (mapcar (lambda (p) (concat p MJR-uname)) '("/Users/"  ;; OSX & Windows
                                                                                                         "/home/"
                                                                                                         "/u/")))))
  ;; Set MJR-home-bin, MJR-home-cor, & MJR-home-dot
  (if MJR-home
      (dolist (vvp '((MJR-home-bin . ("bin"  "" ))
                     (MJR-home-cor . ("core" "" ))
                     (MJR-home-dot . (""        ))))
        (let ((variable   (car vvp))
              (candidates (cdr vvp)))
          (dolist (candidate candidates)
            (let ((p (concat MJR-home "/" candidate)))
              (if (file-exists-p p) (set-if-auto-config variable p)))))))
  ;; Set MJR-location
  (set-if-auto-config 'MJR-location (cond ((or (string-equal user-real-login-name "a0864027")
                                               (file-exists-p "/apps/")
                                               (file-exists-p "/apps/flames/data")
                                               (file-exists-p "/home/flames/data")
                                               (file-exists-p "/Users/a0864027"))        "WORK:TI")
                                          ((or (file-exists-p "/home/Shared/core/")
                                               (file-exists-p "/media/sf_richmit/core/")
                                               (file-exists-p "/Users/richmit"))         "HOME")
                                          ('t                                            "UNKNOWN")))
  ;; Set MJR-platform
  (set-if-auto-config 'MJR-platform (cond ((string-match "mingw-nt"  system-configuration) "WINDOWS-MGW")
                                          ((string-match "mingw32"   system-configuration) "WINDOWS-MGW")
                                          ((string-match "mingw64"   system-configuration) "WINDOWS-MGW")
                                          ((string-match "cygwin"    system-configuration) "WINDOWS-CYG")
                                          ((string-match "linux"     system-configuration) "LINUX")
                                          ((string-match "darwin"    system-configuration) "DARWIN")
                                          ('t                                              "UNKNOWN"))))

(MJR-quiet-message "MJR: INIT: STAGE: Auto-Meta-Config: MJR-expert-mode: %s" MJR-expert-mode)
(MJR-quiet-message "MJR: INIT: STAGE: Auto-Meta-Config: MJR-pookie-mode: %s" MJR-pookie-mode)
(MJR-quiet-message "MJR: INIT: STAGE: Auto-Meta-Config: MJR-uname:       %s" MJR-uname)
(MJR-quiet-message "MJR: INIT: STAGE: Auto-Meta-Config: MJR-home:        %s" MJR-home)
(MJR-quiet-message "MJR: INIT: STAGE: Auto-Meta-Config: MJR-home-bin:    %s" MJR-home-bin)
(MJR-quiet-message "MJR: INIT: STAGE: Auto-Meta-Config: MJR-home-cor:    %s" MJR-home-cor)
(MJR-quiet-message "MJR: INIT: STAGE: Auto-Meta-Config: MJR-home-dot:    %s" MJR-home-dot)
(MJR-quiet-message "MJR: INIT: STAGE: Auto-Meta-Config: LOCATION:        %s" MJR-location)
(MJR-quiet-message "MJR: INIT: STAGE: Auto-Meta-Config: PLATFORM:        %s" MJR-platform)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: STAGE: initializing package manager packages...")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq package-enable-at-startup nil)
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: STAGE: Require Section...")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'compile)
(require 'paren)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: STAGE: Autoloads for things in init file...")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'thing-at-point-looking-at "thingatpt"  "Return non-nil if point is in or just after a match for REGEXP." t)
(autoload 'thing-at-point            "thingatpt"  "Return string for thing at point."                               t)
(autoload 'image-mode-as-text        "image-mode" "Load image as text")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: STAGE: Define MJR Functions..")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun MJR-find-newest-core-package (package-name)
  "Find the preferred package version in my 'core' elisp repository.

Packages are named: FOO-SOMETHING or FOO=SOMETHING
  * FOO is generally the name of the package as provided by the author
  * SOMETHING is generally the date I downloaded it, the version, etc...

The preferred version is the LAST one sorted in ASCII order. Normally all versions have the - form above, so you get
the one with the highest version number or most recent date.  To override this behavior, use the = name form -- = sorts after -,
so it gets picked up."
  (let ((elisp-path (concat MJR-home-cor "/elisp")))
    (if (file-exists-p elisp-path)
        (let ((candidate-path-names (directory-files elisp-path 't (concat "^" package-name "[=-]"))))
          (if candidate-path-names
              (let ((best-directory-path (find-if #'file-directory-p (sort candidate-path-names (lambda (a b) (string-lessp b a))))))
                (if best-directory-path
                    (file-name-as-directory best-directory-path))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun MJR-dired-flag-latex-junk (&optional zap-extra-hard)
  "Flag LaTeX temp files/directories for deletion.
With prefix argument, also mark ps, html, dvi, and ps files."
  (interactive "P")
  (if (not (string-equal (symbol-name major-mode) "dired-mode"))
      (message "MJR-dired-flag-latex-junk-files: Must be in dired-mode to use this function!")
      (let* ((dired-marker-char dired-del-marker)
             (ext-to-zap        (append '("snm" "vrb" "nav" "tex~" "bak" "tex.bak" "lg" "idv" "aux" "toc" "log" "out" "hlog")
                                        (if zap-extra-hard '("html" "pdf" "dvi" "ps"))))
             (re-to-zap         (mapcar (lambda (ex) (concat "^.+\\." ex "$")) ext-to-zap)))
        (dired-mark-if
         (let ((fn (dired-get-filename t t)))
           (if (and fn (file-exists-p fn))
               (if (looking-at-p dired-re-dir)
                   (string-equal (file-name-nondirectory fn) "auto")
                   (and (file-exists-p (concat (file-name-base fn) ".tex"))
                        (find-if (lambda (re) (string-match re fn)) re-to-zap)))))
         "LaTeX temp file"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun MJR-dired-flag-junk ()
  "Flag junk files for deletion -- see MJR-dired-flag-latex-junk-files too."
  (interactive)
  (if (not (string-equal (symbol-name major-mode) "dired-mode"))
      (message "MJR-dired-flag-latex-junk-files: Must be in dired-mode to use this function!")
      (let ((dired-marker-char dired-del-marker)
            (re-to-zap         '("^NUL$"
                                 "^.+~$"
                                 "^.+\\.bak$"
                                 "^.+\\.bak[0-9]$"
                                 "^.+\\.xvpics$"
                                 "^\\.#.+")))
        (dired-mark-if
         (let ((fn (dired-get-filename t t)))
           (and fn
                (file-exists-p fn)
                (not (looking-at-p dired-re-dir))
                (find-if (lambda (re) (string-match re fn)) re-to-zap)))
         "Jumk file"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun MJR-ascii-table ()
  "Pop up a buffer with an ASCI table in it"
  (interactive)
  (let ((ascii-buffer (generate-new-buffer "*ascii-table*")))
    (with-current-buffer ascii-buffer
      (set-buffer-file-coding-system 'utf-8-unix)
      (erase-buffer)
      (loop with cs = '("NUL" "SOH" "STX" "ETX" "EOT" "NEQ" "ACK" "BEL" "BS "
                        "HT " "LF " "VT " "NP " "CR " "SO " "SI " "DLE" "DC1"
                        "DC2" "DC3" "DC4" "NAK" "SYN" "ETB" "CAN" "EM " "SUB"
                        "ESC" "FS " "GS " "RS " "US " "SP")
            with i = 0
            for r from 0 upto (1- 16)
            do (loop for c from 0 upto (1- 8)
                     do (setf i (+ r (* c 16)))
                     do (insert (format (if (< c 6) " |%3d %2x " " |%4d %2x ") i i))
                     do (cond ((< i 33)  (insert (elt cs i)))
                              ((> i 126) (insert "DEL"))
                              ((= c 2)   (progn (insert-char i) (insert " ")))
                              ((> c 6)   (progn (insert-char i) (insert "  ")))
                              ('t        (insert-char i))))
            do (insert " | \n"))
      (goto-char (point-min)))
    (switch-to-buffer ascii-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun MJR-eval-region (eval-how)
        "Evaluate the region as calc, elisp, or lisp (via slime), put the result in the kill ring.  With prefix arg, insert result into buffer.

Note: In non-interactive mode, the result is also displayed as a message for 'calc' and 'elisp'.

Interaction with options:
  * delete-selection-mode is non-NIL => Then a prefix arg will cause the result to *replace* the region
  * transient-mark-mode is non-NIL   => If region is not active, then interactive mode is used (results not put on kill ring)
                                        * for 'calc' ... like calling quick-calc             -- i.e. C-c * q
                                        * for 'elisp' .. like calling eval-expression        -- i.e. M-:
                                        * for 'lisp' ... like calling slime-interactive-eval -- i.e. C-: (in a slime buffer)"
        (interactive (list (if (require 'ido nil :noerror)
                               (ido-completing-read "Eval how: " '("calc" "elisp" "lisp"))
                               (read-string "Eval how: " "calc"))))
        (if (or (null transient-mark-mode) (region-active-p))
            (let* ((reg-min  (if (mark) (min (point) (mark)) (point-min)))
                   (reg-max  (if (mark) (max (point) (mark)) (point-max)))
                   (val      (if (< reg-min reg-max)
                                 (cond
                                  ((string-equal eval-how "calc")  (kill-new (calc-eval (buffer-substring-no-properties reg-min reg-max))))
                                  ((string-equal eval-how "elisp") (kill-new (format "%s" (eval (car (read-from-string (buffer-substring-no-properties reg-min reg-max)))))))
                                  ((string-equal eval-how "lisp")  (slime-eval-save (buffer-substring-no-properties reg-min reg-max)))))))
              (if val
                  (if current-prefix-arg
                      (progn (goto-char reg-max)
                             (insert "=")
                             (yank))
                      (message "MJR-eval-region: Value: %s" val))
                  (message "MJR-eval-region: Something went wrong")))
            (cond
             ((string-equal eval-how "calc")  (call-interactively #'quick-calc))
             ((string-equal eval-how "elisp") (call-interactively #'eval-expression))
             ((string-equal eval-how "lisp")  (call-interactively #'slime-interactive-eval)))))

(global-set-key (kbd "ESC ESC :") 'MJR-eval-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun MJR-calc-eval-multibase-region ()
  "Evaluate the region as calc code, and insert at end of region result in several bases."
  (interactive)
  (let* ((reg-min  (if (mark) (min (point) (mark)) (point-min)))
         (reg-max  (if (mark) (max (point) (mark)) (point-max)))
         (in-num   (if (< reg-min reg-max) (buffer-substring-no-properties reg-min reg-max))))
    (if in-num
        (progn (goto-char reg-max)
               (insert (concat " = "
                               (calc-eval (list in-num 'calc-number-radix 10))
                               "  "
                               (calc-eval (list in-num 'calc-number-radix 16))
                               "  "
                               (calc-eval (list in-num 'calc-number-radix 2))
                               "  "
                               (calc-eval (list in-num 'calc-number-radix 8)))))
        (message "MJR-calc-eval-multibase-region: Something went wrong"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (file-exists-p (concat MJR-home-bin "/curl"))
    (progn
      (MJR-quiet-message "MJR: INIT: STAGE: Define MJR Functions: MJR-insert-from-web: DEFINED!")
      (defun MJR-insert-from-web (url)
        "Insert snippet from web."
        (interactive (list (read-string "URL: " "https://www.mitchr.me/")))
        (call-process-shell-command (concat MJR-home-bin "/curl -s" url) nil 't nil)))
    (MJR-quiet-message "MJR: INIT: STAGE: Define MJR Functions: MJR-insert-from-web: NOT defined!  We could not find the curl command"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun MJR-describe-region-or-char ()
  "Provide info about region or char under point.

Intended to be bound to M-=.  When mark=point or no mar, call describe-char.  Otherwise call count-words-region."
  (interactive)
  (if (and (mark) (not (= (point) (mark))))
      (call-interactively #'count-words-region)
      (call-interactively #'describe-char)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun MJR-view-file-or-url-at-point ()
  "If point is on a file name or URL, then load with an external viewer/editor.  (open-as with prefix argument on Windows)"
  (interactive)
  (if (and (not (string-equal MJR-platform "WINDOWS-MGW")) current-prefix-arg)
      (error "MJR-view-file-or-url-at-point: Prefix argument only supported on native Windows Emacs"))
  (let ((fap (ffap-guess-file-name-at-point)))
    (if fap
        (if (or (ffap-url-p fap) (file-exists-p fap))
            (cond ((string-equal MJR-platform "WINDOWS-MGW") (w32-shell-execute (if current-prefix-arg "openas" "open") fap))
                  ((string-equal MJR-platform "WINDOWS-CYG") (start-process-shell-command "start" "start" "start " fap))
                  ((string-equal MJR-platform "DARWIN")      (start-process-shell-command "open" "open" "open " fap))
                  ((string-equal MJR-platform "LINUX")       (let ((ttr (or (cdr (assoc (upcase (file-name-extension fap)) (list (cons "PDF"  "mjrpdfview")
                                                                                                                                 (cons "JPEG" "mjrimgview")
                                                                                                                                 (cons "JPG"  "mjrimgview")
                                                                                                                                 (cons "GIF"  "mjrimgview")
                                                                                                                                 (cons "PNG"  "mjrimgview"))))
                                                                            "xdg-open")))
                                                               (start-process-shell-command ttr ttr (concat ttr " " fap))))
                  ('t                                        (message "MJR: MJR-view-file-or-url-at-point: ERROR: Found a file, but patform is unknown")))
            (message "MJR: MJR-view-file-or-url-at-point: ERROR: File a name at point, but it did not exist in the filesystem: %s" fap))
        (message "MJR: MJR-view-file-or-url-at-point: ERROR: Cound not find a filename name at point!"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun MJR-follow-mode ()
  "Close all open windows in the current frame; create vertical windows with current buffer; and activate follow-mode.

The number of verical windows will be min(8, max(2, prefix-arg))"
  (interactive)
  (let* ((cols  (min 8 (max 2 (prefix-numeric-value current-prefix-arg)))))
    (delete-other-windows)
    (dotimes (i (1- cols))
      (split-window-horizontally))
    (balance-windows)
    (follow-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun MJR-open-cwd ()
  "Open buffer's CWD in a file browser (explorer on windows, dolphon or nautilus on Linux)"
  (interactive)
  (cond ((string-equal MJR-platform "WINDOWS-MGW") (w32-shell-execute "open" "."))
        ((string-equal MJR-platform "WINDOWS-CYG") (w32-shell-execute "open" "."))
        ((string-equal MJR-platform "DARWIN")      (start-process-shell-command "open" "open" "open " "."))
        ((string-equal MJR-platform "LINUX")       (let ((fme (find-if #'executable-find (list "dolphin" "nautilus"))))
                                                     (if fme
                                                         (start-process "file-explorer" nil fme (expand-file-name "."))
                                                         (message "MJR: MJR-open-cwd: ERROR: Could not find file manager binary"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: STAGE: Define MJR Functions: MJR-unfill: DEFINED!")
(defun MJR-unfill ()
  "Unfill paragraph or region."
  (interactive "*")
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph nil (region-active-p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: STAGE: Define MJR Functions: MJR-date: DEFINED!")
(defun MJR-date (date-stamp-format)
  "Insert the date at the point when called interactively

If called interactively a date/time stamp is inserted at the point using a format determined by the prefix argument:

  |-----+-------------+----------------------+---------------+-----------------------------------------------|
  | ARG | KEYS        | DATE FORMAT          |               |                                               |
  |-----+-------------+----------------------+---------------+-----------------------------------------------|
  |  -5 | M-- 4 C-c d | %s                   | UNIX          | Seconds since 1970-01-01 00:00:00 Z           |
  |  -4 | M-- 4 C-c d | %Y-%m-%dT%H:%M#Z     | ISO 8601\secs | sitemap, XML, SLQ (#Z is +HH:MM or -HH:MM tz) |
  |  -3 | M-- 3 C-c d | %Y-%m-%d %H:%M #Z    |               |                                               |
  |  -2 | M-- 2 C-c d | %Y-%m-%d %H:%M       |               |                                               |
  |  -1 | M--   C-c d | %Y-%m-%d             |               |                                               |
  |   0 | M-0         |                      |               | NOP -- empty string                           |
  |   1 |       C-c d | %Y-%m-%d             |               |                                               |
  |   2 | M-2   C-c d | %Y-%m-%d %H:%M:%S    |               |                                               |
  |   3 | M-3   C-c d | %Y-%m-%d %H:%M:%S #Z |               |                                               |
  |   4 | C-u   C-c d | %Y-%m-%dT%H:%M:%S#Z  | ISO 8601      | sitemap, XML, SLQ (#Z is +HH:MM or -HH:MM tz) |
  |   5 | M-5   C-c d | %s                   | UNIX          | Seconds since 1970-01-01 00:00:00 Z           |
  |   6 | M-6   C-c d | %Y%m%d%H%M%S         |               | Frequently used for file names                |
  |-----+-------------+----------------------+---------------+-----------------------------------------------|

When not called interactively, this function returns the time as a string.  The argument is a format string."
  (interactive (list (prefix-numeric-value current-prefix-arg)))
  (let ((date-stamp-format (if (stringp date-stamp-format)
                               date-stamp-format
                               (if (integerp date-stamp-format)
                                   (cond ((= date-stamp-format  6) "%Y%m%d%H%M%S"         )
                                         ((= date-stamp-format  5) "%s"                   )
                                         ((= date-stamp-format  4) "%Y-%m-%dT%H:%M:%S#Z"  )
                                         ((= date-stamp-format  3) "%Y-%m-%d %H:%M:%S #Z" )
                                         ((= date-stamp-format  2) "%Y-%m-%d %H:%M:%S"    )
                                         ((= date-stamp-format  1) "%Y-%m-%d"             )
                                         ((= date-stamp-format -5) "%s"                   )
                                         ((= date-stamp-format -4) "%Y-%m-%dT%H:%M#Z"     )
                                         ((= date-stamp-format -3) "%Y-%m-%d %H:%M #Z"    )
                                         ((= date-stamp-format -2) "%Y-%m-%d %H:%M"       )
                                         ((= date-stamp-format -1) "%Y-%m-%d"             ))))))
    (if date-stamp-format
        (let* ((curtime   (if (minusp (prefix-numeric-value current-prefix-arg))
                              (let ((current-prefix-arg nil)) (org-read-date 't 't))
                              (current-time))) ;; Must protect org-read-date from prefix argument
               (time-list (decode-time curtime))
               (tzo       (nth 8 time-list))
               (zhr       (/ tzo 3600))
               (zmn       (/ (- (abs tzo) (* (abs zhr) 3600)) 60))
               (dstrtmp1  (format-time-string date-stamp-format curtime))
               (dstrtmp2  (replace-regexp-in-string "#Z" (if (zerop zmn)
                                                             (format "%+02d" zhr)
                                                             (format "%+02d:%02d" zhr zmn)) dstrtmp1 't 't))
               (dstr      (replace-regexp-in-string "UTC" "Z" dstrtmp2 't 't)))
          (if (called-interactively-p 'interactive) (insert dstr) dstr))
        "")))

;; Makes MJR-date work with delete-selection-mode
(put 'MJR-date 'delete-selection
     (lambda ()
       (not (run-hook-with-args-until-success
             'self-insert-uses-region-functions))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: STAGE: Define MJR Functions: MJR-quick-code-comment: DEFINED!")
(defun MJR-quick-code-comment (lab-tag date-stamp-format)
  "Add 'MJR' comment at point/around region.  Time stamp determined by prefix argument (See: MJR-date).

The 'MJR' comments come in one of two forms:
  * Region: Two formatted comment lines are added on the line before and line after the current region.
    The format (Date is optional) for the two comments:
      * Start comment:  'MJR TYPE BEGIN ----------------------- DATE'
      * Ending comment: 'MJR TYPE END ------------------------- DATE'
  * Line: A formatted comment line is added after the current line (cursor placed ready for content)
    The CONTEXT will be the name of the current function or buffer name.
    The format will be (note <DATE> and/or CONTEXT might be missing):
     * 'MJR TYPE NOTE <DATE> CONTEXT: '"
  (interactive (let ((lab-tags  '("TODO"   ;; TODO Marker
                                  "MOD"    ;; Modification
                                  "REVIEW" ;; Code review comment
                                  "SCM"))  ;; Transient comment normally pasted into SCM logs at checkin time
                     (line-mode (not (and mark-active (mark) (not (= (mark) (point)))))))
                 (list (if line-mode
                           (if (require 'ido nil :noerror)
                               (ido-completing-read "Line Mode Comment Type: " '("SCM" "TODO" "REVIEW" "MOD"))
                               (read-string "Line Mode Comment Type: " "SCM" 'lab-tags))
                           (if (require 'ido nil :noerror)
                               (ido-completing-read "Line Mode Comment Type: " '("MOD" "SCM" "REVIEW" "TODO"))
                               (read-string "Region Mode Comment Type: " "MOD" 'lab-tags)))
                       (prefix-numeric-value current-prefix-arg))))
  (let* ((lab-tag      (if (string-equal lab-tag "") "" (concat lab-tag " ")))
         (line-mode    (not (and mark-active (mark) (not (= (mark) (point))))))
         (region-start (if line-mode (point) (min (point) (mark))))
         (region-end   (if line-mode (point) (max (point) (mark))))
         (date-stamp   (MJR-date date-stamp-format))
         (com-start    (if (boundp 'comment-start) (symbol-value 'comment-start) ""))
         (com-end      (if (boundp 'comment-end)   (symbol-value 'comment-end)   "")))
    (if (string-equal com-start ";")
        (setq com-start ";;"))
    (if line-mode
        (let ((context-name (or (case major-mode
                                  ('org-mode (condition-case nil
                                                 (nth 4 (org-heading-components))
                                               (error nil)))
                                  (otherwise (add-log-current-defun)))
                                (if (buffer-file-name)
                                    (file-name-nondirectory (buffer-file-name)))
                                (buffer-name)
                                "")))
          (end-of-line)
          (insert (concat "\n" com-start " MJR " lab-tag "NOTE"
                          (if (not (zerop (length date-stamp))) " <") date-stamp (if (not (zerop (length date-stamp))) ">")
                          (if (not (zerop (length context-name))) " ") context-name ": "
                          com-end))
          (backward-char (length com-end)))
        (let ((com-1 (concat " MJR " lab-tag "BEGIN ----------------------- " date-stamp))
              (com-2 (concat " MJR " lab-tag "END ------------------------- " date-stamp)))
          (goto-char region-end)
          (end-of-line)
          (insert (concat "\n" com-start com-2 com-end))
          (goto-char region-start)
          (beginning-of-line)
          (insert (concat com-start com-1 com-end "\n"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (file-exists-p (concat MJR-home-cor "/codeBits/cheaderSTD.txt"))
    (progn
      (MJR-quiet-message "MJR: INIT: STAGE: Define MJR Functions: MJR-fix-c-includes: DEFINED!")
      (defun MJR-fix-c-includes ()
        "Fixes C/C++ #include lines to match my standard header format.

         In dired-mode operate on marked (or current) file(s); otherwise operate on current buffer.

         The standard header format is defined by a header DB stored in core/codeBits/cheaderSTD.txt.

         Returns number of files modified in dired-mode; otherwise the number of lines modified."
        (interactive)
        (let ((proc-buffer (lambda ()
                             (let ((case-fold-search   nil)
                                   (include-line-regex "^\\s-*#[iI][nN][cC][lL][uU][dD][eE]\\s-*\\([<\"]\\)\\([^>\"]+\\)\\([>\"]\\).*$") ;; Optional whitespace, "include" case insenstive, optional whitespace, < or " (1), non-empty string not containing < or " (2), < or " (3), optional stuff, end of line
                                   (golden-includes    nil)
                                   (change-count       0))
                               ;; Read golden include data
                               (with-temp-buffer
                                 (insert-file-contents (concat MJR-home-cor "/codeBits/cheaderSTD.txt"))
                                 (goto-char (point-min))
                                 (while (re-search-forward include-line-regex nil t)
                                   (let ((golden-include-line (match-string 0))
                                         (golden-include-name (match-string 2)))
                                     (push (cons golden-include-name golden-include-line) golden-includes))))
                               ;; Process Buffer
                               (goto-char (point-min))
                               (while (re-search-forward include-line-regex nil t)
                                 (let ((include-line        (match-string 0))
                                       (include-delim-start (match-string 1))
                                       (include-name        (match-string 2))
                                       (include-delim-end   (match-string 3)))
                                   (if (or (and (string-equal include-delim-start "<")  (string-equal include-delim-end ">"))
                                           (and (string-equal include-delim-start "\"") (string-equal include-delim-end "\"")))
                                       (let ((golden-line (cdr (assoc include-name golden-includes))))
                                         (if (and golden-line (not (string-equal golden-line include-line)))
                                             (progn
                                               (replace-match golden-line nil nil)
                                               (incf change-count)))))))
                               change-count))))
          (if (string-equal (symbol-name major-mode) "dired-mode")
              (let ((prefix-value (prefix-numeric-value current-prefix-arg))
                    (file-change-count 0))
                (mapc (lambda (file-name)
                        (message "MJR-fix-c-includes: Processing file: %s" file-name)
                        (with-temp-file file-name
                          (insert-file-contents file-name)
                          (if (and (< prefix-value 2) (< 0 (funcall proc-buffer)))
                              (progn (incf file-change-count)
                                     (copy-file file-name (make-backup-file-name file-name) 1)))))
                      (dired-get-marked-files))
                line-change-count)
              (save-excursion
                (funcall proc-buffer))))))
    (message "MJR: INIT: STAGE: Define MJR Functions: MJR-fix-c-includes: NOT defined!  We could not find the codeBits/cheaderSTD.txt file!"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (file-exists-p (concat MJR-home-cor "/codeBits/"))
    (progn
      (MJR-quiet-message "MJR: INIT: STAGE: Define MJR Functions: MJR-prepend-header: DEFINED!")
      (defun MJR-prepend-header (cat-str file-type)
        "Determine the buffer file type, and populate the buffer with an appropriate header and, if the buffer is empty, a template.

         Some text in the templates gets expanded: YYYY-MM-DD, YYYY, FILENAME, FILEPATHNAME

         Headers and templates are categorized into groups appropriate to different uses: MJR, TI, HWP."
        (interactive
         (let ((cat-str-v (mapcar #'file-name-nondirectory
                                  (cl-remove-if (lambda (f) (or (not (file-directory-p f))
                                                                (not (let ((case-fold-search nil))
                                                                       (string-match "/[A-Z]+$" f)))))
                                                (directory-files (concat MJR-home-cor "/codeBits/") 't)))))
           (let ((da-cat (if (require 'ido nil :noerror)
                             (ido-completing-read "Category: " cat-str-v nil 't nil nil (find "MJR" cat-str-v :test #'string-equal))
                             (read-string "Category: " (or (find "MJR" cat-str-v :test #'string-equal) (first cat-str-v)) 'cat-str-v))))
             (let ((file-type-v (append '("AUTO")
                                       (mapcar #'file-name-extension (file-expand-wildcards (concat MJR-home-cor "/codeBits/" da-cat "/top.*"))))))
               (let ((da-type (if (require 'ido nil :noerror)
                                  (ido-completing-read "Type: " file-type-v nil 't nil nil "AUTO")
                                  (read-string "Type: " "AUTO" 'file-type-v))))
                 (list da-cat da-type))))))
        (let ((cur-file-name (buffer-file-name)))
          (let ((file-type (if (string= file-type "AUTO")
                               (if cur-file-name
                                   (cdr (find-if (lambda (re) (string-match (car re) cur-file-name))
                                                 (list (cons "^makefile$" "make")
                                                       (cons "^Doxyfile$" "doxyfile")
                                                       (cons "\..+$"      (file-name-extension cur-file-name))))))
                               file-type)))
            (if file-type
                (let* ((src-path      (concat MJR-home-cor "/codeBits/"))
                       (top-file-name (concat src-path "/" cat-str "/top."      file-type))
                       (tpl-file-name (concat src-path "/" cat-str "/template." file-type)))
                  (message top-file-name)
                  (if (file-exists-p top-file-name)
                      (if (file-readable-p top-file-name)
                          (let ((top-string (with-temp-buffer
                                              (insert-file-contents top-file-name)
                                              (buffer-string)))
                                (do-template (and (= (point-min) (point-max)) (file-readable-p tpl-file-name))))
                            (dolist (cur-rpl (list (cons "YYYY-MM-DD"   (MJR-date "%Y-%m-%d"))
                                                   (cons "YYYY"         (MJR-date "%Y"))
                                                   (cons "FILENAME"     (if cur-file-name (file-name-nondirectory cur-file-name)))
                                                   (cons "FILEPATHNAME" cur-file-name)))
                              (if (cdr cur-rpl)
                                  (setq top-string (replace-regexp-in-string (car cur-rpl) (cdr cur-rpl) top-string 't 't))))
                            (goto-char (point-min))
                            (insert top-string)
                            (if do-template
                                (if  (file-readable-p tpl-file-name)
                                     (insert-file-contents tpl-file-name)
                                     (MJR-quiet-message "MJR: MJR-prepend-header: ERROR: Could not find TEMPLATE file for this file type")))
                            (message "MJR: MJR-prepend-header: INFO: Header prepended"))
                          (message "MJR: MJR-prepend-header: ERROR: Found TOP file, but can not read it"))
                      (message "MJR: MJR-prepend-header: ERROR: Could not find TOP file for this file type")))
                (message "MJR: MJR-prepend-header: ERROR: Could not figure out file type"))))))
    (message "MJR: INIT: STAGE: Define MJR Functions: MJR-prepend-header: NOT defined!  We could not find the codeBits directory!"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (file-exists-p (concat MJR-home-bin "/latexit.rb"))
    (progn
      (MJR-quiet-message "MJR: INIT: STAGE: Define MJR Functions: MJR-latexit: DEFINED!")
      (defun MJR-latexit ()
        "This function runs latex on the highlighted region, and displays the result with xpdf.

        Use the latexit.rb script in my home directory -- handy to have inside of Emacs..."
        (interactive)
        (let* ((reg-min  (if (mark) (min (point) (mark)) (point-min)))
               (reg-max  (if (mark) (max (point) (mark)) (point-max))))
          (if (file-exists-p (concat MJR-home-bin "/latexit.rb"))
              (shell-command-on-region reg-min reg-max (concat MJR-home-bin "/latexit.rb -"))
              (message "MJR: MJR-latexit: ERROR: Could not find the latexit.rb command!")))))
    (MJR-quiet-message "MJR: INIT: STAGE: Define MJR Functions: MJR-latexit: NOT defined!  We could not find the latexit.rb command"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (and (string-equal MJR-location "WORK:TI")
         (file-exists-p "/usr/local/bin/de"))
    (progn
      (MJR-quiet-message "MJR: INIT: STAGE: Define MJR Functions: MJR-de: DEFINED!")
      (defun MJR-de (at-2-get pfx-arg srch-str)
        "Lookup the people in LDAP

         Lookup the contents of the current region or the current-word with 'de' and insert requested attributes at the point -- replace
         region with a prefix argument.  'de' is a TI tool used on UNIX systems to lookup stuff in TI's LDAP servers.  It can query on
         various items including aid, xid, and real name.  Comma characters are removed from data returned, and if multiple elements are
         found they delimited commas.  The inserted string will have at least one leading and trailing space.

         Note that this uses quite a bit of shell magic and not regex replacement in Emacs.  This is because older Emacs versions have no
         such functions, and newer versions GNU Emacs and XEmacs have different (incompatible) functions for the same thing.  So, more
         shell and less Emacs is the hack of the day..."
        (interactive (let ((com-attrs  '("mail"                     ;; Email address
                                         "commonName"               ;; Common name
                                         "telephoneNumber"          ;; Telephone number
                                         "alphaPagerNumber"         ;; Alpha pager number
                                         "buildingCode"             ;; Building name
                                         "cellularTelephoneNumber"  ;; Cellular phone number
                                         "costCenter"               ;; Cost center
                                         "digitalPagerNumber"       ;; Digital pager number
                                         "division"                 ;; Division
                                         "emailDestination"         ;; Physical email address
                                         "empCurrentStatus"         ;; Employment status
                                         "employeeNumber"           ;; ITSS userid
                                         "facsimileTelephoneNumber" ;; Fax number
                                         "firstName"                ;; First name
                                         "fullName"                 ;; Full name
                                         "givenName"                ;; Nickname
                                         "officeCoordinates"        ;; Office coordinates
                                         "org"                      ;; Organization number
                                         "pcDrop"                   ;; PC drop
                                         "siteName"                 ;; Site
                                         "sn"                       ;; Last name
                                         "supervisor")))            ;; Supervisor
                       (list (if (require 'ido nil :noerror)
                                 (ido-completing-read "Attribute: " com-attrs)
                                 (read-string "Attribute: " "mail" 'com-attrs))
                             (prefix-numeric-value current-prefix-arg)
                             (if (mark) (buffer-substring (point) (mark)) (current-word 't)))))
        (if srch-str
            (if (and (> (length srch-str) 2) (< (length srch-str) 100))
                (if (file-exists-p "/usr/local/bin/de")
                    (progn
                      (if (and (mark) (not (= pfx-arg 1))) (kill-region (point) (mark)))
                      (let ((fnd-str (shell-command-to-string
                                      (concat "/usr/local/bin/de -r -attr '"  at-2-get "' "
                                              srch-str " | grep '" at-2-get
                                              "' | sed 's/^.*: //' | sed 's/,//g' | tr '\\012' ','"))))
                        (if (< (length fnd-str) 2)
                            (message "MJR-de: ERROR: No entries found!")
                            (insert (concat " " (substring fnd-str 0 -1) " ")))))
                    (message "MJR-de: ERROR: The 'de' utility was not found!"))
                (message "MJR-de: ERROR: The search string is probably wrong: %s" srch-str))
            (message "MJR-de: ERROR: Could not figure out what to surch for.  Try marking some text."))))
    (MJR-quiet-message "MJR: INIT: STAGE: Define MJR Functions: MJR-de: ERROR: Not defined!  Not at TI"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: STAGE: Define MJR Functions: MJR-thingy-lookeruper: DEFINED!")

(defvar MJR-thingy-lookeruper-methods nil
  "A list defineing various things that may be looked up via MJR-thingy-lookeruper.  Each entry is a list containing three values:
   * name of lookup
   * Regular expression, or list of regular expressions, to match aginst buffer major mode string. Use nil to match any mode.
   * Function used to pull string from buffer.
     This should be as specific as possible.
   * List of methods to use to do the lookups.  List contains strings with a shell command and/or lisp functions.
     For strings: The first word is expected to be a shell command, and will only be considered if the file exists or
     an executable of that name may be found on the path.  %U will be replaced with the URL hexified search
     string, and %Q will be replaced with the search string")
;; Various methods useful at work
(if (string-equal MJR-location "WORK:TI")
    (progn
      (if (file-exists-p "/usr/local/bin/de")
          ;; Use 'de' to search the TI directory
          (push (list "TI-LDAP-de"
                      nil
                      (lambda () (and (thing-at-point-looking-at ".+" 20) (match-string 0)))
                      '("/usr/local/bin/de '%Q'"))
                MJR-thingy-lookeruper-methods))
      (if (executable-find "ldapsearch")
          (progn
            ;; Use ldapsearch to search For a TI Employee Name (cn and fullName)
            (push (list "TI Person Name"
                        nil
                        (lambda () (and (thing-at-point-looking-at "\\([a-zA-Z][a-zA-Z]*[, ] *[a-zA-Z][a-zA-Z]*\\)" 10) (match-string 0)))
                        '("ldapsearch -z 0 -x -h ldap.directory.ti.com -b 'o=ti,c=us' '(&(objectClass=person)(|(cn~=%Q)(fullName~=%Q)))'"))
                  MJR-thingy-lookeruper-methods)
            ;; Use ldapsearch to search For a TI Employee E-Mail (mailAlias, emailDestination, and mail)
            (push (list "TI E-Mail"
                        nil
                        (lambda () (and (thing-at-point-looking-at "\\([[:graph:]][[:graph:]]*@[[:graph:]]*ti\\.com\\)" 30) (match-string 0)))
                        '("ldapsearch -z 0 -x -h ldap.directory.ti.com -b 'o=ti,c=us' '(&(objectClass=person)(|(mailAlias=%Q)(emailDestination=%Q)(mail=%Q)))'"))
                  MJR-thingy-lookeruper-methods)
            ;; Use ldapsearch to search For a TI Employee ID (IDNumber)
            (push (list "Employee-ID"
                        nil
                        (lambda () (and (thing-at-point-looking-at "\\(a[0-9]\\{7\\}\\|x[a-zA-Z0-9]\\{7\\}\\)" 10) (match-string 0)))
                        '("ldapsearch -z 0 -x -h ldap.directory.ti.com -b 'o=ti,c=us' '(&(objectClass=person)(IDNumber=%Q))'"))
                  MJR-thingy-lookeruper-methods)
            ;; Use ldapsearch to search For a TI Site code (SITE)
            (push (list "SiteCode"
                        nil
                        (lambda () (and (thing-at-point-looking-at "\\b\\([A-Z][A-Z0-9]\\{3\\}\\)\\b" 20) (match-string 1)))
                        '("ldapsearch -z 0 -x -h ldap.directory.ti.com -b 'ou=tiSiteCodes,ou=applications,o=ti,c=us' '(&(objectClass=tiSite)(siteName=%Q))'"))
                  MJR-thingy-lookeruper-methods)
            ;; Use ldapsearch to search For a TI Org3 (SBE)
            (push (list "Org3"
                        nil
                        (lambda () (and (thing-at-point-looking-at "\\b\\([0-9]\\{3\\}\\)\\b" 20) (match-string 1)))
                        '("ldapsearch -z 0 -x -h ldap.directory.ti.com -b 'ou=grpStructure,ou=data,ou=applications,o=ti,c=us' '(o=%Q)'"))
                  MJR-thingy-lookeruper-methods)
            ;; Use ldapsearch to search For a TI Org6 (SBE-1)
            (push (list "Org6"
                        nil
                        (lambda () (and (thing-at-point-looking-at "\\b\\([0-9]\\{6\\}\\)\\b" 20) (match-string 1)))
                        '("ldapsearch -z 0 -x -h ldap.directory.ti.com -b 'ou=divStructure,ou=data,ou=applications,o=ti,c=us' '(o=%Q)'"))
                  MJR-thingy-lookeruper-methods)
            ;; Use ldapsearch to search For a TI Org9 (SBE-2)
            (push (list "Org9"
                        nil
                        (lambda () (and (thing-at-point-looking-at "\\b\\([0-9]\\{9\\}\\)\\b" 20) (match-string 1)))
                        '("ldapsearch -z 0 -x -h ldap.directory.ti.com -b 'ou=deptStructure,ou=data,ou=applications,o=ti,c=us' '(o=%Q)'"))
                  MJR-thingy-lookeruper-methods)
            ;; Use ldapsearch to search For a TI Cost Center (SBE-3)
            (push (list "CostCenter"
                        nil
                        (lambda () (and (thing-at-point-looking-at "\\b\\([0-9]\\{8\\}\\)\\b" 20) (match-string 1)))
                        '("ldapsearch -z 0 -x -h ldap.directory.ti.com -b 'ou=finStructure,ou=data,ou=applications,o=ti,c=US' '(o=%Q)'"))
                  MJR-thingy-lookeruper-methods)
            ;; Use ldapsearch to search For a TI Division 003 Cost Center (SBE-3)
            (push (list "CostCenterUS5"
                        nil
                        (lambda () (and (thing-at-point-looking-at "\\b\\([0-9]\\{5\\}\\)\\b" 20) (match-string 1)))
                        '("ldapsearch -z 0 -x -h ldap.directory.ti.com -b 'ou=finStructure,ou=data,ou=applications,o=ti,c=US' '(o=003%Q)'"))
                  MJR-thingy-lookeruper-methods)))))
;; Lookup a local group ID (Numeric group ID)
(push (list "gid"
            nil
            (lambda () (and (thing-at-point-looking-at "\\b\\([0-9]+\\)\\b" 20) (match-string 1)))
            '("getent group %Q"))
      MJR-thingy-lookeruper-methods)
;; Lookup a local group name
(push (list  "gname"
             nil
             (lambda () (and (thing-at-point-looking-at "\\b\\([a-zA-Z][a-zA-Z0-9_-]+\\)\\b" 20) (match-string 1)))
             '("getent group %Q"))
      MJR-thingy-lookeruper-methods)
;; Lookup a local user ID (Numeric user ID)
(push (list "uid"
            nil
            (lambda () (and (thing-at-point-looking-at "\\b\\([0-9]+\\)\\b" 20) (match-string 1)))
            '("getent passwd %Q"))
      MJR-thingy-lookeruper-methods)
;; Look up a user name (uname)
(push (list "uname-long"
            nil
            (lambda () (let ((tmp (and (thing-at-point-looking-at "\\b\\([a-zA-Z][a-zA-Z0-9_-]+\\)\\b" 20) (match-string 1))))
                         (and tmp (find tmp (system-users) :test #'string-equal))))
            '("/home/sysadmin/bin/pde %Q" "getent passwd %Q"))
      MJR-thingy-lookeruper-methods)
;; Look up a word via dictionary.reference.com
(push (list "dictionary"
            nil
            (lambda () (and (thing-at-point-looking-at "\\b\\([a-zA-Z'-]+\\)\\b" 20) (match-string 1)))
            (if (string-equal MJR-platform "WINDOWS-MGW")
                (list (lambda (lstr) (browse-url (concat "http://dictionary.reference.com/browse/" (url-hexify-string lstr) "?s=t"))))
                (list (concat MJR-home-bin "/browser -foreground 100 -new-window 'http://dictionary.reference.com/browse/%U?s=t' &"))))
      MJR-thingy-lookeruper-methods)
;; Look via google
(push (list "google"
            nil
            (lambda () (and (thing-at-point-looking-at ".+" 20) (match-string 0)))
            (if (string-equal MJR-platform "WINDOWS-MGW")
                (list (lambda (lstr) (browse-url (concat "http://google.com/#q=" (url-hexify-string string-to-lookup)))))
                (list (concat MJR-home-bin "/browser -foreground 100 -new-window 'http://google.com/#q=%U' &"))))
          MJR-thingy-lookeruper-methods)
;; Look up a user name (uname)
(push (list "uname-short"
            nil
            (lambda () (let ((tmp (and (thing-at-point-looking-at "\\b\\([a-zA-Z][a-zA-Z0-9_-]+\\)\\b" 20) (match-string 1))))
                         (and tmp (find tmp (system-users) :test #'string-equal))))
            '("getent passwd %Q"))
          MJR-thingy-lookeruper-methods)
;; Just toss it into a browser
(push (list "URL"
            nil
            (lambda () (thing-at-point 'url))
            (list #'browse-url))
          MJR-thingy-lookeruper-methods)
;; Lookup host name
(push (list "DNS"
            nil
            (lambda () (and (thing-at-point-looking-at "\\([.a-zA-Z0-9_-]+\\.\\(com\\|edu\\|org\\|gov\\)\\)\\b" 20) (match-string 1)))
            '("nslookup %Q"
              "dig %Q"))
      MJR-thingy-lookeruper-methods)
;; Look up file data
(push (list "fstat"
            nil
            (lambda () (let ((tmp (ffap-guess-file-name-at-point)))
                         (and tmp (file-exists-p tmp) tmp)))
            (list (concat MJR-home-bin "/fstat.pl '%Q'")
                  "stat '%Q'"))
      MJR-thingy-lookeruper-methods)
;; Lookup a lisp symbol in an interactive elisp session or an elisp source file
(push (list "elisp-symbol"
            (list "^lisp-interaction-mode$" "^emacs-lisp-mode$" )
            #'symbol-at-point
            (list #'describe-symbol))
      MJR-thingy-lookeruper-methods)
;; Lookup a symbol in an R session or R source code file
(push (list "r-symbol"
            (list "^inferior-ess-mode$" "^ess-mode$")
            (lambda () (let ((tmp (symbol-at-point))) (and tmp (symbol-name tmp))))
            (list (lambda (thingy) (ess-help thingy))))
      MJR-thingy-lookeruper-methods)
;; Lookup a symbol in an interactive SLIME REPL or common lisp buffer in the hyperspec
(push (list "hyperspec"
            (list "^slime-repl-mode$" "^lisp-mode$")
            #'symbol-at-point
            (list (lambda (thingy) (hyperspec-lookup (symbol-name thingy)))))
      MJR-thingy-lookeruper-methods)
;; Lookup a symbol via devdocs.io for ruby, perl, & python buffers
(push (list "devdocs.io"
            (list "^ruby-mode$" "^perl-mode$" "^python-mode$" "^c\\+\\+-mode$")
            #'symbol-at-point
            (list (lambda (thingy) (browse-url (concat "http://devdocs.io/#q="
                                                       (cdr (assoc major-mode (list (cons 'ruby-mode   "ruby-2.4")
                                                                                    (cons 'perl-mode   "perl-5.26")
                                                                                    (cons 'c++-mode    "cpp")
                                                                                    (cons 'python-mode "python-3.6"))))
                                                       " "
                                                       (url-hexify-string (symbol-name thingy)))))))
      MJR-thingy-lookeruper-methods)
;; Look for a man page
(push (list "man"
            "^shell-mode$"
            (lambda () (and (thing-at-point-looking-at "\\b\\([a-zA-Z0-9_-]+\\)\\b" 20) (match-string 1)))
            (list #'man))
      MJR-thingy-lookeruper-methods)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar MJR-thingy-lookeruper-methods-clean nil
  "An optimized version of MJR-thingy-lookeruper-methods.

Created dynamically by MJR-thingy-lookeruper when MJR-thingy-lookeruper-methods-clean is NUL.
Format is like MJR-thingy-lookeruper-methods, but the third element is a single action -- not a list.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun MJR-thingy-lookeruper (string-to-lookup lookup-method)
  "Extensible looker upper of thingys at the point or in the active region (active region support requires transient-mark-mode).

Interactive use:
  * If the region is not active, each search method will be used to find various things at the point, and the user will be queried
    as to which kind of lookup to perform based on what was found.
  * If the region is active, then the entire region is used as the search string and the user can select ANY lookup method.

The list MJR-thingy-lookeruper-methods describes the kinds of things that can be looked up. Examples include uname, gname, uid,
gid, host name, dictionary word, and Google search."
  (interactive
   (let ((have-region    (and transient-mark-mode (region-active-p) (mark) (buffer-substring-no-properties (min (point) (mark)) (max (point) (mark)))))
         (search-strings nil)
         (search-methods nil))
     ;; Create MJR-thingy-lookeruper-methods-clean if it is empty
     (if (null MJR-thingy-lookeruper-methods-clean)
         (progn
           (setq MJR-thingy-lookeruper-methods-clean
                 (loop for (cur-method-name cur-method-mode-regex cur-method-tap cur-method-search-command-list) in MJR-thingy-lookeruper-methods
                       for cur-method-search-command = (find-if (lambda (f) (if (stringp f)
                                                                                (let ((command-to-find (first (split-string f))))
                                                                                  (or (file-exists-p   command-to-find)
                                                                                      (executable-find command-to-find)))
                                                                                f))
                                                                cur-method-search-command-list)
                       when cur-method-search-command
                       collect (list cur-method-name cur-method-mode-regex cur-method-tap cur-method-search-command)))
           (message "MJR-thingy-lookeruper: Cleaning list (%d -> %d)"
                    (length MJR-thingy-lookeruper-methods)
                    (length MJR-thingy-lookeruper-methods-clean))))
     (dolist (cur-method MJR-thingy-lookeruper-methods-clean)
       (destructuring-bind (cur-method-name cur-method-mode-regex cur-method-tap cur-method-search-command-list) cur-method
         (if (if (stringp cur-method-mode-regex)
                 (string-match cur-method-mode-regex (symbol-name major-mode))
                 (or (not cur-method-mode-regex)
                     (find-if (lambda (x) (string-match x (symbol-name major-mode))) cur-method-mode-regex)))
             (let ((fnd-thingy (or have-region (and cur-method-tap (funcall cur-method-tap)))))
               (if fnd-thingy
                   (setq search-strings (append search-strings (list fnd-thingy))
                         search-methods (append search-methods (list cur-method-name))))))))
     (cond ((= 1 (length search-strings)) (list (car search-strings) (car search-methods)))
           ((not (null search-strings))   (let ((i-lookup-method (if (require 'ido nil :noerror)
                                                                     (ido-completing-read "Lookup Method: " search-methods)
                                                                     (read-string "Lookup Method: " ""))))
                                            (let ((idx (position i-lookup-method search-methods :test #'string-equal)))
                                              (if idx
                                                  (list (nth idx search-strings) (nth idx search-methods))
                                                  (error "MJR-thingy-lookeruper: Method is invalid")))))
           ('t                            (error "MJR-thingy-lookeruper: Could not find a string to lookup")))))
  (message "MJR-thingy-lookeruper: '%s' via '%s'" string-to-lookup lookup-method)
  (let ((the-command (nth 3 (find-if (lambda (c) (string-equal lookup-method (car c))) MJR-thingy-lookeruper-methods-clean))))
    (if the-command
        (if (stringp the-command)
            (let ((the-command-x the-command))
              (mapcar (lambda (subs-vals) (setq the-command-x (replace-regexp-in-string (car subs-vals) (cadr subs-vals) the-command-x 't 't)))
                      (list (list "%U" (url-hexify-string string-to-lookup))
                                        ;If you uncomment the next line, then uncomment the autoload for browse-url-encode-url
                                        ;(list "%B" (browse-url-encode-url string-to-lookup))
                            (list "%Q" string-to-lookup)))
              (message "CMD: %s" the-command-x)
              (if (string-match "&$" the-command)
                  (call-process-shell-command the-command-x)
                  (shell-command the-command-x)))
            (funcall the-command string-to-lookup))
        (message "MJR-thingy-lookeruper: Unable to find working lookup command for %s!" lookup-method))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;g
(MJR-quiet-message "MJR: INIT: STAGE: Define MJR Functions: MJR-stats-numbers-in-column: DEFINED!")
(defun MJR-stats-numbers-in-column (stats-to-compute)
  "Compute various statistics on the the numbers highlighted by a rectangle, and put a summary in the kill ring.

With prefix arg you can pick the statistics to compute."
  (interactive (let ((stat-names  '("sum" "mean"  "min" "max" "sd" "n" "sumsq" "sum,mean,min,max,sd,n,sumsq" )))
                 (list (split-string (if current-prefix-arg
                                         (if (require 'ido nil :noerror)
                                             (ido-completing-read "Stats to compute: " stat-names)
                                             (read-string "Stats to compute: " "sum" 'stat-names))
                                         "sum,mean,min,max,sd,n,sumsq")
                                     "[^a-zA-Z]"))))
  (cl-flet ((MJR-get-numbers-in-column () (save-excursion
                                            (let* ((saved-point (point))
                                                 (saved-mark  (if (mark) (mark) saved-point))
                                                 (min-col     (min (progn (goto-char saved-point) (current-column)) (progn (goto-char saved-mark) (current-column))))
                                                 (min-line    (min (line-number-at-pos saved-point) (line-number-at-pos saved-mark)))
                                                 (max-line    (max (line-number-at-pos saved-point) (line-number-at-pos saved-mark)))
                                                 (list-o-numb nil))
                                            (loop for cur-line from min-line upto max-line do
                                                  (let* ((min-pt (progn (goto-line cur-line) (move-to-column min-col) (point)))
                                                         (max-pt (point-at-eol)))
                                                    (setq list-o-numb (append list-o-numb (list (string-to-number (buffer-substring min-pt max-pt)))))))
                                            ;(kill-new (format "%s" list-o-numb))
                                            list-o-numb)))
            (MJR-stats-cmp     (the-list) (if (listp the-list)
                                              (let* ((the-flist (mapcar (lambda (x) (if (numberp x) (float x))) the-list))
                                                     (the-n     (length the-flist))
                                                     (allfp     (notany 'not the-flist))
                                                     (the-min   (if allfp (apply 'min the-flist)))
                                                     (the-sum   (if allfp (apply '+ the-flist)))
                                                     (the-mean  (if allfp (if (< 0 the-n) (/ (* 1.0 the-sum) the-n))))
                                                     (the-sumsq (if allfp (apply '+ (mapcar (lambda (x) (* x x)) the-flist))))
                                                     (the-var   (if allfp (if (< 0 the-n) (- (/ the-sumsq the-n) (* the-mean the-mean)))))
                                                     (the-sd    (if allfp (if (< 0 the-var) (sqrt the-var))))
                                                     (the-max   (if allfp (apply 'max the-flist))))
                                                (list (cons "min"   the-min)  (cons "max"   the-max) (cons "sum"   the-sum)  (cons "sumsq" the-sumsq)
                                                      (cons "mean"  the-mean) (cons "sd"    the-sd)  (cons "n"     the-n))))))
    (let* ((da-stats  (MJR-stats-cmp (MJR-get-numbers-in-column)))
           (da-string (apply #'concat (mapcar (lambda (da-stat) (format " %s: %s" da-stat (cdr (assoc da-stat da-stats))))
                                              stats-to-compute))))
      (kill-new da-string)
      (message da-string))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: STAGE: Generic Global Emacs Config Stuff...")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if MJR-pookie-mode
    ;; scroll one line at a time
    (setq scroll-step 1))
(if MJR-pookie-mode
    ;; Don't scroll jump when you hit edges of the window
    (setq scroll-conservatively 10000))
(if MJR-pookie-mode
    ;; Don't wrap long lines.
    (setq-default truncate-lines nil)
    ;; Wrap long lines.
    (setq-default truncate-lines 'true))
;; Make the buffer name column width wider (default is 19)
(setq Buffer-menu-name-width 40)
;; Turn on S-arrows for window selection
(windmove-default-keybindings)
;; kill stuff in a read only buffer
(setq kill-read-only-ok t)
;; Want local variables to work;;
(setq enable-local-eval t)
;; No startup message
(setq inhibit-startup-message t)
;; No message in echo area
(setq inhibit-startup-echo-area-message "MJR")
;; No message in *scratch* buffer
(setq initial-scratch-message nil)
;; Make apros Work hard.
(setq apropos-do-all t)
;; Want the regon to be highlighted
(setq transient-mark-mode 'true)
;; Make mouse select, clipboard, and yank buffer sane
(setq select-active-regions   'only)
;; Non-nil -> cut/paste uses primary selection (os depend ant)
(if MJR-pookie-mode
    (setq select-enable-primary nil)
    (setq select-enable-primary 't))
;; Highlight the search strings
(setq search-highlight t)
;; Highlight query replace
(setq query-replace-highlight t)
;; Want quite errors
(setq visible-bell t)
;; Tab must be 4 spaces.
(setq-default tab-width 4)
;; Insert spaces instead of tabs
(setq-default indent-tabs-mode nil)
;; Give up on an autosave after 5 sec
(setq auto-save-timeout 5)
;; Always put a final newline in a file
(setq require-final-newline t)
;; Don't ask when we revert-buffer
(setq revert-without-query '(".*"))
;; Keep the compilation window from growing on large displays
(setq compilation-window-height 12)
;; Ask before save
(setq compilation-ask-about-save t)
;; Let us have minibuffers within minibufers
(if MJR-expert-mode (setq enable-recursive-minibuffers t))
;; Show matching parens
(show-paren-mode 1)
;; Put file size in mode line
(size-indication-mode)
;; Title the frame so the window manager can find Emacs correctly
(setq frame-title-format "GNU Emacs: %b")
;; Turn off menus, tool bars, and scroll bars
(if MJR-expert-mode (menu-bar-mode -1)   (menu-bar-mode 1)  )
(if MJR-expert-mode (tool-bar-mode -1)   (tool-bar-mode 1)  )
(if MJR-expert-mode (scroll-bar-mode -1) (scroll-bar-mode 1))
;; Novice override stuff.  Allow these commands.
(if MJR-expert-mode (put 'eval-expression  'disabled nil))
(if MJR-expert-mode (put 'upcase-region    'disabled nil))
(if MJR-expert-mode (put 'downcase-region  'disabled nil))
(if MJR-expert-mode (put 'narrow-to-region 'disabled nil))
;; Set the colors
;; (set-background-color "white")
;; (set-foreground-color "black")
;; (set-cursor-color "red")
;; (set-border-color "black")
;; (set-mouse-color "black")
;; Set up default window selection keys (S-arrows)
(if (version< emacs-version "23.1") (windmove-default-keybindings))
;; Global font lock mode
(global-font-lock-mode 1)
;; Do *not* automatically save abbrev files
(setq save-abbrevs nil)
;; Make buffer pull down menu list all buffers
(setq buffers-menu-max-size nil)
;; Set the mark ring size
(setq mark-ring-max 64)
;; Only split vertically
(setq split-width-threshold nil)
;; Keep SQL buffers in the current window
(add-to-list 'same-window-buffer-names "*SQL*")
;; Setup various handy auto-mode-alist items
(add-to-list 'auto-mode-alist '("\\.sql.m4$"                . sql-mode))            ;; SQL with m4
(add-to-list 'auto-mode-alist '("\\.txt.m4$"                . text-mode))           ;; Text with m4
(add-to-list 'auto-mode-alist '("\\.elisp$"                 . emacs-lisp-mode))     ;; Emacs lisp code
(add-to-list 'auto-mode-alist '("\\.clisp$"                 . lisp-mode))           ;; SLIME-lisp-mode
(add-to-list 'auto-mode-alist '("\\.[fF]95$"                . f90-mode))            ;; Use f90 mode with fortran 1995
(add-to-list 'auto-mode-alist '("\\.[fF]0[38]$"             . f90-mode))            ;; Use f90 mode with fortran 2003 and 2008
(add-to-list 'auto-mode-alist '("\\.[mM][oO][dD]$"          . f90-mode))            ;; Use f90 mode with fortran modules
(add-to-list 'auto-mode-alist '("\\.[fF]200[38]$"           . f90-mode))            ;; Use f90 mode with fortran 2003 and 2008
(add-to-list 'auto-mode-alist '("\\.[fF]77$"                . fortran-mode))        ;; Use fortran mode for f77
(add-to-list 'auto-mode-alist '("\\.[fF][oO][rR]$"          . fortran-mode))        ;; Use fortran mode for f77
(add-to-list 'auto-mode-alist '("^/tmp/pico\\.[0-9][0-9]*$" . mail-mode))           ;; alpine tmp files -- use mail-mode
(add-to-list 'auto-mode-alist '("tmp/mutt/\\.*mutt"         . mail-mode))           ;; mutt tmp files -- use mail-mode
(add-to-list 'auto-mode-alist '("\\.svg$"                   . image-mode-as-text))  ;; Prevent SVG rendering on load
;; Fringe on the right only
(fringe-mode '(0 . 8))
;; prettify-symbols in prog-mode.el
;; (setq prettify-symbols-unprettify-at-point 't)
;; (global-prettify-symbols-mode t)
(if (string-equal MJR-platform "WINDOWS-MGW")
    (let ((found-shell (find-if #'file-exists-p (list "c:/msys64/usr/bin/bash.exe"
                                                      ))))
      (if found-shell
          (progn (setq shell-file-name          found-shell)
                 (setq explicit-shell-file-name found-shell)
                 (setq explicit-bash.exe-args '("--noediting" "--login" "-i"))))))
;; In *scratch* buffers, print everything
(setq eval-expression-print-length nil
      eval-expression-print-level  nil)
;; Set the modeline
(setq-default eol-mnemonic-unix "/LF ")
(setq-default eol-mnemonic-dos  "/CRLF ")
(setq-default eol-mnemonic-mac  "/CR ")
(setq-default mode-line-format  '("%e "
                                  mode-line-mule-info
                                  mode-line-client
                                  mode-line-modified
                                  mode-line-remote
                                  mode-line-frame-identification
                                  mode-line-buffer-identification
                                  "   "
                                  "%p/%I L:%l C:%c"                                     ; Normally done with 'mode-line-position'
                                  " P:" (:eval (format "%d" (point)))
                                  ;" M:" (:eval (format "%d" (mark)))
                                  " "
                                  (:eval (propertize (if (boundp 'vc-mode)                 ; VC backend
                                                         (if vc-mode
                                                             (if (and vc-mode
                                                                      (string-match "^ *Git:" vc-mode)
                                                                      (or (dolist (x minor-mode-list)
                                                                            (if (string-match "magit" (symbol-name x))
                                                                                (return 't)))
                                                                          (require 'magit nil 't)))
                                                                 (propertize "Git"
                                                                             'help-echo
                                                                             "mouse-1: magit-status"
                                                                             'local-map
                                                                             '(keymap
                                                                               (mode-line keymap
                                                                                          (mouse-1 . magit-status)))
                                                                             'mouse-face 'mode-line-highlight)
                                                                 vc-mode)
                                                             (propertize "!VC" 'help-echo "No version control detected" ))
                                                         (propertize "VC?" 'help-echo "ERROR: Unable to detect version control"))
                                                     'face '(:foreground "blue" :weight bold)))
                                  "  "
                                  mode-line-modes                                           ; Modes
                                  "   "
                                  (:eval (format-time-string "%Y-%m-%d %H:%M"               ; Date/time the way I like it
                                                             (current-time)))
                                  " "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: STAGE: Built-in Mode Config...")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: PKG SETUP: man")
(if (string-equal MJR-platform "WINDOWS-MGW")
    (eval-after-load "man"
      '(progn (MJR-quiet-message "MJR: POST-INIT(%s): EVAL-AFTER: man" (MJR-date "%Y-%m-%d_%H:%M:%S"))
              (if (file-exists-p "/msys64/usr/share/man")
                  (setenv "MANPATH" "/usr/share/man")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: PKG SETUP: vc")
;; Use ediff for = binding
(eval-after-load "vc-hooks"
  '(progn (MJR-quiet-message "MJR: POST-INIT(%s): EVAL-AFTER: vc-hooks" (MJR-date "%Y-%m-%d_%H:%M:%S"))
          (define-key vc-prefix-map "=" 'vc-ediff)))
;; No confirmation for C-x v v and C-x v i, and C-x v u
(setq vc-suppress-confirm 't)
;; Create messages for VC command output
(setq vc-command-messages 't)
;; No frame for control buffer
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; split window horizontally for diff
(setq ediff-split-window-function 'split-window-horizontally)
;; split window vertically for merge
(setq ediff-merge-split-window-function 'split-window-vertically)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (MJR-quiet-message "MJR: INIT: PKG SETUP: semantic")
;; (require 'semantic)
;; (setq semantic-decoration-styles nil)
;; (semantic-mode 1)
;; (require 'semantic/sb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: PKG SETUP: rmail")
(eval-after-load "rmail"
  '(progn (MJR-quiet-message "MJR: POST-INIT(%s): EVAL-AFTER: rmail!" (MJR-date "%Y-%m-%d_%H:%M:%S"))
          (let ((sepath (find-if #'file-exists-p '("/Users/Shared/mail/"))))
            (if sepath
                (setq rmail-secondary-file-directory sepath)))
          (setq rmail-secondary-file-regexp "\\.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: PKG SETUP: browse-url")
;; Need this if we support %B in MJR-thingy-lookeruper
;(autoload 'browse-url-encode-url "browse-url" "Escape annoying characters in URL that will confuse a web browser." t)
(eval-after-load "browse-url"
  '(progn (MJR-quiet-message "MJR: POST-INIT(%s): EVAL-AFTER: browse-url!" (MJR-date "%Y-%m-%d_%H:%M:%S"))
          ;; Use firefox
          (setq browse-url-browser-function
                (list (cons "hyperspec"                  #'eww-browse-url)
                      (cons "/SS/exampleCode/ruby"       #'eww-browse-url)
                      (cons "/SS/exampleCode/AUPG"       #'eww-browse-url)
                      (cons "/SS/exampleCode/blas"       #'eww-browse-url)
                      (cons "/SS/exampleCode/boost"      #'eww-browse-url)
                      (cons "/SS/exampleCode/cfitsio"    #'eww-browse-url)
                      (cons "/SS/exampleCode/cpp"        #'eww-browse-url)
                      (cons "/SS/exampleCode/curl"       #'eww-browse-url)
                      (cons "/SS/exampleCode/DB"         #'eww-browse-url)
                      (cons "/SS/exampleCode/F77"        #'eww-browse-url)
                      (cons "/SS/exampleCode/fltk"       #'eww-browse-url)
                      (cons "/SS/exampleCode/Fortran"    #'eww-browse-url)
                      (cons "/SS/exampleCode/glut"       #'eww-browse-url)
                      (cons "/SS/exampleCode/GMP"        #'eww-browse-url)
                      (cons "/SS/exampleCode/GSL"        #'eww-browse-url)
                      (cons "/SS/exampleCode/HDF5"       #'eww-browse-url)
                      (cons "/SS/exampleCode/mpi"        #'eww-browse-url)
                      (cons "/SS/exampleCode/NetCDF"     #'eww-browse-url)
                      (cons "/SS/exampleCode/openssl"    #'eww-browse-url)
                      (cons "/SS/exampleCode/postscript" #'eww-browse-url)
                      (cons "/SS/exampleCode/R"          #'eww-browse-url)
                      (cons "/SS/exampleCode/random"     #'eww-browse-url)
                      (cons "/SS/exampleCode/ruby"       #'eww-browse-url)
                      (cons "/SS/exampleCode/sqlite"     #'eww-browse-url)
                      (cons "/SS/exampleCode/vtk"        #'eww-browse-url)
                      (cons "."                          (if (string-match MJR-platform "WINDOWS-MGW")
                                                             #'browse-url-default-browser
                                                             #'browse-url-firefox))))
          ;; Put stuff in a new window
          (setq browse-url-new-window-flag 't)
          ;; Really don't put things in new tabs!!
          (setq browse-url-firefox-new-window-is-tab nil)
          ;;Set the browser approprately
          (if (not (string-match MJR-platform "WINDOWS-MGW"))
              (if (file-exists-p (concat MJR-home-bin "/browser"))
                  (setq browse-url-firefox-program (concat MJR-home-bin "/browser"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: PKG SETUP: time")
(if (require 'time nil :noerror)
    (progn
      ;; Time zone list for display-time-world
      (require 'time)
      ;; Put time and date in mode line
      (setq display-time-mail-string "") ;; Get rid of mail indicator in mode line
      (setq display-time-day-and-date 't)
      ;; (setq display-time-string-forms '((propertize (format "%04d-%02d-%02d %02d:%02d %s"
      ;;                                                 (string-to-int year)
      ;;                                                 (string-to-int month)
      ;;                                                 (string-to-int day)
      ;;                                                 (string-to-int 12-hours)
      ;;                                                 (string-to-int minutes)
      ;;                                                 (upcase am-pm))
      ;;                                         'face 'mode-line-emphasis)))
      (setq display-time-format "%Y-%m-%d %I:%M%p")
      (display-time)
      ;; Configure the cities I like for (display-time-world )
      (setq display-time-world-list '(("America/Los_Angeles"  "San Jose")
                                      ("America/Phoenix"      "Tucson")
                                      ("America/Chicago"      "Dallas")
                                      ("America/New_York"     "Toronto")
                                      ("Europe/London"        "London")
                                      ("Europe/Paris"         "Nice")
                                      ("Asia/Calcutta"        "Bangalore")
                                      ("Asia/Tokyo"           "Tokyo"))))
    (MJR-quiet-message "MJR: INIT: PKG SETUP: time: WARNING: Could not load package"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: PKG SETUP: server")
(eval-after-load "server"
  '(progn (MJR-quiet-message "MJR: POST-INIT(%s): EVAL-AFTER: server" (MJR-date "%Y-%m-%d_%H:%M:%S"))
          ;; Set a default name for the server each emacs instance will create
          (setq server-name (format "mjr-emacs-server-%d" (emacs-pid)))))
(autoload 'server-running-p "server" "Test whether server NAME is running." t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: PKG SETUP: speedbar")
(eval-after-load "speedbar"
  '(progn (MJR-quiet-message "MJR: POST-INIT(%s): EVAL-AFTER: speedbar!" (MJR-date "%Y-%m-%d_%H:%M:%S"))
          (dolist (ext '(".lisp" ".clisp" ".rb" ".f90" ".sql" ".f95" ".f08" ".f03" ".f2008"
                         ".f2003" ".F95" ".F08" ".F03" ".F2008" ".F2003" ".for" ".FOR" ".ps" "m2" "maple" "bash" "sh" ))
            (speedbar-add-supported-extension ext))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (not MJR-pookie-mode)
    (if (string-greaterp emacs-version "26")
        (progn (MJR-quiet-message "MJR: INIT: PKG SETUP: global-display-line-numbers-mode")
               (customize-set-variable  'global-display-line-numbers-mode nil)
               (customize-set-variable  'display-line-numbers-widen       't)
               (dolist (m '(ess-mode-hook
                            text-mode-hook
                            prog-mode-hook))
                 (add-hook m (lambda ()
                               (MJR-quiet-message "MJR: POST-INIT(%s): HOOK: +display-line-numbers-mode(%s)" (MJR-date "%Y-%m-%d_%H:%M:%S") major-mode)
                               (display-line-numbers-mode 1))))
               (dolist (m '(lisp-interaction-mode-hook))
                 (add-hook m (lambda ()
                               (MJR-quiet-message "MJR: POST-INIT(%s): HOOK: -display-line-numbers-mode(%s)" (MJR-date "%Y-%m-%d_%H:%M:%S") major-mode)
                               (display-line-numbers-mode -1)))))
        (progn (MJR-quiet-message "MJR: INIT: PKG SETUP: linum")
               (if (require 'linum nil :noerror)
                   (progn (global-linum-mode -1)
                          (dolist (m '(ess-mode-hook
                                       text-mode-hook
                                       prog-mode-hook))
                            (add-hook m (lambda ()
                                          (MJR-quiet-message "MJR: POST-INIT(%s): HOOK: +linum-mode" (MJR-date "%Y-%m-%d_%H:%M:%S"))
                                          (linum-mode 1)))))
                   (MJR-quiet-message "MJR: INIT: PKG SETUP: linum: WARNING: Could not load package"))
               (MJR-quiet-message "MJR: INIT: PKG SETUP: linum: WARNING: SKIP: pookie mode!"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: PKG SETUP: cmake")
(let ((cmake-path (find-if (lambda (p) (file-exists-p (concat p "/cmake-mode.el")))
                           (list "/usr/local/big/cmake/3.6.2/share/cmake-3.6/editors/emacs"
                                 "/usr/local/big/cmake/3.3.2/share/cmake-3.3/editors/emacs"
                                 "/usr/share/cmake-3.0/editors/emacs"))))
  (if cmake-path
      (progn
        (add-to-list 'load-path cmake-path)
        (add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))
        (add-to-list 'auto-mode-alist '("CMakeLists.txt\\'" . cmake-mode))
        (autoload 'cmake-mode "cmake-mode" "CMake mode" t)
        (eval-after-load "cmake-mode"
          '(progn (MJR-quiet-message "MJR: POST-INIT(%s): EVAL-AFTER: cmake!" (MJR-date "%Y-%m-%d_%H:%M:%S"))))
        ;;(require 'cmake-mode)
        )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: PKG SETUP: dired")
(eval-after-load "dired"
  '(progn (MJR-quiet-message "MJR: POST-INIT(%s): EVAL-AFTER: dired!" (MJR-date "%Y-%m-%d_%H:%M:%S"))
          (add-hook 'dired-mode-hook
                    (lambda ()
                      (MJR-quiet-message "MJR: POST-INIT(%s): HOOK: diredc-mode-hook" (MJR-date "%Y-%m-%d_%H:%M:%S"))
                      (if (boundp 'ido-enable-replace-completing-read)
                          (setq ido-enable-replace-completing-read nil))))  ;; If ido is loaded, make sure we don't use it in dired
          (define-key dired-mode-map (kbd "<mouse-3>") (lambda (event)
                                                         "In Dired, visit the file or directory name you click on in DIRED WINDOW!."
                                                         (interactive "e")
                                                         (let (window pos file)
                                                           (save-excursion
                                                             (setq window (posn-window (event-end event))
                                                                   pos (posn-point (event-end event)))
                                                             (if (not (windowp window))
                                                                 (error "No file chosen"))
                                                             (set-buffer (window-buffer window))
                                                             (goto-char pos)
                                                             (setq file (dired-get-file-for-visit)))
                                                           (select-window window)
                                                           (find-file (file-name-sans-versions file t)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (MJR-quiet-message "MJR: INIT: PKG SETUP: git")
(let ((git-el-files (find-if (lambda (pl) (every #'file-exists-p pl))
                             (mapcar (lambda (p) (mapcar (lambda (f) (concat (file-name-as-directory p) f))
                                                         '("git.el" "git-blame.el")))
                                     (list (or (MJR-find-newest-core-package "git") "")
                                           "/usr/share/git-core/emacs/"
                                           "/apps/free/git/2.3.5/bin/git/share/git-core/emacs")))))
  (if git-el-files
      (progn
        (dolist (git-el-file git-el-files)
          (load git-el-file))

        ;; (let ((path-to-git "c:/Program Files/Git/bin/"))
        ;;   (if (file-exists-p path-to-git)
        ;;       (progn (setq        explicit-shell-file-name (concat path-to-git "bash.exe"))
        ;;              (setq        shell-file-name          (concat path-to-git "bash.exe"))
        ;;              (setq        explicit-sh.exe-args     '("--login" "-i"))
        ;;              (setenv      "SHELL"                  (concat path-to-git "bash.exe"))
        ;;              (setenv      "PATH"                   (concat path-to-git ";" (getenv "PATH")))
        ;;              (add-to-list 'exec-path               path-to-git)
        ;;              )
          )
      (MJR-quiet-message "MJR: INIT: PKG SETUP: git: WARNING: Could not find custom git package")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: PKG SETUP: sh-mode")
(eval-after-load "sh-script"
  '(progn (MJR-quiet-message "MJR: POST-INIT(%s): EVAL-AFTER: sh-script!" (MJR-date "%Y-%m-%d_%H:%M:%S"))
          (add-hook 'sh-mode-hook (lambda ()
                                    (MJR-quiet-message "MJR: POST-INIT(%s): HOOK: sh-mode-hook" (MJR-date "%Y-%m-%d_%H:%M:%S"))
                                    (setq sh-basic-offset 2
                                          sh-indentation 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: PKG SETUP: term/ansi-term")
(eval-after-load "term"
  '(progn (MJR-quiet-message "MJR: POST-INIT(%s): EVAL-AFTER: term!" (MJR-date "%Y-%m-%d_%H:%M:%S"))
          ;; Note eshell-destroy-buffer-when-process-dies set to non-NIL will kill term sessions wehn over, but we want more.  What we do here is pause before
          ;; we destroy a term buffer if it has existed for 1 second or less.
          (advice-add 'term-handle-exit :after (lambda (&rest rest) (let ((dt buffer-display-time)
                                                                          (ct (current-time)))
                                                                      (if (or (null dt) (and (= (first ct) (first dt))
                                                                                             (<= (abs (- (second ct) (second dt))) 1)))
                                                                          (read-char "Press any key to exit terminal buffer."))
                                                                      (kill-buffer))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: PKG SETUP: eshell")
(eval-after-load "em-term"
  '(progn (MJR-quiet-message "MJR: POST-INIT(%s): EVAL-AFTER: em-term!" (MJR-date "%Y-%m-%d_%H:%M:%S"))
          (mapc (lambda (i) (add-to-list 'eshell-visual-commands i)) '("s" "sn" "sscreen" "sscreen.sh"
                                                                       "t" "tn" "td" "tnn" "stmux" "stmux.sh"
                                                                       "hexDump.rb" "hexDump"
                                                                       "byteAnalysis.rb" "byteAnalysis"
                                                                       "getSecret.sh" "getSecret"
                                                                       "hlflt.rb" "hlflt"
                                                                       "logTail.rb" "logTail"
                                                                       ))
          (mapcar (lambda (i) (add-to-list 'eshell-visual-subcommands i)) '(("git" "help" "log" "l" "ll" "diff" "show")))))
(eval-after-load "esh-mode"
  '(progn (MJR-quiet-message "MJR: POST-INIT(%s): EVAL-AFTER: esh-mode!" (MJR-date "%Y-%m-%d_%H:%M:%S"))
          (defun MJR-eshell-insert-last-word (n)
            (interactive "p")
            (insert (car (reverse (split-string (eshell-previous-input-string (- n 1)))))))
          (defun eshell/emacs (&rest args)
            (mapc #'find-file (mapcar #'expand-file-name (eshell-flatten-list (reverse args))))
            nil)
          (defun eshell/em (&rest args)
            (mapc #'find-file (mapcar #'expand-file-name (eshell-flatten-list (reverse args))))
            nil)
          (defun eshell/vi (&rest args)
            (mapc #'find-file (mapcar #'expand-file-name (eshell-flatten-list (reverse args))))
            nil)
          (defun eshell/vim (&rest args)
            (mapc #'find-file (mapcar #'expand-file-name (eshell-flatten-list (reverse args))))
            nil)
          (defun eshell/less (&rest args)
            (mapc #'view-file (mapcar #'expand-file-name (eshell-flatten-list (reverse args))))
            nil)
          (defun eshell/more (&rest args)
            (mapc #'view-file (mapcar #'expand-file-name (eshell-flatten-list (reverse args))))
            nil)
          (defun eshell/view (&rest args)
            (mapc #'view-file (mapcar #'expand-file-name (eshell-flatten-list (reverse args))))
            nil)
          (defun eshell/lsc (&rest args)
            (funcall #'eshell/ls args)
            nil)
          (defun eshell/dired (&rest args)
            (if (null args)
                (dired "cc.")
                (mapc #'dired (mapcar #'expand-file-name (eshell-flatten-list (reverse args)))))
            nil)
          (defun eshell/perldoc (&rest args)
            (if (not (null args))
                (funcall #'cperl-perldoc (apply 'eshell-flatten-and-stringify args)))
            nil)
          (defun eshell/dcd (&rest args)
            "Like eshell/dcd, but looks in directory-abbrev-alist too"
            (if (not (null args))
                (let* ((arg (apply 'eshell-flatten-and-stringify args))
                       (gom (cdr (assoc (concat "^" arg) directory-abbrev-alist))))
                  (if gom
                      (funcall #'eshell/cd gom)
                      (apply #'eshell/cd args)))
                nil))
          (setq eshell-cmpl-cycle-completions nil)
          (setq eshell-history-size 1048576)
          (add-hook 'eshell-mode-hook
                    (function (lambda ()
                                (MJR-quiet-message "MJR: POST-INIT(%s): HOOK: eshell-mode-hook" (MJR-date "%Y-%m-%d_%H:%M:%S"))
                                (MJR-try-theme)  ;; Duno why, but eshell needs to have the theme reapplied after it starts...
                                (setq pcomplete-cycle-completions nil)
                                (local-set-key "\M-."          'MJR-eshell-insert-last-word)
                                (local-set-key (kbd "<up>")    'previous-line)
                                (local-set-key (kbd "<down>")  'next-line)
                                (local-set-key (kbd "C-p")     'eshell-previous-input)
                                (local-set-key (kbd "C-n")     'eshell-next-input)
                                (if (not (server-running-p))
                                    (server-start))
                                (setenv "PAGER" "cat")
                                (setenv "EDITOR" (format "emacsclient -s %s" server-name)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (string-match MJR-platform "WINDOWS-MGW")
    (defun MJR-term (PROGRAM-TO-RUN)
      (interactive '(t))
      (message "Terminal not supported on WINDOWS"))
    (defun MJR-term (PROGRAM-TO-RUN)
      "Fire off an ansi-term with a nice, default buffer name.  Sets the EDITOR environment variable and makes sure emacs-server is running."
      (interactive (list (if (require 'ido nil :noerror)
                             (ido-completing-read "Program to run: " (remove-if-not #'file-exists-p (list "/bin/zsh"
                                                                                                          "/bin/bash"
                                                                                                          (concat MJR-home-bin "/t")
                                                                                                          (concat MJR-home-bin "/tn")
                                                                                                          (concat MJR-home-bin "/td")
                                                                                                          (concat MJR-home-bin "/tnn")
                                                                                                          (concat MJR-home-bin "/sn"))))
                             (read-string "Program to run: " "bash"))))
      (let* ((cns (file-name-nondirectory PROGRAM-TO-RUN))
             (bbn (upcase (or (second (assoc cns '(("s"   "screen")
                                                   ("sn"  "screen")
                                                   ("tn"  "tmux")
                                                   ("tnn" "tmux")
                                                   ("td"  "tmux")
                                                   ("t"   "tmux")))) cns))))
        (if (not (server-running-p))
            (server-start))
        (setenv "EDITOR" (format "emacsclient -s %s" server-name))
        (ansi-term PROGRAM-TO-RUN (dotimes (i 99)
                                    (if (not (get-buffer (format "*%s<%02d>*" bbn (1+ i))))
                                        (return (format "%s<%02d>" bbn (1+ i)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun MJR-emerge-git-conflict (file-name)
  "Merge a git conflict with ediff merge.  WARNING: sane people use magit for this, therefore this function is only for use by the insane."
  (interactive "fFile to merge: ")
  (loop for buf = (find-buffer-visiting file-name)
        while buf
        do (kill-buffer buf))
  (let ((fileA (make-temp-file "ediffmerge"))
        (fileB (make-temp-file "ediffmerge"))
        (fileM (make-temp-file "ediffmerge")))
    (call-process "git" nil (list :file fileA) nil "show" (concat ":2:" (file-name-nondirectory file-name))) ;; ME
    (call-process "git" nil (list :file fileB) nil "show" (concat ":3:" (file-name-nondirectory file-name))) ;; THEM
    (call-process "git" nil (list :file fileM) nil "show" (concat ":1:" (file-name-nondirectory file-name))) ;; UP
    (ediff-merge-files-with-ancestor fileA fileB fileM #'ediff-cleanup-mess file-name)
    (delete-file fileA)
    (delete-file fileB)
    (delete-file fileM)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun MJR-rotate-replace (rotate-list &optional match-what)
  "Parallel search and replace strings (name inspired by ROTATEF function).
ROTATE-LIST is a list of strings.  String n'th is replaced with n+1'th, with the final string being replaced by the first.
MATCH-WHAT argument determines what to match (word, symbol, and string).  NIL means string.
When run interactively MATCH-WHAT is set via the prefix arg: 1=>query, 4=>word, 16=>symbol, 32=>string
Operation is limited to region if a region is active."
  (interactive (list (let ((c 1)
                           (s nil))
                       (while (let ((in-str (read-string (format "Enter string or [enter] -- String(%d) : " c))))
                                (incf c)
                                (if (not (string-equal "" in-str))
                                    (push in-str s))))
                       (if (> (length s) 1)
                           s
                           (error "MJR-rotate-replace: Must provide at least two strings!!")))
                     (let* ((npfx (prefix-numeric-value current-prefix-arg)))
                       (cond ((>= npfx  32) "string")
                             ((>= npfx  16) "symbol")
                             ((>= npfx   4) "word")
                             ((>= npfx   1) (if (require 'ido nil :noerror)
                                                (ido-completing-read "Match Type: " '("word" "symbol" "string") nil 't)
                                                (completing-read "Match Type (word, symbol, or string): " '("word" "symbol" "string") nil 't "word")))))))

  (if (not (listp rotate-list))
      (error "MJR-rotate-replace: rotate-list argument must be a list!!!")
      (if (some (lambda (x) (not (stringp x))) rotate-list)
          (error "MJR-rotate-replace: rotate-list argument must be a list of strings!!!")
          (let ((rl-len (length rotate-list)))
            (if (< rl-len 2)
                (error "MJR-rotate-replace: rotate-list argument must contain at least two strings!!!")
                (let ((paren (if match-what
                                 (if (not (stringp match-what))
                                     (error "MJR-rotate-replace: match-what argument must be NIL or a string!!!")
                                     (let ((n (position match-what '("word" "symbol" "string") :test #'string-equal)))
                                       (if (not n)
                                           (error "MJR-rotate-replace: match-what argument is a string, but not valid (word, symbol, string)!!!")
                                           (elt (list 'words 'symbols nil) n)))))))
                  (save-excursion
                    (let* ((reg-min  (if (and (mark) (or (null transient-mark-mode) (region-active-p)))
                                         (min (point) (mark))
                                         (point)))
                           (reg-max  (if (and (mark) (or (null transient-mark-mode) (region-active-p)))
                                         (max (point) (mark))
                                         (point-max))))
                      (goto-char reg-min)
                      (while (re-search-forward (regexp-opt rotate-list paren) reg-max 't)
                        (replace-match (elt rotate-list (mod (1+ (position (match-string-no-properties 0) rotate-list :test #'string-equal)) rl-len))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: PKG SETUP: mail-mode setup...")
(eval-after-load "sendmail"
  '(progn (MJR-quiet-message "MJR: POST-INIT(%s): EVAL-AFTER: mail-mode!" (MJR-date "%Y-%m-%d_%H:%M:%S"))
          (setq compose-mail-user-agent-warnings nil)
          (add-hook 'mail-mode-hook
                    (lambda ()
                      (MJR-quiet-message "MJR: POST-INIT(%s): HOOK: mail-mode-hook(1)" (MJR-date "%Y-%m-%d_%H:%M:%S"))
                      (if (string-equal (substring (buffer-name) 0 5) "mutt-")
                          (let ((da-frames (frame-list)))
                            (mail-to)
                            ;;(flyspell-mode)
                            (dolist (da-frame da-frames)
                              (set-frame-position da-frame 1 1)
                              (set-frame-size     da-frame 150 64))))))
          (defface message-even-quoted-text-face
            '((((class color) (min-colors 88) (background light))    :foreground "magenta")
              (((class color) (min-colors 88) (background dark))     :foreground "magenta")
              (((class color) (min-colors 16) (background light))    :foreground "magenta")
              (((class color) (min-colors 16) (background dark))     :foreground "magenta")
              (((class color) (min-colors 8))                        :foreground "magenta")
              (t :inverse-video t))
            "Face for quoted messages at even level"
            :group 'basic-faces)
          (defface message-odd-quoted-text-face
            '((((class color) (min-colors 88) (background light))    :foreground "red")
              (((class color) (min-colors 88) (background dark))     :foreground "red")
              (((class color) (min-colors 16) (background light))    :foreground "red")
              (((class color) (min-colors 16) (background dark))     :foreground "red")
              (((class color) (min-colors 8))                        :foreground "red")
              (t :inverse-video t))
            "Face for quoted messages at odd level"
            :group 'basic-faces)
          (add-hook 'mail-mode-hook
                    (lambda ()
                      (MJR-quiet-message "MJR: POST-INIT(%s): HOOK: mail-mode-hook (2)" (MJR-date "%Y-%m-%d_%H:%M:%S"))
                      (font-lock-add-keywords nil
                                              '(("^[ \t]*>[^>\n].*$"          (0 'message-odd-quoted-text-face))
                                                ("^[ \t]*>$"                  (0 'message-odd-quoted-text-face))
                                                ("^[ \t]*>>[^>\n].*$"         (0 'message-even-quoted-text-face))
                                                ("^[ \t]*>>$"                 (0 'message-even-quoted-text-face))
                                                ("^[ \t]*>>>[^>\n].*$"        (0 'message-odd-quoted-text-face))
                                                ("^[ \t]*>>>$"                (0 'message-odd-quoted-text-face))
                                                ("^[ \t]*>>>>[^>\n].*$"       (0 'message-even-quoted-text-face))
                                                ("^[ \t]*>>>>$"               (0 'message-even-quoted-text-face))
                                                ("^[ \t]*>>>>>[^>\n].*$"      (0 'message-odd-quoted-text-face))
                                                ("^[ \t]*>>>>>$"              (0 'message-odd-quoted-text-face))
                                                ("^[ \t]*>>>>>>[^>\n].*$"     (0 'message-even-quoted-text-face))
                                                ("^[ \t]*>>>>>>$"             (0 'message-even-quoted-text-face))
                                                ("^[ \t]*>>>>>>>[^>\n].*$"    (0 'message-odd-quoted-text-face))
                                                ("^[ \t]*>>>>>>>$"            (0 'message-odd-quoted-text-face))
                                                ("^[ \t]*>>>>>>>>[^>\n].*$"   (0 'message-even-quoted-text-face))
                                                ("^[ \t]*>>>>>>>>$"           (0 'message-even-quoted-text-face))
                                                ("^[ \t]*>>>>>>>>>[^>\n].*$"  (0 'message-odd-quoted-text-face))
                                                ("^[ \t]*>>>>>>>>>$"          (0 'message-odd-quoted-text-face))
                                                ("^[ \t]*>>>>>>>>>>[^>\n].*$" (0 'message-even-quoted-text-face))
                                                ("^[ \t]*>>>>>>>>>>$"         (0 'message-even-quoted-text-face))
                                                ("^[ \t]*>.*$"                (0 'message-odd-quoted-text-face))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: PKG SETUP: org-mode setup...")
(if 't ;; The big if is here so we can disable org-mode configuration before we do an ELPA package install of org...
    (progn
      (let ((org-path (MJR-find-newest-core-package "org")))
        (if org-path
            (add-to-list 'load-path (concat org-path "lisp"))))

      (require 'org-install)
      ;;(require 'org-habit)

      (setq org-replace-disputed-keys t)  ;; Don't override S-<arrow> keys

      (if (not (require 'htmlize nil :noerror))
          (MJR-quiet-message "MJR: INIT: PKG SETUP: htmlize: WARNING: Could not load package in init."))

      (org-babel-do-load-languages
       'org-babel-load-languages
       '((awk         . t)
         (C           . t)
         (css         . t)
         (dot         . t)
         (emacs-lisp  . t)
         (eval        . t)
         (fortran     . t)
         (gnuplot     . t)
         (java        . t)
         (js          . t)
         (latex       . t)
         (lisp        . t)
         (matlab      . t)
         (maxima      . t)
         (octave      . t)
         (perl        . t)
         (python      . t)
         (R           . t)
         (ruby        . t)
         (shell       . t)
         (sql         . t)
         (sqlite      . t)))

      ;; (require 'ob)
      ;; (require 'ob-awk)
      ;; (require 'ob-C)
      ;; (require 'ob-css)
      ;; (require 'ob-dot)
      ;; (require 'ob-emacs-lisp)
      ;; (require 'ob-eval)
      ;; (require 'ob-fortran)
      ;; (require 'ob-gnuplot)
      ;; (require 'ob-java)
      ;; (require 'ob-js)
      ;; (require 'ob-latex)
      ;; (require 'ob-lisp)
      ;; (require 'ob-matlab)
      ;; (require 'ob-maxima)
      ;; (require 'ob-octave)
      ;; (require 'ob-perl)
      ;; (require 'ob-python)
      ;; (require 'ob-R)
      ;; (require 'ob-ruby)
      ;; (require 'ob-shell)
      ;; (require 'ob-sql)
      ;; (require 'ob-sqlite)

      (defun MJR-org-babel-execute-src-block ()
        "Wrap org-babel-execute-src-block"
        (interactive)
        (let ((org-confirm-babel-evaluate nil))
          (funcall-interactively #'org-babel-execute-src-block)))

      (defun MJR-org-babel-execute-subtree ()
        "Wrap org-babel-execute-subtree"
        (interactive)
        (let ((org-confirm-babel-evaluate nil))
          (funcall-interactively #'org-babel-execute-subtree)))

      (setq org-html-head-extra "<style>pre {background-color: #0f0f0f; color: #f0f0f0;}</style>") ;; Dark background for code.
      (setq org-html-postamble "Created with %c")
      (setq org-src-fontify-natively t)                         ;; Prety colors
      (setq org-src-tab-acts-natively t)                        ;; tab as in source mode
      (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))  ;; Make sure *.org files use org-mode
      (setq org-log-done 'time)                                 ;; Log timestamps on done TODO items
      (setq org-indent-mode t)                                  ;; Indent stuff
      (setq org-startup-folded nil)                             ;; Start with un-folded view
      (setq org-export-with-sub-superscripts nil)               ;; "_" and "^" are not special
      (setq org-confirm-babel-evaluate 't)                      ;; Ask about evals
      (setq org-export-babel-evaluate nil)                      ;; Do NOT eval on export
      (setq org-log-into-drawer "LOGBOOK")                      ;; Put TODO changes and notes in LOGBOOK drawer
      (setq org-agenda-files
            (let* ((tdp (concat MJR-home "/TODO/")))
              (if (file-directory-p tdp)
                  (directory-files tdp 't "\.org$"))))          ;; My generic TODO file
      (setq org-babel-min-lines-for-block-output 0)             ;; Always put babel results in blocks
      (add-hook 'org-mode-hook                                  ;; Get my favorite keys back
                (lambda ()
                  (MJR-quiet-message "MJR: POST-INIT(%s): HOOK: org-mode-hook" (MJR-date "%Y-%m-%d_%H:%M:%S"))
                  (turn-on-font-lock)
                  (local-set-key  (kbd "C-c C-v C-s")    'MJR-org-babel-execute-subtree)   ;; use "C-c C-v s" to eval with confirmation prompts
                  (local-set-key  (kbd "C-c C-v C-e")    'MJR-org-babel-execute-src-block) ;; use "C-c C-v e" to eval with confirmation prompts
                  (local-set-key  (kbd "C-c a")          'org-agenda)
                  ))
      ;; Need to set the R path on Windows...
      (if (string-equal MJR-platform "WINDOWS-MGW")
          (let ((found-r (find-if #'file-exists-p (list "c:/Program Files/Microsoft/R Open/R-3.4.4/bin/x64/Rterm.exe"
                                                        "c:/Program Files/Microsoft/R Open/R-3.4.2/bin/x64/Rterm.exe"
                                                        "c:/Program Files/Microsoft/R Open/R-3.4.1/bin/x64/Rterm.exe"
                                                        "c:/Program Files/Microsoft/R Open/R-3.4.0/bin/x64/Rterm.exe"))))
            (setq org-babel-R-command found-r)))

      ;; Setup orgtbl in other modes
      (if nil
          (dolist (m '(emacs-lisp-mode-hook
                       fortran-mode-hook
                       perl-mode-hook
                       cperl-mode-hook
                       lisp-mode-hook
                       ess-mode-hook
                       sh-mode-hook
                       c++-mode-hook
                       java-mode-hook
                       js-mode-hook
                       python-mode-hook
                       ruby-mode-hook
                       text-mode-hook
                       html-mode-hook
                       c-mode-hook))
            (add-hook m (lambda ()
                          (MJR-quiet-message "MJR: POST-INIT(%s): HOOK: +orgtbl-mode" (MJR-date "%Y-%m-%d_%H:%M:%S"))
                          (turn-on-orgtbl)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: PKG SETUP: calender (solar stuff)")
(eval-after-load "solar"
  '(progn (MJR-quiet-message "MJR: POST-INIT(%s): EVAL-AFTER: solar!" (MJR-date "%Y-%m-%d_%H:%M:%S"))
          (if (version<= "23.1" emacs-version) (calendar-set-date-style 'iso))
          (setq calendar-location-name "Dallas, TX")
          (setq calendar-latitude 32.00)
          (setq calendar-longitude -96.00)))
(eval-after-load "cal-dst"
  '(progn (MJR-quiet-message "MJR: POST-INIT(%s): EVAL-AFTER: cal-dst!" (MJR-date "%Y-%m-%d_%H:%M:%S"))
          (setq calendar-time-zone -360)
          (setq calendar-standard-time-zone-name "CST")
          (setq calendar-daylight-time-zone-name "CDT")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: PKG SETUP: root-help setup...")
(if (file-exists-p "/usr/share/emacs/site-lisp/root-help.el")
    (progn (autoload 'root-shell "root-help" "Run ROOT (the C++ Data Analysis Framework) shell" t)
           ;; Put any pre-load root config stuff here...
           (eval-after-load "root-help"
             '(progn (MJR-quiet-message "MJR: POST-INIT(%s): EVAL-AFTER: root-help!" (MJR-date "%Y-%m-%d_%H:%M:%S"))
                     ;; Put any post-load root config stuff here...
                     ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: PKG SETUP: gdb-mode (GUD) setup...")
(add-hook 'gdb-mode-hook
          (function (lambda ()
                      (MJR-quiet-message "MJR: POST-INIT(%s): HOOK: gdb-mode-hook" (MJR-date "%Y-%m-%d_%H:%M:%S"))
                      (setq gdb-many-windows           't)   ;; Use lots of windows for gdb
                      (setq gdb-use-separate-io-buffer nil)  ;; No I/O window
                      (tool-bar-mode 1)                      ;; Start up the tool-bar when we start up gdb
                      (add-hook 'kill-buffer-hook            ;; Set a kill-buffer-hook to get rid of the tool-bar when we leave.
                                (function (lambda ()
                                            (MJR-quiet-message "MJR: POST-INIT(%s): HOOK: kill-buffer-hook" (MJR-date "%Y-%m-%d_%H:%M:%S"))
                                            (tool-bar-mode -1)))
                                't 't))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: PKG SETUP: tramp-mode setup...")
(if (require 'tramp nil :noerror)
    (progn (if (and (string-equal MJR-location "WORK:TI")
                    (string-equal MJR-platform "WINDOWS-MGW"))
               (customize-set-variable 'tramp-password-prompt-regexp
                                       (concat "^.*"
                                               (regexp-opt'("password:"
                                                            "Password:"
                                                            "Enter the passcode from your smartphone or hard token"
                                                            )
                                                          t)
                                               ".*$")))
           (setq tramp-default-user   (if (string-equal MJR-location "WORK:TI") (user-real-login-name) MJR-uname)
                 tramp-terminal-type  "dumb"
                 tramp-default-method "sshx")
           ;;                                         host                    user         method
           (add-to-list 'tramp-default-method-alist '("\\`localhost\\'"       "\\`root\\'" "sudo"))
           ;;                                     FROM RE         TO VALUE
           (add-to-list 'directory-abbrev-alist '("^/rut"       . "/root@localhost:/"))
           (if (file-exists-p (concat MJR-home-dot "/.tramp.el"))
               (load-file (concat MJR-home-dot "/.tramp.el"))))
    (MJR-quiet-message "MJR: INIT: PKG SETUP: tramp: WARNING: Could not load package"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: PKG SETUP: aspell setup...")
(eval-after-load "ispell"
  '(progn (MJR-quiet-message "MJR: POST-INIT(%s): EVAL-AFTER: ispell!" (MJR-date "%Y-%m-%d_%H:%M:%S"))
          (if (and nil (file-exists-p "c:/Program Files (x86)/hunspell-1.3.2-3/bin/hunspell.exe"))  ;; Perminatly turned off -- I use msys2 for emacs now
              (progn ;; Windows with hunspell
                (setq ispell-program-name "c:/Program Files (x86)/hunspell-1.3.2-3/bin/hunspell.exe")
                (setq ispell-local-dictionary "en_US")
                (setq ispell-local-dictionary-alist '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8))))
              (let ((aspell-path (find-if #'file-exists-p
                                          (list "/usr/bin/hunspell"
                                                "/bin/hunspell"
                                                "/usr/local/bin/aspell"
                                                "/opt/local/bin/aspell"
                                                "/bin/aspell"
                                                "/usr/bin/aspell"
                                                "C:/msys64/local/bin/aspell.exe"
                                                "C:/msys64/mingw64/bin/aspell.exe"
                                                "C:/msys64/bin/aspell.exe"))))
                (if aspell-path
                    (setq-default ispell-program-name aspell-path)
                    (setq-default ispell-program-name "ispell"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: PKG SETUP: emacs-lisp-mode setup...")
;; Don't indent the third argument if the if form differently...
(put 'if 'lisp-indent-function nil)
(add-hook 'emacs-lisp-mode-hook
          (function (lambda ()
                      (MJR-quiet-message "MJR: POST-INIT(%s): HOOK: emacs-lisp-mode-hook" (MJR-date "%Y-%m-%d_%H:%M:%S"))
                      (local-set-key "\C-c\C-c" 'byte-recompile-directory)
                      (local-set-key "\C-c\C-b" 'byte-compile-file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: PKG SETUP: c-mode setup...")
(add-hook 'c-mode-common-hook
          (function (lambda ()
                      (MJR-quiet-message "MJR: POST-INIT(%s): HOOK: c-mode-common-hook" (MJR-date "%Y-%m-%d_%H:%M:%S"))
                      (setq c-inhibit-startup-warnings-p   t)
                      (setq c-echo-syntactic-information-p nil)
                      (setq c-progress-interval            1)
                      (c-add-style "MJR"
                                   `("k&r"
                                     (c-doc-comment-style        . 'javadoc)
                                     (c-tab-always-indent        . t)
                                     (c-recognize-knr-p          . nil)
                                     (c-basic-offset             . 2)
                                     (c-comment-only-line-offset . 0)
                                     (c-offsets-alist (inclass      . ++)
                                                      (access-label . -)
                                                      (case-label   . +))))
                      (c-set-style "MJR")
                      (local-set-key "\C-c\C-c" 'compile))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: PKG SETUP: fortran-mode setup...")
(add-hook 'fortran-mode-hook
          (function (lambda ()
                      (MJR-quiet-message "MJR: POST-INIT(%s): HOOK: fortran-mode-hook" (MJR-date "%Y-%m-%d_%H:%M:%S"))
                      (setq comment-start               "c")
                      (setq fortran-comment-indent-char " ")
                      (setq fortran-blink-matching-if   t)
                      (setq fortran-comment-region      "c     "))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: PKG SETUP: perl-mode setup...")
(add-hook 'perl-mode-hook
          (function (lambda ()
                      (MJR-quiet-message "MJR: POST-INIT(%s): HOOK: perl-mode-hook" (MJR-date "%Y-%m-%d_%H:%M:%S"))
                      (setq perl-indent-level 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: PKG SETUP: auctex setup...")
(if (locate-library "auctex")
    (MJR-quiet-message "MJR: INIT: PKG SETUP: auctex found in non-core...")
    (let ((core-atex-base (MJR-find-newest-core-package "auctex")))
      (if core-atex-base
          (progn
            (MJR-quiet-message "MJR: INIT: PKG SETUP: auctex found in core... %s" core-atex-base)
            (add-to-list 'load-path core-atex-base)
            (load "auctex.el" t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: PKG SETUP: TeX-mode setup...")
(if (or (string-equal MJR-platform "WINDOWS-MGW")
        (string-equal MJR-platform "WINDOWS-CYG"))
    (add-hook 'TeX-mode-hook
              (function (lambda ()
                          (MJR-quiet-message "MJR: POST-INIT(%s): HOOK: TeX-mode-hook" (MJR-date "%Y-%m-%d_%H:%M:%S"))
                          (setq ispell-parser 'nroff)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: PKG SETUP: latex-mode setup...")
(add-hook 'latex-mode-hook
          (function (lambda ()
                      (MJR-quiet-message "MJR: POST-INIT(%s): HOOK: latex-mode-hook" (MJR-date "%Y-%m-%d_%H:%M:%S"))
                      (setq tex-font-script-display 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: PKG SETUP: Macaulay 2 setup...")
(let ((mac-path (find-if #'file-exists-p
                         (list "/usr/local/big/Macaulay2/1.7/share/emacs/site-lisp"
                               "/Applications/Macaulay2-1.2/share/emacs/site-lisp/"
                               "/Applications/Macaulay2-1.1/share/emacs/site-lisp/"
                               "/usr/local/share/emacs/site-lisp"
                               "/usr/share/emacs/site-lisp"))))
  (if (and mac-path (file-exists-p (concat mac-path "/M2-init.el")))
      (progn (add-to-list 'load-path mac-path)
             (load "M2-init" t)) ;; M2-init is a list of autoloads
      (MJR-quiet-message "MJR: INIT: PKG SETUP: Macaulay 2 not found...")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: PKG SETUP: Maxima setup...")
(let ((max-path (find-if (lambda (p) (file-exists-p (concat p "/maxima.el")))
                         (list "/usr/local/big/maxima-5.40.0_sbcl-1.3.19/share/maxima/5.40.0/emacs" ;; Custom build on Debian 9
                               "/usr/local/big/maxima/5.38.1_sbcl-1.3.11/share/maxima/5.38.1/emacs" ;; Custom build on Debian 8
                               "/usr/local/big/maxima/5.38.0_sbcl-1.3.4/share/maxima/5.36.0/emacs"  ;; Custom build on Debian 8
                               "/usr/local/big/maxima/5.36.0_sbcl-1.2.11/share/maxima/5.36.0/emacs" ;; Custom build on Debian 8
                               "/usr/local/big/maxima/5.37.0_sbcl-1.2.14/share/maxima/5.37.0/emacs" ;; Custom build on Debian 8
                               "/home/richmit/s/linux/local/share/maxima/5.29.1/emacs"              ;; Custom biuld on linux
                               "/opt/local/share/maxima/5.16.3/emacs"                               ;; Typical MacOS X with macports
                               "/usr/share/maxima/5.38.1/emacs/"                                    ;; Standard location for Debian 9
                               "/usr/share/maxima/5.34.1/emacs/"                                    ;; Standard location for Debian 8
                               "/usr/share/maxima/5.21.1/emacs"                                     ;; Standard location for Ubuntu 11.04
                               "C:/maxima-5.40.0/share/maxima/5.40.0/emacs/"))))                    ;; Standard location on Windows 10
  (if max-path
      (progn (MJR-quiet-message "MJR: INIT: PKG SETUP: Specific version of maxima.el found...")
             (add-to-list 'load-path max-path)
             (autoload 'maxima      "maxima" "Run Maxima in a window" t)
             (autoload 'maxima-mode "maxima" "Edit Maxima code" t)
             (add-to-list 'auto-mode-alist '("\\.mac$" . maxima-mode))
             (eval-after-load "maxima"
               '(progn (MJR-quiet-message "MJR: POST-INIT(%s): EVAL-AFTER: maxima!" (MJR-date "%Y-%m-%d_%H:%M:%S"))
                       (let ((max-cmd  (find-if #'file-exists-p
                                                (list "/usr/local/big/maxima-5.40.0_sbcl-1.3.19/bin/maxima" ;; Custom build on Debian 9 @ home
                                                      "/usr/local/big/maxima/5.38.1_sbcl-1.3.11/bin/maxima" ;; Custom build on Debian 8 @ home
                                                      "/usr/local/big/maxima/5.38.0_sbcl-1.3.4/bin/maxima"  ;; Custom build on Debian 8 @ home
                                                      "/usr/local/big/maxima/5.37.0_sbcl-1.2.14/bin/maxima" ;; Custom build on Debian 8 @ home
                                                      "/usr/local/big/maxima/5.36.0_sbcl-1.2.11/bin/maxima" ;; Custom build on Debian 8 @ home
                                                      "/home/richmit/s/linux/local/bin/maxima"              ;; Custom biuld on linux @ TI
                                                      "/opt/local/bin/maxima"                               ;; Typical MacOS X with macports
                                                      "/usr/local/bin/maxima"                               ;; Typical place
                                                      "/usr/bin/maxima"                                     ;; Standard place for Debian & Ubuntu
                                                      "C:/maxima-5.40.0/bin/maxima.bat"))))                 ;; Standard location on Windows 10
                         (if max-cmd
                             (progn
                               (MJR-quiet-message "MJR: INIT: PKG SETUP: Specific Maxima binary found...")
                               (setq maxima-command (or max-cmd "maxima"))))
                         (cond ((file-exists-p "/usr/lib/maxima/5.21.1/binary-gcl")  (setq maxima-args "-l gcl")) ;; old format for ubuntu 10.xx
                               ((file-exists-p "/usr/lib/maxima/5.27.0/binary-gcl")  nil)
                               ('t                                                   (setq maxima-args '("-l" "sbcl"))))
                         (add-hook 'inferior-maxima-mode-hook
                                   (lambda ()
                                     (MJR-quiet-message "MJR: POST-INIT(%s): HOOK: inferior-maxima-mode-hook" (MJR-date "%Y-%m-%d_%H:%M:%S"))
                                     (font-lock-mode 1)
                                     (local-set-key (kbd "TAB") 'inferior-maxima-complete)))))))
      (MJR-quiet-message "MJR: INIT: PKG SETUP: Maxima not found...")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: PKG SETUP: bookmarks setup...")
(eval-after-load "bookmark"
  '(progn (MJR-quiet-message "MJR: POST-INIT(%s): EVAL-AFTER: bookmarks!" (MJR-date "%Y-%m-%d_%H:%M:%S"))
          (bookmark-maybe-load-default-file)
          (setq bookmark-save-flag 0) ;; Save bookmark file after each change to the bookmark list.
          ;; Make sure some bookmarks exist and point to the right thing.
          (if (not MJR-pookie-mode)
              (dolist (bp (list (cons "templates-tex"      (concat MJR-home-cor "/texinputs/"))                 ;; Templates and input files: TeX/LaTeX
                                (cons "templates-org"      (concat MJR-home-cor "/org-mode/"))                  ;; Templates and input files: org-mode
                                (cons "lispy-prod"         (concat MJR-home-cor "/lispy/"))                     ;; *mjrcalc*: Production copy
                                (cons "lispy-dev"          (concat MJR-home "/world/my_prog/lispStuff/lispy/")) ;; *mjrcalc*: Development copy
                                (cons "note-cheatsheet"    (concat MJR-home "/world/stuff/my_ref/"))            ;; Notes: Cheat sheets
                                (cons "note-computer"      (concat MJR-home "/world/stuff/notes/computer/"))    ;; Notes: Computer stuff
                                (cons "ref-code-R"         (concat MJR-home "/world/my_prog/learn/R"))          ;; Refrence code: R
                                (cons "ref-code-ruby"      (concat MJR-home "/world/my_prog/learn/ruby"))       ;; Refrence code: Ruby
                                (cons "dotfiles"           (concat MJR-home "/world/dotfiles/"))                ;; dot fiel repo
                                (cons "world"              (concat MJR-home "/world/"))                         ;; All my stuff. ;)
                                (cons "home-win"           (list                                                ;; Windows Home directory -- VM/cygwin/windows
                                                            (concat "C:\\Users\\" (user-real-login-name))        ;; Windows (native/mingw/msys2/etc...)
                                                            (concat "/c/Users/" (user-real-login-name))          ;; msys2
                                                            (concat "/cygwin/c/Users/" (user-real-login-name))   ;; Cygwin
                                                            "/media/sf_winHome"                                  ;; Virtual Box mount on Windows
                                                            (concat MJR-home "/winHome/")                        ;; Link on most platforms
                                                            "~/winHOme"))                                        ;; Link on most platforms
                                (cons "home-msys2"         (list                                                ;; msys2 home directory
                                                            (concat "C:/msys64/home/" (user-real-login-name))))
                                (cons "home-cyg"           (list                                                ;; cygwin home directory
                                                            (concat "/home/" (user-real-login-name))             ;; Runing under cygwin
                                                            (concat "c:/cygwin64/home/" (user-real-login-name))  ;; Running Under msys2
                                                            ))
                                (cons "doc1"               (list                                                ;; ebook repo #1
                                                            "/Users/Shared/Doc1/"                                ;; OSX
                                                            "D:\\Doc1\\"                                         ;; Windows
                                                            "/cygdrive/d/Doc1"                                   ;; Cygwin
                                                            "/media/sf_D_DRIVE/Doc1"))                           ;; Virtual Box mount on Windows
                                (cons "doc2"               (list                                                ;; ebook repo #1 (cs, science, etc..)
                                                            "/Users/Shared/Doc2/index.org"                       ;; OSX
                                                            "/cygdrive/d/Doc2//index.org"                        ;; Cygwin
                                                            "D:\\Doc2\\index.org"                                ;; Windows (native/mingw/msys2/etc...)
                                                            "/media/sf_D_DRIVE/Doc2/index.org"))                 ;; Virtual Box mount on Windows
                                (cons "doc3"               (list                                                ;; ebook repo #3 (math books)
                                                            "/Users/Shared/Doc3/index.org"                       ;; OSX
                                                            "/cygdrive/d/Doc3/index.org"                         ;; Cygwin
                                                            "D:\\Doc3\\index.org"                                ;; Windows (native/mingw/msys2/etc...)
                                                            "/media/sf_D_DRIVE/Doc3/index.org"))))               ;; Virtual Box mount on Windows
                (let* ((bmk-name             (car bp))
                       (bmk-target-options   (cdr bp))
                       (bmk-target-validated (if (stringp bmk-target-options)
                                                 (if (file-exists-p bmk-target-options) bmk-target-options)
                                                 (find-if #'file-exists-p bmk-target-options)))
                       (bmk-target-expanded (and bmk-target-validated (expand-file-name bmk-target-validated))))
                  (if (and bmk-name bmk-target-expanded)
                      (let ((bmkl (list bmk-name (cons 'filename bmk-target-expanded))))
                        (if (assoc bmk-name bookmark-alist)
                            (setcdr (assoc bmk-name bookmark-alist) bmkl)
                            (add-to-list 'bookmark-alist bmkl))))))
              (MJR-quiet-message "MJR: INIT: PKG SETUP: bookmarks: SKIP: Pookie mode"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: PKG SETUP: iMaxima setup...")
(eval-after-load "imaxima"
  '(progn (MJR-quiet-message "MJR: POST-INIT(%s): EVAL-AFTER: imaxima!" (MJR-date "%Y-%m-%d_%H:%M:%S"))
          ;; The type size used in LaTeX. Options: 9, 10, 11, 12
          (setq imaxima-pt-size 9)
          ;; Default size of font. Options: "small", "normalsize", "large", "Large", "LARGE", "huge", "Huge"
          (setq imaxima-fnt-size "small")
          ;; Scale all images by this factor. Default: 1.0
          (setq imaxima-scale-factor 2.0)
          ;; Use maxima mode
          (setq imaxima-use-maxima-mode-flag 't)))
(autoload 'imaxima "imaxima" "Maxima mode with typeset results" t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: PKG SETUP: MapleV")
(let ((maplev-base-path (MJR-find-newest-core-package "maplev")))
  (if maplev-base-path
      (let ((maplev-path (concat maplev-base-path "lisp/")))
        (if (file-exists-p (concat maplev-path "maplev.el"))
            (progn (MJR-quiet-message "MJR: INIT: PKG SETUP: MAPLEV found... %s" maplev-path)
                   (add-to-list 'load-path maplev-path)
                   (autoload 'maplev "maplev" "Maple editing mode" t)
                   (autoload 'cmaple "maplev" "Interactive Maple session" t)
                   (add-to-list 'auto-mode-alist '("\\.mpl$" . maplev-mode))
                   (eval-after-load "maplev"
                     `(progn (MJR-quiet-message "MJR: POST-INIT(%s): EVAL-AFTER: maplev!" (MJR-date "%Y-%m-%d_%H:%M:%S"))
                             (setq maplev-include-path (list ,maplev-base-path ,(concat maplev-base-path "/maple")))
                             (setq maplev-copyright-owner "Mitch J. Richling")
                             (setq maplev-default-release "2018")
                             (setq maplev-release "2018")
                             (setq maplev-executable-alist '(("2018" . ("c:/Program Files/Maple 2018/bin.X86_64_WINDOWS/cmaple.exe"
                                                                        nil
                                                                        "c:/Program Files/Maple 2018/bin.X86_64_WINDOWS/mint.exe"))
                                                             ("2017" . ("c:/Program Files/Maple 2017/bin.X86_64_WINDOWS/cmaple.exe"
                                                                        nil
                                                                        "c:/Program Files/Maple 2017/bin.X86_64_WINDOWS/mint.exe"))))



                             (setq maplev-mint-query nil)
                             (setq maplev-description-quote-char ?\"))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: PKG SETUP: OCTAVE setup...")
(if (not (string-equal MJR-platform "WINDOWS-MGW"))
    (let ((octave-base-path (MJR-find-newest-core-package "octave")))
      (if octave-base-path
          (progn (MJR-quiet-message "MJR: INIT: PKG SETUP: OCTAVE found... %s" octave-base-path)
                 (add-to-list 'load-path (concat MJR-home-cor octave-base-path))
                 (autoload 'octave-mode  "octave-mod" "Octave editing mode" t)
                 (setq auto-mode-alist
                       (cons '("\\.m$" . octave-mode) auto-mode-alist))
                 (add-hook 'octave-mode-hook
                           (lambda ()
                             (MJR-quiet-message "MJR: POST-INIT(%s): HOOK: octave-mode-hook" (MJR-date "%Y-%m-%d_%H:%M:%S"))
                             (abbrev-mode 1)
                             (auto-fill-mode 1)
                             (font-lock-mode 1)))
                 (autoload 'run-octave   "octave-inf" "Interactive Octave" t)
                 (add-hook 'inferior-octave-mode-hook
                           (lambda ()
                             (MJR-quiet-message "MJR: POST-INIT(%s): HOOK: inferior-octave-mode-hook" (MJR-date "%Y-%m-%d_%H:%M:%S"))
                             (font-lock-mode 1))))
          (MJR-quiet-message "MJR: INIT: PKG SETUP: OCTAVE Not Found...")))
    (MJR-quiet-message "MJR: INIT: PKG SETUP: octave: WARNING: Setup suppressed on windows"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: PKG SETUP: procesing mode setup...")
(let ((processing-path (MJR-find-newest-core-package "processing")))
  (if processing-path
      (progn (MJR-quiet-message "MJR: INIT: PKG SETUP: processing found... %s" processing-path)
             (add-to-list 'load-path processing-path)
             (add-to-list 'auto-mode-alist '("\\.pde$" . processing-mode))
             (autoload 'processing-mode "processing-mode" "Processing mode" t))
      (MJR-quiet-message "MJR: INIT: PKG SETUP: Processing Mode Not Found...")))

;;slime-eval-save slime-interactive-eval


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: PKG SETUP: HYPERSPEC...")
(eval-after-load "hyperspec"
  '(progn (MJR-quiet-message "MJR: POST-INIT(%s): EVAL-AFTER: hyperspec!" (MJR-date "%Y-%m-%d_%H:%M:%S"))
          (let ((spec-to-use (find-if #'file-exists-p
                                      (list "/usr/share/doc/hyperspec/"
                                            "/Users/Shared/Doc2/software-dev/LISP/hyperspec/"
                                            "/media/sf_D_DRIVE/Doc2/software-dev/LISP/hyperspec/"
                                            "d:/Doc2/software-dev/LISP/hyperspec/Body"
                                            "/opt/local/share/doc/lisp/HyperSpec-7-0/HyperSpec/"))))
            (if spec-to-use
                (setq common-lisp-hyperspec-root (concat "file:" spec-to-use))
                (MJR-quiet-message "MJR INIT: WARNING: Using remote hyperspec: %s"
                                   (setq common-lisp-hyperspec-root "http://www.lispworks.com/reference/HyperSpec/"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: PKG SETUP: SLIME...")
(if (require 'slime-autoloads nil :noerror)
    (MJR-quiet-message "MJR: INIT: PKG SETUP: slime found in non-core...")
    (let ((slime-path (MJR-find-newest-core-package "slime")))
      (if slime-path
          (progn (MJR-quiet-message "MJR: INIT: PKG SETUP: slime found in core... %s" slime-path)
                 (add-to-list 'load-path slime-path)
                 (require 'slime-autoloads)))))
(if (require 'slime-autoloads nil :noerror)
    (eval-after-load "slime"
      '(progn (MJR-quiet-message "MJR: POST-INIT(%s): EVAL-AFTER: slime!" (MJR-date "%Y-%m-%d_%H:%M:%S"))
              (let ((slime-lisp-bin (find-if #'file-exists-p
                                             (list "/usr/local/big/sbcl/1.3.11/bin/sbcl"
                                                   "/usr/local/big/sbcl/1.3.4/bin/sbcl"
                                                   "/usr/local/big/sbcl/1.2.14/bin/sbcl"
                                                   "/usr/local/big/sbcl/1.2.11/bin/sbcl"
                                                   "/home/richmit/s/linux/local/bin/sbcl"
                                                   "/usr/local/bin/sbcl-run"
                                                   "/usr/local/bin/sbcl"
                                                   "/opt/local/bin/sbcl"
                                                   "/usr/bin/sbcl"
                                                   "C:\\PROGRA~1\\STEELB~1\\1.3.18\\SBCL.EXE"
                                                   "C:\\PROGRA~1\\STEELB~1\\1.3.12\\SBCL.EXE"
                                                   "C:\\PROGRA~1\\STEELB~1\\1.0.51\\SBCL.EXE"))))
                (if slime-lisp-bin
                    (setq inferior-lisp-program slime-lisp-bin)
                    (MJR-quiet-message "MJR INIT: WARNING: No working lisp found"))
                (slime-setup '(slime-repl)) ; Setup (use SLIME-REPL)
                (setq lisp-simple-loop-indentation  1
                      lisp-loop-keyword-indentation 6
                      lisp-loop-forms-indentation   6)
                (setq slime-net-coding-system 'utf-8-unix)
                (add-hook 'lisp-mode-hook
                          (lambda ()
                            (MJR-quiet-message "MJR: POST-INIT(%s): HOOK: lisp-mode-hook" (MJR-date "%Y-%m-%d_%H:%M:%S"))
                            (setq slime-net-coding-system 'utf-8-unix)))
                ;; REPL bindings
                (define-key slime-repl-mode-map "\M-."            'slime-edit-definition-with-etags) ;; A somewhat slimey version of find-tag
                (define-key slime-repl-mode-map "\M-,"            'tags-loop-continue)               ;; Use  tags contineu for M,
                (define-key slime-repl-mode-map (kbd "ESC ESC .") 'slime-edit-definition)            ;; Put SLIMEy M. on MM.
                (define-key slime-repl-mode-map (kbd "ESC ESC ,") 'slime-pop-find-definition-stack)  ;; Put SLIMEy M, on MM,
                (define-key slime-repl-mode-map (kbd "C-p")       'slime-repl-backward-input)        ;; Previous history on C-p
                (define-key slime-repl-mode-map (kbd "C-n")       'slime-repl-forward-input)         ;; Previous history on C-p
                (define-key slime-repl-mode-map (kbd "C-c s")     'slime-selector)                   ;; Slime Selector
                ;; CODE bindings
                (define-key slime-mode-map "\M-."                 'slime-edit-definition-with-etags) ;; A somewhat slimey version of find-tag
                (define-key slime-mode-map "\M-,"                 'tags-loop-continue)               ;; Use  tags contineu for M,
                (define-key slime-mode-map (kbd "ESC ESC .")      'slime-edit-definition)            ;; Put SLIMEy M. on MM.
                (define-key slime-mode-map (kbd "ESC ESC ,")      'slime-pop-find-definition-stack)  ;; Put SLIMEy M, on MM,
                (define-key slime-mode-map (kbd "C-c s")          'slime-selector)                   ;; Slime Selector
                ;;(define-key slime-mode-map "\M-." 'find-tag)
                ;; M-x slime-who-calls       Show function callers.
                ;; M-x slime-who-references  Show references to global variable.
)))
    (MJR-quiet-message "MJR: INIT: PKG SETUP: SLIME Not Found..."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: PKG SETUP: ido...")
(if (require 'ido nil :noerror)
    (progn (if MJR-pookie-mode
               (setq ido-use-filename-at-point nil)                                              ;; Disable ffap in pookie mode
               (setq ido-use-filename-at-point 'guess))                                          ;; Eable ffap in non-pookie mode
           (setq ido-auto-merge-work-directories-length -1)                                      ;; Don't swtich to other directories if file not found
           (setq ido-use-url-at-point nil)                                                       ;; Don't look for URLs at point
           (dolist (directory-re '("\\`RCS/" "\\`auto/"                                          ;; Set ignore directory list
                                   "\\`\\.git/"))
             (add-to-list 'ido-ignore-directories directory-re))
           (dolist (ext '("fasl" "ufasl" "fas" "lib" "o"                                         ;; File extentiosn to ignore -- ido-ignore-extensions no workie for me
                          "dvi" "asd" "so"
                          "aux" "bbl" "bcf" "blg" "log"
                          "out" "run.xml"))
             (add-to-list 'ido-ignore-files (concat "\\." ext "$")))
           (dolist (file-re '("\\`RCS/" "\\`\\.git/"                                             ;; Some files to ignore
                              "\\`\\.DS_Store" "\\`a\\.out"))
             (add-to-list 'ido-ignore-files file-re))
           (dolist (buffer-re '("\\`\\*ESS\\*" "\\`\\*slime-events\\*"                           ;; Some buffers to ignore
                                "\\`\\*Apropos\\*"
                                "\\`\\*Completions\\*" "\\`\\*vc\\*"
                                "\\`\\log-edit-files\\*"
                                "\\`\\*slime-compilation\\*"
                                "\\`\\*inferior-lisp\\*"
                                "\\`\\*Warning\\*"))
             (add-to-list 'ido-ignore-buffers buffer-re))
           (setq ido-file-extensions-order '(".org" ".lisp" ".R" ".rb" ".tex" ".txt"             ;; Order to list files only differing by extension
                                             ".hpp" ".cpp" ".h" ".c" ".asd" ".log"))
           (setq ido-enable-flex-matching t)                                                     ;; List everything that matches input (not just at start)
           (setq ido-everywhere t)                                                               ;; Turn it on everyplace
           (if MJR-pookie-mode
               (ido-mode 'buffers)                                                               ;; pookie mode: Start ido mode for buffers only
               (ido-mode 1))                                                                     ;; non-pookie mode: Start ido mode for buffers and files
           (defun MJR-wack-back-to-slash ()                                                      ;; Zap everything backward till a "/" character
             "Zap characters backward till a slash(/) character"
               (interactive)
             (goto-char (point-max))
             (let ((sp (point))
                   (ep (progn (search-backward "/")
                              (forward-char)
                              (point))))
               (if (not (= sp ep))
                   (kill-region sp ep))))
           ;;(define-key (cdr ido-minor-mode-map-entry) [remap write-file] nil)                  ;; Zap ido for write-file
           (define-key ido-common-completion-map (kbd "<M-backspace>") #'MJR-wack-back-to-slash) ;; Bind MJR-wack-back-to-slash
           (define-key ido-common-completion-map (kbd "<M-DEL>")       #'MJR-wack-back-to-slash)
           )
    (MJR-quiet-message "MJR: INIT: PKG SETUP: ido: WARNING: Could not load ido package"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: PKG SETUP: package...")
(if (require 'package nil :noerror)
    (progn
      (and 't  (add-to-list 'package-archives '("melpa"        . "http://melpa.org/packages/")        t))
      (and nil (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t))
      (package-initialize))
    (MJR-quiet-message "MJR: INIT: PKG SETUP: package: WARNING: Could not load package package"))

;; * To refresh package contents: M-x package-refresh-contents
;; * To install a package:        M-x package-install
;; * To list packages:            M-x package-list-packages
;; * The only thing I currently add is magit.  This is how you install it without the UI:
;;      (progn (package-refresh-contents)
;;             (package-install "magit"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: PKG SETUP: sunrise-commander")
(let ((sunrise-commander-path (MJR-find-newest-core-package "sunrise-commander")))
  (if sunrise-commander-path
      (progn (MJR-quiet-message "MJR: INIT: PKG SETUP: sunrise-commander found... %s" sunrise-commander-path)
             (eval-after-load "sunrise-commander"
               '(progn (MJR-quiet-message "MJR: POST-INIT(%s): EVAL-AFTER: sunrise-commander!" (MJR-date "%Y-%m-%d_%H:%M:%S"))
                       ))
             (add-to-list 'load-path sunrise-commander-path)
             (autoload 'sunrise "sunrise-commander" "Sunrise Commander" t)
             (require 'sunrise-x-tree))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: PKG SETUP: hs-minor-mode")
(progn
  (eval-after-load "hideshow"
    '(progn (MJR-quiet-message "MJR: POST-INIT(%s): EVAL-AFTER: hs-minor-mode!" (MJR-date "%Y-%m-%d_%H:%M:%S"))
            (add-hook 'c-mode-common-hook   'hs-minor-mode)
            (add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
            (add-hook 'fortran-mode-hook    'hs-minor-mode)
            (add-hook 'java-mode-hook       'hs-minor-mode)
            (add-hook 'lisp-mode-hook       'hs-minor-mode)
            (add-hook 'perl-mode-hook       'hs-minor-mode)
            (defun MJR-hs-toggle ()
              "Prefix arg determines action: No prefix => toggle hide for current block; otherwise, hide everything if arg is 4, else show everything."
              (interactive)
              (if current-prefix-arg
                  (if (= (prefix-numeric-value current-prefix-arg) 4)
                      (hs-hide-all)
                      (hs-show-all))
                  (hs-toggle-hiding)))
            (global-set-key (kbd "C-c h") 'MJR-hs-toggle)))
  (require 'hideshow))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: PKG SETUP: yasnippet")
(let ((yasnippet-snip-path (remove-if-not #'file-exists-p (list (concat MJR-home-cor   "/yasnippets")
                                                                (concat MJR-home-cor   "/yasnippet")))))
  (if yasnippet-snip-path
      (progn
        (MJR-quiet-message "MJR: INIT: PKG SETUP: yasnippet snippet directory found... %s" yasnippet-snip-path)
        (eval-after-load "yasnippet"
          '(progn (MJR-quiet-message "MJR: POST-INIT(%s): EVAL-AFTER: yasnippet!" (MJR-date "%Y-%m-%d_%H:%M:%S"))
                  (setq yas-snippet-dirs yasnippet-snip-path)
                  ;; Nix the default tab binding..
                  (define-key yas-minor-mode-map (kbd "<tab>") nil)
                  (define-key yas-minor-mode-map (kbd "TAB")   nil)
                  ;; Add in-key-map binding for my own expand
                  (define-key yas-minor-mode-map (kbd "ESC ESC TAB") 'MJR-expand)
                  ;; Nix yas-fallback behaviour so C-c m won't complain when expand fails
                  (setq yas-fallback-behavior nil)
                  ;; How we get some global templates
                  (add-hook 'yas-minor-mode-hook
                            (lambda ()
                              (MJR-quiet-message "MJR: POST-INIT(%s): HOOK: yas-minor-mode-hook" (MJR-date "%Y-%m-%d_%H:%M:%S"))
                              (yas-activate-extra-mode 'fundamental-mode)))
                  ;; Make yas work everyplace
                  (yas-global-mode 1)
                  ;; Call yas-expand or yas-insert-snippet depending on if region is active
                  (defun MJR-expand ()
                    "If no region is active, use yas-expand to attempt yasnippet expantion; otherwise call yas-insert-snippet."
                    (interactive)
                    ;; (if (not (find 'fundamental-mode minor-mode-list))
                    ;;     (yas-activate-extra-mode 'fundamental-mode))
                    (if (and transient-mark-mode (region-active-p))
                        (funcall-interactively #'yas-insert-snippet)
                        (if (not (funcall-interactively #'yas-expand))
                            (funcall-interactively #'yas-insert-snippet))))
                  (global-set-key (kbd "C-c m") 'MJR-expand)))
        (require 'yasnippet))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: PKG SETUP: ESS")
(eval-after-load "ess-site"
  '(progn (MJR-quiet-message "MJR: POST-INIT(%s): EVAL-AFTER: ess!" (MJR-date "%Y-%m-%d_%H:%M:%S"))
          (setq ess-fancy-comments nil)
          (setq ess-handy-commands '(("set-width"        . ess-execute-screen-options)
                                     ("rdired"           . ess-rdired)
                                     ("change-directory" . ess-change-directory)
                                     ("help-apropos"     . ess-display-help-apropos)
                                     ("help-index"       . ess-display-package-index)
                                     ("help-object"      . ess-display-help-on-object)
                                     ("search"           . ess-execute-search)
                                     ("vignettes"        . ess-display-vignettes)))
          (progn (ess-toggle-underscore 't)
                 (ess-toggle-underscore nil))
          (let ((found-r (find-if #'file-exists-p (list "c:/Program Files/Microsoft/R Open/R-3.4.4/bin/x64/Rterm.exe"
                                                        "c:/Program Files/Microsoft/R Open/R-3.4.2/bin/x64/Rterm.exe"
                                                        "c:/Program Files/Microsoft/R Open/R-3.4.1/bin/x64/Rterm.exe"
                                                        "c:/Program Files/Microsoft/R Open/R-3.4.0/bin/x64/Rterm.exe"))))
            (if found-r
                (setq inferior-R-program-name found-r)))
          (add-to-list 'ess-style-alist
                       '(mjr-ess-style
                         (ess-indent-level . 2)                       ;; * (ess-indent-level . 4)
                         (ess-first-continued-statement-offset . 2)   ;; * (ess-first-continued-statement-offset . 0)
                         (ess-continued-statement-offset . 2)         ;; * (ess-continued-statement-offset . 4)
                         (ess-brace-offset . -2)                      ;; * (ess-brace-offset .  0)
                         (ess-expression-offset . 2)                  ;; * (ess-expression-offset . 4)
                         (ess-else-offset . 0)                        ;; = (ess-else-offset . 0)
                         (ess-close-brace-offset . 0)                 ;; = (ess-close-brace-offset . 0))
                         (ess-brace-imaginary-offset . 0)             ;; ?
                         (ess-continued-brace-offset . 0)             ;; ?
                         (ess-arg-function-offset . nil)              ;; * (ess-arg-function-offset . 4)
                         (ess-arg-function-offset-new-line . nil)     ;; * (ess-arg-function-offset-new-line '(4))
                         ))
          (setq ess-default-style 'mjr-ess-style)
          (add-hook 'ess-mode-hook
                    (lambda ()
                      (MJR-quiet-message "MJR: POST-INIT(%s): HOOK: ess-mode-hook" (MJR-date "%Y-%m-%d_%H:%M:%S"))
                      (progn (ess-toggle-underscore 't)
                             (ess-toggle-underscore nil))
                      (ess-set-style 'mjr-ess-style)))
          (add-hook 'inferior-ess-mode-hook
                    (lambda ()
                      (MJR-quiet-message "MJR: POST-INIT(%s): HOOK: inferior-ess-mode-hook" (MJR-date "%Y-%m-%d_%H:%M:%S"))
                                        ;(local-set-key (kbd "C-p") 'comint-previous-input) ;; Don't need.  Set in comint-mode-hook
                                        ;(local-set-key (kbd "C-n") 'comint-next-input)      ;; Don't need.  Set in comint-mode-hook
                      (progn (ess-toggle-underscore 't)
                             (ess-toggle-underscore nil))
                      (ess-set-style 'mjr-ess-style)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: PKG SETUP: comint")
(eval-after-load "comint"
  '(progn (MJR-quiet-message "MJR: POST-INIT(%s): EVAL-AFTER: comint!" (MJR-date "%Y-%m-%d_%H:%M:%S"))
          (add-hook 'comint-mode-hook
                    (lambda ()
                      (MJR-quiet-message "MJR: POST-INIT(%s): HOOK: comint-mode-hook" (MJR-date "%Y-%m-%d_%H:%M:%S"))
                      (local-set-key (kbd "C-p") 'comint-previous-input)
                      (local-set-key (kbd "C-n") 'comint-next-input)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: PKG SETUP: dos-w32")
(eval-after-load "dos-w32"
  '(progn (MJR-quiet-message "MJR: POST-INIT(%s): EVAL-AFTER: dos-w32!" (MJR-date "%Y-%m-%d_%H:%M:%S"))
          ;; I use a bash shell not DOS.  On DOS the value should be "NUL", but for base I need to reset it back to "/dev/null"
          (setq null-device "/dev/null")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: STAGE: Theme....")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(require 'custom)
(defun MJR-try-theme ()
  (if MJR-pookie-mode
      nil
      (if (load-theme 'mjr-dark 't  )
          (MJR-quiet-message "MJR: INIT: THEME: Loaded mjr-dark!")
          (progn (MJR-quiet-message "MJR: INIT: THEME: Failed to load mjr-dark!  Trying manjo-dark")
                 (if (load-theme 'manjo-dark 't)
                     (MJR-quiet-message "MJR: INIT: THEME: manjo-dark!")
                     (MJR-quiet-message "MJR: INIT: THEME: Failed to load mjr-dark!"))))))
(MJR-try-theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: STAGE: Setup global aliases....")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defalias 'sipell                      'ispell)                         ;; typo optimization
(defalias 'sipell-comments-and-strings 'ispell-comments-and-strings)    ;; typo optimization
(defalias 'ispell-code                 'ispell-comments-and-strings)    ;; better name
(defalias 'sipell-code                 'ispell-comments-and-strings)    ;; better name + typo optimization
(defalias 'code-indent                 'indent-region)                  ;; better name

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: STAGE: Setup global keys....")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Git rid of the suspend
(global-set-key (kbd "C-z")       nil)
(global-set-key (kbd "C-x C-z")   nil)

;; Island keys
(global-set-key (kbd "C-<right>") 'forward-word)
(global-set-key (kbd "C-<left>")  'backward-word)
(global-set-key (kbd "M-<right>") 'forward-sexp)
(global-set-key (kbd "M-<left>")  'backward-sexp)
(global-set-key (kbd "<del>")     'delete-char)

;; Mouse left and right
(global-set-key (kbd "<mouse-6>") 'scroll-right)
(global-set-key (kbd "<mouse-7>") 'scroll-left)

;; filco emulation on mbp
(global-set-key (kbd "s-l")       'scroll-down-command) ; Super+l
(global-set-key (kbd "s-.")       'scroll-up-command)   ; Super+.

;; Override C-x C-b
(global-set-key (kbd "C-x C-b")   'buffer-menu)

;; Random key bindings
(global-set-key (kbd "C-x r a")   'append-to-register)
(global-set-key (kbd "ESC ESC y") '(lambda () (interactive) (popup-menu 'yank-menu)))
(global-set-key (kbd "ESC ESC g") 'goto-line)
(global-set-key (kbd "ESC ESC ;") 'MJR-quick-code-comment)
(global-set-key (kbd "ESC =")     'MJR-describe-region-or-char)
(cond ((require 'magit nil 't) (global-set-key (kbd "C-c g") 'magit-status))
      ((require 'it    nil 't) (global-set-key (kbd "C-c g") 'git-status))
      ('t                      ((MJR-quiet-message "MJR INIT: WARNING: Unable to bind C-c g to #'magit-status or #'git-status"))))
(global-set-key (kbd "C-c a")     'org-agenda)
(global-set-key (kbd "C-c b")     'browse-url)
(global-set-key (kbd "C-c c")     'compile)
(global-set-key (kbd "C-c d")     'MJR-date)
(global-set-key (kbd "C-c e")     'MJR-open-cwd)
(global-set-key (kbd "C-c f")     'ffap)
(global-set-key (kbd "C-c l")     'MJR-thingy-lookeruper)
(global-set-key (kbd "C-c s")     (lambda () (interactive) (eshell (or (and current-prefix-arg (prefix-numeric-value current-prefix-arg)) 0))))
(global-set-key (kbd "C-c t")     'MJR-term)
(global-set-key (kbd "C-c v")     'MJR-view-file-or-url-at-point)
;global-set-key (kbd "C-c m")     'MJR-expand        ;;; Set in mode code: yasnippet
;global-set-key (kbd "C-c s")     'slime-selector    ;;; Set in mode code: slime
;global-set-key (kbd "C-c h")     'MJR-hs-toggle     ;;; Set in mode code: hs-minor-mode


;; Not global, but the mini-buffer is kinda everyplace...
(define-key minibuffer-local-map (kbd "C-p") 'previous-history-element)
(define-key minibuffer-local-map (kbd "C-n") 'next-history-element)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: STAGE: Emacs Customization System....")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(customize-set-variable  'TeX-auto-untabify           t)
(customize-set-variable  'delete-selection-mode       t)
(customize-set-variable  'indicate-buffer-boundaries  'right)
(customize-set-variable  'indicate-empty-lines        t)
(customize-set-variable  'Man-notify-method           'pushy)
(customize-set-variable  'LaTeX-item-indent           0)
(customize-set-variable  'TeX-PDF-mode                t)
(customize-set-variable  'TeX-auto-save               t)
(customize-set-variable  'TeX-parse-self              t)
(customize-set-variable  'ansi-color-faces-vector     [default default default italic underline success warning error])
(customize-set-variable  'ansi-color-names-vector     ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
(customize-set-variable  'safe-local-variable-values  '((Syntax . ANSI-Common-LISP)))

(setq custom-file "~/.emacs.d/custom.el")
(if (file-exists-p custom-file)
    (load custom-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(MJR-quiet-message "MJR: INIT: STAGE: Done Customizing Emacs....")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (defun MJR-rev-line ()
;;  "Reverse the characters on the current line"
;;   (interactive)
;;   (beginning-of-line)
;;   (insert (reverse (delete-and-extract-region (line-beginning-position)
;;                                               (line-end-position)))))
