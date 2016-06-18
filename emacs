;; -*- Mode:emacs-lisp; Syntax:ANSI-Common-LISP; Coding:us-ascii-unix; fill-column:158 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; @file      emacs
;; @author    Mitch Richling <http://www.mitchr.me>
;; @brief     My Emacs dot file.@EOL
;; @std       Emacs Lisp
;; @copyright 
;;  @parblock
;;  Copyright (c) 1989-2015, Mitchell Jay Richling <http://www.mitchr.me> All rights reserved.
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
;; Stuff to play with later:
;;  * htmlfontify.el - (v23.2) 
;;  * Bubbles - like SameGame.
;;  * display-time-world
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "MJR: INIT: STAGE: Start Customizing Emacs....")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq gc-cons-threshold 50000000)
(add-hook 'emacs-startup-hook (lambda ()
                                (message "MJR: POST-INIT(%s): HOOK: emacs-startup-hook" (MJR-date "%Y-%m-%d_%H:%M:%S"))
                                (setq gc-cons-threshold 800000)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "MJR: INIT: STAGE: Manual-Meta-Config...")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set MJR-expert-mode, MJR-pookie-mode, & MJR-uname
(let ((urln (user-real-login-name)))
  (cond ((find urln '("richmit"
                      "a0864027"
                      "mjr") :test #'string=) (setq MJR-expert-mode 't
                                                    MJR-pookie-mode nil
                                                    MJR-uname       "richmit"))
        ((string= urln "jrichli")             (setq MJR-expert-mode nil
                                                    MJR-pookie-mode 't
                                                    MJR-uname       "jrichli"))
        ('t                                   (setq MJR-expert-mode 't
                                                    MJR-pookie-mode nil
                                                    MJR-uname urln))))

;; Set MJR-home
(setq MJR-home (or (find-if #'file-exists-p (mapcar (lambda (p) (concat p MJR-uname)) '("/Users/"
                                                                                        "/home/"
                                                                                        "/u/")))
                   (expand-file-name "~")))
;; Set MJR-home-bin, MJR-home-cor, & MJR-home-dot
(if MJR-home
    (progn
      (if (file-exists-p (concat MJR-home "/bin" )) (setq MJR-home-bin (concat MJR-home "/bin"))  (setq MJR-home-bin "/"))    ;; Where to look for scripts
      (if (file-exists-p (concat MJR-home "/core")) (setq MJR-home-cor (concat MJR-home "/core")) (setq MJR-home-cor "/"))   ;; Location for 'core' data
      (if (file-exists-p (concat MJR-home "/"    )) (setq MJR-home-dot (concat MJR-home "/"    )) (setq MJR-home-dot "/")))) ;; Location dot files 

(message "MJR: INIT: STAGE: Manual-Meta-Config: MJR-expert-mode: %s" MJR-expert-mode)
(message "MJR: INIT: STAGE: Manual-Meta-Config: MJR-pookie-mode: %s" MJR-pookie-mode)
(message "MJR: INIT: STAGE: Manual-Meta-Config: MJR-home:        %s" MJR-home)
(message "MJR: INIT: STAGE: Manual-Meta-Config: MJR-home-bin:    %s" MJR-home-bin)
(message "MJR: INIT: STAGE: Manual-Meta-Config: MJR-home-cor:    %s" MJR-home-cor)
(message "MJR: INIT: STAGE: Manual-Meta-Config: MJR-uname:       %s" MJR-uname)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "MJR: INIT: STAGE: Auto-Meta-Config...")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq MJR-location (cond ((or (file-exists-p "/apps/")
                              (file-exists-p "/apps/flames/data")
                              (file-exists-p "/home/flames/data"))    "TI")
                         ((file-exists-p "/home/Shared/core/")        "HOME")
                         ('t                                          "UNKNOWN")))

(setq MJR-platform (cond ((string-match "mingw-nt"  system-configuration) "WINDOWS")
                         ((string-match "linux"     system-configuration) "LINUX")
                         ((string-match "darwin"    system-configuration) "DARWIN")
                         ('t                                              "UNKNOWN")))

(message "MJR: INIT: STAGE: Auto-Meta-Config: LOCATION: %s" MJR-location)
(message "MJR: INIT: STAGE: Auto-Meta-Config: PLATFORM: %s" MJR-platform)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "MJR: INIT: STAGE: Require Section...")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)
(require 'compile)
(require 'paren)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "MJR: INIT: STAGE: Define MJR Functions..")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun MJR-insert-from-web (url)
  "Insert snippet from web."
  (interactive (list (read-string "URL: " "http://www.mitchr.me/")))  
;; MJR TODO NOTE <2016-06-02 16:49:11 CDT> MJR-insert-from-web: Make sure we have curl...
  (call-process-shell-command "curl" nil 't nil "-s" url))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun MJR-view-pdf-at-point ()
  "If point is on a pdf file name, then load it up with mjrpdfview"
  (interactive)
  (let ((fap (ffap-guess-file-name-at-point)))
    (if fap
        (if (string-match "\\.pdf$" fap)
            (if (file-exists-p fap)
                (start-process-shell-command "mjrpdfview" "mjrpdfview" (concat MJR-home-bin "/mjrpdfview")  fap)
                ;; We don't use "start-process" as we may need shell expansion for the file name
                (message "MJR: MJR-view-pdf-at-point: ERROR: File a name at point, but it did not exist in the filesystem: %s" fap))
            (message "MJR: MJR-view-pdf-at-point: ERROR: Found a file name at piont, but it was not a PDF: %s" fap))
        (message "MJR: MJR-view-pdf-at-point: ERROR: Cound not find a filename name at point!"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun MJR-follow-mode ()
  "Close all open windows in the current frame; create 2, 3, or 4 vertical windows with current buffer; and activate follow-mode.

Prefix arg =  1: 2 vertical windows  (default)
Prefix arg =  4: 3 vertical windows  (C-u)
Prefix arg = 16: 5 vertical windows  (C-u C-u)"
  (interactive)
  (let* ((npfx  (prefix-numeric-value current-prefix-arg))
         (cols  (cond ((>= npfx  16) 4)
                      ((>= npfx   4) 3)
                      ((>= npfx   1) 2))))
    (delete-other-windows)
    (dotimes (i (1- cols))
      (split-window-horizontally))
    (balance-windows)
    (follow-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (file-exists-p (concat MJR-home-bin "/browser"))
    (progn
      (message "MJR: INIT: STAGE: Define MJR Functions: MJR-dict: DEFINED!")
      (defun MJR-dict ()
        "Lookup current word via dictionary.reference.com in a browser window."
        (interactive)
        (save-restriction
          (let (cur-word-loc-start cur-word-loc-end cur-word old-loc)
            (setq old-loc (point))
            (skip-chars-backward "A-Za-z0?9")
            (setq cur-word-loc-start (point))
            (skip-chars-forward "A-Za-z0?9")
            (setq cur-word-loc-end (point))
            (setq cur-word (buffer-substring cur-word-loc-start cur-word-loc-end))
            (goto-char old-loc)
            (call-process-shell-command (concat MJR-home-bin
                                                "/browser -foreground 100 -new-window 'http://dictionary.reference.com/browse/" 
                                                cur-word
                                                "?s=t'"))))))
    (message "MJR: INIT: STAGE: Define MJR Functions: MJR-dict: NOT defined!  We could not find the browser command"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "MJR: INIT: STAGE: Define MJR Functions: MJR-unfill: DEFINED!")
(defun MJR-unfill ()
  "Unfill paragraph or region."
  (interactive "*")
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph nil (region-active-p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "MJR: INIT: STAGE: Define MJR Functions: MJR-date: DEFINED!")
(defun MJR-date (dformat)
  "Insert the date at the point when called interactively

If called interactively a date/time stamp is inserted at the point using a format determined by the prefix argument:

  |-----+---------------+----------------------+---------------+-----------------------------------------------|
  | ARG | KEYS          | DATE FORMAT          |               |                                               |
  |-----+---------------+----------------------+---------------+-----------------------------------------------|
  |  -5 | M-- 4 C-c C-d | %s                   | UNIX          | Seconds since 1970-01-01 00:00:00 Z           |
  |  -4 | M-- 4 C-c C-d | %Y-%m-%dT%H:%M#Z     | ISO 8601\secs | sitemap, XML, SLQ (#Z is +HH:MM or -HH:MM tz) |
  |  -3 | M-- 3 C-c C-d | %Y-%m-%d %H:%M #Z    |               |                                               |
  |  -2 | M-- 2 C-c C-d | %Y-%m-%d %H:%M       |               |                                               |
  |  -1 | M--   C-c C-d | %Y-%m-%d             |               |                                               |
  |   1 |       C-c C-d | %Y-%m-%d             |               |                                               |
  |   2 | M-2   C-c C-d | %Y-%m-%d %H:%M:%S    |               |                                               |
  |   3 | M-3   C-c C-d | %Y-%m-%d %H:%M:%S #Z |               |                                               |
  |   4 | C-u   C-c C-d | %Y-%m-%dT%H:%M:%S#Z  | ISO 8601      | sitemap, XML, SLQ (#Z is +HH:MM or -HH:MM tz) |
  |   5 | M-5   C-c C-d | %s                   | UNIX          | Seconds since 1970-01-01 00:00:00 Z           |
  |-----+---------------+----------------------+---------------+-----------------------------------------------|

When not called interactively, this function returns the time as a string.  The argument is a format string."
  (interactive (let ((npfx  (prefix-numeric-value current-prefix-arg)))
                 (cond ((= npfx  5) (list "%s"                   ))
                       ((= npfx  4) (list "%Y-%m-%dT%H:%M:%S#Z"  ))
                       ((= npfx  3) (list "%Y-%m-%d %H:%M:%S #Z" ))
                       ((= npfx  2) (list "%Y-%m-%d %H:%M:%S"    ))
                       ((= npfx  1) (list "%Y-%m-%d"             ))
                       ((= npfx -5) (list "%s"                   ))
                       ((= npfx -4) (list "%Y-%m-%dT%H:%M#Z"     ))
                       ((= npfx -3) (list "%Y-%m-%d %H:%M #Z"    ))
                       ((= npfx -2) (list "%Y-%m-%d %H:%M"       ))
                       ((= npfx -1) (list "%Y-%m-%d"             ))
                       ('t          (list nil)                   )))) 
  (if dformat
      (let* ((curtime   (if (plusp (prefix-numeric-value current-prefix-arg)) (current-time) (org-read-date 't 't)))
             (time-list (decode-time curtime))
             (tzo       (nth 8 time-list))
             (zhr       (/ tzo 3600))
             (zmn       (/ (- (abs tzo) (* (abs zhr) 3600)) 60))
             (dstrtmp1  (format-time-string dformat curtime))
             (dstrtmp2  (replace-regexp-in-string "#Z" (if (zerop zmn)
                                                           (format "%+02d" zhr)
                                                           (format "%+02d:%02d" zhr zmn)) dstrtmp1 't 't))
             (dstr      (replace-regexp-in-string "UTC" "Z" dstrtmp2 't 't)))
        (if (called-interactively-p) (insert dstr) dstr))
      (message "MJR: MJR-Date: ERROR: Invalid prefix argument")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "MJR: INIT: STAGE: Define MJR Functions: MJR-quick-code-comment: DEFINED!")
(defun MJR-quick-code-comment (lab-tag date-stamp-pref)
  "Add (dated if no prefix-arg) 'MJR' comments at point or around region.

The 'MJR' comments come in one of two forms:
  * Region: Two formatted comment lines are added on the line before and line after the current region.
    The format (Date is optional) for the two comments:
      * Start comment:  'MJR TYPE BEGIN ----------------------- DATE'
      * Ending comment: 'MJR TYPE END ------------------------- DATE'
  * Line: A formatted comment line is added after the current line (cursor placed ready for content)
    The CONTEXT will be the name of the current function or buffer name.
    The format will be one of the following four depending on availability of date and context information: 
     * 'MJR TYPE NOTE <DATE> CONTEXT: '
     * 'MJR TYPE NOTE <DATE>: '
     * 'MJR TYPE NOTE CONTEXT: '
     * 'MJR TYPE NOTE: '"
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
         (date-fmt     "%Y-%m-%d %H:%M:%S %Z")
         (date-stamp   (if (< date-stamp-pref 2) (MJR-date date-fmt)))
         (com-start    (if (boundp 'comment-start) (symbol-value 'comment-start) ""))
         (com-end      (if (boundp 'comment-end)   (symbol-value 'comment-end)   "")))
    (if (string-equal com-start ";")
        (setq com-start ";;"))
    (if line-mode
        (let ((context-name (or (add-log-current-defun) (buffer-name)))) ;; Did have: (buffer-file-name) 
          (end-of-line)
          (insert (concat "\n" com-start " MJR " lab-tag "NOTE" (if date-stamp " <") date-stamp
                          (if date-stamp ">") (if context-name " ") context-name ": " com-end))
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
(if (file-exists-p (concat MJR-home-cor "/codeBits/"))
    (progn
      (message "MJR: INIT: STAGE: Define MJR Functions: MJR-PrependHeader: DEFINED!")
      (defun MJR-PrependHeader (cat-str file-type)
        "Determine the buffer file type, and populate the buffer with an appropriate header and, if the buffer is empty, a template.

         Some text in the templates gets expanded: YYYY-MM-DD, YYYY, FILENAME, FILEPATHNAME

         Headers and templates are categorized into groups appropriate to different uses: MJR, TI, HWP."
        (interactive (let ((cat-str-v '("MJR" "TI" "HWP")))
                       (list (if (require 'ido nil :noerror)
                                 (ido-completing-read "Category: " cat-str-v)
                                 (read-string "Category: " "MJR" 'cat-str-v))
                             (read-string "Type: " "AUTO"))))
        (let ((cur-file-name (buffer-file-name)))
          (if cur-file-name
              (let ((file-type (if (string= file-type "AUTO")
                                   (cdr (find-if (lambda (re) (string-match (car re) cur-file-name))
                                                 (list (cons "^makefile$" "make")
                                                       (cons "^Doxyfile$" "doxyfile")
                                                       (cons ".*"         (file-name-extension cur-file-name)))))
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
                                                       (cons "FILENAME"     (file-name-nondirectory cur-file-name))
                                                       (cons "FILEPATHNAME" cur-file-name)))
                                  (setq top-string (replace-regexp-in-string (car cur-rpl) (cdr cur-rpl) top-string 't 't)))
                                (goto-char (point-min))
                                (insert top-string)
                                (if do-template
                                    (if  (file-readable-p tpl-file-name)
                                         (insert-file-contents tpl-file-name)
                                         (message "MJR: MJR-PrependHeader: ERROR: Could not find TEMPLATE file for this file type")))
                                (message "MJR: MJR-PrependHeader: INFO: Header prepended"))
                              (message "MJR: MJR-PrependHeader: ERROR: Found TOP file, but can not read it"))
                          (message "MJR: MJR-PrependHeader: ERROR: Could not find TOP file for this file type")))
                    (message "MJR: MJR-PrependHeader: ERROR: Could not figure out file type")))
              (message "MJR: MJR-PrependHeader: ERROR: Could not figure out the file name for buffer")))))
    (message "MJR: INIT: STAGE: Define MJR Functions: MJR-PrependHeader: NOT defined!  We could not find the browser command"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (file-exists-p (concat MJR-home-bin "/latexit.rb"))
    (progn
      (message "MJR: INIT: STAGE: Define MJR Functions: MJR-latexit: DEFINED!")
      (defun MJR-latexit()
        "This function runs latex on the highlighted region, and displays the result with xpdf.

        Use the latexit.rb script in my home directory -- handy to have inside of Emacs..."
        (interactive)
        (let* ((reg-min  (if (mark) (min (point) (mark)) (point-min)))
               (reg-max  (if (mark) (max (point) (mark)) (point-max))))
          (if (file-exists-p "~/bin/latexit.rb")
              (shell-command-on-region reg-min reg-max (concat MJR-home-bin "/latexit.rb -"))
              (message "MJR: MJR-latexit: ERROR: Could not find the latexit.rb command!")))))
    (message "MJR: INIT: STAGE: Define MJR Functions: MJR-latexit: NOT defined!  We could not find the latexit.rb command"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (string-equal MJR-location "TI")
    (progn
      (message "MJR: INIT: STAGE: Define MJR Functions: MJR-de: DEFINED!")
      (defun MJR-de (at-2-get pfx-arg srch-str)
        "Lookup the people in LDAP

         Lookup the contents of the current region or the current-word with 'de' and insert requested attributes at the point -- replace
         region with a prefix argument.  'de' is a TI tool used on UNIX systems to lookup stuff in TI's LDAP servers.  It can query on
         various items including aid, xid, and real name.  Comma characters are removed from data returned, and if multiple elements are
         found they delimited commas.  The inserted string will have at least one leading and trailing space.

         Note that this uses quite a bit of shell magic and not regex replacement in Emacs.  This is because older Emacs versions have no
         such functions, and newer versions GNU Emacs and XEmacs have different (incompatible) functions for the same thing.  So, more
         shell and less Emacs is the hack of the day..."
        (interactive (let ((com-attrs  '("commonName"               ;; Common name
                                         "telephoneNumber"          ;; Telephone number
                                         "mail"                     ;; Email address
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
                       (list (read-string "Attribute: " "mail"  'com-attrs)
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
    (message "MJR: INIT: STAGE: Define MJR Functions: MJR-de: ERROR: Not defined!  Not at TI"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "MJR: INIT: STAGE: Define MJR Functions: MJR-stats-numbers-in-column: DEFINED!")
(defun MJR-stats-numbers-in-column ()
  "Compute various statistics on the the numbers highlighted by a rectangle, and put a summary in the kill ring."
  (interactive)
  (cl-flet ((MJR-get-numbers-in-column () (let* 
                                              ((saved-point (point))
                                               (saved-mark  (if (mark) (mark) saved-point))
                                               (min-col     (min (progn (goto-char saved-point) (current-column)) (progn (goto-char saved-mark) (current-column))))
                                               (min-line    (min (line-number-at-pos saved-point) (line-number-at-pos saved-mark)))
                                               (max-line    (max (line-number-at-pos saved-point) (line-number-at-pos saved-mark)))
                                               (list-o-numb nil))
                                            (loop for cur-line from min-line upto max-line do
                                                  (let*
                                                      ((min-pt (progn (goto-line cur-line) (move-to-column min-col) (point)))
                                                       (max-pt (point-at-eol)))
                                                    (setq list-o-numb (append list-o-numb (list (string-to-number (buffer-substring min-pt max-pt)))))))
                                            (kill-new (format "%s" list-o-numb))
                                            list-o-numb))
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
                                                     (the-max   (if allfp (apply 'max the-flist)))
                                                     )
                                                (list the-min the-max the-sum the-sumsq the-mean the-sd the-n)))))  
    (let ((the-sumary (apply 'format 
                             "STATS: min: %s, max: %s, sum: %s, sumsq: %s, mean: %s, sd: %s, n: %s" 
                             (MJR-stats-cmp (MJR-get-numbers-in-column)))))
      (kill-new the-sumary)
      (message the-sumary))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "MJR: INIT: STAGE: Generic Global Emacs Config Stuff...")
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
;; Replace highlighted region with new text or delete (like windows)
(delete-selection-mode t)
;; Make mouse select, clipboard, and yank buffer sane
(setq select-active-regions   'only)
(setq x-select-enable-primary 't)
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
;; Get rid of mail indicator in mode line
(setq display-time-mail-string "")
;; Put file size in mode line
(size-indication-mode)
;; Put column number in mode line
(column-number-mode t)
;; Put line number in mode line
(line-number-mode t)
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
                                        ;Global font lock mode
(global-font-lock-mode 1)
                                        ; Set the mark ring size
(setq mark-ring-max 64)
;; Only split vertically
(setq split-width-threshold nil)
;; Set up a shell for emacs shell mode to use.
(setq explicit-shell-file-name "/bin/bash")
;; Set the browser approprately
(if (file-exists-p (concat MJR-home-bin "/browser"))
    (setq browse-url-firefox-program (concat MJR-home-bin "/browser")))

;; Setup various handy auto-mode-alist items
(add-to-list 'auto-mode-alist '("\\.sql.m4$"                . sql-mode))          ;; SQL with m4
(add-to-list 'auto-mode-alist '("\\.txt.m4$"                . sql-mode))          ;; Text with m4
(add-to-list 'auto-mode-alist '("\\.elisp$"                 . emacs-lisp-mode))   ;; Emacs lisp code
(add-to-list 'auto-mode-alist '("\\.clisp$"                 . lisp-mode))         ;; SLIME-lisp-mode
(add-to-list 'auto-mode-alist '("\\.[fF]95$"                . f90-mode))          ;; Use f90 mode with fortran 1995
(add-to-list 'auto-mode-alist '("\\.[fF]0[38]$"             . f90-mode))          ;; Use f90 mode with fortran 2003 and 2008
(add-to-list 'auto-mode-alist '("\\.[mM][oO][dD]$"          . f90-mode))          ;; Use f90 mode with fortran modules
(add-to-list 'auto-mode-alist '("\\.[fF]200[38]$"           . f90-mode))          ;; Use f90 mode with fortran 2003 and 2008
(add-to-list 'auto-mode-alist '("\\.[fF]77$"                . fortran-mode))      ;; Use fortran mode for f77
(add-to-list 'auto-mode-alist '("\\.[fF][oO][rR]$"          . fortran-mode))      ;; Use fortran mode for f77
(add-to-list 'auto-mode-alist '("emacs--SS-X-X-X-X$"        . emacs-lisp-mode))   ;; My GNU Emacs dot file. :)
(add-to-list 'auto-mode-alist '("^/tmp/pico\\.[0-9][0-9]*$" . mail-mode))         ;; alpine tmp files -- use mail-mode
(add-to-list 'auto-mode-alist '("tmp/mutt/\\.*mutt"         . mail-mode))         ;; mutt tmp files -- use mail-mode
(add-to-list 'auto-mode-alist '("\\.R$"                     . R-mode))            ;; Use R mode

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "MJR: INIT: STAGE: Built-in Mode Config...")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "MJR: INIT: PKG SETUP: vc")
;; Use ediff for = binding
(eval-after-load "vc-hooks"
  '(progn (message "MJR: POST-INIT(%s): EVAL-AFTER: vc-hooks" (MJR-date "%Y-%m-%d_%H:%M:%S"))
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
(message "MJR: INIT: PKG SETUP: time")
(if (require 'time nil :noerror)
    (progn
      ;; Time zone list for display-time-world
      (require 'time)
      ;; Put time and date in mode line
      (setq display-time-day-and-date 't)
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
    (message "MJR: INIT: PKG SETUP: time: WARNING: Could not load package"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "MJR: INIT: PKG SETUP: server")
(eval-after-load "server"
  '(progn (message "MJR: POST-INIT(%s): EVAL-AFTER: server" (MJR-date "%Y-%m-%d_%H:%M:%S"))
          ;; Set a default name for the server each emacs instance will create
          (setq server-name (format "mjr-emacs-server-%d" (emacs-pid)))))
(autoload 'server-running-p "server" "Test whether server NAME is running." t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "MJR: INIT: PKG SETUP: speedbar")
(eval-after-load "speedbar"
  '(progn (message "MJR: POST-INIT(%s): EVAL-AFTER: speedbar!" (MJR-date "%Y-%m-%d_%H:%M:%S"))
          (dolist (ext '(".lisp" ".clisp" ".rb" ".f90" ".sql" ".f95" ".f08" ".f03" ".f2008"
                         ".f2003" ".F95" ".F08" ".F03" ".F2008" ".F2003" ".for" ".FOR" ".ps" "m2" "maple" "bash" "sh" ))
            (speedbar-add-supported-extension ext))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "MJR: INIT: PKG SETUP: linum")
(if (not MJR-pookie-mode)
    (if (require 'linum nil :noerror)
        (progn (global-linum-mode -1)
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
                               (message "MJR: POST-INIT(%s): HOOK: +linum-mode" (MJR-date "%Y-%m-%d_%H:%M:%S"))
                               (linum-mode 1)))))
        (message "MJR: INIT: PKG SETUP: linum: WARNING: Could not load package"))
    (message "MJR: INIT: PKG SETUP: linum: WARNING: SKIP: pookie mode!"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "MJR: INIT: PKG SETUP: dired")
(eval-after-load "dired"
  '(progn (message "MJR: POST-INIT(%s): EVAL-AFTER: dired!" (MJR-date "%Y-%m-%d_%H:%M:%S"))
          (add-hook 'dired-mode-hook
                    (lambda ()
                      (message "MJR: POST-INIT(%s): HOOK: diredc-mode-hook" (MJR-date "%Y-%m-%d_%H:%M:%S"))
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
(message "MJR: INIT: PKG SETUP: git")
(let ((git-el-files (list (concat MJR-home-cor "/elisp/git/git.el")
                          (concat MJR-home-cor "/elisp/git/git-blame.el"))))
  (if (every #'file-exists-p git-el-files)
      (dolist (git-el-file git-el-files)
        (load git-el-file))
      (message "MJR: INIT: PKG SETUP: git: WARNING: Could not find custom git package")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "MJR: INIT: PKG SETUP: sh-mode")
(eval-after-load "sh-script"
  '(progn (message "MJR: POST-INIT(%s): EVAL-AFTER: sh-script!" (MJR-date "%Y-%m-%d_%H:%M:%S"))
          (add-hook 'sh-mode-hook (lambda ()
                                    (message "MJR: POST-INIT(%s): HOOK: sh-mode-hook" (MJR-date "%Y-%m-%d_%H:%M:%S"))
                                    (setq sh-basic-offset 2
                                          sh-indentation 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "MJR: INIT: PKG SETUP: eshell")
(eval-after-load "esh-mode"
  '(progn (message "MJR: POST-INIT(%s): EVAL-AFTER: esh-mode!" (MJR-date "%Y-%m-%d_%H:%M:%S"))
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
          (defun eshell/egit (&rest args)
            (apply 'eshell-exec-visual (cons "git" args)))
          (setq eshell-cmpl-cycle-completions nil)
          (setq eshell-history-size 1048576)
          (add-hook 'eshell-mode-hook
                    (function (lambda ()
                                (message "MJR: POST-INIT(%s): HOOK: eshell-mode-hook" (MJR-date "%Y-%m-%d_%H:%M:%S"))
                                (MJR-try-theme)  ;; Duno why, but eshell needs to have the theme reapplied after it starts...
                                (setq pcomplete-cycle-completions nil)
                                (local-set-key "\M-."  'MJR-eshell-insert-last-word)
                                (if (not (server-running-p))
                                    (server-start))
                                (setenv "PAGER" "cat")
                                (setenv "EDITOR" (format "emacsclient -s %s" server-name)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun MJR-term (PROGRAM-TO-RUN NAME-OF-BUFFER)
  "Fire off an ansi-term with a nice, default buffer name."
  (interactive (let* ((daProg (if (require 'ido nil :noerror)
                                  (ido-completing-read "Program to run: " '("sn" "bash"))
                                  (read-string "Program to run: " "bash")))
                      (daName (read-string "Buffer name: " (concat "TERM:" (let ((cns (file-name-nondirectory daProg)))
                                                                             (cond ((string-equal cns "sn")   "screen")
                                                                                   ((string-equal cns "s")    "screen")
                                                                                   ((string-equal cns "bash") "shell")
                                                                                   ((string-equal cns "ksh")  "shell")
                                                                                   ((string-equal cns "ash")  "shell")
                                                                                   ((string-equal cns "sh")   "shell")
                                                                                   ((string-equal cns "csh")  "shell")
                                                                                   ((string-equal cns "tcsh") "shell")
                                                                                   ('t                        cns)))))))
                 (list daProg daName)))
  (if (not (server-running-p))
      (server-start))
  (setenv "EDITOR" (format "emacsclient -s %s" server-name))
  (ansi-term PROGRAM-TO-RUN NAME-OF-BUFFER))

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
(message "MJR: INIT: PKG SETUP: mail-mode setup...")
(eval-after-load "sendmail"
  '(progn (message "MJR: POST-INIT(%s): EVAL-AFTER: mail-mode!" (MJR-date "%Y-%m-%d_%H:%M:%S"))
          (setq compose-mail-user-agent-warnings nil)
          (add-hook 'mail-mode-hook
                    (lambda ()
                      (message "MJR: POST-INIT(%s): HOOK: mail-mode-hook(1)" (MJR-date "%Y-%m-%d_%H:%M:%S"))
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
                      (message "MJR: POST-INIT(%s): HOOK: mail-mode-hook (2)" (MJR-date "%Y-%m-%d_%H:%M:%S"))
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
(message "MJR: INIT: PKG SETUP: org-mode setup...")
(if (not (string-equal MJR-platform "WINDOWS"))
    (progn
      (require 'org-install)
      ;;(require 'org-habit)

      (if (not (require 'htmlize nil :noerror))
          (message "MJR: INIT: PKG SETUP: htmlize: WARNING: Could not load package in init."))

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
         (sh          . t)
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
      ;; (require 'ob-sh)
      ;; (require 'ob-sql)
      ;; (require 'ob-sqlite)

      (setq org-html-head-extra "<style>pre {background-color: #0f0f0f; color: #f0f0f0;}</style>") ;; Dark background for code.
      (setq org-html-postamble "Created with %c")
      
      (setq org-src-fontify-natively t)                         ;; Prety colors
      (setq org-src-tab-acts-natively t)                        ;; tab as in source mode
      (add-hook 'org-mode-hook 'turn-on-font-lock)              ;; Make sure we turn on font-lock
      (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))  ;; Make sure *.org files use org-mode
      (setq org-log-done 'time)                                 ;; Log timestamps on done TODO items
      (setq org-indent-mode t)                                  ;; Indent stuff
      (setq org-startup-folded nil)                             ;; Start with un-folded view
      (setq org-export-with-sub-superscripts nil)               ;; "_" and "^" are not special
      (setq org-confirm-babel-evaluate 't)                      ;; Ask about evals
      (setq org-export-babel-evaluate nil)                      ;; Do NOT eval on export
      (setq org-log-into-drawer "LOGBOOK")                      ;; Put TODO changes and notes in LOGBOOK drawer
      (setq org-agenda-files (list "~/TODO.org"))               ;; My generic TODO file
      (setq org-babel-min-lines-for-block-output 0)             ;; Always put babel results in blocks

      (add-hook 'org-mode-hook                                  ;; Get my favorite keys back
                (lambda ()
                  (message "MJR: POST-INIT(%s): HOOK: org-mode-hook" (MJR-date "%Y-%m-%d_%H:%M:%S"))
                  ;;(local-set-key (kbd "C-c C-d")   'MJR-date)
                  (local-set-key  (kbd "C-c a")      'org-agenda)
                  ))
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
                          (message "MJR: POST-INIT(%s): HOOK: +orgtbl-mode" (MJR-date "%Y-%m-%d_%H:%M:%S"))
                          (turn-on-orgtbl))))))
    (message "MJR: INIT: PKG SETUP: org-mode setup... ABORT! (no org-mode on windows)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "MJR: INIT: PKG SETUP: calender (solar stuff)")
(eval-after-load "solar"
  '(progn (message "MJR: POST-INIT(%s): EVAL-AFTER: solar!" (MJR-date "%Y-%m-%d_%H:%M:%S"))
          (if (version<= "23.1" emacs-version) (calendar-set-date-style 'iso))
          (setq calendar-location-name "Dallas, TX")
          (setq calendar-latitude 32.00)
          (setq calendar-longitude -96.00)))
(eval-after-load "cal-dst"
  '(progn (message "MJR: POST-INIT(%s): EVAL-AFTER: cal-dst!" (MJR-date "%Y-%m-%d_%H:%M:%S"))
          (setq calendar-time-zone -360)
          (setq calendar-standard-time-zone-name "CST")
          (setq calendar-daylight-time-zone-name "CDT")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "MJR: INIT: PKG SETUP: root-help setup...")
(if (file-exists-p "/usr/share/emacs/site-lisp/root-help.el")
    (progn (autoload 'root-shell "root-help" "Run ROOT (the C++ Data Analysis Framework) shell" t)
           ;; Put any pre-load root config stuff here...
           (eval-after-load "root-help"
             '(progn (message "MJR: POST-INIT(%s): EVAL-AFTER: root-help!" (MJR-date "%Y-%m-%d_%H:%M:%S"))
                     ;; Put any post-load root config stuff here...
                     ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "MJR: INIT: PKG SETUP: gdb-mode (GUD) setup...")
(add-hook 'gdb-mode-hook
          (function (lambda ()
                      (message "MJR: POST-INIT(%s): HOOK: gdb-mode-hook" (MJR-date "%Y-%m-%d_%H:%M:%S"))
                      (setq gdb-many-windows           't)   ;; Use lots of windows for gdb
                      (setq gdb-use-separate-io-buffer nil)  ;; No I/O window
                      (tool-bar-mode 1)                      ;; Start up the tool-bar when we start up gdb
                      (add-hook 'kill-buffer-hook            ;; Set a kill-buffer-hook to get rid of the tool-bar when we leave.
                                (function (lambda ()
                                            (message "MJR: POST-INIT(%s): HOOK: kill-buffer-hook" (MJR-date "%Y-%m-%d_%H:%M:%S"))
                                            (tool-bar-mode -1)))
                                't 't))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "MJR: INIT: PKG SETUP: tramp-mode setup...")
(if (require 'tramp nil :noerror)
    (progn (setq tramp-default-user   MJR-uname
                 tramp-default-method "sshx")
           ;;                                         host              user         method
           (add-to-list 'tramp-default-method-alist '("\\`localhost\\'"       "\\`root\\'" "sudo"))
           ;;                                     FROM RE         TO VALUE
           (add-to-list 'directory-abbrev-alist '("^/rut"       . "/root@localhost:/"))
           (if (file-exists-p (concat MJR-home-dot "/.tramp.el"))
               (load-file (concat MJR-home-dot "/.tramp.el"))))
    (message "MJR: INIT: PKG SETUP: tramp: WARNING: Could not load package"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "MJR: INIT: PKG SETUP: aspell setup...")
(let ((aspell-path (find-if #'file-exists-p
                            (list "/usr/bin/hunspell"
                                  "/bin/hunspell"
                                  "/usr/local/bin/aspell"
                                  "/opt/local/bin/aspell"
                                  "/bin/aspell"
                                  "/usr/bin/aspell"))))
  (if aspell-path
      (setq-default ispell-program-name aspell-path)
      (setq-default ispell-program-name "ispell")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "MJR: INIT: PKG SETUP: emacs-lisp-mode setup...")
;; Don't indent the third argument if the if form differently...
(put 'if 'lisp-indent-function nil)
(add-hook 'emacs-lisp-mode-hook
          (function (lambda ()
                      (message "MJR: POST-INIT(%s): HOOK: emacs-lisp-mode-hook" (MJR-date "%Y-%m-%d_%H:%M:%S"))
                      (local-set-key "\C-c\C-c" 'byte-recompile-directory)
                      (local-set-key "\C-c\C-b" 'byte-compile-file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "MJR: INIT: PKG SETUP: c-mode setup...")
(add-hook 'c-mode-common-hook
          (function (lambda ()
                      (message "MJR: POST-INIT(%s): HOOK: c-mode-common-hook" (MJR-date "%Y-%m-%d_%H:%M:%S"))
                      (setq c-inhibit-startup-warnings-p   t)
                      (setq c-echo-syntactic-information-p nil)
                      (setq c-progress-interval            1)
                      (c-add-style "MJR"
                                   `("k&r"
                                     (c-doc-comment-style . 'javadoc)
                                     (c-tab-always-indent . t)
                                     (c-recognize-knr-p . nil)
                                     (c-basic-offset . 2)
                                     (c-comment-only-line-offset . 0)
                                     (c-offsets-alist
                                      (inclass . ++)
                                      (access-label . -))))
                      (c-set-style "MJR")
                      (local-set-key (kbd "C-c C-d")   'MJR-date)  ;; This one is wacked, restore it
                      (local-set-key "\C-c\C-c" 'compile))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "MJR: INIT: PKG SETUP: fortran-mode setup...")
(add-hook 'fortran-mode-hook
          (function (lambda ()
                      (message "MJR: POST-INIT(%s): HOOK: fortran-mode-hook" (MJR-date "%Y-%m-%d_%H:%M:%S"))
                      (setq comment-start               "c")
                      (setq fortran-comment-indent-char " ")
                      (setq fortran-blink-matching-if   t)
                      (setq fortran-comment-region      "c     ")
                      (local-set-key (kbd "C-c C-d")   'MJR-date)))) ;; This one is wacked, restore it

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "MJR: INIT: PKG SETUP: perl-mode setup...")
(add-hook 'perl-mode-hook
          (function (lambda ()
                      (message "MJR: POST-INIT(%s): HOOK: perl-mode-hook" (MJR-date "%Y-%m-%d_%H:%M:%S"))
                      (setq perl-indent-level 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "MJR: INIT: PKG SETUP: auctex setup...")
(let ((atex-path (find-if (lambda (p) (file-exists-p (concat p "/auctex.el")))
                          (list "/usr/local/share/emacs/site-lisp/"
                                (concat MJR-home-cor "/elisp/auctex/share/site-lisp/")))))
  (if atex-path
      (progn (add-to-list 'load-path atex-path)
             (load "auctex.el" nil t t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "MJR: INIT: PKG SETUP: PoV-mode setup...")
(let ((pov-path (find-if #'file-exists-p 
                         (list (concat MJR-home-cor "/elisp/pov-mode")))))
  (if pov-path
      (progn
        (add-to-list 'load-path pov-path)
        (autoload 'pov-mode "pov-mode" "PoVray scene file mode" t)
        (add-to-list 'auto-mode-alist '("\\.pov\\'" . pov-mode))
        (add-to-list 'auto-mode-alist '("\\.inc\\'" . pov-mode)))
      (message "MJR: INIT: PKG SETUP: PoV-mode not found...")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "MJR: INIT: PKG SETUP: latex-mode setup...")
(add-hook 'latex-mode-hook
          (function (lambda ()
                      (message "MJR: POST-INIT(%s): HOOK: latex-mode-hook" (MJR-date "%Y-%m-%d_%H:%M:%S"))
                      (setq tex-font-script-display 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "MJR: INIT: PKG SETUP: Macaulay 2 setup...")
(let ((mac-path (find-if #'file-exists-p
                         (list "/usr/local/big/Macaulay2/1.7/share/emacs/site-lisp"
                               "/Applications/Macaulay2-1.2/share/emacs/site-lisp/"
                               "/Applications/Macaulay2-1.1/share/emacs/site-lisp/"
                               "/usr/local/share/emacs/site-lisp"
                               "/usr/share/emacs/site-lisp"))))
  (if (file-exists-p (concat mac-path "/M2-init.el"))
      (progn (add-to-list 'load-path mac-path)
             (load "M2-init" t)) ;; M2-init is a list of autoloads
      (message "MJR: INIT: PKG SETUP: Macaulay 2 not found...")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "MJR: INIT: PKG SETUP: Maxima setup...")
(let ((max-path (find-if (lambda (p) (file-exists-p (concat p "/maxima.el")))
                         (list "/usr/local/big/maxima/5.38.0_sbcl-1.3.4/share/maxima/5.36.0/emacs"  ;; Custom build on Debian 8
                               "/usr/local/big/maxima/5.36.0_sbcl-1.2.11/share/maxima/5.36.0/emacs" ;; Custom build on Debian 8
                               "/usr/local/big/maxima/5.37.0_sbcl-1.2.14/share/maxima/5.37.0/emacs" ;; Custom build on Debian 8
                               "~/s/linux/local/share/maxima/5.29.1/emacs"                          ;; Custom biuld on linux
                               "/opt/local/share/maxima/5.16.3/emacs"                               ;; Typical MacOS X with macports
                               "/usr/share/maxima/5.34.1/emacs/"                                    ;; Standard location for Debian 8
                               "/usr/share/maxima/5.21.1/emacs"))))                                 ;; Standard location for Ubuntu 11.04
  (if max-path
      (progn (message "MJR: INIT: PKG SETUP: Specific version of maxima.el found...")
             (add-to-list 'load-path max-path)
             (autoload 'maxima "maxima" "Run Maxima in a window" t)
             (eval-after-load "maxima"
               '(progn (message "MJR: POST-INIT(%s): EVAL-AFTER: maxima!" (MJR-date "%Y-%m-%d_%H:%M:%S"))
                       (let ((max-cmd  (find-if #'file-exists-p 
                                                (list "/usr/local/big/maxima/5.38.0_sbcl-1.3.4/bin/maxima"  ;; Custom build on Debian 8 @ home
                                                      "/usr/local/big/maxima/5.37.0_sbcl-1.2.14/bin/maxima" ;; Custom build on Debian 8 @ home
                                                      "/usr/local/big/maxima/5.36.0_sbcl-1.2.11/bin/maxima" ;; Custom build on Debian 8 @ home
                                                      "~/s/linux/local/bin/maxima"                          ;; Custom biuld on linux @ TI
                                                      "/opt/local/bin/maxima"                               ;; Typical MacOS X with macports
                                                      "/usr/local/bin/maxima"                               ;; Typical place
                                                      "/usr/bin/maxima"))))                                 ;; Standard place for Debian & Ubuntu
                         (if max-cmd
                             (progn
                               (message "MJR: INIT: PKG SETUP: Specific Maxima binary found...")
                               (setq maxima-command (or max-cmd "maxima"))))
                         (cond ((file-exists-p "/usr/lib/maxima/5.21.1/binary-gcl")  (setq maxima-args "-l gcl")) ;; old format for ubuntu 10.xx
                               ((file-exists-p "/usr/lib/maxima/5.27.0/binary-gcl")  nil)
                               ('t                                                   (setq maxima-args '("-l" "sbcl"))))
                         (add-hook 'inferior-maxima-mode-hook
                                   (lambda ()
                                     (message "MJR: POST-INIT(%s): HOOK: inferior-maxima-mode-hook" (MJR-date "%Y-%m-%d_%H:%M:%S"))
                                     (font-lock-mode 1)
                                     (local-set-key (kbd "TAB") 'inferior-maxima-complete)))))))
      (message "MJR: INIT: PKG SETUP: Maxima not found...")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "MJR: INIT: PKG SETUP: bookmarks setup...")
(eval-after-load "bookmark"
  '(progn (message "MJR: POST-INIT(%s): EVAL-AFTER: bookmarks!" (MJR-date "%Y-%m-%d_%H:%M:%S"))
          (if (file-exists-p "~/world/stuff/notes/computer/") (add-to-list 'bookmark-alist '(cnotes (filename . "~/world/stuff/notes/computer/"))))
          (if (file-exists-p "/Users/Shared/Doc2/index.org")  (add-to-list 'bookmark-alist '(doc2   (filename . "/Users/Shared/Doc2/index.org"))))
          (if (file-exists-p "~/world/stuff/my_ref/")         (add-to-list 'bookmark-alist '(ref    (filename . "~/world/stuff/my_ref/"))))
          ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "MJR: INIT: PKG SETUP: iMaxima setup...")
(eval-after-load "imaxima"
  '(progn (message "MJR: POST-INIT(%s): EVAL-AFTER: imaxima!" (MJR-date "%Y-%m-%d_%H:%M:%S"))
          ;; The type size used in LaTeX. Options: 9, 10, 11, 12
          (setq imaxima-pt-size 9)
          ;; Default size of font. Options: "small", "normalsize", "large", "Large", "LARGE", "huge", "Huge"
          (setq imaxima-fnt-size "small")
          ;; Scale all images by this factor. Default: 1.0
          (setq imaxima-scale-factor 2.0)
          ;; Use maxima mode 
          (setq imaxima-use-maxima-mode-flag 't)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup GAP -- NOTE: Must remove comint.el from GAP mode distribution!!!
                                        ;(if (and (file-exists-p (concat MJR-home-cor "/elisp/gap"))
                                        ;         (file-exists-p "/usr/local/bin/gap"))
                                        ;    (progn
                                        ;      (add-to-list 'load-path (concat MJR-home-cor "/elisp/gap"))
                                        ;      (autoload 'gap-mode "gap-mode" "Gap editing mode" t)
                                        ;      (setq auto-mode-alist (append (list '("\\.gap$" . gap-mode))
                                        ;                                    auto-mode-alist))
                                        ;      (autoload 'gap "gap-process" "Run GAP in emacs buffer" t)
                                        ;      (setq gap-executable "/usr/local/bin/gap")
                                        ;      (setq gap-start-options (list "-b"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "MJR: INIT: PKG SETUP: OCTAVE setup...")
(if (and (file-exists-p (concat MJR-home-cor "/elisp/octave")))
    (progn (message "MJR: INIT: PKG SETUP: OCTAVE found...")
           (add-to-list 'load-path (concat MJR-home-cor "/elisp/octave"))
           (autoload 'octave-mode  "octave-mod" "Octave editing mode" t)
           (setq auto-mode-alist 
                 (cons '("\\.m$" . octave-mode) auto-mode-alist))
           (add-hook 'octave-mode-hook 
                     (lambda () 
                       (message "MJR: POST-INIT(%s): HOOK: octave-mode-hook" (MJR-date "%Y-%m-%d_%H:%M:%S"))
                       (abbrev-mode 1) 
                       (auto-fill-mode 1) 
                       (font-lock-mode 1)))
           (autoload 'run-octave   "octave-inf" "Interactive Octave" t)
           (add-hook 'inferior-octave-mode-hook
                     (lambda () 
                       (message "MJR: POST-INIT(%s): HOOK: inferior-octave-mode-hook" (MJR-date "%Y-%m-%d_%H:%M:%S"))
                       (font-lock-mode 1))))
    (message "MJR: INIT: PKG SETUP: OCTAVE Not Found..."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "MJR: INIT: PKG SETUP: procesing mode setup...")
(let ((processing-loc (find-if #'file-exists-p
                               (list (concat MJR-home-cor "/elisp/processing")))))
  (if processing-loc
      (progn (message "MJR: INIT: PKG SETUP: Processing Mode found in CORE.")
             (add-to-list 'load-path processing-loc)
             (add-to-list 'auto-mode-alist '("\\.pde$" . processing-mode))
             (autoload 'processing-mode "processing-mode" "Processing mode" t))
      (message "MJR: INIT: PKG SETUP: Processing Mode Not Found...")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "MJR: INIT: PKG SETUP: SLIME...")
(let ((slime-loc (find-if #'file-exists-p
                          (list (concat MJR-home-cor "/elisp/slime")
                                (concat MJR-home-cor "/elisp/slime-2011-05-19")))))
  (if slime-loc
      (progn (message "MJR: INIT: PKG SETUP: SLIME found...")
             (add-to-list 'load-path slime-loc)
             (require 'slime-autoloads)
             (eval-after-load "slime"
               '(progn (message "MJR: POST-INIT(%s): EVAL-AFTER: slime!" (MJR-date "%Y-%m-%d_%H:%M:%S"))
                       (let ((slime-lisp-bin (find-if #'file-exists-p
                                                      (list "/usr/local/big/sbcl/1.3.4/bin/sbcl"
                                                            "/usr/local/big/sbcl/1.2.14/bin/sbcl"
                                                            "/usr/local/big/sbcl/1.2.11/bin/sbcl"
                                                            "~/s/linux/local/bin/sbcl"
                                                            "/usr/local/bin/sbcl-run"
                                                            "/usr/local/bin/sbcl"
                                                            "/opt/local/bin/sbcl"
                                                            "/usr/bin/sbcl"
                                                            "C:\\PROGRA~1\\STEELB~1\\1.0.51\\SBCL.EXE")))
                             (spec-to-use (find-if #'file-exists-p
                                                   (list "/usr/share/doc/hyperspec/"
                                                         "/Users/Shared/Doc2/software-dev/LISP/hyperspec/"
                                                         "/opt/local/share/doc/lisp/HyperSpec-7-0/HyperSpec/"))))                    
                         (if slime-lisp-bin
                             (setq inferior-lisp-program slime-lisp-bin)
                             (message "MJR INIT: WARNING: No working lisp found"))
                         (if spec-to-use
                             (setq common-lisp-hyperspec-root (concat "file:" spec-to-use))
                             (message "MJR INIT: WARNING: Using remote hyperspec: %s"
                                      (setq common-lisp-hyperspec-root "http://www.lispworks.com/reference/HyperSpec/")))
                         (slime-setup '(slime-repl)) ; Setup (use SLIME-REPL)
                         (setq lisp-simple-loop-indentation  1
                               lisp-loop-keyword-indentation 6
                               lisp-loop-forms-indentation   6)
                         (setq slime-net-coding-system 'utf-8-unix)
                         (add-hook 'lisp-mode-hook
                                   (lambda ()
                                     (message "MJR: POST-INIT(%s): HOOK: lisp-mode-hook" (MJR-date "%Y-%m-%d_%H:%M:%S"))
                                     (setq slime-net-coding-system 'utf-8-unix)))
                         ;; REPL bindings
                         (define-key slime-repl-mode-map "\M-." 'slime-edit-definition-with-etags)           ;; A somewhat slimey version of find-tag
                         (define-key slime-repl-mode-map "\M-," 'tags-loop-continue)                         ;; Use  tags contineu for M,
                         (define-key slime-repl-mode-map (kbd "ESC ESC .") 'slime-edit-definition)           ;; Put SLIMEy M. on MM.
                         (define-key slime-repl-mode-map (kbd "ESC ESC ,") 'slime-pop-find-definition-stack) ;; Put SLIMEy M, on MM,
                         ;; CODE bindings
                         (define-key slime-mode-map "\M-." 'slime-edit-definition-with-etags)           ;; A somewhat slimey version of find-tag
                         (define-key slime-mode-map "\M-," 'tags-loop-continue)                         ;; Use  tags contineu for M,
                         (define-key slime-mode-map (kbd "ESC ESC .") 'slime-edit-definition)           ;; Put SLIMEy M. on MM.
                         (define-key slime-mode-map (kbd "ESC ESC ,") 'slime-pop-find-definition-stack) ;; Put SLIMEy M, on MM,
                         ;;(define-key slime-mode-map "\M-." 'find-tag)
                         ;; M-x slime-who-calls       Show function callers.
                         ;; M-x slime-who-references  Show references to global variable.
                         (global-set-key (kbd "C-c s")   'slime-selector) ; Switch back to slime REPL
                         (global-set-key (kbd "C-c C-s") 'slime-selector)))))
      (message "MJR: INIT: PKG SETUP: SLIME Not Found...")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "MJR: INIT: PKG SETUP: ido...")
(if (not MJR-pookie-mode)
    (if (require 'ido nil :noerror)
        (progn (setq ido-use-filename-at-point 'guess)
               (setq ido-use-url-at-point nil)
               ;; Set ignore directory list
               (dolist (directory-re '("\\`RCS/" "\\`auto/" "\\`\\.git/"))
                 (add-to-list 'ido-ignore-directories directory-re))
               ;; I can't get ido-ignore-extensions to work
               (dolist (ext '("fasl" "ufasl" "fas" "lib" "o" "dvi" "asd" "so"
                              "aux" "bbl" "bcf" "blg" "log" "out" "run.xml")) 
                 (add-to-list 'ido-ignore-files (concat "\\." ext "$"))) 
               ;; Set ignore file list
               (dolist (file-re '("\\`RCS/" "\\`\\.git/" "\\`\\.DS_Store" "\\`a\\.out"))
                 (add-to-list 'ido-ignore-files file-re))
               ;; Set ignore buffer list
               (dolist (buffer-re '("\\`\\*ESS\\*" "\\`\\*slime-events\\*" "\\`\\*Apropos\\*"
                                    "\\`\\*Completions\\*" "\\`\\*vc\\*" "\\`\\log-edit-files\\*"
                                    "\\`\\*slime-compilation\\*" "\\`\\*inferior-lisp\\*" "\\`\\*Warning\\*"))
                 (add-to-list 'ido-ignore-buffers buffer-re))
               ;; Order of matches based on extension -- for some reason this doesn't work
               (setq ido-file-extensions-order '(".org" ".lisp" ".R" ".rb" ".tex" ".txt" ".hpp" ".cpp" ".h" ".c" ".asd" ".log"))
               ;;(setq ido-ignore-extensions t)
               ;;(add-to-list completion-ignored-extensions ".dvi")
               (setq ido-enable-flex-matching t)
               (setq ido-everywhere t)
               (ido-mode 1)
               ;; Wack the pathname at the end of the input string
               (define-key ido-common-completion-map (kbd "<M-backspace>") (lambda ()
                                                                             (interactive)
                                                                             (goto-char (point-max))
                                                                             (let ((sp (point))
                                                                                   (ep (progn (search-backward "/") (forward-char) (point))))
                                                                               (if (not (= sp ep))
                                                                                   (kill-region sp ep)))))
               (define-key ido-common-completion-map (kbd "<M-DEL>") (lambda ()
                                                                       (interactive)
                                                                       (goto-char (point-max))
                                                                       (let ((sp (point))
                                                                             (ep (progn (search-backward "/") (forward-char) (point))))
                                                                         (if (not (= sp ep))
                                                                             (kill-region sp ep))))))
        (message "MJR: INIT: PKG SETUP: ido: WARNING: Could not load ido package"))
    (message "MJR: INIT: PKG SETUP: ido: SKIP: Pookie mode"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "MJR: INIT: PKG SETUP: package...")
(if (require 'package nil :noerror)
    (progn
      (add-to-list 'package-archives
                   '("melpa" . "http://melpa.org/packages/") t)
      ;; (add-to-list 'package-archives
      ;;              '("melpa-stable" . "http://stable.melpa.org/packages/") t)
      )
    (message "MJR: INIT: PKG SETUP: package: WARNING: Could not load package package"))
;; To refresh package contents: package-refresh-contents
;; To install a package: package-install
;; To list packages: package-list-packagesx

;;(package-refresh-contents)
;;(package-install "magit")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "MJR: INIT: PKG SETUP: ESS")
(let ((ess-path (concat MJR-home-cor "/elisp/ess/lisp/")))
  (if (file-exists-p (concat ess-path "ess-site.el"))
      (progn (message "MJR: INIT: PKG SETUP: ESS found... %s" ess-path)
             (add-to-list 'load-path ess-path)
             (autoload 'R "ess-site" "Call 'R', the 'GNU S' system from the R Foundation." t)
             (autoload 'R-mode "ess-site" "Call 'R', the 'GNU S' system from the R Foundation." t)
             (eval-after-load "ess-site"
               '(progn (message "MJR: POST-INIT(%s): EVAL-AFTER: ess!" (MJR-date "%Y-%m-%d_%H:%M:%S"))
                       (setq ess-fancy-comments nil)
                       (add-to-list 'ess-style-alist
                                    '(mjr-ess-style
                                      (ess-indent-level . 2)                       ;; * (ess-indent-level . 4) 
                                      (ess-first-continued-statement-offset . 2)   ;; * (ess-first-continued-statement-offset . 0) 
                                      (ess-continued-statement-offset . 0)         ;; * (ess-continued-statement-offset . 4) 
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
                                   (message "MJR: POST-INIT(%s): HOOK: ess-mode-hook" (MJR-date "%Y-%m-%d_%H:%M:%S"))
                                   (ess-toggle-underscore nil)
                                   (ess-set-style 'mjr-ess-style))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "MJR: INIT: STAGE: Theme....")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(require 'custom)
(defun MJR-try-theme ()
  (if MJR-pookie-mode
      nil
      (if (load-theme 'mjr-dark 't  )
          (message "MJR: INIT: THEME: Loaded mjr-dark!")
          (progn (message "MJR: INIT: THEME: Failed to load mjr-dark!  Trying manjo-dark")
                 (if (load-theme 'manjo-dark 't)
                     (message "MJR: INIT: THEME: manjo-dark!")
                     (message "MJR: INIT: THEME: Failed to load mjr-dark!"))))))
(MJR-try-theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "MJR: INIT: STAGE: Setup global aliases....")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defalias 'sipell                      'ispell)                         ;; typo optimization
(defalias 'sipell-comments-and-strings 'ispell-comments-and-strings)    ;; typo optimization
(defalias 'ispell-code                 'ispell-comments-and-strings)    ;; better name
(defalias 'sipell-code                 'ispell-comments-and-strings)    ;; better name + typo optimization
(defalias 'code-indent                 'indent-region)                  ;; better name

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "MJR: INIT: STAGE: Setup global keys....")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Git rid of the suspend
(global-set-key (kbd "C-z")       nil)
(global-set-key (kbd "C-x C-z")   nil)
;; 'island' keys
(global-set-key (kbd "C-<right>") 'forward-word)
(global-set-key (kbd "C-<left>")  'backward-word)
(global-set-key (kbd "M-<right>") 'forward-sexp)
(global-set-key (kbd "M-<left>")  'backward-sexp)
(global-set-key (kbd "<del>")     'delete-char)

;; Mouse left and right
(global-set-key (kbd "<mouse-6>") 'scroll-right)
(global-set-key (kbd "<mouse-7>") 'scroll-left)

(global-set-key (kbd "s-l") 'scroll-down-command) ; Super+l -- filco emulation on mbp
(global-set-key (kbd "s-.") 'scroll-up-command)   ; Super+. -- filco emulation on mbp

;; Random key bindings
(global-set-key (kbd "ESC ESC y") '(lambda () (interactive) (popup-menu 'yank-menu)))
(global-set-key (kbd "ESC ESC ;") 'MJR-quick-code-comment)
(global-set-key (kbd "ESC ESC g") 'goto-line)
(global-set-key (kbd "C-c C-d")   'MJR-date)
(global-set-key (kbd "C-c C-f")   'ffap)
(global-set-key (kbd "C-c C-c")   'compile)
(global-set-key (kbd "C-x r a")   'append-to-register)

;; Not global, but the mini-buffer is kinda everyplace...
(define-key minibuffer-local-map (kbd "C-p") 'previous-history-element)
(define-key minibuffer-local-map (kbd "C-n") 'next-history-element)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "MJR: INIT: STAGE: Emacs Customization System....")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-notify-method (quote pushy))
 '(LaTeX-item-indent 0)
 '(TeX-PDF-mode t)
 '(TeX-auto-save t)
 '(TeX-auto-untabify t)
 '(TeX-parse-self t)
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"]) 
 '(delete-selection-mode t)
 '(fringe-mode (quote (0)) nil (fringe))
 '(indicate-buffer-boundaries (quote right))
 '(indicate-empty-lines t)
 '(safe-local-variable-values (quote ((Syntax . ANSI-Common-LISP)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(linum ((t (:inherit (shadow default) :foreground "deep pink"))))
 '(subscript ((t (:inherit (font-lock-string-face)))))
 '(superscript ((t (:inherit (font-lock-string-face)))))
 '(Info-title-1-face                      ((t (:height 1.0))))
 '(Info-title-2-face                      ((t (:height 1.0))))
 '(Info-title-3-face                      ((t (:height 1.0))))
 '(change-log-file-face                   ((t (:height 1.0))))
 '(custom-face-tag-face                   ((t (:height 1.0))))
 '(custom-group-tag-face                  ((t (:height 1.0))))
 '(custom-group-tag-face-1                ((t (:height 1.0))))
 '(custom-variable-tag-face               ((t (:height 1.0))))
 '(font-lock-function-name-face           ((t (:height 1.0))))
 '(widget                                 ((t (:height 1.0))))
 '(font-lock-preprocessor-face            ((t (:italic n :slant normal))))
 '(font-lock-comment-face                 ((t (:italic n :slant normal))))
 '(font-lock-doc-face                     ((t (:italic n :slant normal))))
 '(cperl-hash-face                        ((t (:italic n :slant normal))))
 '(gnus-cite-attribution-face             ((t (:italic n :slant normal))))
 '(gnus-emphasis-bold-italic              ((t (:italic n :slant normal))))
 '(gnus-emphasis-italic                   ((t (:italic n :slant normal))))
 '(gnus-emphasis-underline-bold-italic    ((t (:italic n :slant normal))))
 '(gnus-emphasis-underline-italic         ((t (:italic n :slant normal))))
 '(gnus-header-content                    ((t (:italic n :slant normal))))
 '(gnus-header-content-face               ((t (:italic n :slant normal))))
 '(gnus-header-newsgroups                 ((t (:italic n :slant normal))))
 '(gnus-header-newsgroups-face            ((t (:italic n :slant normal))))
 '(gnus-signature                         ((t (:italic n :slant normal))))
 '(gnus-signature-face                    ((t (:italic n :slant normal))))
 '(gnus-summary-low-ancient               ((t (:italic n :slant normal))))
 '(gnus-summary-low-ancien-facet          ((t (:italic n :slant normal))))
 '(gnus-summary-low-read                  ((t (:italic n :slant normal))))
 '(gnus-summary-low-read-face             ((t (:italic n :slant normal))))
 '(gnus-summary-low-ticked                ((t (:italic n :slant normal))))
 '(gnus-summary-low-ticked-face           ((t (:italic n :slant normal))))
 '(gnus-summary-low-undownloaded          ((t (:italic n :slant normal))))
 '(gnus-summary-low-unread                ((t (:italic n :slant normal))))
 '(gnus-summary-low-unread-face           ((t (:italic n :slant normal))))
 '(message-header-newsgroups              ((t (:italic n :slant normal))))
 '(message-header-newsgroups-face         ((t (:italic n :slant normal))))
 '(bbdb-company                           ((t (:italic n :slant normal))))
 '(change-log-acknowledgement-face        ((t (:italic n :slant normal))))
 '(change-log-date-face                   ((t (:italic n :slant normal))))
 '(epa-field-body                         ((t (:italic n :slant normal))))
 '(epa-validity-disabled                  ((t (:italic n :slant normal))))
 '(epa-validity-low                       ((t (:italic n :slant normal))))
 '(epa-validity-medium                    ((t (:italic n :slant normal))))
 '(excerpt                                ((t (:italic n :slant normal))))
 '(vm-highlight-url-face                  ((t (:italic n :slant normal)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message "MJR: INIT: STAGE: Done Customizing Emacs....")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;       (setq load-path (cons (format "%s/.emacs.d/maple" (getenv "HOME")) load-path))
;; (autoload 'maplev "maplev" "Maple editing mode" t)
;; (setq 
;;  auto-mode-alist (cons (cons (concat "\\." (regexp-opt '("mpl" "tst") t)
;;                                      "$")
;;                              'maplev-mode)
;;                        auto-mode-alist)
;;  maplev-copyright-owner "Joseph S. Riel" ; this is for applying copyrights to Maple code you create
;;  maplev-default-release "15"
;;  maplev-executable-alist 
;;  '(
;;    ("15" . ("maple15" nil "mint15"))
;;    ("14" . ("maple14" nil "mint14"))
;;    ("13" . ("maple13" nil "mint13")))
;;  maplev-mint-query nil
;;  maplev-description-quote-char ?\"
;;  )
