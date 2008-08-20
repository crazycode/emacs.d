;;; smarter-find.el -- Find a file in any of a predetermined set of directories
;;; Copyright (c) 2001, 2002, 2005
;;;   by HIRAOKA Kazuyuki <khi@users.sourceforge.jp>
;;; (smart-find.el: Copyright (c) 1990 by Wayne Mesard.)
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; The GNU General Public License is available by anonymouse ftp from
;;; prep.ai.mit.edu in pub/gnu/COPYING.  Alternately, you can write to
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139,
;;; USA.
;;--------------------------------------------------------------------

;;; DESCRIPTION:
;;   This is an enhanced version of smart-find.el.
;;   (Original smart-find.el is written by Wayne Mesard.)
;; 
;;   Allows you to quickly find a file in one of a prespecified set of
;;   directories, without requiring you to think about (or specify)
;;   which directory the file is in.
;;   
;;   Does filename completion and possible match listings, just like
;;   regular old find-file.
;;
;;   By default SMARTER-FIND-FILE is bound to Control-C Control-F.
;;   To prevent this, add "(setq dont-bind-my-keys t)" to the top
;;   of your ~/.emacs file.
;;   
;;   To specify the search path SMARTER-FIND-FILE uses, set the variable 
;;   SMARTER-FIND-PATH.  E.g., put something like this in your ~/.emacs:
;;
;;     (setq smarter-find-path
;;        '("~" "/usr/include" "/usr/include/sys" "$myworkdir"))
;;
;;   If the variable SMARTER-FIND-PRETEND-SMART-FIND is non-nil,
;;   SMART-FIND-FILE, SMART-FIND-FILE-PATH, and SMART-FIND-EXCLUDED-FILE-PATH
;;   can be used instead of
;;   SMARTER-FIND-FILE, SMARTER-FIND-EXCLUDED-PATH and SMARTER-FIND-PATH,
;;   respectively.

;;; HISTORY:
;;  1.0.3 hira - [2005-06-04]
;;    Fix: TAB after "~/foo/~" wrongly opened a new buffer "~" directly.
;;    Fix: "foo//bar" should be converted not to "foo/bar" but "/bar".
;;    Fix: No match was found when case-fold-search is non-nil and
;;    there exist "foo.txt" and "Foo.txt".
;;  1.0.2 hira - [2002/12/05]
;;    Fix on smarter-find-pretend-smart-find (must be defined earlier).
;;    Tried to fix for emacs19 (defalias-maybe). But incomplete (completing-read).
;;  1.0.1 hira - [2002/06/09]
;;    Fix on completion around "~/.sawfish/" and "~/.sawfishrc".
;;  1.0 hira - [2002/06/03] Fix on completion from "~/bin/" to "~/bin"
;;  0.9 hira - [2002/06/02] Fix on double-counting of "~/bin" and "~/bin/".
;;  0.8 hira - [2002/04/18] Fix on stupid algorithm which caused slow response.
;;  0.7 hira - [2001/12/31] smarter-find-excluded-path
;;  0.6 hira - [2001/02/24] All buffers' dirs are added to path.
;;                          Fix on ignored current directory.
;;  0.5 hira - [2001/02/20] Set this-command for makeshift fix on TAB-bug.
;;  0.4 hira - [2001/02/16] Use a safe key for makeshift fix on TAB-bug.
;;  0.3 hira - [2001/02/13] Fix on double-counting of "~" and "~/".
;;                          Makeshift fix on TAB-bug.
;;  0.2 hira - [2001/02/05] Fix around ~ ~/. ~/.. etc.
;;  0.1 hira - [2001/02/02] Created, based on smart-find.el
;;--------------------------------------------------------------------

;;; Features added to the original smart-find.el:
;; - Can deal with subdirectories.
;;   (This does not mean to search all subdirectories of smarter-find-path.
;;   When "foo/bar/baz" is written in the minibuffer, <PATH>/foo/bar/baz is
;;   searched for all <PATH> in smarter-find-path.)
;; - Check not only the first matching path but all paths.
;; - Show path names in list of matches.
;; - Current directory is implicitly added to smarter-find-path. Moreover,
;;   all buffers' directories are implicitly added to smarter-find-path.
;; - Can deal with "/foo", "foo//bar", "~/foo" "foo/~/bar", "./foo", etc.
;; - Create a new file for non-existing name.
;; - When an unique path is decided, replace the content of minibuffer
;;   with it.
;; - Null string is completed to current directory (or first dir in path).
;; - Can exclude specified paths.

;;; Problems(?):
;; - [tab] may cause scroll of an unrelated window.
;;   (A makeshift debug is applied at now.[2001/02/13])
;; - If you type [RETURN] when a relative file name is shown in the
;;   minibuffer, it always means <current directory>/<name>.
;;   In order to open a file in another directory, you must confirm that
;;   its absolute name is shown in the minibuffer, or, you must select one
;;   explicitly in *Completions* buffer.

;;; Future plans:
;; - Specify all directories under ~/foo by "~/foo/*" or "~/foo/**".
;;   The latter means recursive search.
;; - Use "find" or "locate" to search ALL files.
;;--------------------------------------------------------------------

;;; 
;;; USER OPTIONS
;;; 

(defvar smarter-find-pretend-smart-find t
  "*If non-nil, keep conpatibility with the original smart-find.
Namely, SMART-FIND-FILE and SMART-FIND-FILE-PATH can be used instead of
SMARTER-FIND-FILE and SMARTER-FIND-PATH, respectively.")

(if smarter-find-pretend-smart-find
    (defvar smart-find-file-path '("~")
      "*Which directories smart-find-file should examine.")
  (defvar smarter-find-path '("~")
    "*Which directories smarter-find-file should examine."))

(if smarter-find-pretend-smart-find
    (defvar smart-find-excluded-file-path '()
      "*Which directories smart-find-file should NOT examine.")
  (defvar smarter-find-excluded-path '()
    "*Which directories smarter-find-file should NOT examine."))

(defvar smarter-find-show-path t
  "*If non-nil, show path names in list of matches.")
(defvar smarter-find-add-current-directory t
  "*If non-nil, current directory is implicitly added to smarter-find-path.")
(defvar smarter-find-add-all-buffer-directories t
  "*If non-nil, all buffers' directories are implicitly added to smarter-find-path.
Current directory is always added consequently.")
(defvar smarter-find-allow-new-file t
  "*If non-nil, new file is created for non-existing name.
Only SMART(ER)-FIND-FILE is affected by this variable.")
(defvar smarter-find-null-means-default t
  "*If non-nil, null string is interpreted as current directory.")
; (defvar smarter-find-check-current-directory-first t
;   "*If non-nil, try 'smarter' finding only when no matching in current directory.")
; (defvar smarter-find-completion-buffer-name "*Completions*"
;   "Jump to this buffer when multiple \"exact-matching\" files are found.
; Set nil to disable this feature.")

;;; 
;;; KEY BINDINGS
;;; 

(if (not (and (boundp 'dont-bind-my-keys) dont-bind-my-keys))
    (global-set-key "\C-c\C-f"
		    (if smarter-find-pretend-smart-find
			'smart-find-file
		      'smarter-find-file)))

;;; 
;;; SYSTEM VARIABLES
;;; 

(if smarter-find-pretend-smart-find
    (defvar smart-find-file-match))

(defvar file-name-history nil)
(defvar smarter-find-next-string nil)
; (defvar smarter-find-right-after-change nil)
(defvar smarter-find-default-directory nil)

(defvar smarter-find-makeshift-minibuffer-complete t
  "If non-nil, a makeshift debug on TAB-bug is applied.
Otherwise, hitting TAB (minibuffer-complete) may cause
scroll of an unrelated window sometimes.
See also `smarter-find-harmless-key'.")

(if smarter-find-makeshift-minibuffer-complete
    (defun smarter-find-minibuffer-complete ()
      "Dummy function for makeshift debug on TAB-bug."
      (minibuffer-complete)))
  
;;; 
;;; COMMANDS
;;; 

(defun smarter-find-file (filename)
  (interactive
   (list (smarter-find-read-file-name "Smarter find file: "
				      nil nil
				      (not smarter-find-allow-new-file)
				      nil)))
  (find-file filename))

;;; 
;;; PUBLIC FUNCTIONS
;;; 

(defun smarter-find-read-file-name (prompt &optional
					   directory default existing initial)
  ;; Compatible with read-file-name, perhaps.
  (setq smarter-find-default-directory
	(smarter-find-name-as-directory (or directory default-directory)))
  (let ((predicate nil)
	(require-match existing)
	(hist 'file-name-history)
	(inherit-input-method nil)
	s)
    (setq smarter-find-next-string (or initial ""))
    ;; In completion of file names, their length can shrink.
    ;; For example, "foo//bar" will turn to "/bar".
    ;; This is the reason why we need a loop here.
    ;; (I can't remember whether this comment is correct or not.)
    (while smarter-find-next-string
      (setq initial smarter-find-next-string
	    smarter-find-next-string nil)
      ;; global variable smarter-find-next-string may be set
      ;; in smarter-find-internal.
      (setq s (completing-read prompt 'smarter-find-internal
			       predicate require-match initial
			       hist default inherit-input-method)))
    (if smarter-find-pretend-smart-find
	(setq smart-find-file-match s))
    s))

;;; 
;;; PRIVATE FUNCTIONS
;;; 

;;; Main ;;;;;;;;;;;;;;;;;

;; see info ==> snap://Info-mode/elisp#Programmed Completion
(defun smarter-find-internal (str predicate action)
  "Internal subroutine for smarter-find.  Do not call this."
  ;; Predicate is ignored at now.
;   (setq smarter-find-right-after-change nil)
  (cond ((null action) (smarter-find-complete str))
	((eq action 'lambda) (smarter-find-verify str))
	(t (smarter-find-all str))))

;;; Verify ;;;;;;;;;;;;;;;;;

(defun smarter-find-verify (str)
  (file-exists-p str))

;;; List all ;;;;;;;;;;;;;;;

(defun smarter-find-all (str)
  (let* ((d (file-name-directory str))
	 (f (file-name-nondirectory str))
	 (dms (smarter-find-dir-and-match d f t)))
    (cond ((null dms) (smarter-find-all-not-found))
	  ((null (cdr dms)) (smarter-find-all-unique-dir dms))
	  (t (smarter-find-all-multi-dir dms)))))

(defun smarter-find-all-not-found () nil)
(defun smarter-find-all-unique-dir (dir-and-match)
  (smarter-find-all-without-path dir-and-match))
(defun smarter-find-all-multi-dir (dir-and-match)
  (if smarter-find-show-path
      (smarter-find-all-with-path dir-and-match)
    (smarter-find-all-without-path dir-and-match)))

(defun smarter-find-all-with-path (dir-and-match)
  (smarter-find-all-collect dir-and-match
			    '(lambda (dir f)
			       (concat (smarter-find-normalize-name dir)
				       f))))
(defun smarter-find-all-without-path (dir-and-match)
  (smarter-find-all-collect dir-and-match '(lambda (dir f) f)))
(defun smarter-find-all-collect (dir-and-match transfer)
  (let ((transed (mapcar `(lambda (dm)
			    (smarter-find-all-in-dir (car dm) (cdr dm)
						     ,transfer))
			 dir-and-match)))
    (apply 'append transed)))
(defun smarter-find-all-in-dir (dir files transfer)
  (mapcar `(lambda (f) (funcall ,transfer ,dir f)) files))

;;; Complete ;;;;;;;;;;;;;;;;

(defun smarter-find-complete (str)
  (smarter-find-care-irregular-case!! str)
  (let* ((d (file-name-directory str))
	 (f (file-name-nondirectory str))
	 (dms (smarter-find-dir-and-match d f nil))
	 (r (cond ((null dms) (smarter-find-no-matched))
		  ((cdr dms) (smarter-find-multi-dir-matched d f dms))
		  (t (smarter-find-unique-dir-matched d f dms)))))
    (if (smarter-find-irregular-completion-p r str)
	(smarter-find-change-string!! r))
    r))

(defun smarter-find-care-irregular-case!! (str)
  (if (and smarter-find-null-means-default (string= str ""))
      (smarter-find-complete-default!!))
  (let ((s (smarter-find-normalize-name str t)))
    (if (not (string= s str))
	(smarter-find-change-string!! s))))

(defun smarter-find-irregular-completion-p (result orig)
  (and (not (null result))
       (not (eq result t))
       (not (string= (smarter-find-common-string (list result orig))
		     orig))))

(defun smarter-find-complete-default!! ()
  (smarter-find-change-string!! smarter-find-default-directory))
(defun smarter-find-no-matched () nil)
(defun smarter-find-multi-dir-matched (input-dir input-file dir-and-match)
  (let ((matches (mapcar '(lambda (dm)
			    (let ((match (cdr dm)))
			      (smarter-find-match-name match input-file)))
			 dir-and-match)))
    (smarter-find-concat-name input-dir (smarter-find-common-string matches))))

(defun smarter-find-unique-dir-matched (input-dir input-file dir-and-match)
  (let* ((dm (car dir-and-match))
	 (abs-dir (car dm))
	 (match-raw (cdr dm))
	 (match (smarter-find-match-name match-raw input-file))
	 (abs-match (smarter-find-concat-name abs-dir match)))
    (cond ((not (smarter-find-absolute-p input-dir))
	   (smarter-find-complete-absolute!! abs-match))
	  ((eq match-raw t)  ;; unique and exact
	   (smarter-find-unique-exact-matched))
	  (t
	   (smarter-find-nonexact-matched abs-match)))))

(defun smarter-find-complete-absolute!! (abs-match)
  (smarter-find-change-string!! abs-match))
(defun smarter-find-unique-exact-matched () t)
(defun smarter-find-nonexact-matched (abs-match) abs-match)

;;; 
;;; UTILITIES
;;; 

;;; Completion ;;;;;;;;;;;;;;;;;;;;;

(defvar smarter-find-once-more-completion t)

(defun smarter-find-change-string!! (new-str)
  ;; Automatically try completion to new-str once.
  (when smarter-find-once-more-completion
    (setq smarter-find-once-more-completion nil)
    (if (and smarter-find-makeshift-minibuffer-complete
	     (eq this-command 'minibuffer-complete))
	(setq this-command 'smarter-find-minibuffer-complete))
    ;; Note that (smarter-find-complete "~") is nil.
    (setq new-str (smarter-find-match-name (smarter-find-complete new-str)
 					   new-str t))) ;; new
; 					   new-str nil))) ;; old
  (setq smarter-find-once-more-completion t)
  (setq smarter-find-next-string new-str)
;   (setq smarter-find-right-after-change t)
  (exit-minibuffer)
  )

;;; Match ;;;;;;;;;;;;;;;;;;;;;;;;

(defun smarter-find-dir-and-match (dir file all)
  (let* ((paths (smarter-find-get-path (concat dir file)))
	 (dms (mapcar '(lambda (path)
			 (smarter-find-match-for-path path dir file all))
		      paths)))
    (smarter-find-remove-if 'null dms)))

(defun smarter-find-match-for-path (path dir file all)
  (let ((abs-dir (smarter-find-concat-name path dir)))
    (if (not (file-exists-p abs-dir))
	nil
      (let ((c (cond (all (file-name-all-completions file abs-dir))
		     (t (file-name-completion file abs-dir)))))
	(cond ((null c) nil)
	      (t (cons abs-dir c)))))))

(defun smarter-find-match-name (match try &optional care-null-case)
  (cond ((eq match t) try)
        ((and (null match) care-null-case) try)
        (t match)))

(defvar smarter-find-current-directory-regexp "^\\.\\.?/"
  "File names before which `default-directory' should be completed.")

(defun smarter-find-get-all-path ()
  (let* ((path-raw (smarter-find-normalize-name-list
		    (cond ((boundp 'smarter-find-path)
			   smarter-find-path)
			  ((boundp 'smarter-find-file-path) ;; unofficial
			   smarter-find-file-path)
			  (t
			   smart-find-file-path))))
	 (path-excluded (smarter-find-normalize-name-list
			 (cond ((boundp 'smarter-find-excluded-path)
				smarter-find-excluded-path)
			       ((boundp 'smarter-find-excluded-file-path) ;; unofficial
				smarter-find-excluded-file-path)
			       (t
				smart-find-excluded-file-path)))))
    (cond
     (smarter-find-add-all-buffer-directories
      (smarter-find-add-all-buffer-path path-raw path-excluded))
     (smarter-find-add-current-directory
      (smarter-find-add-path smarter-find-default-directory
			     path-raw path-excluded))
     (t
      path-raw))))

; (defun smarter-find-get-all-path ()
;   (let* ((path-raw (cond ((boundp 'smarter-find-path)
; 			      smarter-find-path)
; 			 ((boundp 'smarter-find-file-path) ;; unofficial
; 			  smarter-find-file-path)
; 			 (t
; 			  smart-find-file-path)))
; 	 (path-excluded (cond ((boundp 'smarter-find-excluded-path)
; 			       smarter-find-excluded-path)
; 			      ((boundp 'smarter-find-excluded-file-path) ;; unofficial
; 			       smarter-find-excluded-file-path)
; 			      (t
; 			       smart-find-excluded-file-path)))
; 	 (path-extended (cond
; 			 (smarter-find-add-all-buffer-directories
; 			  (smarter-find-add-all-buffer-path path-raw))
; 			 (smarter-find-add-current-directory
; 			  (smarter-find-add-path smarter-find-default-directory
; 						 path-raw))
; 			 (t
; 			  path-raw)))
; 	 (path-cleaned (smarter-find-subtract-paths path-excluded path-extended)))
;     path-cleaned))

(defun smarter-find-get-path (filename)
  "Candidate paths for filename.
Paths in `smarter-find-excluded-path' are excluded.
Return `smarter-find-path' if FILENAME is relative.
Return (nil) if FILENAME is absolute.
Exceptionally, return (`default-directory') if FILENAME matches to
`smarter-find-current-directory-regexp'.
In the first case, `smart-find-file-path' is used instead when
`smarter-find-path' is not bounded."
  (let ((path (cond
	       ((null filename)
		(smarter-find-get-all-path))
		((string-match smarter-find-current-directory-regexp filename)
		 (list smarter-find-default-directory))
		((smarter-find-absolute-p filename)
		 '(nil))
		(t
		 (smarter-find-get-all-path)))))
    (mapcar 'smarter-find-name-as-directory path)))

(defun smarter-find-add-all-buffer-path (original-paths excluded-paths)
  (let* ((ds0 (mapcar '(lambda (b)
			 (file-name-directory (or (buffer-file-name b) "")))
		      (buffer-list)))
; 	 (ds (mapcar 'file-name-as-directory
; 		     (smarter-find-remove-if 'null ds0)))
	 (ds (mapcar 'directory-file-name
		     (smarter-find-remove-if 'null ds0)))
k	 (ps original-paths))
    (mapcar '(lambda (d) (setq ps (smarter-find-add-path d ps excluded-paths)))
	    ds)
    ps))

(defun smarter-find-add-path (new-path original-paths excluded-paths)
  (let ((n (smarter-find-normalize-name new-path)))
    (if (or (smarter-find-member-if '(lambda (x) (string= x n)) original-paths)
	    (smarter-find-member-if '(lambda (x) (string= x n)) excluded-paths))
	original-paths
      (cons n original-paths))))

; (defun smarter-find-subtract-paths (excluded-paths original-paths)
;   "Remove EXCLUDED-PATHS from ORIGINAL-PATHS."
;   (smarter-find-remove-if '(lambda (x)
; 			     (smarter-find-member-if
; 			      '(lambda (y) (smarter-find-equal x y))
; 			      excluded-paths))
; 			  original-paths))

;;; File name ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Don't forget:
;; (file-name-directory "~") ==> nil
;; (file-name-nondirectory "~") ==> "~"
;; (file-name-absolute-p "~") ==> t
;; (expand-file-name nil) ==> error
;; (expand-file-name "~") ==> "/home/hira"

(defun smarter-find-normalize-name-sub (filename &optional not-force-dir)
  ;; foo/~/bar/ ==> ~/bar/
  (let ((f (abbreviate-file-name (substitute-in-file-name filename))))
    (if (and (file-directory-p f) (not not-force-dir))
	(file-name-as-directory f)
      f)))

(defun smarter-find-absolute-p (filename)
  (and filename
       (file-name-absolute-p (smarter-find-normalize-name-sub filename))))

(defun smarter-find-expand-name (filename)
  (if (null filename)
      ""
    (let ((d (file-name-directory filename))
	  (f (file-name-nondirectory filename)))
      (if (smarter-find-absolute-p d)
	  ;; foo/./bar ==> foo/bar
	  ;; foo/. ==> foo/.
	  (concat (expand-file-name d) f)
	filename))))

(defun smarter-find-normalize-name (filename &optional not-force-dir)
  ;; convert annoying "foo/~/bar" and "foo//bar" first.
  (setq filename (substitute-in-file-name filename))
  (smarter-find-normalize-name-sub (smarter-find-expand-name filename)
				   not-force-dir))

(defun smarter-find-normalize-name-list (filename-list)
  (mapcar 'smarter-find-normalize-name filename-list))

(defun smarter-find-concat-name (parent child)
  (let* ((f (if (smarter-find-absolute-p child)
		child
	      (concat (smarter-find-name-as-directory parent) child)))
	 (g (smarter-find-normalize-name f t)))
    (if (string= child "")
	(smarter-find-name-as-directory g)
      g)))

; (defun smarter-find-concat-name (parent child)
;   (setq parent (smarter-find-expand-name parent))
;   (setq child (smarter-find-expand-name child))
;   (let* ((f (if (smarter-find-absolute-p child)
; 		child
; 	      (concat (smarter-find-name-as-directory parent) child)))
; 	 (g (abbreviate-file-name (substitute-in-file-name f))))
;     (if (string= child "")
; 	(smarter-find-name-as-directory g)
;       g)))

(defun smarter-find-equal (a b)
  "Compare two file names."
  (or (string= a b)
      (apply 'string=
	     (mapcar '(lambda (d)
			(file-name-as-directory (smarter-find-normalize-name d)))
		     (list a b)))))

; (defun smarter-find-equal (a b)
;   "Compare two file names."
;   (apply 'string=
; 	 (mapcar '(lambda (d)
; 		    (file-name-as-directory (smarter-find-normalize-name d)))
; 		 (list a b))))

; (defun smarter-find-equal (a b)
;   "Compare two file names."
;   ;; Not correct for "foo//bar", "foo/~/bar", etc.
;   (apply 'string=
; 	 (mapcar '(lambda (x)
; 		    (expand-file-name x smarter-find-default-directory))
; 		 (list a b))))

(defun smarter-find-name-as-directory (name)
  (if (or (null name) (string= name ""))
      nil
    (file-name-as-directory name)))

;;; String and list ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun smarter-find-common-string (string-list)
  "Common header part of strings
 (\"fooxy\" \"fooxz\" \"fooa\") ==> \"foo\""
  (if (null string-list)
      nil
    (let ((s (car string-list))
	  (rest (cdr string-list)))
      (while rest
	(setq s (smarter-find-common-string2 s (car rest))
	      rest (cdr rest)))
      s)))

(defun smarter-find-common-string2 (a b)
  "Common header part of two strings.
\"fooxy\" and \"fooz\" ==> \"foo\""
  (let ((i 0))
    (catch 'exit
      (while (< i (min (length a) (length b)))
	(if (not (string= (smarter-find-downcase-maybe (substring a i (1+ i)))
                          (smarter-find-downcase-maybe (substring b i (1+ i)))))
	    (throw 'exit nil)
	  (setq i (+ i 1)))))
    (substring a 0 i)))

(defun smarter-find-downcase-maybe (str)
  (if case-fold-search
      (downcase str)
    str))

(defun smarter-find-member-if (predicate a)
  ;; Temptation to require cl-*. Hum...
  (cond ((null a) nil)
	((apply predicate (list (car a))) (car a))
	(t (smarter-find-member-if predicate (cdr a)))))

(defun smarter-find-remove-if (predicate a)
  ;; Temptation to require cl-*. Hum...
  (let ((ans-rev nil)
	(rest a))
    (while rest
      (if (not (funcall predicate (car rest)))
	  (setq ans-rev (cons (car rest) ans-rev)))
      (setq rest (cdr rest)))
    (reverse ans-rev)))

;;;
;;; MISC
;;;

;; The following debug on "[tab] after [tab]" is incomplete. [2001/02/13]

;; [tab] after [tab]
;;
;; Without this advice, double [tab] will scroll an unrelated window
;; if smarter-find-change-string!! is called by the first [tab].

; (defadvice minibuffer-complete (around smarter-find-minibuffer-complete-ad
; 				       ()
; 				       activate)
;   "Do minibuffer-complete-word instead when it is right after smarter-find-change-string!!"
;   (if smarter-find-right-after-change
;       (minibuffer-complete-word)
;     ad-do-it))

;; compatibility

(when smarter-find-pretend-smart-find
  (defalias 'smart-find-file
    'smarter-find-file)
  (defalias 'smart-find-file-jump-to-completion-buffer
    'smarter-find-jump-to-completion-buffer)
  (defalias 'smart-find-file-read-file-name
    'smarter-find-read-file-name))

;; emacs19 doesn't have "defalias-maybe"?
; (when smarter-find-pretend-smart-find
;   (defalias-maybe 'smart-find-file
;     'smarter-find-file)
;   (defalias-maybe 'smart-find-file-jump-to-completion-buffer
;     'smarter-find-jump-to-completion-buffer)
;   (defalias-maybe 'smart-find-file-read-file-name
;     'smarter-find-read-file-name))
