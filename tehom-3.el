;;; tehom-3.el --- Routines to save & restore complex elisp objects

;; Copyright (C) 1999 by Tom Breton

;; Author: Tom Breton <tob@world.std.com>
;; Created: 11 June 1999
;; Version: 1.0
;; Keywords: extensions

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package is intended to easily save and restore the state of
;; any moderately complex elisp application.  To use it, represent all
;; the persisting state of your app as one elisp object, and call
;; tehom-save-object and tehom-restore-object when ending / restarting
;; it.  That's all.

;; This package requires cust-print and cl-read.  For best results,
;; they should be byte-compiled.  

;; It does not require either cust-print or cl-read to be set up, in
;; the sense that you need to add any code to support them.  They need
;; only be accessible.  Then you simply call tehom-save-object and
;; tehom-restore-object without further attention to the mechanics.

;; This package can read and write circular references (thanks to
;; cl-read and cust-print) within a single complex object, but using
;; it more than once (with separate savefiles) to write/read some
;; interlinked objects will not (and couldn't even in theory) resolve
;; inter-references between them.  So if you have to save and restore
;; interlinked objects, combine them into a single object.

;; A single save file can only contain one elisp object as one sexp.
;; If there is more than one sexp in a save file, only the first will
;; be read.

;; The entry points are tehom-save-object and tehom-restore-object

;;; Code:

(require 'cust-print)
(require 'cl-read)

(defconst tehom-save-transfer-buf-name " *Transfer buffer*" "" )


;;;; Utility functions

( defun tehom-comment-protect-string (string-0)
  "Return the string, but in comment syntax.

This function handles only comment-start, not comment-end, and only
handles lisp comment syntax.  Which is to say, it does no more than is
needed for the tehom-3 package. It could easily be generalized."

  (let*
      (
       (comment-protector (concat "\n" ";;" ))
       (string-1
	(split-string string-0 "\n" ))

       (string-2
	(mapconcat
	 'identity
	 string-1
	 comment-protector))

       (string-3 (concat comment-protector string-2)))
    
    string-3))

;;;; Helper functions

(defun tehom-write-custprinted-object-to-buf ( object savebuf)
  "Fill SAVEBUF with the printed representation of OBJECT. "

  (custom-print-install)
  
  (let
      ( (print-circle t)
	(custom-print-vectors t))

    (custom-prin1 object savebuf ))

  (custom-print-uninstall))

;;;; Entry points

( defun tehom-save-object (object filename &optional comment-0)
  "Save the elisp object OBJECT to the file FILENAME.

If COMMENT-0 is passed, the file will begin with that text as a
comment."

  
  (let*
      (
       (buf (get-buffer-create tehom-save-transfer-buf-name))

       (comment
	(or comment-0
	    "Data saved by tehom-save-object, written by Tom Breton" )))
    
    
    (with-current-buffer buf

      ;;The buffer is just for transfer, so we don't need undo.
      (setq buffer-undo-list t)
      (buffer-disable-undo)

      ;;Clear the buffer so nothing interferes.
      (erase-buffer)

      ;;Insert an explanatory comment.
      (insert "\n;;" (tehom-comment-protect-string comment) "\n" )

      ;;Write the data.
      (tehom-write-custprinted-object-to-buf object buf)

      ;;Write to that file, not caring whether it already exists.
      (write-file filename)
      
      ;;Erase what's in here.  No need to keep it.  We don't delete
      ;;our buffer because re-using later it is more efficient.
      (erase-buffer)
      (set-buffer-modified-p nil)

      )))


( defun tehom-restore-object (filename)
  "Return the object restored from the file FILENAME."
  
  (let*
      ((buf (get-buffer-create tehom-save-transfer-buf-name))
       object)
    
    (with-current-buffer buf
      (setq buffer-undo-list t)
      (buffer-disable-undo)

      ;;Get the file contents into our buffer.
      (insert-file-contents filename t nil nil t )
      
      ;;Go to the start of the buffer.
      (goto-char (point-min))

      ;;Enable cl-reader.  We enable it permanently, since this is the
      ;;only thing this buffer is used for
      (setq cl-read-active t)

      ;;Read specially. We can just use reader::read-from-buffer since
      ;;we know we're looking at a buffer.
      (setq object ( reader::read-from-buffer buf ))

      ;;Erase what's in here.  No need to keep it.  We don't delete
      ;;our buffer because re-using later it is more efficient.
      (erase-buffer)
      (set-buffer-modified-p nil)

      ;;Return the object
      object)))

(provide 'tehom-3)

;;; tehom-3.el ends here