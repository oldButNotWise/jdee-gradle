;;; jdee-gradle.el --- Gradle support for JDEE       -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Netflix, Inc.

;; Author: Stan Lanning <slanning@netflix.com>
;; Keywords: java, tools, gradle

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'gradle)
(require 'cl-lib)

(defgroup jdee-gradle nil
  "JDEE Gradle"
  :group 'jdee
  :prefix "jdee-gradle-")

(defcustom jdee-gradle-project-root nil
  "*Base directory for the Gradle project.
For sub-projects, this is the directory of the top-level project."
  :group 'jdee-gradle
  :type 'string)
(make-local-variable 'jdee-gradle-project-root)

(defcustom jdee-gradle-project-name nil
  "Name of the Gradle project.
For sub-projects, this is the name of the top-level project."
  :group 'jdee-gradle
  :type 'string)
(make-local-variable 'jdee-gradle-project-name)

(defcustom jdee-gradle-subproject-root nil
  "Root directory of the Gradle subproject, if any."
  :group 'jdee-gradle
  :type 'string)
(make-local-variable 'jdee-gradle-subproject-root)

(defcustom jdee-gradle-subproject-name nil
  "Name of the Gradle sub-project, if any."
  :group 'jdee-gradle
  :type 'string)
(make-local-variable 'jdee-gradle-subproject-name)

(defun jdee-gradle-module-dir ()
  "The name of this gradle (sub)project root directory."
  (or jdee-gradle-subproject-root jdee-gradle-project-root))

(defun jdee-gradle-set-project (&optional name dir force-top-level-p)
  "Define a Gradle project.
Automatically determines if the project is a sub-project of a containing multi-project
unless force-top-level-p is true.
If not given dir defaults to the directory containing the project file currently being loaded.
The top-level projects name defaults to the base name of dir; for sub-projects it defaults to
MAIN-BASE where MAIN is the name of the top-level project and BASE is the base name of the dir.
Returns the value of dir that was found."
  (cond ((not (null dir)) )
        ((not (null jdee-loading-project-file))
         (setq dir (file-name-directory jdee-loading-project-file)))
        (t (error "No directory given")))
  (if (not (file-exists-p (expand-file-name "build.gradle" dir)))
         (error "No Gradle build file found in %s" dir))
  (if (null name)
      (setq name (let ((base (file-name-base dir)))
                   (if jdee-gradle-project-name
                       (concat jdee-gradle-project-name "-" base)
                     base))))
  (if (and jdee-gradle-project-root (not force-top-level-p))
      ;; Already found a gradle project higher up in the food chain; assume this is a sub-project
      (setq jdee-gradle-subproject-root dir
            jdee-gradle-subproject-name name)
    ;; Top-level project: either a single project or the root of a multi-project
    (setq jdee-gradle-project-root dir
          jdee-gradle-project-name name
          jdee-gradle-subproject-root nil
          jdee-gradle-subproject-name nil))
  (jdee-set-project-name name)
  (jdee-set-variables
   `(jdee-build-function 'jdee-gradle-build)
   )
  dir)

(defun jdee-gradle-get-default-directory () 
  "Gets the default-directory according to the value of
`jdee-gradle-project-root'."
  (if (or (null jdee-gradle-project-root) (string= jdee-gradle-project-root ""))
      default-directory
    jdee-gradle-project-root))

(defun jdee-gradle-with-project-root (fn)
  "Execute fn in the Gradle root project directory.
See `gradle-with-project-root-func'."
  (let ((default-directory (jdee-gradle-get-default-directory)))
    (gradle--with-current-directory fn)))

;;
;; Hooks when visiting a file in a Gradle JDEE project
;;

(defcustom jdee-gradle-project-hooks nil
  "Specifies a list of functions to be run when a gradle JDEE project
becomes active."
  :group 'jdee-gradle
  :type '(repeat (function :tag "Function")))

(defun jdee-gradle-project-hook ()
  (if jdee-gradle-project-root
      (run-hooks 'jdee-gradle-project-hooks)))

(add-hook 'jdee-project-hooks 'jdee-gradle-project-hook)

;;
;; Building with Gradle
;;

(defun jdee-gradle-compile-fixup-task-newlines (buffer msg)
  ;; Gradle can leave the task name (like ":myproj:compileJava") at the beginning of a line, resulting in lines like
  ;; :myproj:compileJava/home/myname/myproj/src/main/java/com/mycorp/myproj/MyClass.java:84: error: invalid method declaration; return type required
  ;; This function scans over the buffer, looking for likely such lines, and inserts an appropriate newline.
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (while (re-search-forward "^\\(:[^/]+\\)\\(/.+:[0-9]+:\\)" nil t)
        (replace-match "\\1\n\\2" nil nil)))))

(defmacro jdee-with-compile-finish-fn (cleanup &rest body)
  "Install a post-compile cleanup fn."
  (declare (indent 1))
  ;; This is complicated beause the finish functions are called asynchronously,
  ;; and so simple dynamic binding doesn't work.
  `(progn
     (setq compilation-finish-functions 
           (lambda (buf msg)
             (funcall ,cleanup buf msg)
             (run-hook-with-args 'jdee-compile-finish-hook buf msg)
             (setq compilation-finish-functions nil)))
     ,@body))

(defcustom jdee-gradle-get-tasks-function 'jdee-gradle-get-tasks
  "Function to call to get the list of tasks to execute when doing a Gradle build.
See also `jdee-gradle-build-options'."
  :group 'jdee-gradle
  :type 'function)

(defun jdee-gradle-get-tasks ()
  "Gets the Gradle task(s) to run when doing a build;
see `jdee-gradle-get-tasks-function'.
This runs Gradle to get a list of all available tasks,
and then prompts you pick from that list."
  (gradle--input-commandline))

(defun jdee-gradle-get-subproject-tasks ()
  "Gets the Gradle task(s) to run when doing a build;
see `jdee-gradle-get-tasks-function'.
This runs Gradle to get a list of all available tasks
and filters out those not appropriate for the current subproject."
  (let ((tasks (jdee-gradle-get-tasks)))
    (if (null jdee-gradle-subproject-name)
        tasks
      ;; TODO -- fix this up
      (remove-if (lambda (x) 
                   (not (string-match (concat "\\(^-\\|" (regexp-quote jdee-gradle-subproject-name) ":\\)") x)))
                 tasks))))

(defcustom jdee-gradle-build-options '("--console=plain")
  "List of additional options to pass to gradle."
  :group 'jdee-gradle
  :type 'list)

(defun jdee-gradle-build ()
  "Invokes `gradle-run' to build the current project.
The tasks to execute are found by calling `jdee-gradle-get-tasks'.
This function can be used as the value of `jdee-build-function'."
  (interactive)
  (let* ((default-directory (jdee-gradle-get-default-directory))
         (gradle-with-project-root-func 'jdee-gradle-with-project-root))
    (jdee-with-compile-finish-fn 'jdee-gradle-compile-fixup-task-newlines
      (gradle-run (append jdee-gradle-build-options (funcall jdee-gradle-get-tasks-function))))))


(provide 'jdee-gradle)

;;; jdee-gradle.el ends here
