;;;
;;; Emacs project file for the single-project example build.
;;;

(require 'jdee-gradle)

(jdee-gradle-set-project "example")

;; Load the generated prj file, if it exists
(if (not (load (expand-file-name "prj-generated" jdee-gradle-project-root) t))
    ;; It wasn't found, so set some reasonable defaults
    (jdee-set-variables
     `(jdee-sourcepath ',(list (expand-file-name "src/main/java" jdee-gradle-project-root)
                               (expand-file-name "src/test/java" jdee-gradle-project-root)))
     `(jdee-build-class-path ',(list (expand-file-name "src/classes/main" jdee-gradle-project-root)
                                     (expand-file-name "src/classes/test" jdee-gradle-project-root)))
     `(jdee-global-classpath ',(list (expand-file-name "src/classes/main" jdee-gradle-project-root)
                                     (expand-file-name "src/classes/test" jdee-gradle-project-root)))
     ))

;; And then some other settings (which may override values in the generated prj)
(jdee-set-variables
 `(jdee-compile-option-directory ,(let ((src-file (buffer-file-name)))
                                    (expand-file-name (if (and src-file (string-match-p "/test/" src-file))
                                                          "build/classes/test"
                                                        "build/classes/main") jdee-gradle-project-root)))
 )

;;
;; Javadoc
;;
(jdee-set-variables 
 `(jdee-jdk-doc-url ,(format "http://docs.oracle.com/javase/%s/docs/api/index.html" (jdee-java-minor-version)))
 `(jdee-help-docsets '((nil ,(format "http://docs.oracle.com/javase/%s/docs/api" (jdee-java-minor-version))
                            ,(format "1.%s" (jdee-java-minor-version)))
                       ("TestNG" "http://testng.org/javadocs" nil)
                       ("JUnit" "http://junit.org/junit4/javadoc/latest" nil)
                       ("Example" ,(concat "file://" (expand-file-name "build/docs/javadoc" p)) nil)))
 )

;;
;; Coding style
;;
(jdee-set-variables 
 ;; Organize imports
 `(jdee-import-auto-sort t)
 `(jdee-import-auto-sort-function 'jdee-import-organize)
 `(jdee-import-group-of-rules '(("^com\\.mycorp" . "00_mycorp")
                                ("^mycorp" . "10_mycorp")
                                ("^com" . "20_com")
                                ("^org" . "30_org")
                                ("^java" . "99_java")
                                (".+" . "90_other")
                                ))
 `(jdee-import-default-group-name "90_other")
 `(jdee-import-sorted-groups 'gor)
 ;; Wizards and generated code
 `(jdee-gen-final-methods nil)
 `(jdee-wiz-tostring-prefix "{")
 `(jdee-wiz-tostring-postfix "}")
 `(jdee-wiz-tostring-static-members nil)
 `(jdee-javadoc-version-tag-template nil)
 `(jdee-javadoc-author-tag-template nil)
 )

;;
;; Debugging
;; Want to use JDIBug, but that isn't directly supported, so declare JDEbug as the next best thing
;;
(let ((port "5005")                   ;default for IntelliJ
      (heap 1024))
  (jdee-set-variables
   `(jdee-db-option-heap-size '((,heap . "megabytes") 
                                (,heap . "megabytes")))
   `(jdee-db-option-verbose   '(nil t nil)) ;print GC mesages
   `(jdee-debugger '("JDEbug"))
   `(jdee-db-option-connect-socket '(nil ,port))
   `(jdee-run-option-debug '("Server" "Socket" nil nil ,port t))
   `(jdee-bug-server-socket '(t ,port))
   `(jdee-bug-debugger-host-address "localhost")
   `(jdibug-connect-hosts '(,(concat "localhost:" port)))
   ))

;;
;; Other tools
;;
(jdee-set-variables
 `(jdee-checkstyle-style (expand-file-name "src/main/config/checkstyle/checkstyle.xml" jdee-gradle-project-root))
 `(jdee-xref-db-base-directory ,(expand-file-name "build/jdee" jdee-gradle-project-root))
 `(jdee-xref-store-prefixes '("com.mycorp"))
 )
