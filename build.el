;; Minimal script to build the *.html files and export them
;; to the ./dist directory (Branch gh-pages).
;;
;; 
;;

(setq package-archives
      '(	
	;;("melpa" . "https://melpa.milkbox.net/packages/")
	;;("popkit" . "http://elpa.popkit.org/packages/")
	("melpa" . "https://melpa.org/packages/")		
	("gnu"       . "http://elpa.gnu.org/packages/")
	))

(package-initialize)


(defun packages-require (&rest packs)
  "Install and load a package. If the package is not available
   installs it automaticaly. 
  "
  (mapc  (lambda (package)
           (unless (package-installed-p package)
                   (package-install package)    
                   ;;#'package-require
                   ))

         packs))


(packages-require  'htmlize)

(require 'org)
(require 'htmlize)
(require 'ox-publish)
(require 'ox-html)


(require 'htmlize) ;; Required to colorize source codes.

(require 'tuareg)  ;; Required to add syntax highlight to OCaml code  

(require 'haskell-mode) ;; Required to add syntax highlight to Haskell code

;; (require 'clojure-mode) ;; Require do add syntax highlight to Clojure code.

(require 'fsharp-mode)


;; ====================================================== ;;


;; Don't format underscore to subscript when exporting. 
;;
(setq org-export-with-sub-superscripts nil)


(setq org-export-babel-evaluate nil)


(setq org-entities-user
      '(("lbr" "[" nil "[" "[" "[" "[") ; left square bracket
        ("rbr" "]" nil "]" "]" "]" "]"))) ; right square bracket


(setq org-src-fontify-natively t)

;; Htmlize output set by css 
(setq org-html-htmlize-output-type 'css)

;; (setq org-html-htmlize-output-type 'inline-css) ;; default

;;  Org-html htmlize font prefix
(setq org-html-htmlize-font-prefix "org-")


(defun insert-line (line)
  (insert (concat line "\n")))

(defun export-to-html ()
  (interactive)

  (let ((filename  (concat (file-name-base (buffer-file-name)) ".html")))

  ;; Go to top of buffer 
  (goto-char (point-min))

  (insert-line "#+INFOJS_OPT: view:info toc:t ltoc:t ftoc:nil mouse:underline button:nil path:/theme/org-info.js")
  (insert-line "#+HTML_HEAD: <link href='/theme/style.css' rel='stylesheet'>" )
  ;; (insert-line "#+STARTUP: content")
  
  ;; Export current buffer to Html 
  (org-html-export-as-html)

  ;; Delete file if exists in order to overwrite it.
  (if (file-exists-p filename)
      (delete-file filename))

  ;; Append buffer content to file.
  (append-to-file (point-min) (point-max) filename)

  ))

