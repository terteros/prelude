;; init.el --- Emacs configuration

;; INSTALL PACKAGES
;; --------------------------------------

(require 'package)

;(add-to-list 'package-archives
;             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives
       '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(ein
    elpy
    flycheck
    material-theme
    spacemacs-theme
    spaceline
    py-autopep8
    neotree
    ))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)

;; BASIC CUSTOMIZATION
;; --------------------------------------

(setq inhibit-startup-message t) ;; hide the startup message
(load-theme 'solarized-dark t)
;(load-theme 'spacemacs-theme t)
;(load-theme 'spacemacs-dark t)
(require 'spaceline-config)
(spaceline-emacs-theme)

(global-linum-mode t) ;; enable line numbers globally
(set-face-attribute 'default nil :height 110)

;; MY PART
(display-splash-screen)
(remove-hook 'comint-output-filter-functions
             'comint-postoutput-scroll-to-bottom)
(desktop-save-mode 1)

(eval-after-load "savehist"
 '(add-to-list 'savehist-additional-variables 'command-history))
(savehist-mode 1)
(global-undo-tree-mode -1)

;; PYTHON CONFIGURATION
;; --------------------------------------


(setenv "WORKON_HOME" "/home/batuhan/anaconda3/envs")
;(setq elpy-rpc-python-command "/home/batuhan/anaconda3/bin/python")
(pyvenv-mode 1)
(pyvenv-activate "/home/batuhan/anaconda3/envs/bmenv")

(elpy-enable)

;; a magic fix for elpy not seeing virtualenv:
;; https://emacs.stackexchange.com/questions/52652/elpy-doesnt-recognize-i-have-virtualenv-installed
(setq elpy-rpc-virtualenv-path 'current)

(setq elpy-rpc-backend "jedi")

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")



;; Elpy Tricks
;; use flycheck not flymake with elpy
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;; enable autopep8 formatting on save
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;; use rgrep when elpy-goto-definition fails
(defun elpy-goto-definition-or-rgrep ()
  "Go to the definition of the symbol at point, if found. Otherwise, run `elpy-rgrep-symbol'."
  (interactive)
  (ring-insert find-tag-marker-ring (point-marker))
  (condition-case nil (elpy-goto-definition)
    (error (elpy-rgrep-symbol
            (concat "\\(def\\|class\\)\s" (thing-at-point 'symbol) "(")))))
(define-key elpy-mode-map (kbd "M-.") 'elpy-goto-definition-or-rgrep)

;; use rgrep when xref-find-references fails
(defun xref-find-references-or-rgrep ()
  "xref-find-references for the symbol at point, if found. Otherwise, run `elpy-rgrep-symbol'."
  (interactive)
  (ring-insert find-tag-marker-ring (point-marker))
  (condition-case nil (xref-find-references)
    (error (elpy-rgrep-symbol
            (thing-at-point 'symbol)))))
;;(define-key elpy-mode-map (kbd "M-?") 'xref-find-references-or-rgrep)


;;fix sp-convolute-sexp conflicting with xref-find-references
(custom-set-variables
 '(sp-override-key-bindings (quote (("M-?")))))

;;MAGIT
;(global-set-key (kbd "C-x g") 'magit-status)

;;RAZER BINDING
(global-set-key [XF86Tools] 'other-window) ;C-x o
(global-set-key [XF86Launch5] 'other-frame) ;C-x 5 o
(global-set-key [XF86Launch6] 'comment-region)
(global-set-key [XF86Launch7] 'uncomment-region)
(global-set-key [XF86Launch8] 'projectile-find-file)

;; Toggle window dedication
(defun toggle-window-dedicated ()
  "Toggle whether the current active window is dedicated or not"
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window
                                 (not (window-dedicated-p window))))
       "Window '%s' is dedicated"
     "Window '%s' is normal")
   (current-buffer)))



;; ;;AUCTEX
;; (setq TeX-auto-save t)
;; (setq TeX-parse-self t)
;; (setq-default TeX-master nil)

(defun respy ()
  "Restart python console before evaluate buffer or region to avoid various uncanny conflicts, like not reloding modules even when they are changed"
  (interactive)
  (kill-process "Python")
  (sleep-for 0.1)
  (kill-buffer "*Python*")
  (elpy-shell-send-region-or-buffer))

;;UTF-8
(define-coding-system-alias 'UTF-8 'utf-8)


(put 'dired-find-alternate-file 'disabled nil)

;;NEOTREE
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
;; init.el ends here
