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

(defvar my-packages
  '(elpy
    flycheck
    material-theme
    spacemacs-theme
    spaceline
    py-autopep8
    neotree
    all-the-icons
    all-the-icons-dired
    ))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      my-packages)

;; BASIC CUSTOMIZATION
;; --------------------------------------
(setq inhibit-startup-message t) ;; hide the startup message
;(load-theme 'solarized-light t)
(load-theme 'spacemacs-dark t)
(require 'spaceline-config)
(spaceline-emacs-theme)

(global-linum-mode t) ;; enable line numbers globally
(set-face-attribute 'default nil :height 110)

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
(pyvenv-activate "/home/batuhan/anaconda3/envs/hmr-pytorch")
(elpy-enable)
;; a magic fix for elpy not seeing virtualenv:
;; https://emacs.stackexchange.com/questions/52652/elpy-doesnt-recognize-i-have-virtualenv-installed
(setq elpy-rpc-virtualenv-path 'current)

(setq elpy-rpc-backend "jedi")
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")

;; use flycheck not flymake with elpy:
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;;fix sp-convolute-sexp conflicting with xref-find-references
(custom-set-variables
 '(sp-override-key-bindings (quote (("M-?")))))

;;MAGIT
;(global-set-key (kbd "C-x g") 'magit-status)

;;RAZER BINDING
;; --------------------------------------
(global-set-key [XF86Tools] 'other-window) ;C-x o
(global-set-key [XF86Launch5] 'other-frame) ;C-x 5 o
(global-set-key [XF86Launch6] 'comment-region)
(global-set-key [XF86Launch7] 'uncomment-region)
(global-set-key [XF86Launch8] 'projectile-find-file)


;;MY COOL SCRIPTS
;; --------------------------------------

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

;; my RTX2080ti machine:
;; /ssh:batuhan@25.22.217.63:/home/batuhan
(defun connect-remote ()
  (interactive)
  (dired "/ssh:batuhan@25.22.217.63:/home/batuhan/Source"))

(defun connect-noisy-cricket ()
  (interactive)
  (dired "/ssh:batuhan@10.60.0.20:/home/batuhan/Source"))

(defun open-my-config ()
  "Open my-config.el"
  (interactive)
  (find-file "/home/batuhan/.emacs.d/personal/my-config.el"))
(defun reload-init ()
  (interactive)
  (load-file user-init-file))

;;UTF-8
(define-coding-system-alias 'UTF-8 'utf-8)

;;DIRED
;; --------------------------------------
(put 'dired-find-alternate-file 'disabled nil)

(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
(add-hook 'dired-mode-hook
          (lambda ()
            ;;(dired-omit-mode)
            (dired-hide-details-mode)
            ;; (dired-sort-toggle-or-edit)
            ))
(setq dired-listing-switches "-alh --group-directories-first") ;-lGXh
(defun mhj/dwim-toggle-or-open ()
  "Toggle subtree or open the file."
  (interactive)
  (if (file-directory-p (dired-get-file-for-visit))
      (progn
        (dired-subtree-toggle)
        (revert-buffer))
    (dired-find-file)))
(defun mhj/mouse-dwim-to-toggle-or-open (event)
  "Toggle subtree or the open file on mouse-click in dired."
  (interactive "e")
  (let* ((window (posn-window (event-end event)))
         (buffer (window-buffer window))
         (pos (posn-point (event-end event))))
    (progn
      (with-current-buffer buffer
        (goto-char pos)
        (mhj/dwim-toggle-or-open)))))
(use-package dired-subtree
  :demand
  :bind
  (:map dired-mode-map
        ("<enter>" . mhj/dwim-toggle-or-open)
        ("<return>" . mhj/dwim-toggle-or-open)
        ("<tab>" . mhj/dwim-toggle-or-open)
        ("<down-mouse-1>" . mhj/mouse-dwim-to-toggle-or-open))
  :config
  (progn
    ;; Function to customize the line prefixes (I simply indent the lines a bit)
    (setq dired-subtree-line-prefix (lambda (depth) (make-string (* 2 depth) ?\s)))
    (setq dired-subtree-use-backgrounds nil)))
(defun mhj/toggle-project-explorer ()
  "Toggle the project explorer window."
  (interactive)
  (let* ((buffer (dired-noselect (projectile-project-root)))
         (window (get-buffer-window buffer)))
    (if window
        (mhj/hide-project-explorer)
      (mhj/show-project-explorer))))
(defun mhj/show-project-explorer ()
  "Project dired buffer on the side of the frame.
Shows the projectile root folder using dired on the left side of
the frame and makes it a dedicated window for that buffer."
  (let ((buffer (dired-noselect (projectile-project-root))))
    (progn
      (display-buffer-in-side-window buffer '((side . left) (window-width . 0.15)))
      (set-window-dedicated-p (get-buffer-window buffer) t))))
(defun mhj/hide-project-explorer ()
  "Hide the project-explorer window."
  (let ((buffer (dired-noselect (projectile-project-root))))
    (progn
      (delete-window (get-buffer-window buffer))
      (kill-buffer buffer))))

(global-set-key [f8] 'mhj/toggle-project-explorer)

;; init.el ends here
