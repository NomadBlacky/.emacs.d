;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; El-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

(el-get-bundle use-package)
(el-get-bundle key-chord)
(el-get-bundle expand-region)
(el-get-bundle multiple-cursors)
(el-get-bundle popwin)
(el-get-bundle projectile)
(el-get-bundle emmet-mode)
(el-get-bundle recentf-ext)
(el-get-bundle magit)
(el-get-bundle open-junk-file)
(el-get-bundle google-translate)
(el-get-bundle anzu)
(el-get-bundle elscreen)
(el-get-bundle darkroom)
(el-get-bundle company)
(el-get-bundle helm)
(el-get-bundle helm-projectile)
(el-get-bundle helm-ag)
(el-get-bundle scala-mode)
(el-get-bundle markdown-mode)
(el-get-bundle markdown-preview-mode)
(el-get-bundle yaml-mode)
(el-get-bundle fish-mode)
(el-get-bundle kotlin-mode)
(el-get-bundle dockerfile-mode)
(el-get-bundle web-mode)
(el-get-bundle js2-mode)
(el-get-bundle smartrep)
(el-get-bundle linum-off)
(el-get-bundle tarao/el-get-lock)
(el-get-bundle twittering-mode)
(el-get-bundle tide)
(el-get-bundle swap-buffers)

;; Lock package versions.
(el-get-lock)

;; Local packages
(add-to-list 'load-path "~/.emacs.d/local/")
(require 'digdag-mode)

;; TODO: Replace package specify settings with use-package.el
(use-package helm
  :bind (("C-x C-f" . helm-find-files)))

(use-package open-junk-file
  :config
  (setq open-junk-file-format "~/Dropbox/Documents/notebook/memo/%Y-%m-%d-%H%M%S.md")
  :bind
  ("C-c j" . 'open-junk-file)
  )

(use-package markdown-preview-mode
  :config
  (setq markdown-preview-stylesheets
        (list
         "http://thomasf.github.io/solarized-css/solarized-dark.min.css"
         "~/.emacs.d/markdown-preview.css"
         ))
  :bind
  (:map markdown-mode-map
        ("C-c C-p" . markdown-preview-mode))
  )

(use-package twittering-mode
  :bind (("C-c C-t" . 'twittering-update-status-interactive)))

(use-package tide
  :config
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    ;; company is an optional dependency. You have to
    ;; install it separately via package-install
    ;; `M-x package-install [ret] company`
    (company-mode +1))
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save))
  :bind (("C-S-g" . tide-jump-to-definition))
  )

(use-package swap-buffers
  :bind ("C-c o" . swap-buffers))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Change the directory that save the backup files,
(setq backup-directory-alist
      (cons (cons ".*" (expand-file-name "~/.emacs.d/backup"))
	    backup-directory-alist))
(setq auto-save-file-name-transforms
        `((".*", (expand-file-name "~/.emacs.d/backup/") t)))

;; Disable the startup message.
(setq inhibit-startup-message t)

;; Open this file.
(defun open-init-el ()
  "Visiting '~/.emacs.d/init.el'."
  (interactive)
  (switch-to-buffer (find-file-noselect "~/.emacs.d/init.el")))
(bind-key "C-c C-i" 'open-init-el)

;; Share OS clipboard
(setq x-select-enable-clipboard t)

;; Don't use tabs
(setq-default indent-tabs-mode nil)

;; Enable delete-selection-mode
(delete-selection-mode t)

;; Quick Note
(defun create-scratch-buffer nil
   "Create a scratch buffer."
   (interactive)
   (switch-to-buffer (get-buffer-create "*Quick Note*"))
   (markdown-mode))
(key-chord-define-global "qn" 'create-scratch-buffer)

;; Sync X clipboard
(when (eq system-type 'gnu/linux)
  (setq interprogram-paste-function
        (lambda ()
          (shell-command-to-string "xsel -b -o")))
  (setq interprogram-cut-function
        (lambda (text &optional rest)
          (let* ((process-connection-type nil)
                 (proc (start-process "xsel" "*Messages*" "xsel" "-b" "-i")))
            (process-send-string proc text)
            (process-send-eof proc)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode / Package Specific Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; recentf-ext
(require 'recentf-ext)
(setq recentf-max-saved-items 500)

;; dired
(setq dired-dwim-target t)

;; helm
(bind-key "M-x" 'helm-M-x)
(bind-key "C-x C-f" 'helm-find-files)
(bind-key "C-x b" 'helm-buffers-list)
(bind-key "C-x f" 'helm-recentf)
(bind-key "M-y" 'helm-show-kill-ring)

;; company
(require 'company)
(global-company-mode)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 2)
(setq company-selection-wrap-around t)
(setq company-dabbrev-downcase nil)

(define-key company-active-map (kbd "M-n") nil)
(define-key company-active-map (kbd "M-p") nil)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-h") nil)

(defun company--insert-candidate2 (candidate)
  (when (> (length candidate) 0)
    (setq candidate (substring-no-properties candidate))
    (if (eq (company-call-backend 'ignore-case) 'keep-prefix)
        (insert (company-strip-prefix candidate))
      (if (equal company-prefix candidate)
          (company-select-next)
          (delete-region (- (point) (length company-prefix)) (point))
        (insert candidate))
      )))

(defun company-complete-common2 ()
  (interactive)
  (when (company-manual-begin)
    (if (and (not (cdr company-candidates))
             (equal company-common (car company-candidates)))
        (company-complete-selection)
      (company--insert-candidate2 company-common))))

(define-key company-active-map [tab] 'company-complete-common2)
(define-key company-active-map [backtab] 'company-select-previous)


;; emmet-mode
(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Use by markup languages.
(add-hook 'css-mode-hook  'emmet-mode) ;; Use by CSS.
(add-hook 'web-mode-hook  'emmet-mode)
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2))) ;; The indent is 2 spaces.
(define-key emmet-mode-keymap (kbd "C-j") 'emmet-expand-line) ;; Expand code with 'C-j'

;; linum-mode
(setq linum-format "%3d|")
(global-linum-mode t)
(require 'linum-off)

;; electric-pair-mode
(electric-pair-mode t)

;; global-auto-revert-mode
(global-auto-revert-mode t)

;; web-mode
(defun web-mode-hook ()
  "My web-mode hook."
  ;; offsets
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  ;; paddings
  (setq web-mode-script-padding 2)
  (setq web-mode-style-padding 2)
  
  (add-hook 'web-mode-hook 'web-mode-hook))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.page\\'" . web-mode))

;; js2-mode
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

;; java-mode
(add-to-list 'auto-mode-alist '("\\.cls\\'" . java-mode)) ;; Apex
(defun my-java-mode-setup ()
  (setq c-default-style "linux")
  (setq c-basic-offset 4)
  (c-set-offset 'arglist-intro '+)
  (c-set-offset 'arglist-close 0))
(add-hook 'java-mode-hook 'my-java-mode-setup)

;; scala-mode
(require 'scala-mode)
(add-to-list 'auto-mode-alist '("\\.sc\\'" . scala-mode)) ;; for Scala Scripts

;; magit
(require 'magit)
(unbind-key "M-m")
(bind-key "M-m M-s" 'magit-status)
(bind-key "M-m M-l" 'magit-log-all)
(bind-key "M-m M-f" 'magit-log-buffer-file)
;; foreground colors
(set-face-foreground 'magit-diff-context "unspecified")
(set-face-foreground 'magit-diff-context-highlight "unspecified")
(set-face-foreground 'magit-diff-added "#00ff00")
(set-face-foreground 'magit-diff-added-highlight "#00ff00")
(set-face-foreground 'magit-diff-removed "#ff0000")
(set-face-foreground 'magit-diff-removed-highlight "#ff0000")
(set-face-foreground 'magit-diff-hunk-heading "#eeeeaa")
(set-face-foreground 'magit-diff-hunk-heading-highlight "unspecified")
(set-face-foreground 'magit-diff-added "#00ff00")
(set-face-foreground 'magit-diff-added "#00ff00")
(set-face-foreground 'magit-diff-removed "#ff0000")
(set-face-foreground 'magit-diff-removed-highlight "#ff0000")
;; background colors
(set-face-background 'magit-diff-base "unspecified")
(set-face-background 'magit-diff-base-highlight "#333333")
(set-face-background 'magit-diff-context "unspecified")
(set-face-background 'magit-diff-context-highlight "#333333")
(set-face-background 'magit-diff-file-heading "#555533")
(set-face-background 'magit-diff-file-heading-highlight "#555533")
(set-face-background 'magit-diff-hunk-heading "#555533")
(set-face-background 'magit-diff-hunk-heading-highlight "#555533")
(set-face-background 'magit-diff-added "unspecified")
(set-face-background 'magit-diff-added-highlight "#333333")
(set-face-background 'magit-diff-removed "unspecified")
(set-face-background 'magit-diff-removed-highlight "#333333")

;; markdown-preview-mode
(setq markdown-command "marked")

;; google-translate
(bind-key "C-c C-g" 'google-translate-at-point)

;; popwin
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
(setq popwin:popup-window-position 'bottom)
(push '("Google Translate") popwin:special-display-config)

;; projectile
(projectile-global-mode)
(helm-projectile-on)

;; anzu
(global-anzu-mode +1)

;; multiple-cursors & smartrep
(require 'multiple-cursors)
(require 'smartrep)
(declare-function smartrep-define-key "smartrep")
(global-set-key (kbd "C-M-c") 'mc/edit-lines)
(global-set-key (kbd "C-M-r") 'mc/mark-all-in-region)
(global-unset-key "\C-t")
(smartrep-define-key global-map "C-t"
  '(("C-t"      . 'mc/mark-next-like-this)
    ("n"        . 'mc/mark-next-like-this)
    ("p"        . 'mc/mark-previous-like-this)
    ("m"        . 'mc/mark-more-like-this-extended)
    ("u"        . 'mc/unmark-next-like-this)
    ("U"        . 'mc/unmark-previous-like-this)
    ("s"        . 'mc/skip-to-next-like-this)
    ("S"        . 'mc/skip-to-previous-like-this)
    ("*"        . 'mc/mark-all-like-this)
    ("d"        . 'mc/mark-all-like-this-dwim)
    ("i"        . 'mc/insert-numbers)
    ("o"        . 'mc/sort-regions)
    ("O"        . 'mc/reverse-regions)))

;; elscreen
(setq elscreen-prefix-key (kbd "C-z"))
(elscreen-start)
(setq elscreen-tab-display-kill-screen nil)
(setq elscreen-tab-display-control nil)
(when (< (length (elscreen-get-screen-list)) 2)
  (elscreen-create))
(global-set-key (kbd "<C-tab>") 'elscreen-next)
(global-set-key (kbd "<C-S-iso-lefttab>") 'elscreen-previous)

;; key-chord
(setq key-chord-two-keys-delay 0.05)
(key-chord-mode t)

;; darkroom
(require 'darkroom)

;; menu-bar-mode
(menu-bar-mode -1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key Bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Global
;;
(bind-key* "C-o" 'other-window)

;;
;; Editing
;;
(bind-key* "C-h" 'backward-delete-char-untabify)
(bind-key "M-h" 'backward-kill-word)
(bind-key "C-c l" 'goto-line)
(bind-key "RET" 'newline-and-indent)
(bind-key "C-c C-c" 'comment-or-uncomment-region)
(bind-key "C-c r" 'rectangle-mark-mode)
(bind-key "C-:" 'er/expand-region)
(bind-key "C-c a" 'align-regexp)
(bind-key "C-c C-r" 'replace-regexp)

;;
;; Buffers
;;
(bind-key "<f5>" 'revert-buffer)
(bind-key "C-x C-k" 'kill-this-buffer)
(key-chord-define-global "xb" 'helm-buffers-list)

;;
;; Windows
;;
(bind-key "C-o" 'other-window)
(key-chord-define-global "w1" 'delete-other-windows)
(key-chord-define-global "w2" 'split-window-right)
(key-chord-define-global "w3" 'split-window-below)
(key-chord-define-global "w0" 'delete-window)

;;
;; Modes
;;
(bind-key "<f9>" 'linum-mode)

;;
;; Others
;;
(bind-key "<f12>" 'suspend-emacs)
(bind-key "<C-M-return>" 'shell-command)
(bind-key "M-RET" 'shell-command)
(bind-key "C-x C-c" 'kill-this-buffer)
(bind-key "C-x <f12>" 'save-buffers-kill-terminal)
(key-chord-define-global "dk" 'describe-key)
(key-chord-define-global "eb" 'eval-buffer)
(key-chord-define-global "df" 'describe-function)
(key-chord-define-global "pf" 'helm-projectile-find-file)
(key-chord-define-global "pd" 'helm-projectile-find-dir)
(key-chord-define-global "pg" 'helm-projectile-ag)

;; for Mac
(when (eq system-type 'darwin)
  (setq ns-command-modifier (quote meta)))

;; for GUI
(if window-system
    (progn
      (load-theme 'tango-dark)
      (set-frame-parameter nil 'alpha 97)
      ;; hl-line
      (global-hl-line-mode t)
      (set-face-background 'hl-line "#003300")
      (bind-key "C-@" 'er/expand-region)
      (unbind-key "C-\\") ; Disable the mozc key binding.
      (tool-bar-mode 0)
      (set-face-attribute 'default nil :family "Migu 1M" :height 140)
      ))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-keyword-face ((t (:foreground "Cyan"))))
 '(font-lock-type-face ((t (:foreground "dark orange" :weight bold :height 0.9))))
 '(helm-selection ((t (:background "dark olive green" :distant-foreground "black"))))
 '(hl-line ((t (:background "#666600"))))
 '(magit-branch-remote ((t (:foreground "lime green"))))
 '(magit-diff-context ((t (:foreground "#fff"))))
 '(magit-diff-context-highlight ((t (:background "#555555" :foreground "#ccc"))))
 '(magit-diff-hunk-heading-highlight ((t (:background "#555533" :foreground "#fff"))))
 '(magit-diff-hunk-heading-selection ((t (:inherit magit-diff-hunk-heading-highlight :foreground "chocolate"))))
 '(magit-section-heading ((t (:foreground "chocolate" :weight bold))))
 '(magit-section-highlight ((t (:background "dim gray"))))
 '(minibuffer-prompt ((t (:foreground "cyan"))))
 '(smerge-markers ((t (:background "dim gray"))))
 '(smerge-refined-removed ((t (:inherit smerge-refined-change :background "red3"))))
 '(smerge-upper ((t (:background "red4"))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "powder blue")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(clean-buffer-list-delay-special 86400)
 '(ensime-startup-notification nil)
 '(google-translate-default-source-language "en")
 '(google-translate-default-target-language "ja")
 '(js-indent-level 2)
 '(js-switch-indent-offset 2)
 '(package-selected-packages (quote (tide swap-buffers fish-mode darkroom company))))
