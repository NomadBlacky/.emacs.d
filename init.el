;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Load the packages of downloaded with Cask.
(require 'cask "~/.cask/cask.el")
(cask-initialize)

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

;; Define a function that add the directories to load-path.
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
     (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
         (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
             (normal-top-level-add-subdirs-to-load-path))))))

;; Open the init.el with 'C-c C-i'
(defun open-init-el ()
  "Visiting '~/.emacs.d/init.el'."
  (interactive)
  (switch-to-buffer (find-file-noselect "~/.emacs.d/init.el")))
(bind-key "C-c C-i" 'open-init-el)

;; Insert current date and time.
(defun insert-date-time ()
  (interactive)
  (insert (format-time-string "%Y/%m/%d %H:%M:%S")))

;; Share clipboard
(setq x-select-enable-clipboard t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode / Package Specific Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; recentf-ext
(require 'recentf-ext)
(setq recentf-max-saved-items 100)

;; dired
(setq dired-dwim-target t)
(setq dired-toggle-window-size 30)
(bind-key "C-c C-p" 'dired-toggle)

;; helm
(bind-key "M-x" 'helm-M-x)
(bind-key "M-j" 'helm-mini)
(bind-key "C-x C-f" 'helm-find-files)
(bind-key "C-x b" 'helm-buffers-list)
(bind-key "C-c f" 'helm-recentf)

;; yas
(yas-global-mode 1)

;; auto-complete
(ac-config-default)
(bind-key "C-j" 'auto-complete)
(bind-key "C-n" 'ac-next ac-complete-mode-map)
(bind-key "C-p" 'ac-previous ac-complete-mode-map)
(bind-key "C-f" 'ac-complete ac-complete-mode-map)

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

;; hl-line
(hl-line-mode t)
(set-face-background 'hl-line "#004400")

;; auto-insert-mode
(auto-insert-mode 1)
(add-to-list 'auto-insert-alist '("\\.rb" . "ruby.rb"))
(add-to-list 'auto-insert-alist '("Gemfile" . "Gemfile"))
(setq auto-insert-directory "~/.emacs.d/auto-insert-templates/")

;; electric-pair-mode
(electric-pair-mode t)

;; global-auto-revert-mode
(global-auto-revert-mode t)

;; robe
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'robe-mode-hook 'ac-robe-setup)

;; ruby-block
(require 'ruby-block)
(ruby-block-mode t)
(setq ruby-block-highlight-toggle t)

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; web-mode
(defun web-mode-hook ()
  (setq web-mode-markup-indent-offset 4)
  (add-hook 'web-mode-hook 'web-mode-hook))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

;; scala-mode
(require 'scala-mode)
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime)
(setq ensime-completion-style 'auto-complete)

(defun scala/completing-dot-company ()
  (cond (company-backend
	 (company-complete-selection)
	 (scala/completing-dot))
	(t
	 (insert ".")
	 (company-complete))))

(defun scala/completing-dot-ac ()
  (insert ".")
  (ac-trigger-key-command t))

;; Interactive commands

(defun scala/completing-dot ()
  "Insert a period and show company completions."
  (interactive "*")
  (eval-and-compile (require 'ensime))
  (eval-and-compile (require 's))
  (when (s-matches? (rx (+ (not space)))
		    (buffer-substring (line-beginning-position) (point)))
    (delete-horizontal-space t))
  (cond ((not (and (ensime-connected-p) ensime-completion-style))
	 (insert "."))
	((eq ensime-completion-style 'company)
	 (scala/completing-dot-company))
	((eq ensime-completion-style 'auto-complete)
	 (scala/completing-dot-ac))))

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


;; mozc
(require 'mozc)
(set-language-environment "Japanese")
(setq default-input-method "japanese-mozc")
(prefer-coding-system 'utf-8)

;; twitter
(add-hook 'twittering-mode-hook
	  (lambda ()
	    (mapc (lambda (pair)
		    (let ((key (car pair))
			  (func (cdr pair)))
		      (define-key twittering-mode-map
			(read-kbd-macro key) func)))
		  '(("H" . twittering-home-timeline)
		    ("F" . twittering-friends-timeline)
		    ("R" . twittering-replies-timeline)
		    ("U" . twittering-user-timeline)
		    ("W" . twittering-update-status-interactive)))))
(setq twittering-use-master-password t)
(setq twittering-timer-interval 300)
(setq twittering-username "blac_k_ey")
(add-hook 'twittering-new-tweets-hook 'twittering-mention-notification-func)
(defun twittering-mention-notification-func ()
  "Send notification with 'notify-send'."
  (when (and twittering-username
	     (boundp 'twittering-new-tweets-statuses))
    (dolist (tweet twittering-new-tweets-statuses)
      (when (string-match-p
	     (format "@%s" twittering-username)
	     (alist-get 'text tweet))
	(start-process "twittering-notify" nil "notify-send"
		       "New Mention"
		       (alist-get 'text tweet))))))

;; python
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; markdown-preview-mode
(setq markdown-command "marked")

;; google-translate
(bind-key "C-c C-g" 'google-translate-at-point)
(custom-set-variables
 '(google-translate-default-source-language "en")
 '(google-translate-default-target-language "ja"))

;; popwin
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)
(setq popwin:popup-window-position 'bottom)
(push '("Google Translate") popwin:special-display-config)

;; edit-server
(edit-server-start)
(setq edit-server-new-frame nil)

;; projectile
(projectile-global-mode)
(helm-projectile-on)

;; ctags
;;(require 'ctags)
;;(setq tags-revert-without-query t)
;;(bind-key "<f7>" 'ctags-create-or-update-tags-table)
;;(bind-key "M-." 'ctags-search)

;; anzu
(global-anzu-mode +1)

;; desktop
(desktop-save-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key Bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Editing
;;

(bind-key "C-h" 'backward-delete-char-untabify)
(bind-key "M-h" 'backward-kill-word)
(bind-key "C-c l" 'goto-line)
(bind-key "RET" 'newline-and-indent)
(bind-key "C-c C-c" 'comment-or-uncomment-region)
(bind-key "C-c r" 'rectangle-mark-mode)
(bind-key "C-@" 'er/expand-region)

;;
;; Buffers
;;

(bind-key "C-z" 'next-buffer)
(bind-key "M-z" 'previous-buffer)
(bind-key "<f5>" 'revert-buffer)
(bind-key "C-c b" 'buffer-menu)

;;
;; Windows
;;
(bind-key "C-o" 'other-window)
(bind-key "C-c C-f" 'find-file-other-window)

;;
;; Modes
;;
(bind-key "<f9>" 'linum-mode)
(bind-key "C-c t" 'multi-term)

;;
;; Others
;;
(bind-key "<f12>" 'suspend-emacs)
(bind-key "<C-M-return>" 'shell-command)
(bind-key "M-RET" 'shell-command)

;; for GUI
(if window-system
    (progn
      (color-theme-initialize)
      (color-theme-ld-dark)
      (set-frame-parameter nil 'alpha 90)
      (bind-key "C-x C-c" 'kill-this-buffer)
      (bind-key "C-x <f12>" 'save-buffers-kill-terminal)
      (unbind-key "C-\\") ; Disable the mozc key binding.
      (tool-bar-mode 0)
      (add-to-list 'default-frame-alist '(font . "Migu 1M-10"))
      ))
