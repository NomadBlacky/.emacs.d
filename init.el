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

;; Load local configurations
(add-to-load-path "./conf.d/")
(when (file-exists-p "./conf.d/local.el")
  (load-file "conf.d/local.el"))


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

;; Don't use tabs
(setq-default indent-tabs-mode nil)

;; Enable delete-selection-mode
(delete-selection-mode t)

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
(bind-key "C-x C-f" 'helm-find-files)
(bind-key "C-x b" 'helm-buffers-list)
(bind-key "C-x f" 'helm-recentf)

;; yas
(yas-global-mode 1)

;; auto-complete
;;(ac-config-default)
;;(bind-key "C-j" 'auto-complete)
;;(bind-key "C-n" 'ac-next ac-complete-mode-map)
;;(bind-key "C-p" 'ac-previous ac-complete-mode-map)
;;(bind-key "C-f" 'ac-complete ac-complete-mode-map)

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

;; hl-line
(global-hl-line-mode t)
(set-face-background 'hl-line "#003300")

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
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (add-hook 'web-mode-hook 'web-mode-hook))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.page\\'" . web-mode))

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
(global-set-key [zenkaku-hankaku] #'toggle-input-method)

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
(bind-key "C-c C-t" 'twittering-update-status-interactive)

;; python
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; markdown-preview-mode
(setq markdown-command "marked")

;; google-translate
(bind-key "C-c C-g" 'google-translate-at-point)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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
;;(desktop-save-mode t)

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
(when (and (boundp 'lv-elscreen-enabled) lv-elscreen-enabled)
  (setq elscreen-prefix-key (kbd "C-z"))
  (elscreen-start)
  (setq elscreen-tab-display-kill-screen nil)
  (setq elscreen-tab-display-control nil)
  (when (< (length (elscreen-get-screen-list)) 2)
    (elscreen-create))
  (global-set-key (kbd "<C-tab>") 'elscreen-next)
  (global-set-key (kbd "<C-S-iso-lefttab>") 'elscreen-previous))

;; open-junk-file
(setq open-junk-file-format "~/Dropbox/junk/%Y/%m/%Y-%m-%d-%H%M%S.")
(bind-key "C-c j" 'open-junk-file)

;; key-chord
(setq key-chord-two-keys-delay 0.05)
(key-chord-mode t)

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
(bind-key "C-c a" 'align-regexp)
(bind-key "C-c C-r" 'replace-regexp)

;;
;; Buffers
;;
(bind-key "<f5>" 'revert-buffer)
(bind-key "C-x C-k" 'kill-this-buffer)

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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-type-face ((t (:foreground "dark orange" :weight bold :height 0.9 :family "Verdana"))))
 '(helm-selection ((t (:background "dark olive green" :distant-foreground "black"))))
 '(magit-branch-remote ((t (:foreground "lime green"))))
 '(magit-diff-hunk-heading-selection ((t (:inherit magit-diff-hunk-heading-highlight :foreground "chocolate"))))
 '(magit-section-heading ((t (:foreground "chocolate" :weight bold))))
 '(magit-section-highlight ((t (:background "dim gray"))))
 '(minibuffer-prompt ((t (:foreground "cyan"))))
 '(smerge-markers ((t (:background "dim gray"))))
 '(smerge-mine ((t (:background "red4"))))
 '(smerge-refined-removed ((t (:inherit smerge-refined-change :background "red3"))))
 '(web-mode-html-tag-bracket-face ((t (:foreground "powder blue")))))
