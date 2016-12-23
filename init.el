
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Cask設定
(require 'cask "~/.cask/cask.el")
(cask-initialize)

;;
;; backup の保存先を変更
;;
(setq backup-directory-alist
      (cons (cons ".*" (expand-file-name "~/.emacs.d/backup"))
	    backup-directory-alist))

(setq auto-save-file-name-transforms
        `((".*", (expand-file-name "~/.emacs.d/backup/") t)))

;; スタートアップ画面を表示しない
(setq inhibit-startup-message t)

;;; load-pathを追加する関数を定義
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
     (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
         (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
             (normal-top-level-add-subdirs-to-load-path))))))

;;; ディレクトリをサブディレクトリごとload-pathに追加
;;(add-to-load-path "elisp")

(defun open-init-el ()
  "Visiting '~/.emacs.d/init.el'."
  (interactive)
  (switch-to-buffer (find-file-noselect "~/.emacs.d/init.el")))
(bind-key "C-c C-i" 'open-init-el)

;; recentf-ext
(require 'recentf-ext)
(setq recentf-max-saved-items 100)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; モード設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; yas
(yas-global-mode 1)

;; auto-complete
(ac-config-default)
(bind-key "C-n" 'ac-next ac-complete-mode-map)
(bind-key "C-p" 'ac-previous ac-complete-mode-map)
(bind-key "C-f" 'ac-complete ac-complete-mode-map)

;; emmet-mode
(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; マークアップ言語全部で使う
(add-hook 'css-mode-hook  'emmet-mode) ;; CSSにも使う
(add-hook 'web-mode-hook  'emmet-mode)
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2))) ;; indent はスペース2個
(define-key emmet-mode-keymap (kbd "C-j") 'emmet-expand-line) ;; C-j で展開

;; linum-mode
(setq linum-format "%3d|")
(global-linum-mode t)

;; auto-insert-mode
(auto-insert-mode 1)
(add-to-list 'auto-insert-alist '("\\.rb" . "ruby.rb"))
(add-to-list 'auto-insert-alist '("Gemfile" . "Gemfile"))
(setq auto-insert-directory "~/.emacs.d/auto-insert-templates/")

;; electric-pair-mode
(electric-pair-mode t)

;; global-auto-revert-mode (Cygwin環境だとうまくいかない)
;;(global-auto-revert-mode t) ;; 自動再読み込み

;; robe
(add-hook 'ruby-mode-hook 'robe-mode)

;; ruby-block
(require 'ruby-block)
(ruby-block-mode t)
(setq ruby-block-highlight-toggle t)

;; 時刻を挿入する関数
(defun insert-date-time ()
  (interactive)
  (insert (format-time-string "%Y/%m/%d %H:%M:%S")))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; キーバインド設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 指定行にジャンプ
(bind-key "C-c l" 'goto-line)

;; linum-mode 切り替え
(bind-key "<f9>" 'linum-mode)

;; multi-term 起動
(bind-key "C-c t" 'multi-term)

;; サスペンド
(bind-key "<f12>" 'suspend-emacs)

;; C-z でバッファ切り替え
(bind-key "C-z" 'next-buffer)
(bind-key "M-z" 'previous-buffer)

;; C-o でウィンドウ切り替え
(bind-key "C-o" 'other-window)
 
;; C-M-Enter でシェルコマンド
(bind-key "<C-M-return>" 'shell-command)

;; M-Enter でシェルコマンド
(bind-key "M-RET" 'shell-command)

;; F5 でバッファ更新
(bind-key "<f5>" 'revert-buffer)

;; インデント
(define-key global-map (kbd "RET") 'newline-and-indent)

;; C-c f で最近開いたファイルを開く
(bind-key "C-c f" 'recentf-open-files)

;; C-c C-c でコメントアウト
(bind-key "C-c C-c" 'comment-or-uncomment-region)

;; C-c C-b バッファメニュー
(bind-key "C-c b" 'buffer-menu)

;; C-c r で矩形選択モード
(bind-key "C-c r" 'rectangle-mark-mode)

;; C-c C-f で別ウィンドウでファイルを開く
(bind-key "C-c C-f" 'find-file-other-window)

;; autocomplate
(bind-key "C-j" 'auto-complete)

;; expand-region
(bind-key "C-@" 'er/expand-region)


;; for GUI
(if window-system
    (progn
      (color-theme-initialize)
      (color-theme-ld-dark)
      (set-frame-parameter nil 'alpha 90)
      (bind-key "C-x C-c" 'kill-this-buffer)
      (tool-bar-mode 0)
      (add-to-list 'default-frame-alist '(font . "ricty-10"))
      ))

;; custom-set-variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(desktop-save-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'upcase-region 'disabled nil)

