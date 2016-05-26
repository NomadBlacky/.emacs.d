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

;; emmet-mode
(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; マークアップ言語全部で使う
(add-hook 'css-mode-hook  'emmet-mode) ;; CSSにも使う
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2))) ;; indent はスペース2個
(eval-after-load "emmet-mode"
  '(define-key emmet-mode-keymap (kbd "C-j") nil)) ;; C-j は newline のままにしておく
(define-key emmet-mode-keymap (kbd "C-i") 'emmet-expand-line) ;; C-i で展開

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

;;
;; ruby ;;
;;

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
