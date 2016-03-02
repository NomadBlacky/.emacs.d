;; Cask設定
(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; バックアップファイルを作成しない
(setq make-backup-files nil)

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
(add-to-list 'auto-insert-alist
	     '("\\.rb" . "ruby.rb"))
(setq auto-insert-directory "~/.emacs.d/auto-insert-templates/")

;; electric-pair-mode
(electric-pair-mode t)

;; global-auto-revert-mode
(global-auto-revert-mode t) ;; 自動再読み込み

;;
;; ruby ;;
;;

;; robe
(add-hook 'ruby-mode-hook 'robe-mode)

;; ruby-block
(require 'ruby-block)
(ruby-block-mode t)
(setq ruby-block-highlight-toggle t)

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
