(defconst FONT_SIZE 10)
(set-face-attribute 'default nil :family "Menlo" :height 140)
(set-fontset-font (frame-parameter nil 'font)
                  'japanese-jisx0208
                  (font-spec :family "Hiragino Kaku Gothic ProN"))
(add-to-list 'face-font-rescale-alist
             '(".*Hiragino Kaku Gothic ProN.*" . 1.2))

(cd "~/")

;;;;;;;;;;;;;;;;;;;;;;;; load path
(add-to-list 'load-path "~/.emacs.d/vendor/")
;;;;;;;;;;;;;;;;;;;;;;;; load path

;;;;;;;;;;;;;;;;;;;;;;;; MELPA
(defvar my-favorite-package-list
  '(highlight-indentation
    key-chord
    elscreen)
  "packages to be installed")
;;;;;;;;;;;;;;;;;;;;;;;; MELPA

;;;;;;;;;;;;;;;;;;;;;;;; packageの自動install
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(dolist (pkg my-favorite-package-list)
  (unless (package-installed-p pkg)
    (package-install pkg)))
;;;;;;;;;;;;;;;;;;;;;;;; packageの自動install


;;; バックアップファイルを作らない
(setq make-backup-files nil)
(setq auto-save-default nil)

;;;;;;;;;;;;;;;;;;;;;;;; 画面分割
;; "C-=" (C-S--), "C-|"で画面分割
(global-set-key (kbd "C-=") 'split-window-vertically)
(global-set-key (kbd "C-|") 'split-window-horizontally)

;; "C-S-hjkl"でウィンドウ移動
(global-set-key (kbd "C-S-h") 'windmove-left)
(global-set-key (kbd "C-S-j") 'windmove-down)
(global-set-key (kbd "C-S-k") 'windmove-up)
(global-set-key (kbd "C-S-l") 'windmove-right)

;;;;;;;;; save
(setq auto-save-default nil)

;;;;;;;;;;;;;;;;;;;;;;;; ruby mode
(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(add-to-list 'auto-mode-alist '("\\.cap$" . ruby-mode))

;;;;;;;;;;;;;;;;;;;;;;;; Color-theme:
(load-theme 'manoj-dark t)

;;;;;;;;;;;;;;;;;;;;;;;; 見た目の変更
;; メニューバー、ツールバー、スクロールバーを消す, Emacs23以降
(tool-bar-mode 0)
(scroll-bar-mode 0)
(horizontal-scroll-bar-mode 0)
(menu-bar-mode 0)


;; 現在行をハイライト
(global-hl-line-mode t)
(defface my-hl-line-face
  '((((class color) (background dark))  ; カラーかつ, 背景が dark ならば
     (:background "DarkSlateBlue" t))   ; 背景を黒に.
    (((class color) (background light)) ; カラーかつ, 背景が light でも
     (:background "DarkSlateBlue" t))   ; 背景を黒に.
    (t (:bold t)))
  "hl-line's my face")
(setq hl-line-face 'my-hl-line-face)

;; 行の折り返し
(require 'key-chord)
(key-chord-define-global "kl" 'toggle-truncate-lines)

;; 対応する括弧を表示させる
(show-paren-mode t)

;; 行番号・桁番号を表示
(line-number-mode t)
(column-number-mode t)

;; タブ、全角スペース、行末のスペースを見えるように (コメントを外すと改行が見えるように)
;;; インデント時にタブを使わないでスペースを使う
(setq-default tab-width 2 indent-tabs-mode nil)

;;(defface my-face-r-1 '((t (:background "gray15"))) nil)
(defface my-face-b-1 '((t (:background "gray"))) nil)
(defface my-face-b-2 '((t (:background "gray20"))) nil)
(defface my-face-u-1 '((t (:foreground "SteelBlue" :underline t))) nil)
;;(defvar my-face-r-1 'my-face-r-1)
(defvar my-face-b-1 'my-face-b-1)
(defvar my-face-b-2 'my-face-b-2)
(defvar my-face-u-1 'my-face-u-1)

(add-hook 'font-lock-mode-hook
          (lambda ()
            (font-lock-add-keywords
             nil
             '(("[ \t]+$" 0 my-face-u-1 append)
               ("\t" 0 'my-face-b-2 append)
               ("　" 0 my-face-b-1 append)
               ))))
;;;;;;;;;;;;;;;;;;;;;;;; 見た目の変更

;;;;;;;;;;;;;;;;;;;;;;;; elscreenの設定
(elscreen-start)
(global-set-key [C-S-right] 'elscreen-next)
(global-set-key (kbd "C->") 'elscreen-next)
(global-set-key [C-S-left]  'elscreen-previous)
(global-set-key (kbd "C-<") 'elscreen-previous)