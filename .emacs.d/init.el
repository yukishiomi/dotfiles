;; load-pathを追加する関数を定義
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

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

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)
; 保存しないファイルの正規表現
(setq desktop-files-not-to-save "\\(^/[^/:]*:\\|\\.diary$\\)")

(desktop-save-mode 1)
(require 'package)
(add-to-list 'package-archives '("marmalade" . "https://marmaladerepo.org/packages/"))
(add-to-list 'package-archives '("melpa"."https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/") t)

(package-initialize)

;; Helm
(require 'helm-config)

(require 'dired-x)
;; 引数のディレクトリとそのサブディレクトリをload-pathに追加
;; ( "elisp" "conf" "public_repos")

;; カスタムファイルを別ファイルにする
(setq custom-file (locate-user-emacs-file "custom.el"))
;; (カスタムファイルが存在しない場合は作成する
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
;; カスタムファイルを読み込む
(load custom-file)

(global-set-key (kbd "C-m") 'newline-and-indent)
;;  "C-t"でウィンドウを切り替える
(global-set-key (kbd "C-t") 'other-window)
;; 行番号を表示する
(column-number-mode t)

;;; tramp(remote)ファイルは復元しない
(setq save-visited-files-ignore-tramp-files t)
(turn-on-save-visited-files-mode)

(setq frame-title-format "%f")
;; 背景色をgray24に変更
(global-linum-mode t)
(set-face-background 'default "gray20")
;; 文字の色を白に変更
(set-face-foreground 'default "white")

;; バックアップとオートセーブファイルを~/.emacs.d/backupsに集める
(add-to-list 'backup-directory-alist (cons "." "~/.emacs.d/backups/"))
(setq auto-save-file-name-transforms `((".*", (expand-file-name "~/.emacs.d/backups/") t)))

(setq auto-save-timeout 15)
(global-auto-revert-mode t)

(global-set-key (kbd "C-M-o") 'helm-occur)

(load-theme 'zenburn t)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;; auto-compoleteの設定
(when (require 'auto-complete-config nil t)
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  (ac-config-default)
  (setq ac-use-menu-map t)
  (setq ac-ignore-case nil))

(when (require 'color-moccur nil t)
  (global-set-key (kbd "M-o") 'occur-by-moccur)
  ;;　スペース区切りでAND検索
  (setq moccur-split-word t)
  ;; ディレクトリ検索の時除外するファイル
  (add-to-list 'dmoccur-exclusion-mask "\\.DS_Store")
  (add-to-list 'dmoccur-exclusion-mask "^#.+#$"))

(when (require 'undohist nil t)
  (undohist-initialize))

(when (require 'undo-tree nil t)
  (global-set-key (kbd "C-'") 'undo-tree-redo)
  (global-undo-tree-mode))

(when (require 'elscreen nil t)
  (elscreen-start)
  (if window-system
      (define-key elscreen-map (kbd "C-z") 'iconify-or-deiconify-frame)
      (define-key elscreen-map (kbd "C-z") 'suspend-emacs)))

;; howmメモ保存の場所）
(setq howm-directory (concat user-emacs-directory "howm"))
;; howm-menuの言語を日本語に
(setq howm-menu-lang 'ja)
;; howmメモを一日一ファイルにする
					; (setq howm-file-name-format "%Y/%m/%Y-%m-%d.howm")

;; howm-modeを読み込む
(when (require 'howm-mode nil t)
  (global-set-key (kbd "C-c ,,") 'howm-menu))

(defun howm-save-buffer-and-kill ()
  (interactive)
  (when (and (buffer-file-name)
	     (howm-buffer-p))
    (save-buffer)
    (kill-buffer nil)))

(define-key howm-mode-map (kbd "C-c C-c") 'howm-save-buffer-and-kill)

;; cua-modeの設定
(cua-mode t)
(setq cua-enable-cua-keys nil)

(require 'typescript-mode)
(when (require 'web-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode)))

;; ruby-mode-hookにruby-electric-modeを追加
(add-hook 'ruby-mode-hook #'ruby-electirc-mode)

(add-hook 'after-init-hook #'global-flycheck-mode)

(require 'tide)
(when (require 'web-mode' nil t)
  ;; 自動的にweb-modeを起動したい拡張子を追加
  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode)))

;; rubyモードで挿入されるutf-8を取り消す
(setq ruby-insert-encoding-magic-comment nil)

(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)

(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "jsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
;; configure jsx-tide checker to run after your default jsx checker
(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)

(setq web-mode-enable-auto-pairing t)
(setq web-mode-enable-auto-closing t)
(defun my-web-mode-hook ()
	(setq web-mode-attr-indent-offset 2)
	(setq web-mode-markup-indent-offset 2)
	(setq web-mode-css-indent-offset 2)
	(setq web-mode-code-indent-offset 2)
	(setq indent-tabs-mode nil)
	(setq tab-width 2))
(add-hook 'web-mode-hook 'my-web-mode-hook)
;; 括弧の自動補完
(require 'smartparens)
(smartparens-global-mode t)
(setq-default sp-highlight-pair-overlay nil)    ;ハイライト機能削除

;; インデント
(electric-indent-mode t)
(setq-default tab-width 2)
;; gtags-modeのキーバインドを有効化				  
(setq gtags-suggested-key-mapping t)
;; ファイル保存人い自動的にタグをアップデート
(setq gtags-auto-update t)
(require 'ctags nil t)
(setq tags-revert-without-query t)
(setq ctags-command "ctags -R --fields=\"+afikKlmnsSzt\" ")


(setq rinari-tags-file-name "TAGS")

(require 'gtags)
(global-set-key "\M-t" 'helm-gtags-find-tag)
(global-set-key "\M-r" 'helm-gtags-find-rtag)
(global-set-key "\M-s" 'helm-gtags-find-symbol)
(global-set-key "\C-t" 'helm-gtags-pop-stack)

(when (require 'multi-term nil t)
  (setq multi-term-program "/bin/zsh"))

(add-to-list 'load-path "/usr/local/share/gtags")

(setenv "PATH" (concat ".:/usr/local/bin" (getenv "PATH")))
(add-to-list 'exec-path "/usr/local/bin/")

;; gnu global support
(add-to-list 'load-path "/usr/local/bin/global")

(when (require 'projectile nil t)
  (projectile-mode)
  (add-to-list
   'projectile-globally-ignored-directories "node-modules")
  (setq projectile-enable-caching t))

(when (require 'helm-projectile nil t)
  (setq projectile-completion-system 'helm))

