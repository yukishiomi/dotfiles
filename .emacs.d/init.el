;; P61 Elisp配置用のディレクトリを作成
;; load-pathを追加する関数を定義
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; 引数のディレクトリとそのサブディレクトリをload-pathに追加
;; (add-to-load-path "elisp" "conf" "public_repos")

;;; P63 Emacsが自動的に書き込む設定をcustom.elに保存する
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

(setq frame-title-format "%f")
(global-linum-mode t)
;; 背景色をgray24に変更
(set-face-background 'default "gray20")
;; 文字の色を白に変更
(set-face-foreground 'default "white")

