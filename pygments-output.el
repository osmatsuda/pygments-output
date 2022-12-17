;;; pygments-output.el ---                      -*- lexical-binding: t; -*-

;; Copyright (C) 2017  osmatsuda

;; Author: osmatsuda <osmatsuda@gmail.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Show formatted text by pygments (http://pygments.org) interactively.
;; Usege: Select a region, then M-x pygments-output.

;;; Code:

(require 'python)

;; cache for lexers and formatters
(defvar pygments-output-last-formatter-alias nil)
(defvar-local pygments-output-all-lexers nil)
(defvar-local pygments-output-all-formatters nil)

(defvar-local pygments-output-source-buffer nil
  "A buffer executed `pygments-output'")

(defun pygments-output--shell-commands-to-string (pycode)
  "Convert PYCODE to a shell command and execute the command.
Return its output that chomped any newline and blank from start and end.
If the output is blank, return nil."
  (let* ((command (concat "python -c " (shell-quote-argument pycode)))
         (result (string-trim (shell-command-to-string command))))
    (unless (string-empty-p result)
      result)))

(defun pygments-output--all-lexers ()
  "Return a list of lexer aliases."
  (or pygments-output-all-lexers
      (setq pygments-output-all-lexers
            (split-string (pygments-output--shell-commands-to-string
                           "from pygments.lexers import get_all_lexers
lxrs = [' '.join(v[1]) for v in get_all_lexers()]
print(' '.join(lxrs))")))))

(defun pygments-output--all-formatters ()
  "Return a list of formatter aliases."
  (or pygments-output-all-formatters
      (setq pygments-output-all-formatters
            (split-string (pygments-output--shell-commands-to-string
                           "from pygments.formatters import get_all_formatters
fmts = [' '.join(v.aliases) for v in get_all_formatters()]
print(' '.join(fmts))")))))

(defun pygments-output--guessed-lexer-name ()
  "Return a lexer name guessed from the current region content,
the current mode-name, or the buffer-file-name.
Default value is `text'."
  (or
   (pygments-output--shell-commands-to-string
    (concat "from pygments.lexers import guess_lexer
try:
    print(guess_lexer(bytes(["
	    (mapconcat #'number-to-string
		       (encode-coding-string (buffer-substring-no-properties
					      (region-beginning)
					      (region-end))
					     'utf-8)
		       ",")
	    "]).decode('utf-8')).aliases[0])
except:
    print('')"))
   (pygments-output--shell-commands-to-string
    (concat "from pygments.lexers import get_lexer_by_name
try:
    print(get_lexer_by_name('"
	    (substring-no-properties (format-mode-line mode-name))
	    "').aliases[0])
except:
    print('')"))
   (when (buffer-file-name)
     (string-trim (shell-command-to-string
		   (concat "pygmentize -N " (buffer-file-name)))))
   "text"))

(defun pygments-output--component-by-name (name type)
  "Return (MODULE . CLASS-NAME).
NAME may be a member of all-lexers or all-formatters.
TYPE should be a symbol: 'lexer or 'formatter."
  (unless (symbolp type)
    (error "TYPE should be a symbol"))
  (let* ((fn-name (concat "get_" (symbol-name type) "_by_name"))
	 (result
          (pygments-output--shell-commands-to-string
           (concat "from pygments." (symbol-name type) "s import " fn-name "
try:
    cmp = " fn-name "('" name "')
    print(cmp.__class__.__module__, cmp.__class__.__name__)
except:
    print('')"))))
    (unless result
      (error "No %s component for %s" (upcase (symbol-name type)) name))
    (apply #'cons (split-string result " "))))

(defun pygments-output--file-ext-of-last-formatter ()
  "Return a file extension which the last used formatter can produce.
Return nil the formatter produces none."
  (let ((result (pygments-output--shell-commands-to-string
                 (concat "from pygments.formatters import get_formatter_by_name
try:
    filenames = get_formatter_by_name('"
                         pygments-output-last-formatter-alias
                         "').filenames
    print(filenames and filenames[0] or '')
except:
    print('')"))))
    (when result
      (substring result 2))))

(defun pygments-output (start end)
  "Show pygments-output:source mode with the current region."
  (interactive "r")
  (when (= start end) (error "There is no region"))
  (let ((source-buffer (current-buffer))
        (pyg-buffer (get-buffer-create "*Pygments Source*"))
        (lexer-name (pygments-output--guessed-lexer-name))
	lexer
        (formatter-name (or pygments-output-last-formatter-alias "html"))
        formatter)
    (with-current-buffer pyg-buffer
      (erase-buffer)
      (setq buffer-undo-list nil)
      (pygments-output:source-mode)
      (setq lexer-name (completing-read
                        (concat "Lexer alias"
                                (unless (or (null lexer-name)
					    (string-empty-p lexer-name))
                                  (concat " (default " lexer-name ")"))
                                ": ")
                        (pygments-output--all-lexers)
                        nil t nil nil lexer-name)
	    lexer (pygments-output--component-by-name lexer-name 'lexer))
      (setq formatter-name (completing-read
                            (concat "Formatter alias"
                                    (concat " (default " formatter-name ")")
                                    ": ")
                            (pygments-output--all-formatters)
                            nil t nil nil formatter-name)
	    formatter (pygments-output--component-by-name formatter-name 'formatter))
      (setq pygments-output-last-formatter-alias formatter-name
	    pygments-output-source-buffer source-buffer)
      (newline)
      (insert "from pygments import highlight")
      (newline)
      (insert "from " (car lexer) " import " (cdr lexer))
      (newline)
      (insert "from " (car formatter) " import " (cdr formatter))
      (newline 2)
      (insert "code = \"\"\""
              (replace-regexp-in-string "\"\\{3\\}"
                                        "\\\\\"\\\\\"\\\\\""
                                        (with-current-buffer source-buffer
                                          (let (str)
                                            (untabify start end)
                                            (setq str
                                                  (buffer-substring-no-properties
                                                   start end))
                                            str)))
              "\"\"\"")
      (newline 2)
      (insert "print(highlight(code, " (cdr lexer) "(), " (cdr formatter) "()))")
      (newline))
    (select-window (display-buffer pyg-buffer))))

(defun pygments-output-exec ()
  "Execute the contents of *Pygments Source* buffer.
Then show the formattered result in *Pygments Result* buffer."
  (interactive)
  (unless (eq (current-buffer) (get-buffer "*Pygments Source*"))
    (error "Executed on wrong buffer: %s" (buffer-name)))
  (let ((tmp-file (python-shell--save-temp-file
		   (substring-no-properties (buffer-string))))
        (source-buffer pygments-output-source-buffer)
        (output-ext (pygments-output--file-ext-of-last-formatter))
        output-buffer)
    (if (string-match (rx bos (or "jpg" "gif" "png" "bmp") eos) output-ext)
        (let ((command (concat "python "
                               tmp-file " > "
                               (buffer-name source-buffer) "." output-ext)))
	  (call-process-shell-command command)
	  (message command))
      (setq output-buffer (get-buffer-create "*Pygments Result*"))
      (with-current-buffer output-buffer
        (erase-buffer)
        (setq buffer-undo-list nil)
        (pygments-output:result-mode)
        (setq pygments-output-source-buffer source-buffer)
        (insert (shell-command-to-string (concat "python " tmp-file))))
      (switch-to-buffer output-buffer))
    (delete-file tmp-file)))

(defun pygments-output-undo ()
  "Back to *Pygments Source* buffer from *Pygments Result* buffer."
  (interactive)
  (let ((pyg-buffer (get-buffer "*Pygments Source*")))
    (if pyg-buffer
	(unless (eq (current-buffer) pyg-buffer)
	  (switch-to-buffer pyg-buffer))
      (pygments-output-quit))))

(defun pygments-output-kill-ring-save ()
  "Save the content of *Pygments Result* to the kill ring by `kill-new'."
  (interactive)
  (unless (eq (current-buffer) (get-buffer "*Pygments Result*"))
    (error "Executed on a wrong buffer: %s" (buffer-name)))
  (when (> (buffer-size) 0)
    (kill-new (buffer-string))
    (message "Saved kill-ring: %s..."
	     (substring-no-properties (car kill-ring) 0
				      (min 25 (length (car kill-ring))))))
  (pygments-output-quit))

(defun pygments-output-save-file ()
  "Save the content of *Pygments Result* to a file."
  (interactive)
  (let ((pyg-result (get-buffer "*Pygments Result*")))
    (unless (eq (current-buffer) pyg-result)
      (error "Executed on a wrong buffer: %s" (buffer-name)))
    (with-temp-buffer
      (erase-buffer)
      (insert (with-current-buffer pyg-result
                (buffer-substring-no-properties (point-min) (point-max))))
      (basic-save-buffer)
      (kill-buffer))
    (pygments-output-quit)))

(defun pygments-output-quit ()
  "Kill *Pygments Source* and *Pygments Result* buffer, then back to the buffer
that you would highlight by pygments."
  (interactive)
  (let ((pyg-buffer (get-buffer "*Pygments Source*"))
        (result-buffer (get-buffer "*Pygments Result*"))
        (source-buffer pygments-output-source-buffer))
    (when result-buffer (kill-buffer result-buffer))
    (when pyg-buffer (kill-buffer pyg-buffer))
    (when (member source-buffer (buffer-list))
      (unless (eq (current-buffer) source-buffer)
        (if (get-buffer-window source-buffer)
	    (select-window (get-buffer-window source-buffer))
	  (and source-buffer (switch-to-buffer source-buffer)))))))

(define-derived-mode pygments-output:source-mode special-mode "pygments-output:source"
  :group 'pygments-output
  (setq buffer-read-only nil)
  (set-syntax-table python-mode-syntax-table)
  (setq-local syntax-propertize-function python-syntax-propertize-function)
  (setq font-lock-defaults '(python-font-lock-keywords))
  (use-local-map
   (let ((map (make-sparse-keymap)))
     (set-keymap-parent map python-mode-map)
     (define-key map (kbd "C-c C-c") 'pygments-output-exec)
     (define-key map (kbd "C-c C-q") 'pygments-output-quit)
     map))
  (setq header-line-format
        (concat " C-c C-c: "
                (propertize "Exec" 'face 'font-lock-type-face)
                ", C-c C-q: "
                (propertize "Quit" 'face 'font-lock-type-face))))

(define-derived-mode pygments-output:result-mode special-mode "pygments-output:result"
  :group 'pygments-output
  (setq buffer-read-only nil)
  (let ((file-ext (pygments-output--file-ext-of-last-formatter))
        parent-map)
    (pcase
	(cond ((string-match (rx bos (or "svg" "html") eos) file-ext)
	       'sgml-mode)
	      ((string-match (rx bos "tex" eos) file-ext)
	       'tex-mode)
	      (t 'text-mode))
      (`sgml-mode
       (require 'sgml-mode)
       (set-syntax-table (sgml-make-syntax-table sgml-specials))
       (setq-local syntax-propertize-function sgml-syntax-propertize-rules)
       (setq font-lock-defaults '((sgml-font-lock-keywords
                                   sgml-font-lock-keywords-1
                                   sgml-font-lock-keywords-2)))
       (setq parent-map sgml-mode-map))
      (`tex-mode
       (require 'tex-mode)
       (set-syntax-table tex-mode-syntax-table)
       (tex-common-initialization)
       (setq parent-map tex-mode-map))
      (`text-mode
       (set-syntax-table text-mode-syntax-table)
       (setq parent-map text-mode-map)))
    (use-local-map
     (let ((map (make-sparse-keymap)))
       (set-keymap-parent map parent-map)
       (define-key map (kbd "C-c C-w") 'pygments-output-save-file)
       (define-key map (kbd "C-c C-k") 'pygments-output-kill-ring-save)
       (define-key map (kbd "C-c C-l") 'pygments-output-undo)
       (define-key map (kbd "C-c C-q") 'pygments-output-quit)
       map)))
  (setq header-line-format
        (concat " C-c C-w: "
                (propertize "Save to" 'face 'font-lock-type-face)
		", C-c C-k: "
		(propertize "Kill new" 'face 'font-lock-type-face)
                ", C-c C-l: "
                (propertize "Undo" 'face 'font-lock-type-face)
                ", C-c C-q: "
                (propertize "Quit" 'face 'font-lock-type-face))))

(provide 'pygments-output)
;;; pygments-output.el ends here
