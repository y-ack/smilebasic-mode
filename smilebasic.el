;;; smilebasic.el --- Major mode for SmileBASIC.

;; Author: Y <https://github.com/y-ack>
;; Keywords: highlight smilebasic
;; Version: 1.0.2

;; This is free and unencumbered software released into the public domain.

;; Anyone is free to copy, modify, publish, use, compile, sell, or
;; distribute this software, either in source code form or as a compiled
;; binary, for any purpose, commercial or non-commercial, and by any
;; means.

;; In jurisdictions that recognize copyright laws, the author or authors
;; of this software dedicate any and all copyright interest in the
;; software to the public domain. We make this dedication for the benefit
;; of the public at large and to the detriment of our heirs and
;; successors. We intend this dedication to be an overt act of
;; relinquishment in perpetuity of all present and future rights to this
;; software under copyright law.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.

;; For more information, please refer to <http://unlicense.org>

;;; Commentary:
;;; Major mode for SmileBASIC.  Supports syntax highlighting and auto-indentation.

;;; Code:

(defgroup smilebasic nil
  "SmileBASIC Source Mode"
  :prefix "smilebasic-mode-" :group nil
  :version "1.0")

(defvar smilebasic-mode-hook nil)

(defun number-to-bytes (num n)
  (set 'bytes (last (cdr (split-char num)) n))
  (concat (make-list (- n (safe-length bytes)) 0)
	  bytes))
(defun smilebasic-header-pad (s n)
  (concat s
	  (make-string (- n (length s)) 0)))

(defun convert-from-sb-txt (begin end)
  "convert a TXT file with header/footer to nice text"
  (set 'smilebasic-header-magic
       (delete-and-extract-region begin (+ begin 2)))
  (set 'smilebasic-header-type
       (delete-and-extract-region begin (+ begin 1)))
  (set 'smilebasic-header-unused-1
       (delete-and-extract-region begin (+ begin 3)))
  (set 'smilebasic-header-icon
       (delete-and-extract-region begin (+ begin 1)))
  (set 'smilebasic-header-unused-2
       (delete-and-extract-region begin (+ begin 1)))
  (set 'smilebasic-header-size
       (delete-and-extract-region begin (+ begin 4)))
  (set 'smilebasic-header-year
       (delete-and-extract-region begin (+ begin 2)))
  (set 'smilebasic-header-month
       (delete-and-extract-region begin (+ begin 1)))
  (set 'smilebasic-header-day
       (delete-and-extract-region begin (+ begin 1)))
  (set 'smilebasic-header-hour
       (delete-and-extract-region begin (+ begin 1)))
  (set 'smilebasic-header-minute
       (delete-and-extract-region begin (+ begin 1)))
  (set 'smilebasic-header-second
       (delete-and-extract-region begin (+ begin 1)))
  (set 'smilebasic-header-compressed
       (delete-and-extract-region begin (+ begin 1)))
  (set 'smilebasic-header-nnid-1
       (delete-and-extract-region begin (+ begin 18)))
  (set 'smilebasic-header-nnid-2
       (delete-and-extract-region begin (+ begin 18)))
  (set 'smilebasic-header-blacklist-1
       (delete-and-extract-region begin (+ begin 4)))
  (set 'smilebasic-header-blacklist-2
       (delete-and-extract-region begin (+ begin 4)))
  (set 'smilebasic-header-unused-3
       (delete-and-extract-region begin (+ begin 16)))
  (set 'end (- end 80))
  (set 'smilebasic-footer (delete-and-extract-region (- end 20) end))
  (set 'end (- end 20)))

(defun convert-to-sb-txt (begin end buffer)
  "convert a SB source buffer to a PRG file with header/footer"
  (goto-char begin)
  (insert smilebasic-header-magic)
  (insert smilebasic-header-type)
  (insert smilebasic-header-unused-1)
  (insert smilebasic-header-icon)
  (insert smilebasic-header-unused-2)
  (insert smilebasic-header-size)
  (insert smilebasic-header-year)
  (insert smilebasic-header-month)
  (insert smilebasic-header-day)
  (insert smilebasic-header-hour)
  (insert smilebasic-header-minute)
  (insert smilebasic-header-second)
  (insert smilebasic-header-compressed)
  (insert smilebasic-header-nnid-1)
  (insert smilebasic-header-nnid-2)
  (insert smilebasic-header-blacklist-1)
  (insert smilebasic-header-blacklist-2)
  (insert smilebasic-header-unused-3)
  (goto-char (+ end 80))
  (insert smilebasic-footer)
  (+ end 100))

					; make smilebasic format available for behind the scenes conversion
(add-to-list 'format-alist
	     '(smilebasic-txt "sbfs PRG header/footer" "\0\0" convert-from-sb-txt convert-to-sb-txt t nil nil))

(defvar smilebasic-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?' "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\; "." table)
    (modify-syntax-entry ?\\ "w" table)
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?\w "w" table)
    (modify-syntax-entry ?, "." table)
    (modify-syntax-entry ?( "(" table)
			 (modify-syntax-entry ?[ "(" table)
					      (modify-syntax-entry ?) ")" table)
			 (modify-syntax-entry ?] ")" table)
    table)
  "Syntax table for smilebasic-mode")

(defvar smilebasic-tab-width 2 "Width of a tab in SmileBASIC mode (default 2)")

(defvar smilebasic-keywords
  '("CONTINUE" "RESTORE" "ELSEIF" "COMMON" "LINPUT" "REPEAT" "RETURN" "ENDIF" "BREAK" "FALSE" "GOSUB" "INPUT" "PRINT" "UNTIL" "WHILE" "CALL" "DATA" "ELSE" "EXEC" "GOTO" "NEXT" "READ" "STOP" "SWAP" "THEN" "TRUE" "WEND" "DEC" "DEF" "DIM" "END" "FOR" "INC" "OUT" "REM" "USE" "VAR" "IF" "ON"))

(defvar smilebasic-functions
  '("EXTFEATURE" "BACKCOLOR" "BACKTRACE" "CLIPBOARD" "PCMSTREAM" "RANDOMIZE" "SPHITINFO" "BGMCLEAR" "BGMPAUSE" "BGSCREEN" "CHKLABEL" "CLASSIFY" "GYROSYNC" "HARDWARE" "MICSTART" "MILLISEC" "PRGNAME$" "RINGCOPY" "SPCOLVEC" "SPUNLINK" "TALKSTOP" "BGCOLOR" "BGCOORD" "BGMCONT" "BGMPLAY" "BGMPRGA" "BGMSETD" "BGMSTOP" "BGSCALE" "BGSTART" "BREPEAT" "BQPARAM" "CALLIDX" "CHKCALL" "CHKFILE" "DISPLAY" "DLCOPEN" "ERRLINE" "FADECHK" "FONTDEF" "FORMAT$" "FREEMEM" "GCIRCLE" "GPUTCHR" "MAINCNT" "MICDATA" "MICSAVE" "MICSIZE" "MICSTOP" "MPCOUNT" "MPLOCAL" "MPNAME$" "MPSTART" "PCMCONT" "PCMSTOP" "PRGEDIT" "PRGGET$" "PRGSIZE" "PRGSLOT" "PROJECT" "RGBREAD" "SNDSTOP" "SPCOLOR" "SPHITRC" "SPHITSP" "SPSCALE" "SPSTART" "STICKEX" "SYSBEEP" "TABSTEP" "TALKCHK" "UNSHIFT" "VISIBLE" "WAVSETA" "XSCREEN" "BGANIM" "BGCLIP" "BGCOPY" "BGFILL" "BGFUNC" "BGHIDE" "BGHOME" "BGLOAD" "BGMCHK" "BGMPRG" "BGMSET" "BGMVAR" "BGMVOL" "BGPAGE" "BGSAVE" "BGSHOW" "BGSTOP" "BIQUAD" "BUTTON" "CHKCHR" "CHKVAR" "DELETE" "DIALOG" "DTREAD" "EFCOFF" "EFCSET" "EFCWET" "ERRNUM" "ERRPRG" "FFTWFN" "GCOLOR" "GPAINT" "GSPOIT" "INKEY$" "LOCATE" "MICPOS" "MPHOST" "MPRECV" "MPSEND" "MPSTAT" "OPTION" "PCMPOS" "PCMVOL" "PRGDEL" "PRGINS" "PRGSET" "RENAME" "RESULT" "RIGHT$" "SCROLL" "SPANIM" "SPCLIP" "SPFUNC" "SPHIDE" "SPHOME" "SPLINK" "SPPAGE" "SPSHOW" "SPSTOP" "SPUSED" "SUBST$" "TMREAD" "VERSON" "WAVSET" "ACCEL" "ARYOP" "BGCHK" "BGCLR" "BGGET" "BGOFS" "BGPUT" "BGROT" "BGVAR" "COLOR" "DATE$" "EFCON" "FILES" "FLOOR" "GCLIP" "GCOPY" "GFILL" "GLINE" "GLOAD" "GPAGE" "GPRIO" "GPSET" "GSAVE" "GYROA" "GYROV" "INSTR" "LEFT$" "MPEND" "MPGET" "MPSET" "ROUND" "RSORT" "SHIFT" "SPCHK" "SPCHR" "SPCLR" "SPCOL" "SPDEF" "SPOFS" "SPROT" "SPSET" "SPVAR" "STICK" "TIME$" "TOUCH" "VSYNC" "WIDTH" "ACLS" "ACOS" "ASIN" "ATAN" "ATTR" "BEEP" "BIN$" "CEIL" "CHR$" "COPY" "COSH" "CSRX" "CSRY" "CSRZ" "FADE" "FILL" "GBOX" "GCLS" "GOFS" "GTRI" "HEX$" "IFFT" "LOAD" "MID$" "PUSH" "RNDF" "SAVE" "SINH" "SORT" "STR$" "TALK" "TANH" "WAIT" "XOFF" "ABS" "ASC" "CLS" "COS" "DEG" "EXP" "FFT" "KEY" "LEN" "LOG" "MAX" "MIN" "POW" "POP" "RAD" "RGB" "RND" "SGN" "SIN" "SQR" "TAN" "VAL" "XON"))

(defvar smilebasic-operators
  '("+" "-" "*" "/" "!" "<" ">" "&&" "||" "<=" ">=" "==" "!=" "<<" ">>"))
(defvar smilebasic-word-operators
  '("DIV" "MOD" "AND" "OR" "XOR" "NOT"))

(defvar smilebasic-constants
  '("#BGROT180" "#BGROT270" "#SPROT180" "#SPROT270" "#TMAGENTA" "#BGROT90" "#FUCHSIA" "#MAGENTA" "#PVRIGHT" "#SPROT90" "#TMAROON" "#TPURPLE" "#TROT180" "#TROT270" "#TYELLOW" "#AOPADD" "#AOPCLP" "#AOPDIV" "#AOPLIP" "#AOPMAD" "#AOPMUL" "#AOPSUB" "#BGREVH" "#BGREVV" "#BGROT0" "#MAROON" "#PURPLE" "#PVLEFT" "#SILVER" "#SPREVH" "#SPREVV" "#SPROT0" "#SPSHOW" "#TBLACK" "#TGREEN" "#TOLIVE" "#TROT90" "#TWHITE" "#WFBLKM" "#WFHAMM" "#WFHANN" "#WFRECT" "#YELLOW" "#BLACK" "#BQAPF" "#BQBPF" "#BQBSF" "#BQHPF" "#BQHSF" "#BQLPF" "#BQLSF" "#BQPEQ" "#CHKUV" "#CHKXY" "#GREEN" "#OLIVE" "#RIGHT" "#SPADD" "#TBLUE" "#TCYAN" "#TGRAY" "#TLIME" "#TNAVY" "#TREVH" "#TREVV" "#TROT0" "#TTEAL" "#WHITE" "#FALSE" "#AQUA" "#BLUE" "#CHKC" "#CHKI" "#CHKR" "#CHKS" "#CHKV" "#CHKZ" "#CYAN" "#DOWN" "#GRAY" "#LEFT" "#LIME" "#NAVY" "#TEAL" "#TRED" "#TRUE" "#OFF" "#RED" "#YES" "#NO" "#ON" "#UP" "#ZL" "#ZR" "#A" "#B" "#L" "#R" "#X" "#Y"))

(defvar smilebasic-font-lock-defaults
  `((
     ("\"\\.\\*\\?[\"$]" . font-lock-string-face)
     ( ,(concat (regexp-opt smilebasic-constants t ) "\\b") . font-lock-constant-face)
     ( ,(regexp-opt smilebasic-operators nil) . font-lock-builtin-face)
     ( ,(regexp-opt smilebasic-word-operators 'words) . font-lock-builtin-face)
     ("\\bCALL[ \t]+\\(SPRITE\\|BG\\)\\b" . font-lock-keyword-face)
     ( ,(regexp-opt smilebasic-keywords 'symbols) . font-lock-keyword-face)
     ( ,(regexp-opt smilebasic-functions 'words) . font-lock-builtin-face)
     ("\\(?:\\b[Dd][Ee][Ff][ \t]+\\)\\([a-zA-Z_][a-zA-Z0-9_]*\\)" (1 font-lock-function-name-face))
     ("@\\w+" . font-lock-warning-face) ; labels (todo custom face classes?)
     ("\\b\\(?:[0-9]*\\.\\)?[0-9]+\\(?:E[+\\-]?[0-9]+\\)?\\#?\\|\\&H[0-9A-F]+\\|\\&B[01]+" . font-lock-constant-face)
     ) nil t))


;; Indentation
(defcustom smilebasic-indent-offset 2
  "*Specifies the indentation offset for `smilebasic-indent-line'.
Statements inside a block are indented this number of columns."
  :type 'integer
  :group 'smilebasic)

(defconst smilebasic-increase-indent-keywords-bol
  (concat "[ \t]*"
	  (regexp-opt '("FOR" "REPEAT" "WHILE" "DEF" "COMMON")
		      'symbols))
  "Regexp string of keywords that increase indentation.
These keywords increase indentation when found at the
beginning of a line.")

(defconst smilebasic-increase-indent-keywords-eol
  (concat "[ \t]*"
	  (regexp-opt '("THEN" "ELSE")
		      'symbols))
  "Regexp string of keywords that increase indentation.
These keywords increase indentation when found at the
end of a line.")

(defconst smilebasic-decrease-indent-keywords-bol
  (concat "[ \t]*"
	  (regexp-opt '("ENDIF" "END" "NEXT" "WEND" "UNTIL" "ELSEIF" "ELSE")
		      'symbols))
  "Regexp string of keywords that decrease indentation.
These keywords decrease indentation when found at the
beginning of a line or after a statement separator (:).")


(defun smilebasic-indent-line ()
  "Indent current line as basic script."
  (let ((indent (smilebasic-calculate-indent))
	beg shift-amt
	(old-pos (- (point-max) (point))))
    (beginning-of-line)
    (setq beg (point))
    (skip-chars-forward " \t")
    (if (looking-at smilebasic-decrease-indent-keywords-bol)
	(setq indent (max (- indent smilebasic-indent-offset))))
    (message "prev indent: %d" indent)
    (setq shift-amt (- indent (current-column)))
    (if (not (zerop shift-amt))
	(progn
	  (delete-region beg (point))
	  (indent-to indent)
	  (if (> (- (point-max) old-pos) (point))
	      (goto-char (- (point-max) old-pos)))))
    shift-amt))


(defun smilebasic-calculate-indent ()
  "Return appropriate indentation for the current line as basic code."
  (save-excursion
    (beginning-of-line)
    (current-indentation)
    (if (bobp)
	0
      (if (re-search-backward "^[ \t]*[^ \t\n\r]" nil t)
	  (if (or (looking-at smilebasic-increase-indent-keywords-bol)
		  (looking-at "[ \t]*\\(ELSEIF\\|IF\\|ELSE[ \t]+IF\\).+THEN[ \t]*\\('.*\\)?$"))
	      (+ (current-indentation) smilebasic-indent-offset)
	    (if (looking-at smilebasic-increase-indent-keywords-eol)
		(+ (current-indentation) smilebasic-indent-offset)
	      (current-indentation)))
	(current-indentation)))))


;; smilebasic mode

(define-derived-mode smilebasic-mode fundamental-mode "SmileBASIC"
  "Major mode for editing SmileBASIC programs"
  :syntax-table smilebasic-syntax-table
  (setq-local indent-line-function 'smilebasic-indent-line)
  (setq-local comment-start "'")
  (setq-local comment-end "")
  (setq-local font-lock-defaults smilebasic-font-lock-defaults)
  (when smilebasic-tab-width
    (setq tab-width smilebasic-tab-width))
  (setq-local smilebasic-header-magic "\x01\x00")
  (setq-local smilebasic-header-type "\x00")
  (setq-local smilebasic-header-unused-1 "\x00")
  (setq-local smilebasic-header-icon "\x00")
  (setq-local smilebasic-header-unused-2 "\x00\x00\x00")
  (setq-local smilebasic-header-size
	      (number-to-bytes (point-max) 4))
  (setq-local smilebasic-header-year
	      (number-to-bytes (string-to-number (format-time-string "%Y")) 2))
  (setq-local smilebasic-header-month
	      (number-to-bytes (string-to-number (format-time-string "%m")) 1))
  (setq-local smilebasic-header-day
	      (number-to-bytes (string-to-number (format-time-string "%d")) 1))
  (setq-local smilebasic-header-hour
	      (number-to-bytes (string-to-number (format-time-string "%l")) 1))
  (setq-local smilebasic-header-minute
	      (number-to-bytes (string-to-number (format-time-string "%M")) 1))
  (setq-local smilebasic-header-second
	      (number-to-bytes (string-to-number (format-time-string "%S")) 1))
  (setq-local smilebasic-header-compressed "\x00")
  (setq-local smilebasic-header-nnid-1
	      (smilebasic-header-pad "NAME" 18))
  (setq-local smilebasic-header-nnid-2
	      (smilebasic-header-pad "NAME" 18))
  (setq-local smilebasic-header-blacklist-1
	      (number-to-bytes 0 4))
  (setq-local smilebasic-header-blacklist-2
	      (number-to-bytes 0 4))
  (setq-local smilebasic-header-unused-3
	      (smilebasic-header-pad "" 16))
  (setq-local smilebasic-footer "emacs SmileBASICMode")
  )

(add-to-list 'auto-mode-alist '("\\.PRG\\'" . smilebasic-mode))

(provide 'smilebasic)

;;; smilebasic.el ends here
