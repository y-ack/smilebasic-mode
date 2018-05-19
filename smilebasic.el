;;(defvar smilebasic-mode-hook nil)

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
  '("CONTINUE" "RESTORE" "ELSEIF" "COMMON" "LINPUT" "REPEAT" "RETURN" "ENDIF" "BREAK" "FALSE" "GOSUB" "INPUT" "PRINT" "UNTIL" "WHILE" "CALL(?:[\u0020\t](?:SP\\|BG))?" "DATA" "ELSE" "EXEC" "GOTO" "NEXT" "READ" "STOP" "SWAP" "THEN" "TRUE" "WEND" "DEC" "DEF" "DIM" "END" "FOR" "INC" "OUT" "REM" "USE" "VAR" "IF" "ON"))

(defvar smilebasic-functions
  '("EXTFEATURE" "BACKCOLOR" "BACKTRACE" "CLIPBOARD" "PCMSTREAM" "RANDOMIZE" "SPHITINFO" "BGMCLEAR" "BGMPAUSE" "BGSCREEN" "CHKLABEL" "CLASSIFY" "GYROSYNC" "HARDWARE" "MICSTART" "MILLISEC" "PRGNAME$" "RINGCOPY" "SPCOLVEC" "SPUNLINK" "TALKSTOP" "BGCOLOR" "BGCOORD" "BGMCONT" "BGMPLAY" "BGMPRGA" "BGMSETD" "BGMSTOP" "BGSCALE" "BGSTART" "BREPEAT" "BQPARAM" "CALLIDX" "CHKCALL" "CHKFILE" "DISPLAY" "DLCOPEN" "ERRLINE" "FADECHK" "FONTDEF" "FORMAT$" "FREEMEM" "GCIRCLE" "GPUTCHR" "MAINCNT" "MICDATA" "MICSAVE" "MICSIZE" "MICSTOP" "MPCOUNT" "MPLOCAL" "MPNAME$" "MPSTART" "PCMCONT" "PCMSTOP" "PRGEDIT" "PRGGET$" "PRGSIZE" "PRGSLOT" "PROJECT" "RGBREAD" "SNDSTOP" "SPCOLOR" "SPHITRC" "SPHITSP" "SPSCALE" "SPSTART" "STICKEX" "SYSBEEP" "TABSTEP" "TALKCHK" "UNSHIFT" "VISIBLE" "WAVSETA" "XSCREEN" "BGANIM" "BGCLIP" "BGCOPY" "BGFILL" "BGFUNC" "BGHIDE" "BGHOME" "BGLOAD" "BGMCHK" "BGMPRG" "BGMSET" "BGMVAR" "BGMVOL" "BGPAGE" "BGSAVE" "BGSHOW" "BGSTOP" "BIQUAD" "BUTTON" "CHKCHR" "CHKVAR" "DELETE" "DIALOG" "DTREAD" "EFCOFF" "EFCSET" "EFCWET" "ERRNUM" "ERRPRG" "FFTWFN" "GCOLOR" "GPAINT" "GSPOIT" "INKEY$" "LOCATE" "MICPOS" "MPHOST" "MPRECV" "MPSEND" "MPSTAT" "OPTION" "PCMPOS" "PCMVOL" "PRGDEL" "PRGINS" "PRGSET" "RENAME" "RESULT" "RIGHT$" "SCROLL" "SPANIM" "SPCLIP" "SPFUNC" "SPHIDE" "SPHOME" "SPLINK" "SPPAGE" "SPSHOW" "SPSTOP" "SPUSED" "SUBST$" "TMREAD" "VERSON" "WAVSET" "ACCEL" "ARYOP" "BGCHK" "BGCLR" "BGGET" "BGOFS" "BGPUT" "BGROT" "BGVAR" "COLOR" "DATE$" "EFCON" "FILES" "FLOOR" "GCLIP" "GCOPY" "GFILL" "GLINE" "GLOAD" "GPAGE" "GPRIO" "GPSET" "GSAVE" "GYROA" "GYROV" "INSTR" "LEFT$" "MPEND" "MPGET" "MPSET" "ROUND" "RSORT" "SHIFT" "SPCHK" "SPCHR" "SPCLR" "SPCOL" "SPDEF" "SPOFS" "SPROT" "SPSET" "SPVAR" "STICK" "TIME$" "TOUCH" "VSYNC" "WIDTH" "ACLS" "ACOS" "ASIN" "ATAN" "ATTR" "BEEP" "BIN$" "CEIL" "CHR$" "COPY" "COSH" "CSRX" "CSRY" "CSRZ" "FADE" "FILL" "GBOX" "GCLS" "GOFS" "GTRI" "HEX$" "IFFT" "LOAD" "MID$" "PUSH" "RNDF" "SAVE" "SINH" "SORT" "STR$" "TALK" "TANH" "WAIT" "XOFF" "ABS" "ASC" "CLS" "COS" "DEG" "EXP" "FFT" "KEY" "LEN" "LOG" "MAX" "MIN" "POW" "POP" "RAD" "RGB" "RND" "SGN" "SIN" "SQR" "TAN" "VAL" "XON"))

(defvar smilebasic-operators
  '("+" "-" "*" "/" "!" "<" ">" "&&" "||" "<=" ">=" "==" "!=" "<<" ">>"))
(defvar smilebasic-word-operators
  '("DIV" "MOD"  "AND" "OR" "XOR" "NOT"))

(defvar smilebasic-constants
  '("#BGROT180" "#BGROT270" "#SPROT180" "#SPROT270" "#TMAGENTA" "#BGROT90" "#FUCHSIA" "#MAGENTA" "#PVRIGHT" "#SPROT90" "#TMAROON" "#TPURPLE" "#TROT180" "#TROT270" "#TYELLOW" "#AOPADD" "#AOPCLP" "#AOPDIV" "#AOPLIP" "#AOPMAD" "#AOPMUL" "#AOPSUB" "#BGREVH" "#BGREVV" "#BGROT0" "#MAROON" "#PURPLE" "#PVLEFT" "#SILVER" "#SPREVH" "#SPREVV" "#SPROT0" "#SPSHOW" "#TBLACK" "#TGREEN" "#TOLIVE" "#TROT90" "#TWHITE" "#WFBLKM" "#WFHAMM" "#WFHANN" "#WFRECT" "#YELLOW" "#BLACK" "#BQAPF" "#BQBPF" "#BQBSF" "#BQHPF" "#BQHSF" "#BQLPF" "#BQLSF" "#BQPEQ" "#CHKUV" "#CHKXY" "#GREEN" "#OLIVE" "#RIGHT" "#SPADD" "#TBLUE" "#TCYAN" "#TGRAY" "#TLIME" "#TNAVY" "#TREVH" "#TREVV" "#TROT0" "#TTEAL" "#WHITE" "#FALSE" "#AQUA" "#BLUE" "#CHKC" "#CHKI" "#CHKR" "#CHKS" "#CHKV" "#CHKZ" "#CYAN" "#DOWN" "#GRAY" "#LEFT" "#LIME" "#NAVY" "#TEAL" "#TRED" "#TRUE" "#OFF" "#RED" "#YES" "#NO" "#ON" "#UP" "#ZL" "#ZR" "#A" "#B" "#L" "#R" "#X" "#Y"))

(defvar smilebasic-font-lock-defaults
  `((
     ("\"\\.\\*\\?[\"$]" . font-lock-string-face)
     ("[Dd][Ee][Ff]\\s*\\([\\w][\\w\\d]+\\)" . font-lock-function-name-face)
     ( ,(concat (regexp-opt smilebasic-constants t ) "\\b") . font-lock-constant-face)
     ( ,(regexp-opt smilebasic-operators nil) . font-lock-builtin-face)
     ( ,(regexp-opt smilebasic-word-operators 'words) . font-lock-builtin-face)
     ( ,(regexp-opt smilebasic-keywords 'symbols) . font-lock-keyword-face)
     ( ,(regexp-opt smilebasic-functions 'words) . font-lock-builtin-face)
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
	  (regexp-opt '("FOR" "REPEAT" "WHILE" "DEF")
              'symbols))
  "Regexp string of keywords that increase indentation.
These keywords increase indentation when found at the
beginning of a line.")

(defconst smilebasic-increase-indent-keywords-eol
  (regexp-opt '("THEN")
              'symbols)
  "Regexp string of keywords that increase indentation.
These keywords increase indentation when found at the
end of a line.")

(defconst smilebasic-decrease-indent-keywords-bol
  (concat "[ \t]*"
	  (regexp-opt '("ENDIF" "END" "NEXT" "WEND" "UNTIL" "ELSEIF")
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
		  (looking-at "[ \t]*\\(ELSEIF\\|IF\\|ELSE[ \t]+IF\\)[^:\r\n]+THEN[ \t]*$"))
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
    (setq tab-width smilebasic-tab-width)))

(add-to-list 'auto-mode-alist '("\\.PRG\\'" . smilebasic-mode))

(provide 'smilebasic-mode)
