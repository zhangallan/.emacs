;;;;;;;;;; Copy command and run in stata

#s:: ; the shortcut (Windows key + s)
ClipSaved := ClipboardAll ; Save the entire clipboard to a variable of your choice.
Clipboard= ;empty the clipboard
send !w	; copy the highlighted region in Emacs
ClipWait, 4
WinGetActiveTitle, active_editor_title ; save title of current active window so we can switch back
; Replace /// (necessary in Stata for multiline commands) and line feeds:
StringReplace, clipboard, clipboard, ///`r`n, , All
clipboard := RegExReplace(clipboard, "\/\*(?:.|[\r\n])*?\*\/") ; Remove comments delimited by /* */ (helped by: http://blog.ostermiller.org/find-comment)
clipboard := RegExReplace(clipboard, "//.*") ; Remove comments delimited by //
;clipboard := RegExReplace(clipboard, "\R+\R", "`r`n") ; Remove blank lines
WinActivate, Stata/MP 14.0 ; Activate the Stata window
Send ^v ; Paste the clipboard (Ctrl-v)
Send {Enter} ; Execute the commands
; Jump back to previous window
WinActivate, %active_editor_title%
Clipboard := ClipSaved ; Restore the original clipboard. Note the use of Clipboard (not ClipboardAll).
ClipSaved = ; Free the memory in case the clipboard was very large.
return

;;;;;;;;;; Copy line and run in stata

#n:: ; the shortcut (Windows key + n)
ClipSaved := ClipboardAll ; Save the entire clipboard to a variable of your choice.
Clipboard= ;empty the clipboard
; copy the line in Emacs
Send jk
Send yy
ClipWait, 4
WinGetActiveTitle, active_editor_title ; save title of current active window so we can switch back
WinActivate, Stata/MP 14.0 ; Activate the Stata window
Send ^v ; Paste the clipboard (Ctrl-v)
Send {Enter} ; Execute the commands
; Jump back to previous window
WinActivate, %active_editor_title%
Clipboard := ClipSaved ; Restore the original clipboard. Note the use of Clipboard (not ClipboardAll).
ClipSaved = ; Free the memory in case the clipboard was very large.
return

;;;;;;;;;;; Send highlighted text to Stata help
#h::
ClipSaved := ClipboardAll ; Save the entire clipboard to a variable of your choice.
Clipboard= ;empty the clipboard
send !w	; copy the highlighted region in Emacs
ClipWait, 4

; Prepends help to the clipboard
ClipboardCommand := Clipboard
Clipboard := "help " . ClipboardCommand

WinActivate, Stata ; Activate the Stata window
Send ^v ; Paste the clipboard (Ctrl-v)
Send {Enter} ; Execute the commands
Clipboard := ClipSaved ; Restore the original clipboard. Note the use of Clipboard (not ClipboardAll).
ClipSaved = ; Free the memory in case the clipboard was very large.
return


;;;;;;;;;; Testing for fixing the regex. Found it. Bad quotes!!!!