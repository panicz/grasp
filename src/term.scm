(define-alias DefaultTerminalFactory
  com.googlecode.lanterna.terminal.DefaultTerminalFactory)

(define-alias TerminalPosition
  com.googlecode.lanterna.TerminalPosition)

(define-alias TerminalSize
  com.googlecode.lanterna.TerminalSize)

(define-alias MouseCaptureMode
  com.googlecode.lanterna.terminal.MouseCaptureMode)

(define-alias ExtendedTerminal
  com.googlecode.lanterna.terminal.ExtendedTerminal)

(define-alias Color
  com.googlecode.lanterna.TextColor)

(define-alias Terminal
  com.googlecode.lanterna.terminal.Terminal)

(define-alias LanternaScreen
  com.googlecode.lanterna.screen.Screen)

(define-alias TerminalScreen
  com.googlecode.lanterna.screen.TerminalScreen)

(define-alias KeyStroke
  com.googlecode.lanterna.input.KeyStroke)

(define-alias KeyType
  com.googlecode.lanterna.input.KeyType)

(define-alias MouseAction
  com.googlecode.lanterna.input.MouseAction)

(define-alias Character java.lang.Character)

(define-alias Letter
  com.googlecode.lanterna.TextCharacter)

(define-alias TextDecoration
  com.googlecode.lanterna.SGR)

(define-alias TextStyle java.util.EnumSet[TextDecoration])

(define-alias CTRL+C-behaviour
  com.googlecode.lanterna.terminal.ansi.UnixLikeTerminal:CtrlCBehaviour)

(define MouseButton:None ::int 0)
(define MouseButton:Left ::int 1)
(define MouseButton:Middle ::int 2)
(define MouseButton:Right ::int 3)
(define MouseButton:WheelUp ::int 4)
(define MouseButton:WheelDown ::int 5)

(define defaultTerminalFactory ::DefaultTerminalFactory
  (DefaultTerminalFactory))

(define (make-terminal
	 #!key
	 (mouse-capture-mode::MouseCaptureMode
	  MouseCaptureMode:CLICK_RELEASE_DRAG)
	 (ctrl+c-behaviour::CTRL+C-behaviour
	  CTRL+C-behaviour:TRAP))
  ::Terminal
  (defaultTerminalFactory:setMouseCaptureMode mouse-capture-mode)
  (defaultTerminalFactory:setUnixTerminalCtrlCBehaviour
    ctrl+c-behaviour)
  (defaultTerminalFactory:createTerminal))

(define (make-terminal-screen)::LanternaScreen
  (TerminalScreen (make-terminal)))
