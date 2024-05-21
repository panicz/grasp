(module-name (extra tile-board))

(import (language define-interface))
(import (language define-type))
(import (language define-object))
(import (language infix))
(import (language match))
(import (language for))
(import (language while))
(import (utils functions))
(import (language fundamental))

(import (editor interfaces painting))
(import (editor types primitive))
(import (editor interfaces elements))
(import (editor types texts))
(import (editor types spaces))
(import (editor document cursor))
(import (editor input transforms))
(import (editor input pane))

(import (editor types extensions extensions))
(import (utils print))

(define-object (DragLetterTile tile::LetterTile board::LetterTileBoard)::Drag
  (define (move! x::real y::real dx::real dy::real)::void
    (set! tile:left (+ tile:left dx))
    (set! tile:top (+ tile:top dy)))

  (define (drop! x::real y::real vx::real vy::real)::void
    (escape-with break
      (for slot ::LetterTileSlot in board:tile-slots
	   (when (and (slot:below? x y)
		      (not slot:content))
	     (set! slot:content tile)
	     (board:scattered-tiles:remove tile)
	     (board:check-move!))))))

(define-object (LetterTile content::gnu.text.Char)::Enchanted
  (define left ::real 0)
  (define top  ::real 0)

  (define label ::string (list->string `(,content)))

  (define inner ::Extent (painter:caption-extent label))

  (define outer ::Extent
    (let* ((horizontal-margin
	    ::real (painter:caption-horizontal-margin))
	   (top-margin ::real
		       (painter:caption-margin-top))
	   (bottom-margin ::real
			  (painter:caption-margin-bottom)))
      (Extent width: (+ inner:width (* horizontal-margin 2))
	      height: (+ inner:height (+ top-margin bottom-margin)))))

  (define (extent)::Extent
    outer)

  (define (draw-border!)::void
    (painter:draw-rounded-rectangle!
     outer:width outer:height))

  (define (draw-content!)::void
    (let* ((horizontal-margin
	    ::real (painter:caption-horizontal-margin))
	   (top-margin ::real
		       (painter:caption-margin-top)))
      (with-translation (horizontal-margin top-margin)
	(painter:draw-caption! label))))

  (define (draw! context::Cursor)::void
    (with-translation (left top)
      (draw-content!)))

  (define (below? x::real y::real)::boolean
    (and (is left <= x < (+ left outer:width))
	 (is top <= y < (+ top outer:height))))
  (Magic))

(define-object (LetterTileSlot content::LetterTile)

  (define (draw-content!)::void
    (if content
	(content:draw-content!)
	(invoke-special LetterTile (this) 'draw-border!)))

  (LetterTile #\#))

(define-object (LetterTileBoard)::Maximizable
  (define size ::Extent
    (let* ((slot ::Tile (LetterTileSlot #!null))
	   (extent ::Extent (slot:extent)))
      (Extent width: (* 5 extent:width)
	      height: (* 5 extent:height))))

  (define (extent)::Extent size)

  (define (set-size! width::real height::real)::void
    (set! size:width width)
    (set! size:height height))

  (define scattered-tiles ::($bracket-apply$
			     java.util.LinkedList
			     LetterTile)
    (java.util.LinkedList))

  (define tile-slots ::java.util.List
    (java.util.ArrayList))

  (define (draw! context::Cursor)::void
    (painter:draw-caption! "LetterTileBoard")
    (for slot ::LetterTileSlot in tile-slots
	 (slot:draw! context))
    (for tile ::LetterTile in scattered-tiles
	 (tile:draw! context)))

  (define (tap! finger::byte x::real y::real)::boolean
    (escape-with return
      (for tile ::LetterTile in scattered-tiles
	   (when (tile:below? x y)
	     ;;(tile:utter!)
	     (return #t)))
      (for slot ::LetterTileSlot in tile-slots
	   (when (and (slot:below? x y)
		      slot:content)
	     ;;(slot:content:utter!)
	     (return #t)))
      (return #f)))

  (define (press! finger::byte x::real y::real)::boolean
    (escape-with return
      (for tile ::LetterTile in scattered-tiles
	   (when (tile:below? x y)
	     (screen:drag! finger (DragLetterTile tile (this)))
	     (return #t)))
      (for slot ::LetterTileSlot in tile-slots
	   (when (and (slot:below? x y)
		      slot:content)
	     (scattered-tiles:addLast slot:content)
	     (screen:drag! finger (DragLetterTile slot:content (this)))
	     (set! slot:content #!null)
	     (return #t)))
      (return #f)))

  (define (check-move!)::void
    (WARN "check-move! not implemented for LetterTileBoard"))

  (define (value)::Object
    (cons (Atom "LetterTileBoard") (empty)))
  
  (MaximizableWidget))
