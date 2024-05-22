(module-name (extra tile-board))

(import (language define-interface))
(import (language define-type))
(import (language define-object))
(import (language define-syntax-rule))
(import (language infix))
(import (language match))
(import (language for))
(import (language while))
(import (language assert))
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
    (WARN "board release")
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

  (define (typename)::String "LetterTile")

  (define (common-fields->string)::String
    (string-append
     " left: "(number->string left)
     " right: "(number->string right)
     " width: "(number->string outer:width)
     " height: "(number->string outer:height)))
  
  (define (fields->string)::String
    (string-append
     " label: "label
     (common-fields->string)))
  
  (Magic))

(define-object (LetterTileSlot content::LetterTile)

  (define (draw-content!)::void
    (if content
	(content:draw-content!)
	(invoke-special LetterTile (this) 'draw-border!)))

  (define (typename)::String "LetterTileSlot")

  (define (fields->string)::String
    (string-append
     " content: "(if content (content:toString) "#!null")
     (invoke-special LetterTile (this) 'common-fields->string)))
  
  (LetterTile #\#))

(define-object (LetterTileBoard solution ::string)::Maximizable
  (define size ::Extent
    (let* ((slot ::Tile (LetterTileSlot #!null))
	   (extent ::Extent (slot:extent)))
      (Extent width: (* 5 extent:width)
	      height: (* 5 extent:height))))

  (define (extent)::Extent size)

  (define (set-size! width::real height::real)::void
    (set! size:width width)
    (set! size:height height)
    (arrange-slots-and-tiles!))

  (define scattered-tiles ::($bracket-apply$
			     java.util.LinkedList
			     LetterTile)
    (java.util.LinkedList))

  (define tile-slots ::java.util.List
    (java.util.ArrayList))

  (define word-break-indices ::java.util.LinkedList
    (java.util.LinkedList))
  
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
    (WARN "board pressed")
    (escape-with return
      (for tile ::LetterTile in scattered-tiles
	   (WARN"testing "tile" against "x" "y)
	   (when (tile:below? x y)
	     (screen:drag! finger (DragLetterTile tile (this)))
	     (return #t)))
      (for slot ::LetterTileSlot in tile-slots
	   (WARN"testing "slot" against "x" "y)
	   (when (and (slot:below? x y)
		      slot:content)
	     (scattered-tiles:addLast slot:content)
	     (screen:drag! finger (DragLetterTile slot:content (this)))
	     (set! slot:content #!null)
	     (return #t)))
      (return #f)))

  (define random ::java.util.Random (java.util.Random))
  
  (define (arrange-slots-and-tiles!)::void
    (unless (tile-slots:isEmpty)
      (assert (not (word-break-indices:isEmpty)))

      (let* ((sample-slot ::LetterTileSlot (tile-slots 0))
	     (slot ::Extent (sample-slot:extent))
	     (max-line-width ::real
			     (floor
			      (/ (- size:width (* 2 slot:width))
				 slot:width)))
	     (words ::java.util.List (java.util.ArrayList))
	     (word-start ::int 0))
	
	(for word-end ::int in word-break-indices
	     (words:add (substring solution word-start word-end))
	     (set! word-start word-end))

	(let ((lines ::java.util.List (java.util.ArrayList))
	      (line ::java.util.List (java.util.ArrayList)))

	  (define (word-width word::string)::real
	    (* slot:width (string-length word)))
	  
	  (define (line-width line ::java.util.List)::real
	    (let ((width ::real (* slot:width
				   (max 0 (- (line:size)
					     1)))))
	      (for word ::string in line
		   (set! width (+ width (word-width word))))
	      width))
	  
	  (lines:add line)
	  
	  (for word ::string in words
	       (when (is (+ (word-width word)
			    (line-width line)) >= max-line-width)
		 (set! line (java.util.ArrayList))
		 (lines:add line))
	       (line:add word))

	  (let* ((interline ::real (ceiling (/ slot:height 2)))
		 (total-height ::real (+ (* slot:height (lines:size))
					 (* interline (- (lines:size) 1))))
		 (top ::real (floor (/ (- size:height total-height) 2)))
		 (slot-index ::int 0))
	    (for line ::java.util.List in lines
	      (let ((left (/ (- size:width 1 (line-width line)) 2)))
		(for word ::string in line
		     (for letter in word
		       (unless (char-whitespace? letter)
			 (let* ((slot ::LetterTileSlot (tile-slots
							slot-index))
				(e ::Extent (slot:extent)))
			   (set! slot:left left)
			   (set! slot:top top)
			   (set! left (+ left e:width))
			   (set! slot-index (+ slot-index 1)))))
		  (set! left (+ left slot:width))))
	      (set! top (+ top slot:height interline)))
	    (for tile ::LetterTile in scattered-tiles
		 (set! tile:left (* (random:nextFloat)
				    (- size:width slot:width)))
		 (set! tile:top (+ top
				   (* (random:nextFloat)
				      (- size:height top
					 slot:height))))))))))
  
  (define (setup-solution! utterance::string)::void
    (set! solution utterance)
    (tile-slots:clear)
    (scattered-tiles:clear)
    (word-break-indices:clear)
    
    (for character in utterance
      (cond
       ((char-whitespace? character)
	(word-break-indices:add (length tile-slots)))
       
       (else
	(tile-slots:add (LetterTileSlot #!null))
	(scattered-tiles:add (LetterTile character)))))
    
    (word-break-indices:add (length tile-slots))
    (arrange-slots-and-tiles!))
  
  (define (check-move!)::void
    (WARN "check-move! not implemented for LetterTileBoard"))

  (define (value)::Object
    (cons (Atom "LetterTileBoard") (empty)))
  
  (MaximizableWidget)
  (setup-solution! solution))
