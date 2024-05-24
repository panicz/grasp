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
    (and-let* ((slot ::LetterTileSlot
		     (or (find (lambda (slot ::LetterTileSlot)
				 (and (eq? slot:content #!null)
				      (slot:below? x y)))
			       board:tile-slots)
			 (and-let* ((slot intersection
					  (minimizing
					   (lambda (slot::LetterTileSlot)
					     (slot:intersection-extent tile))
					   board:tile-slots))
				    ((is intersection > 0.1)))
			   slot))))
      (set! slot:content tile)
      (set! tile:left slot:left)
      (set! tile:top slot:top)
      (board:scattered-tiles:remove tile)
      (board:check-move!))))

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
			  (painter:caption-margin-bottom))
	   (min-width ::real (+ inner:width (* horizontal-margin 2)))
	   (min-height ::real (+ inner:height
				 (+ top-margin bottom-margin)))
	   (side ::real (max min-width min-height)))
      (Extent width: side
	      height: side)))

  (define (extent)::Extent
    outer)

  (define (overlap-extent A-left ::real A-right ::real
			  B-left ::real B-right ::real)
    ::real
    (assert (is A-left < A-right))
    (assert (is B-left < B-right))
    (if (and (is A-left <= B-right)
	     (is A-right >= B-left))
	(let ((overlap (/ (min (- A-right B-left)
			       (- B-right A-left))
			  (- (min A-right B-right)
			     (max A-left B-left)))))
	  (DUMP overlap)
	  overlap)
	0))

  (define (intersection-extent other ::LetterTile)::real
    (let ((right (+ left outer:width))
	  (bottom (+ top outer:height))
	  (other-right (+ other:left other:outer:width))
	  (other-bottom (+ other:top other:outer:height)))
      (* (overlap-extent left right other:left other-right) 
	 (overlap-extent top bottom other:top other-bottom))))
  
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
      (draw-border!)
      (draw-content!)))

  (define (below? x::real y::real)::boolean
    (and (is left <= x < (+ left outer:width))
	 (is top <= y < (+ top outer:height))))

  (define (typename)::String "LetterTile")

  (define (common-fields->string)::String
    (string-append
     " left: "(number->string left)
     " top: "(number->string top)
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

(define-object (LetterTileBoard solution ::string
				say::procedure
				ask ::procedure)
  ::Maximizable
  (define size ::Extent
    (let* ((slot ::Tile (LetterTileSlot #!null))
	   (extent ::Extent (slot:extent)))
      (Extent width: (* 5 extent:width)
	      height: (* 5 extent:height))))

  (define (extent)::Extent size)

  (define (set-size! width::real height::real)::void
    (set! size:width width)
    (set! size:height height)
    (arrange-content!)
    (painter:request-redraw!))

  (define scattered-tiles ::($bracket-apply$
			     java.util.LinkedList
			     LetterTile)
    (java.util.LinkedList))

  (define tile-slots ::java.util.List
    (java.util.ArrayList))

  (define word-break-indices ::java.util.LinkedList
    (java.util.LinkedList))

  (define utter-solution ::LetterTile
    (LetterTile #\👂))

  (define obtain-new-solution ::LetterTile
    (LetterTile #\👄))
  
  (define (draw! context::Cursor)::void
    (utter-solution:draw! context)
    (obtain-new-solution:draw! context)
    (for slot ::LetterTileSlot in tile-slots
	 (slot:draw! context))
    (for tile ::LetterTile in-reverse scattered-tiles
	 (tile:draw! context)))
  
  (define (tap! finger::byte x::real y::real)::boolean
    (cond
     ((find (lambda (tile ::LetterTile)
	     (tile:below? x y))
	   scattered-tiles)
      => (lambda (tile ::LetterTile)
	   (future (say tile:label))
	   #t))

     ((utter-solution:below? x y)
      (future (say solution))
      #t)

     ((obtain-new-solution:below? x y)
      (future
       (and-let* ((new-solution (ask "Podaj nowe słowo")))
	 (setup-solution! new-solution)))
      #t)
     
     ((and (any (lambda (slot::LetterTileSlot)
		  (isnt slot:content eq? #!null))
		tile-slots)
	   (any (lambda (slot::LetterTileSlot)
		  (slot:below? x y))
		tile-slots))
      (future (say (utterance)))
      #t)

     (else
      #f)))

  (define (press! finger::byte x::real y::real)::boolean
    (cond
     ((find (lambda (tile ::LetterTile)
	     (tile:below? x y))
	   scattered-tiles)
      => (lambda (tile ::LetterTile)
	   (future (say tile:label))
	   (scattered-tiles:remove tile)
	   (scattered-tiles:addFirst tile)
	   (screen:drag! finger (DragLetterTile tile (this)))
	   #t))

     ((find (lambda (slot ::LetterTileSlot)
	     (and (slot:below? x y)
		      slot:content))
	   tile-slots)
      => (lambda (slot ::LetterTileSlot)
	   (future (say slot:content:label))
	   (scattered-tiles:addFirst slot:content)
	   (set! slot:content:left slot:left)
	   (set! slot:content:top slot:top)
	   (screen:drag! finger (DragLetterTile slot:content (this)))
	   (set! slot:content #!null)))
     (else
      #f)))
     
  (define random ::java.util.Random (java.util.Random))

  (define (utterance)::string
    (let ((word ::int 0)
	  (result ::string "")
	  (letter ::int 0))
      (for slot ::LetterTileSlot in tile-slots
	   (when (= letter (word-break-indices word))
	     (set! word (+ word 1))
	     (set! letter (+ letter 1))
	     (set! result (string-append result " ")))
	   (set! result (string-append result
				       (if slot:content
					   slot:content:label
					   " ")))
	   (set! letter (+ letter 1)))
      ;;(DUMP result)
      result))
  
  (define (arrange-content!)::void
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
	     (set! word-start (+ word-end 1)))
	
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
		 (half-screen ::real (/ size:width 2))
		 (top ::real (floor (/ (- size:height total-height) 2)))
		 (slot-index ::int 0))

	    (set! utter-solution:left
		  (floor (/ (- half-screen slot:width) 2)))
	    
	    (set! utter-solution:top
		  (floor (/ (- top slot:width) 2)))
	    
	    (set! obtain-new-solution:left
		  (ceiling (+ half-screen (/ (- half-screen slot:width) 2))))

	    (set! obtain-new-solution:top
		  (floor (/ (- top slot:width) 2)))
	    
	    (for line ::java.util.List in lines
	      (let ((left (ceiling (/ (- size:width 1 (line-width line)) 2))))
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
		  (set! left (ceiling (+ left slot:width)))))
	      (set! top (+ top slot:height interline)))
	    (for tile ::LetterTile in scattered-tiles
		 (set! tile:left (ceiling (* (random:nextFloat)
					     (- size:width slot:width))))
		 (set! tile:top (+ top
				   (ceiling
				    (* (random:nextFloat)
				       (- size:height top
					  slot:height)))))))))))
  
  (define (setup-solution! utterance::string)::void
    ;;(WARN "setting up "utterance)
    (set! solution utterance)
    (future (say solution))
    (tile-slots:clear)
    (scattered-tiles:clear)
    (word-break-indices:clear)

    (for i from 0 below (string-length utterance)
	 (let ((character (utterance i)))
	   
	   (cond
	    ((char-whitespace? character)
	     (word-break-indices:add i))
	    
	    (else
	     (tile-slots:add (LetterTileSlot #!null))
	     (scattered-tiles:add (LetterTile character))))))
    
    (word-break-indices:add (string-length utterance))
    (arrange-content!))
  
  (define (check-move!)::void
    ;;(WARN "check-move! not implemented for LetterTileBoard")
    (values))

  (define (value)::Object
    (cons (Atom "LetterTileBoard") (empty)))
  
  (MaximizableWidget)
  (setup-solution! solution))
