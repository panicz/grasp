(define c (Circle left: 10 top: 5 radius: 3 color: #xff3377))

   (Button label: "↑"
action: (lambda ()
  (set! c:top (max (+ c:radius 1)
   (- c:top 1))))) (Button label: "+"
action: (lambda ()
  (set! c:radius (+ c:radius 1)))) 
(Button label: "←"
action: (lambda ()
  (set! c:left (max (+ c:radius 1)
   (- c:left 1))))) (Button label: "→"
action: (lambda ()
  (set! c:left (+ c:left 1)))) 
   (Button label: "↓"
action: (lambda ()
  (set! c:top (+ c:top 1)))) (Button label: "-"
action: (lambda ()
  (set! c:radius (- c:radius 1)))) 

(PreciseCanvas 60 30 (list c))

