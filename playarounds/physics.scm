(define s
  (PhysicalSphere
   center: (vector 10 5)
   radius: 8
   mass: 1.0
   velocity: (vector 0.001 0.01)
   color: #xff3377))

(define t
  (PhysicalSphere
   center: (vector 10 50)
   radius: 7
   mass: 1.0
   velocity: (vector 0.001 -0.01)
   color: #x3377ff))

(PhysicsStage 200 200 (list s t))

