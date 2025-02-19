(define s
  (PhysicalSphere
   center: (vector 10 5)
   radius: 3
   mass: 1.0
   velocity: (vector 0.001 0.01)
   color: #xff3377))

(PhysicsStage 40 20 (list s))

