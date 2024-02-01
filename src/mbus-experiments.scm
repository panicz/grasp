(import (srfi :11))
(import (kawa regex))
(import (language assert))
(import (language define-interface))
(import (language define-type))
(import (language define-object))
(import (language match))
(import (language infix))
(import (language while))
(import (language for))
(import (language examples))
(import (utils functions))
(import (utils binary))
(import (utils fixnum))
(import (utils affine))
(import (utils crypto))
(import (utils print))
(import (utils mbus))

;; long
(e.g.
 (wmbus-parse-frame
  (hex->list "7e 44 8f 41 60 63 14 41 02 04 7a 62 00 70 a5 a4 ca 79 6a e5 31 c3 d2 42 53 a6 aa f7 df b9 3e 83 99 7c ae 14 f0 e2 42 81 bf 27 af 65 7a ab 85 6a f6 84 d2 a3 7a 2d 1f 8a e6 f5 18 c4 60 4e b2 d2 dd 24 62 98 ec 73 3d 36 99 c8 e7 70 b5 7d ec 86 42 08 f9 f4 72 cb 2e 47 9c 90 1f ff 21 b7 30 73 c2 45 88 7c 72 2b 56 66 50 00 a7 fa 5e ae 48 bf 57 1f ee f4 e2 da 5d 48 ca 50 1e fd 24 76 0d")) ===>
  [WMBusFrame manufacturer: "PLO"
              serial-number: 41146360
              type: 04-HEAT-METER
              version: 2
              length: 126
              access-number: 98
              blocks: ([MBusBlock value: (3 0 0 0)
                                  type: C-BCD8
                                  unit: ENERGY-J
                                  scale: 10000000
                                  storage: 0 tariff: 0 subunit: 0
                                  DIB-size: 1 VIB-size: 1 data-size: 4 total-size: 6
                                  source: (12 15 3 0 0 0)]
                       [MBusBlock value: (145 48 24 49)
                                  type: 4-INT32
                                  unit: DATETIME-F
                                  scale: 1
                                  storage: 0 tariff: 0 subunit: 0
                                  DIB-size: 1 VIB-size: 1 data-size: 4 total-size: 6
                                  source: (4 109 145 48 24 49)]
                       [MBusBlock value: (16)
                                  type: 1-INT8
                                  unit: ERROR-FLAGS
                                  scale: 1
                                  storage: 0 tariff: 0 subunit: 0
                                  DIB-size: 1 VIB-size: 2 data-size: 1 total-size: 4
                                  source: (1 253 23 16)]
                       [MBusBlock value: (0)
                                  type: 1-INT8
                                  unit: ERROR-FLAGS
                                  scale: 1
                                  storage: 1 tariff: 0 subunit: 0
                                  DIB-size: 1 VIB-size: 2 data-size: 1 total-size: 4
                                  source: (65 253 23 0)]
                       [MBusBlock value: (0 0 0 0)
                                  type: C-BCD8
                                  unit: ENERGY-J
                                  scale: 10000000
                                  storage: 1 tariff: 0 subunit: 0
                                  DIB-size: 1 VIB-size: 1 data-size: 4 total-size: 6
                                  source: (76 15 0 0 0 0)]
                       [MBusBlock value: (0 0 0 0 0 0 0 0 0 0 ...)
                                  type: D-LVAR
                                  unit: ERROR-FLAGS
                                  scale: 1
                                  storage: 14 tariff: 0 subunit: 0
                                  DIB-size: 2 VIB-size: 2 data-size: 14 total-size: 19
                                  source: (141 7 253 23 14 0 0 0 0 0 ...)]
                       [MBusBlock value: (14 0 0 0 51 77 1 8 0 0 ...)
                                  type: D-LVAR
                                  unit: ENERGY-J
                                  scale: 10000000
                                  storage: 14 tariff: 0 subunit: 0
                                  DIB-size: 2 VIB-size: 1 data-size: 56 total-size: 60
                                  source: (141 7 15 56 14 0 0 0 51 77 ...)]
                       47 47 47 ...) source: (126 68 143 65 96 99 20 65 2 4 ...)])
