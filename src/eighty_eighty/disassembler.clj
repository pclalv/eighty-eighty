(ns eighty-eighty.disassembler)

(def all-opcodes
  "master list generated from http://www.emulator101.com/8080-by-opcode.html"
  (->> (-> "opcodes.txt"
           clojure.java.io/resource
           slurp
           clojure.string/split-lines)
       (map #(clojure.string/split % #"  "))
       (map #(zipmap [:opcode :instruction :size :flags :function] %))
       (map (fn [opcode]
              (if (:size opcode)
                (update opcode :size #(Integer/parseInt %))
                opcode)))
       (map #(update % :opcode read-string))))

(defn disassemble [file]
  (loop [pc 0]
    (print (format "%04x " pc))
    (let [opcode (nth file pc)]
      (case opcode
        0x00
        #_=> (let [] 
               (println "NOP")
               (recur (inc pc)))

        0x01
        #_=> (let [lsb (nth file (+ 1 pc))
                   msb (nth file (+ 2 pc))
                   d16 (+ (bit-shift-left msb 8)
                          lsb)]
               (println "LXI B," (format "#$%x" d16))
               (recur (+ 3 pc)))

        0x02
        #_=> (let []
               (println "STAX B")
               (recur (inc pc)))

        0x03
        #_=> (let []
               (println "INX B")
               (recur (inc pc)))

        0x04
        #_=> (let []
               (println "INR B")
               (recur (inc pc)))

        0x05
        #_=> (let []
               (println "DCR B")
               (recur (inc pc)))

        0x06
        #_=> (let [d8 (nth file (inc pc))]
               (println "MVI B," (format "#$%x" d8))
               (recur (+ 2 pc)))

        0x07
        #_=> (let []
               (println "RLC")
               (recur (inc pc)))

        0x09
        #_=> (let []
               (println "DAD B")
               (recur (inc pc)))

        0x08
        #_=> (let []
               (println (format "#$%x" opcode))
               (recur (inc pc)))

        0xa
        #_=> (let []
               (println "LDAX B")
               (recur (inc pc)))

        0xb
        #_=> (let []
               (println "DCX B")
               (recur (inc pc)))

        0x0c
        #_=> (let []
               (println "INR C")
               (recur (inc pc)))

        0x0d
        #_=> (let []
               (println "DCR C")
               (recur (inc pc)))

        0x0e
        #_=> (let [d8 (nth file (inc pc))]
               (println "MVI C, " (format "#$%x" d8))
               (recur (+ 2 pc)))

        0x0f
        #_=> (let []
               (println "RRC")
               (recur (inc pc)))

        0x10
        #_=> (let []
               (println (format "#$%x" opcode))
               (recur (inc pc)))

        0x11
        #_=> (let [lsb (nth file (+ 1 pc))
                   msb (nth file (+ 2 pc))
                   d16 (+ (bit-shift-left msb 8)
                          lsb)]
               (println "LXI D," (format "#$%x" d16))
               (recur (+ 3 pc)))

        0x12
        #_=> (let []
               (println "STAX D")
               (recur (inc pc)))

        0x13
        #_=> (let []
               (println "INX D")
               (recur (inc pc)))

        0x14
        #_=> (let []
               (println "INR D")
               (recur (inc pc)))

        0x15
        #_=> (let []
               (println "DCR D")
               (recur (inc pc)))

        0x16
        #_=> (let [d8 (nth file (inc pc))]
               (println "MVI D," (format "#$%x" d8))
               (recur (+ 2 pc)))

        0x18
        #_=> (let []
               (println (format "#$%x" opcode))
               (recur (inc pc)))

        0x19
        #_=> (let []
               (println "DAD D")
               (recur (inc pc)))

        0x1a
        #_=> (let []
               (println "LDAX D")
               (recur (inc pc)))

        0x1b
        #_=> (let []
               (println "DCX D")
               (recur (inc pc)))

        0x1c
        #_=> (let []
               (println "INR E")
               (recur (inc pc)))

        0x1d
        #_=> (let []
               (println "DCR E")
               (recur (inc pc)))

        0x1e
        #_=> (let [d8 (nth file (inc pc))]
               (println "MVI E," (format "#$%x" d8))
               (recur (+ 2 pc)))

        0x1f
        #_=> (let []
               (println "RAR")
               (recur (inc pc)))

        0x20
        #_=> (let []
               (println "RIM")
               (recur (inc pc)))

        0x21
        #_=> (let [lsb (nth file (+ 1 pc))
                   msb (nth file (+ 2 pc))
                   d16 (+ (bit-shift-left msb 8)
                          lsb)]
               (println "LXI H," (format "#$%x" d16))
               (recur (+ 3 pc)))

        0x22
        #_=> (let [lsb (nth file (+ 1 pc))
                   msb (nth file (+ 2 pc))
                   adr (+ (bit-shift-left msb 8)
                          lsb)]
               (println "SHLD" (format "$%x" adr))
               (recur (+ 3 pc)))

        0x23
        #_=> (let []
               (println "INX H")
               (recur (inc pc)))

        0x24
        #_=> (let []
               (println "INR H")
               (recur (inc pc)))

        0x25
        #_=> (let []
               (println "DCR H")
               (recur (inc pc)))

        0x26
        #_=> (let [d8 (nth file (inc pc))]
               (println "MVI H," (format "#$%x" d8))
               (recur (+ 2 pc)))

        0x27
        #_=> (let []
               (println "DAA")
               (recur (inc pc)))

        0x28
        #_=> (let []
               (println (format "#$%x" opcode))
               (recur (inc pc))) 

        0x29
        #_=> (let []
               (println "DAD H")
               (recur (inc pc)))

        0x2a
        #_=> (let [lsb (nth file (+ 1 pc))
                   msb (nth file (+ 2 pc))
                   adr (+ (bit-shift-left msb 8)
                          lsb)]
               (println "LHLD" (format "$%x" adr))
               (recur (+ 3 pc)))

        0x2b
        #_=> (let []
               (println "DCX H")
               (recur (inc pc)))

        0x2c
        #_=> (let []
               (println "INR L")
               (recur (inc pc)))

        0x2e
        #_=> (let [d8 (nth file (inc pc))]
               (println "MVI L," (format "#$%x" d8))
               (recur (+ 2 pc)))

        0x2f
        #_=> (let []
               (println "CMA")
               (recur (inc pc)))

        0x30
        #_=> (let []
               (println "SIM")
               (recur (inc pc)))

        0x31
        #_=> (let [lsb (nth file (+ 1 pc))
                   msb (nth file (+ 2 pc))
                   d16 (+ (bit-shift-left msb 8)
                          lsb)]
               (println "LXI SP," (format "#$%x" d16))
               (recur (+ 3 pc)))

        0x32
        #_=> (let [lsb (nth file (+ 1 pc))
                   msb (nth file (+ 2 pc))
                   adr (+ (bit-shift-left msb 8)
                          lsb)]
               (println "STA" (format "$%x" adr))
               (recur (+ 3 pc)))

        0x34
        #_=> (let []
               (println "INR M")
               (recur (inc pc)))

        0x35
        #_=> (let []
               (println "DCR M")
               (recur (inc pc)))

        0x36
        #_=> (let [d8 (nth file (inc pc))]
               (println "MVI M," (format "#$%x" d8))
               (recur (+ 2 pc)))

        0x37
        #_=> (let []
               (println "STC")
               (recur (inc pc)))

        0x38
        #_=> (let []
               (println (format "#$%x" opcode))
               (recur (inc pc)))

        0x39
        #_=> (let []
               (println "DAD SP")
               (recur (inc pc)))

        0x3a
        #_=> (let [lsb (nth file (+ 1 pc))
                   msb (nth file (+ 2 pc))
                   adr (+ (bit-shift-left msb 8)
                          lsb)]
               (println "LDA" (format "$%x" adr))
               (recur (+ 3 pc)))

        0x3b (let []
               (println "DCX SP")
               (recur (inc pc)))

        0x3c
        #_=> (let []
               (println "INR A")
               (recur (inc pc)))

        0x3d
        #_=> (let []
               (println "DCR A")
               (recur (inc pc)))

        0x3e
        #_=> (let [d8 (nth file (inc pc))]
               (println "MVI A," (format "#$%x" d8))
               (recur (+ 2 pc)))

        0x3f
        #_=> (let []
               (println "CMC")
               (recur (inc pc)))

        0x40
        #_=> (let []
               (println "MOV B,B")
               (recur (inc pc)))

        0x41
        #_=> (let []
               (println "MOV B,C")
               (recur (inc pc)))

        0x42
        #_=> (let []
               (println "MOV B,D")
               (recur (inc pc)))

        0x43
        #_=> (clojure.core/let []
               (clojure.core/println "MOV B,E")
               (recur (clojure.core/inc pc)))

        0x44
        #_=> (let []
               (println "MOV B,H")
               (recur (inc pc)))

        0x45
        #_=> (clojure.core/let []
               (clojure.core/println "MOV B,L")
               (recur (clojure.core/inc pc)))

        0x46
        #_=> (let []
               (println "MOV B,M")
               (recur (inc pc)))

        0x47
        #_=> (let []
               (println "MOV B,A")
               (recur (inc pc)))

        0x48
        #_=> (let []
               (println "MOV C,B")
               (recur (inc pc)))

        0x49
        #_=> (let []
               (println "MOV C,C")
               (recur (inc pc)))

        0x4a
        #_=> (let []
               (println "MOV C,D")
               (recur (inc pc)))

        0x4b
        #_=> (let []
               (println "MOV C,E")
               (recur (inc pc)))

        0x4c
        #_=> (clojure.core/let []
               (clojure.core/println "MOV C,H")
               (recur (clojure.core/inc pc)))

        0x4d
        #_=> (clojure.core/let []
               (clojure.core/println "MOV C,L")
               (recur (clojure.core/inc pc)))

        0x4e
        #_=> (let []
               (println "MOV C,M")
               (recur (inc pc)))

        0x4f
        #_=> (let []
               (println "MOV C,A")
               (recur (inc pc)))

        0x50
        #_=> (let []
               (clojure.core/println "MOV D,B")
               (recur (clojure.core/inc pc)))

        0x51
        #_=> (let []
               (clojure.core/println "MOV D,C")
               (recur (clojure.core/inc pc)))

        0x54
        #_=> (let []
               (println "MOV D,H")
               (recur (inc pc)))

        0x55 (let []
               (println "MOV D,L")
               (recur (inc pc)))

        0x56
        #_=> (let []
               (println "MOV D,M")
               (recur (inc pc)))

        0x57
        #_=> (let []
               (println "MOV D,A")
               (recur (inc pc)))

        0x59
        #_=> (clojure.core/let []
               (clojure.core/println "MOV E,C")
               (recur (clojure.core/inc pc)))

        0x5b
        #_=> (let []
               (println "MOV E,E")
               (recur (inc pc)))

        0x5e
        #_=> (let []
               (println "MOV E,M")
               (recur (inc pc)))

        0x5f (let []
               (println "MOV E,A")
               (recur (inc pc)))

        0x60
        #_=> (let []
               (println "MOV H,B")
               (recur (inc pc)))

        0x61
        #_=> (let []
               (println "MOV H,C")
               (recur (inc pc)))

        0x62
        #_=> (clojure.core/let []
               (clojure.core/println "MOV H,D")
               (recur (clojure.core/inc pc)))

        0x63
        #_=> (clojure.core/let []
               (clojure.core/println "MOV H,E")
               (recur (clojure.core/inc pc)))

        0x64
        #_=> (let []
               (println "MOV H,H")
               (recur (inc pc)))

        0x65
        #_=> (let []
               (println "MOV H,L")
               (recur (inc pc)))

        0x66
        #_=> (let []
               (println "MOV H,M")
               (recur (inc pc)))

        0x67
        #_=> (let []
               (println "MOV, H,A")
               (recur (inc pc)))

        0x68
        #_=> (let []
               (println "MOV L,B")
               (recur (inc pc)))

        0x69
        #_=> (let []
               (println "MOV L,C")
               (recur (inc pc)))

        0x6c
        #_=> (let []
               (println "MOV L,H")
               (recur (inc pc)))

        0x6d
        #_=> (let []
               (println "MOV L,L")
               (recur (inc pc)))

        0x6e
        #_=> (let []
               (println "MOV L,M")
               (recur (inc pc)))

        0x6f
        #_=> (let []
               (println "MOV L,A")
               (recur (inc pc)))

        0x70
        #_=> (let []
               (println "MOV M,B")
               (recur (inc pc)))

        0x71
        #_=> (let []
               (println "MOV M,C")
               (recur (inc pc)))

        0x72
        #_=> (let []
               (println "MOV M,D")
               (recur (inc pc)))

        0x73
        #_=> (let []
               (println "MOV M,E")
               (recur (inc pc)))

        0x74
        #_=> (clojure.core/let []
               (clojure.core/println "MOV M,H")
               (recur (clojure.core/inc pc)))

        0x76
        #_=> (let []
               (println "HLT")
               (recur (inc pc)))

        0x77
        #_=> (let []
               (println "MOV M,A")
               (recur (inc pc)))

        0x78 (let []
               (println "MOV A,B")
               (recur (inc pc)))

        0x79
        #_=> (let []
               (println "MOV A,C")
               (recur (inc pc)))

        0x7a
        #_=> (let []
               (println "MOV A,D")
               (recur (inc pc)))

        0x7b
        #_=> (let []
               (println "MOV A,E")
               (recur (inc pc)))

        0x7c
        #_=> (let []
               (println "MOV A,H")
               (recur (inc pc)))

        0x7d
        #_=> (let []
               (println "MOV A,L")
               (recur (inc pc)))

        0x7e
        #_=> (let []
               (println "MOV A,M")
               (recur (inc pc)))

        0x7f
        #_=> (let []
               (println "MOV A,A")
               (recur (inc pc)))

        0x80
        #_=> (let []
               (println "ADD B")
               (recur (inc pc)))

        0x81
        #_=> (let []
               (println "ADD C")
               (recur (inc pc)))

        0x82
        #_=> (let []
               (println "ADD D")
               (recur (inc pc)))

        0x83
        #_=> (let []
               (println "ADD E")
               (recur (inc pc)))

        0x84
        #_=> (let []
               (println "ADD H")
               (recur (inc pc)))

        0x85
        #_=> (let []
               (println "ADD L")
               (recur (inc pc)))

        0x86
        #_=> (let []
               (println "ADD M")
               (recur (inc pc)))

        0x88
        #_=> (let []
               (println "ADC B")
               (recur (inc pc)))

        0x8a
        #_=> (let []
               (println "ADC D")
               (recur (inc pc)))

        0x8b
        #_=> (let []
               (println "ADC E")
               (recur (inc pc)))

        0x8e
        #_=> (let []
               (println "ADC M")
               (recur (inc pc)))

        0x90
        #_=> (let []
               (println "SUB B")
               (recur (inc pc)))

        0x94
        #_=> (let []
               (println "SUB H")
               (recur (inc pc)))

        0x97
        #_=> (let []
               (println "SUB A")
               (recur (inc pc)))

        0x98
        #_=> (clojure.core/let []
               (clojure.core/println "SBB B")
               (recur (clojure.core/inc pc)))

        0x99
        #_=> (let []
               (println "SBB C")
               (recur (inc pc)))

        0x9a
        #_=> (let []
               (println "SBB D")
               (recur (inc pc)))

        0x9b
        #_=> (let []
               (println "SBB E")
               (recur (inc pc)))

        0x9d
        #_=> (let []
               (println "SBB L")
               (recur (inc pc)))

        0x9e
        #_=> (let []
               (println "SBB M")
               (recur (inc pc)))

        0xa0
        #_=> (let []
               (println "ANA B")
               (recur (inc pc)))

        0xa3
        #_=> (let []
               (println "ANA E")
               (recur (inc pc)))

        0xa6
        #_=> (let []
               (println "ANA M")
               (recur (inc pc)))

        0xa7
        #_=> (let []
               (println "ANA A")
               (recur (inc pc)))

        0xa8
        #_=> (let []
               (println "XRA B")
               (recur (inc pc)))

        0xaa
        #_=> (let []
               (println "XRA D")
               (recur (inc pc)))

        0xaf
        #_=> (let []
               (println "XRA A")
               (recur (inc pc)))

        0xb0
        #_=> (let []
               (println "ORA B")
               (recur (inc pc)))

        0xb3
        #_=> (let []
               (println "ORA E")
               (recur (inc pc)))

        0xb4
        #_=> (let []
               (println "ORA H")
               (recur (inc pc)))

        0xb6
        #_=> (let []
               (println "ORA M")
               (recur (inc pc)))

        0xb8
        #_=> (let []
               (println "CMP B")
               (recur (inc pc)))

        0xbb
        #_=> (let []
               (println "CMP E")
               (recur (inc pc)))

        0xbc
        #_=> (let []
               (println "CMP H")
               (recur (inc pc)))

        0xbe
        #_=> (let []
               (println "CMP M")
               (recur (inc pc)))

        0xc0 (let []
               (println "RNZ")
               (recur (inc pc)))

        0xc1
        #_=> (let []
               (println "POP B")
               (recur (inc pc)))

        0xc2
        #_=> (let [lsb (nth file (+ 1 pc))
                   msb (nth file (+ 2 pc))
                   adr (+ (bit-shift-left msb 8)
                          lsb)]
               (println "JNZ" (format "$%x" adr))
               (recur (+ 3 pc)))

        0xc3
        #_=> (let [lsb (nth file (+ 1 pc))
                   msb (nth file (+ 2 pc))
                   adr (+ (bit-shift-left msb 8)
                          lsb)]
               (println "JMP" (format "$%x" adr))
               (recur (+ 3 pc)))

        0xc4 (let [lsb (nth file (+ 1 pc))
                   msb (nth file (+ 2 pc))
                   adr (+ (bit-shift-left msb 8)
                          lsb)]
               (println "CNZ" (format "$%x" adr))
               (recur (+ 3 pc)))

        0xc5
        #_=> (let []
               (println "PUSH B")
               (recur (inc pc)))

        0xc6
        #_=> (let [d8 (nth file (inc pc))]
               (println "ADI" (format "#$%x" d8))
               (recur (+ 2 pc)))

        0xc8
        #_=> (let []
               (println "RZ")
               (recur (inc pc)))

        0xc9
        #_=> (let []
               (println "RET")
               (recur (inc pc)))

        0xca
        #_=> (let [lsb (nth file (+ 1 pc))
                   msb (nth file (+ 2 pc))
                   adr (+ (bit-shift-left msb 8)
                          lsb)]
               (println "JZ" (format "$%x" adr))
               (recur (+ 3 pc)))

        0xcc
        #_=> (let [lsb (nth file (+ 1 pc))
                   msb (nth file (+ 2 pc))
                   adr (+ (bit-shift-left msb 8)
                          lsb)]
               (println "CZ" (format "$%x" adr))
               (recur (+ 3 pc)))

        0xcd
        #_=> (let [lsb (nth file (+ 1 pc))
                   msb (nth file (+ 2 pc))
                   adr (+ (bit-shift-left msb 8)
                          lsb)]
               (println "CALL" (format "$%x" adr))
               (recur (+ 3 pc)))

        0xd0
        #_=> (let []
               (println "RNC")
               (recur (inc pc)))

        0xd1
        #_=> (let []
               (println "POP D")
               (recur (inc pc)))

        0xd2
        #_=> (let [lsb (nth file (+ 1 pc))
                   msb (nth file (+ 2 pc))
                   adr (+ (bit-shift-left msb 8)
                          lsb)]
               (println "JNC" (format "$%x" adr))
               (recur (+ 3 pc)))

        0xd3
        #_=> (let [d8 (nth file (inc pc))]
               (println "OUT" (format "#$%x" d8))
               (recur (+ 2 pc)))

        0xd5
        #_=> (let []
               (println "PUSH D")
               (recur (inc pc)))

        0xd4
        #_=> (let [lsb (nth file (+ 1 pc))
                   msb (nth file (+ 2 pc))
                   adr (+ (bit-shift-left msb 8)
                          lsb)]
               (println "CNC" (format "$%x" adr))
               (recur (+ 3 pc)))

        0xd6
        #_=> (let [d8 (nth file (inc pc))]
               (println "SUI" (format "#$%x" d8))
               (recur (+ 2 pc)))

        0xd8
        #_=> (let []
               (println "RC")
               (recur (inc pc)))

        0xda
        #_=> (let [lsb (nth file (+ 1 pc))
                   msb (nth file (+ 2 pc))
                   adr (+ (bit-shift-left msb 8)
                          lsb)]
               (println "JC" (format "$%x" adr))
               (recur (+ 3 pc)))

        0xdb
        #_=> (let [d8 (nth file (inc pc))]
               (println "IN" (format "#$%x" d8))
               (recur (+ 2 pc)))

        0xde
        #_=> (let [d8 (nth file (inc pc))]
               (println "SBI" (format "#$%x" d8))
               (recur (+ 2 pc)))

        0xe0
        #_=> (clojure.core/let []
               (clojure.core/println "RPO")
               (recur (clojure.core/inc pc)))

        0xe1
        #_=> (let []
               (println "POP H")
               (recur (inc pc)))

        0xe2
        #_=> (let [lsb (nth file (+ 1 pc))
                   msb (nth file (+ 2 pc))
                   adr (+ (bit-shift-left msb 8)
                          lsb)]
               (println "JPO" (format "$%x" adr))
               (recur (+ 3 pc)))

        0xe3
        #_=> (let []
               (println "XTHL")
               (recur (inc pc)))

        0xe5
        #_=> (let []
               (println "PUSH H")
               (recur (inc pc)))

        0xe6
        #_=> (let [d8 (nth file (inc pc))]
               (println "ANI" (format "#$%x" d8))
               (recur (+ 2 pc)))

        0xe9
        #_=> (let []
               (println "PCHL")
               (recur (inc pc)))

        0xeb (let []
               (println "XCHG")
               (recur (inc pc)))

        0xec
        #_=> (let [lsb (nth file (+ 1 pc))
                   msb (nth file (+ 2 pc))
                   adr (+ (bit-shift-left msb 8)
                          lsb)]
               (println "CPE" (format "$%x" adr))
               (recur (+ 3 pc)))

        0xee
        #_=> (let [d8 (nth file (inc pc))]
               (println "XRI" (format "#$%x" d8))
               (recur (+ 2 pc)))

        0xf0
        #_=> (let []
               (println "RP")
               (recur (inc pc)))

        0xf1
        #_=> (let []
               (println "POP PSW")
               (recur (inc pc)))

        0xf5
        #_=> (let []
               (println "PUSH PSW")
               (recur (inc pc)))

        0xf6
        #_=> (let [d8 (nth file (inc pc))]
               (println "ORI" (format "#$%x" d8))
               (recur (+ 2 pc)))

        0xf8
        #_=> (let []
               (println "RM")
               (recur (inc pc)))

        0xfa
        #_=> (let [lsb (nth file (+ 1 pc))
                   msb (nth file (+ 2 pc))
                   adr (+ (bit-shift-left msb 8)
                          lsb)]
               (println "JM" (format "$%x" adr))
               (recur (+ 3 pc)))

        0xfb
        #_=> (let []
               (println "EI")
               (recur (inc pc)))

        0xfc
        #_=> (let [lsb (nth file (+ 1 pc))
                   msb (nth file (+ 2 pc))
                   adr (+ (bit-shift-left msb 8)
                          lsb)]
               (println "CM" (format "$%x" adr))
               (recur (+ 3 pc)))

        0xfe
        #_=> (let [d8 (nth file (inc pc))]
               (println "CPI" (format "#$%x" d8))
               (recur (+ 2 pc)))

        0xff
        #_=> (let []
               (println "RST 7")
               (recur (inc pc)))

        (throw (Exception. (str "unknown opcode: " (format "0x%x" opcode) "\n"
                                (let [oc-data (->> all-opcodes
                                                   (filter #(= opcode (:opcode %)))
                                                   first)]
                                  (cond (= 1 (:size oc-data))
                                        #_=> (list (format "0x%x" opcode)
                                                   `(let []
                                                      (println ~(:instruction oc-data))
                                                      (recur (inc pc))))

                                        :else oc-data)))))))))

