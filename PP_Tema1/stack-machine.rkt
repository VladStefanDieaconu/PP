#lang racket
(require "opcodes.rkt")
(provide make-stack-machine)
(provide run-stack-machine)
(provide get-stack)
(provide get-varnames)
(provide get-consts)
(provide get-names)
(provide get-code)
(provide get-IC)
(provide empty-stack)
(provide make-stack)
(provide push)
(provide pop)
(provide top)


;; TODO 1:
;; Alegeți metoda de reprezentarea a unei stive.
;; Implementați:
(define empty-stack null)
(define (make-stack) empty-stack)

(define (push element stack) (cons element stack))
(define (top stack) (car stack))
(define (pop stack) (cdr stack))

;; TODO 2:
;; Alegeți metoda de reprezentare a unei mașini stivă.
;; Definiți make-stack-machine, acesta trebuie sa primeasca cele 4 segmente de date
;; Veți avea nevoie de o stivă pentru execuție și un counter ca să stiți
;; la ce instrucțiune sunteți.
(define (make-stack-machine stack co-varnames co-consts co-names co-code IC)
  (list stack co-varnames co-consts co-names co-code IC))

;; Definiți funcțiile `get-varnames`, `get-consts`, `get-names`,
;; `get-code`, `get-stack`, `get-IC` care primesc o mașina stivă și întorc
;; componenta respectivă

;; ex:
;; > (get-varnames (make-stack-machine empty-stack 'dummy-co-varnames (hash) (hash) (list) 0))
;; 'dummy-co-varnames
(define (get-varnames stack-machine) (cadr stack-machine))

;; ex:
;; > (get-consts (make-stack-machine empty-stack (hash) 'dummy-co-consts (hash) (list) 0))
;; 'dummy-co-consts
(define (get-consts stack-machine) (caddr stack-machine))

;; ex:
;; > (get-names (make-stack-machine empty-stack (hash) (hash) 'dummy-co-names (list) 0))
;; 'dummy-co-names
(define (get-names stack-machine) (cadddr stack-machine))

;; ex:
;; > (get-code (make-stack-machine empty-stack (hash) (hash) (hash) 'dummy-co-code 0))
;; dummy-co-code
(define (get-code stack-machine) (car (cddddr stack-machine)))

;; Întoarce stiva de execuție.
;; ex:
;; > (get-code (make-stack-machine 'dummy-exec-stack (hash) (hash) (hash) (list) 0))
;; dummy-exec-stack
(define (get-stack stack-machine) (car stack-machine))

;; Întoarce instruction counterul.
;; ex:
;; > (get-code (make-stack-machine empty-stack (hash) (hash) (hash) (list) 0))
;; 0
(define (get-IC stack-machine) (last stack-machine))



(define symbols (list 'STACK 'CO-VARNAMES 'CO-CONSTS 'CO-NAMES 'CO-CODE 'INSTRUCTION-COUNTER))

;; TODO 3:
;; Definiți funcția get-symbol-index care gasește index-ul simbolului in listă.
(define (get-symbol-index symbol)
  (index-of symbols symbol))

;; Definiți funcția update-stack-machine care intoarce o noua mașina stivă
;; înlocuind componenta corespondentă simbolului cu item-ul dat în paremetri.
;; > (get-varnames (update-stack-machine "new-varnames" 'CO-VARNAMES stack-machine))
;; "new-varnames"
;; > (get-varnames (update-stack-machine "new-names" 'CO-NAMES stack-machine))
;; "new-names"
(define (update-stack-machine item symbol stack-machine)
  (let* ([index (get-symbol-index symbol)]
         (l1 (take stack-machine index))
         (l2 (drop stack-machine (add1 index))))
    (append l1 (list item) l2)))

;; Definiți funcția push-exec-stack care primește o masină stivă și o valoare
;; și intoarce o noua mașina unde valoarea este pusă pe stiva de execuție
(define (push-exec-stack value stack-machine)  ; update the stack
  (let* ((stack (get-stack stack-machine)) 
         (new-stack (push value stack)))  
    (update-stack-machine new-stack 'STACK stack-machine)))

;;  Definiți funcția pop-exec-stack care primește o masină stivă
;;  și intoarce o noua mașina aplicând pop pe stiva de execuție.
(define (pop-exec-stack stack-machine)
  (let* ((stack (get-stack stack-machine)) 
         (new-stack (pop stack)))  
  (update-stack-machine new-stack 'STACK stack-machine)))

;; TODO 4:
;; Definiți funcția run-stack-machine care execută operații pană epuizează co-code.
(define (run-stack-machine stack-machine)
  (let iter ([this-machine stack-machine])

    (let* ((co-code (get-code this-machine))
           (IC (get-IC this-machine))
           (len (sub1 (length co-code))))

      (if (< IC len)
        (iter (run-stack-Util this-machine)) ; execute current instruction and continue with the rest 
        this-machine
    ))))


(define (run-stack-Util stack-machine)
  (let* ((stack (get-stack stack-machine))              ; stack-machine
         (co-varnames (get-varnames stack-machine))
         (co-consts (get-consts  stack-machine))
         (co-names (get-names stack-machine))
         (co-code (get-code stack-machine))
         (IC (get-IC stack-machine))
         
         (instr (car (drop co-code IC)))                ; current instruction = (opcode arg)
         (opcode (car instr))                           ; opcode of the current instruction
         (arg (cdr instr))                              ; argument of the current instruction
       
         (next-IC (cond
                    ((or (equal? opcode 'JUMP_ABSOLUTE)
                         (equal? opcode 'POP_JUMP_IF_TRUE)
                         (and (equal? opcode 'POP_JUMP_IF_FALSE) (not (top stack)))) (quotient arg 2))
                    ((and (equal? opcode 'FOR_ITER) (null? (top stack))) (+ (quotient arg 2) IC 1))                  
                    (else (add1 IC))
                   ))
         
         (next-stack-machine (run-one-operation opcode arg next-IC stack-machine))
         )
    (update-stack-machine next-IC 'INSTRUCTION-COUNTER next-stack-machine)
  ))


(define (run-one-operation opcode arg next-IC stack-machine)
  (let ((stack (get-stack stack-machine))                    ; stack-machine
         (co-varnames (get-varnames stack-machine))
         (co-consts (get-consts  stack-machine))
         (co-names (get-names stack-machine))
         (co-code (get-code stack-machine))
         (IC (get-IC stack-machine)))

    (cond                   ; next stack-machine
          ((equal? opcode 'LOAD_CONST) (load-from co-consts arg stack-machine))
          ((equal? opcode 'LOAD_GLOBAL) (load-from co-names arg stack-machine))
          ((equal? opcode 'LOAD_FAST) (load-from co-varnames arg stack-machine))
          ((equal? opcode 'STORE_FAST) (store arg stack-machine))
          ((or (equal? opcode 'BINARY_MODULO) (equal? opcode 'INPLACE_MODULO)) (make remainder stack-machine))
          ((or (equal? opcode 'BINARY_SUBTRACT) (equal? opcode 'INPLACE_SUBTRACT)) (make - stack-machine))                    
          ((or (equal? opcode 'BINARY_ADD) (equal? opcode 'INPLACE_ADD)) (make + stack-machine))    
          ((equal? opcode 'COMPARE_OP) (make (get-cmpop arg) stack-machine))
          ((or (equal? opcode 'POP_TOP) (equal? opcode 'POP_JUMP_IF_TRUE) (equal? opcode 'POP_JUMP_IF_FALSE)) (pop-exec-stack stack-machine))

          ((equal? opcode 'FOR_ITER) (if (null? (top stack))
                                         (pop-exec-stack stack-machine)
                                         (get-next-iter stack-machine)))
          
          ((equal? opcode 'CALL_FUNCTION)
                     (let* ((elems (take stack (add1 arg)))
                            (function (get-function (last elems)))
                            (result (apply function (take elems arg)))

                            (new-stack (push result (drop stack (add1 arg))))
                            (new-stack-machine (update-stack-machine new-stack 'STACK stack-machine)))

                       (update-stack-machine next-IC 'INSTRUCTION-COUNTER new-stack-machine)))
          (else stack-machine))))



; execute operation op and return the new stack-machine.
(define (make operator stack-machine)
  (let* ((arg1 (top (cdr (get-stack stack-machine))))
         (arg2 (top (get-stack stack-machine)))
         (new-stack-machine (pop-exec-stack (pop-exec-stack stack-machine)))) 
    (push-exec-stack (operator arg1 arg2) new-stack-machine)))


; load on stack the given value from the given segment (dict)
(define (load-from dict value stack-machine)
  (let ((item (hash-ref dict value)))
    (push-exec-stack item stack-machine)))


(define (store value stack-machine)
  (let* ((stack (get-stack stack-machine))
         (co-varnames (get-varnames stack-machine))
         (top-elem (top stack))
         (new-varnames (hash-set co-varnames value top-elem)) 
         (new-stack-machine (pop-exec-stack stack-machine)))
        
    (update-stack-machine new-varnames 'CO-VARNAMES new-stack-machine))) ; the stack-machine after replacing the dict with the new one


(define (get-next-iter stack-machine)
  (let* ((stack (get-stack stack-machine))
         (iterator-elem (top stack))
         (first-iter (car iterator-elem))
         (rest-iter (cdr iterator-elem))
         (stack-popped (pop stack))
         (stack-pushed-rest (push rest-iter stack-popped))
         (stack-pushed-first (push first-iter stack-pushed-rest))                
         )
    (update-stack-machine stack-pushed-first 'STACK stack-machine))
  )

