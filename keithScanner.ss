#!/bin/env scheme-script

(import  (rnrs (6)))
(define (print . args)
  (for-each display args))
(define (println . args)
  (apply print args)
  (newline))
(define (usage)
  (let ([progname (car (command-line))])
    (println "USAGE: " progname " yourfile.ss")
    (exit 1)))
(define sctList '(9 32 39 40 41 59 91 93 123 125)) ;ascii values of all single-character-tokens 
;SingleCharToken Contains (40 )41 {123 }125 '39 [91 ]93  ;59 space32 tab9.


;The dfa has many states(8 coded currently). The Start state is state# 0, SCT# 1, SPACE# 2, MCT GENERAL# 3, COMMENT# 4, ERROR # 5, MCT NUMBER #6
;in MCT states, dfa has the special restrition that it can't transition to another mct state. 
;The dfa is always either in or not in an MCT state. 
  
(define (findNextStateType sigma) 
  (if (member sigma sctList) 
      (cond
        [[= sigma 32]
         2 ;2 is the space state
        ]
        [[= sigma 9]
         2 ;2 is the tab state as well
        ]
        [[= sigma 59]
         4 ;4 is the comment state
        ]
        [[= sigma 39]
         4 ;4 is the comment state, and the quote state is being directed here for now. Not sure how to handle '
        ]
        [else
         1 ;it is a default SCT state, brackets etc.
        ] 
      )
      (cond ;MCT STATES
        [[and (>= sigma 48) (<= sigma 57)]
          6 ;NUMBER MCT State
        ]
        [else
        3 ;default mct state
      ]
      )
  )
)

(define (lookUpSCTToken sigma) ;SingleCharToken Contains ()40 41 {}123 125 '39 []93 91 ;59 space32.
  (cond
    [[= sigma 39]
      "QUOTEMK"
    ]
    [[= sigma 40]
      "OPENRD"
    ]
    [[= sigma 41]
      "CLOSERD"
    ]
    [[= sigma 123]
      "OPENCU"
    ]
    [[= sigma 125]
      "CLOSECU"
    ]
    [[= sigma 91]
      "OPENSQ"
    ]
    [[= sigma 93]
      "CLOSESQ"
    ]
    [else
    "SCT_(SHOULD_NOT_SEE_THIS)"
    ]
  )
)

(define (lookUpMCTToken SIGMA)
 (cond
    [[string=? (substring SIGMA 0 (min 1 (string-length SIGMA))) "\""] ;Are these case insensitive? string-ci=? if so. 
      "STRING"
    ]
    [[string=? (substring SIGMA 0 (min 2 (string-length SIGMA))) "#f"] ;Are these case insensitive? string-ci=? if so. 
      "BOOL"
    ]
    [[string=? (substring SIGMA 0 (min 2 (string-length SIGMA))) "#t"]
      "BOOL"
    ]
    [[string=? (substring SIGMA 0 (min 2 (string-length SIGMA))) "if"]
      "IF"
    ]
    [[string=? (substring SIGMA 0 (min 2 (string-length SIGMA))) "#\\"]
      "CHAR"
    ]
    [[string=? (substring SIGMA 0 (min 3 (string-length SIGMA))) "let"]
      "LET"
    ]
    [[string=? (substring SIGMA 0 (min 4 (string-length SIGMA))) "cond"]
      "COND"
    ]
    [[string=? (substring SIGMA 0 (min 5 (string-length SIGMA))) "begin"]
      "BEGIN"
    ] 
    [[string=? (substring SIGMA 0 (min 5 (string-length SIGMA)))  "quote"] ; need clarification on this. Assn says both ' and QUOTE as keywords, and not sure if quote can be used in scheme or not. 
      "QUOTE"
    ]
    [[string=? (substring SIGMA 0 (min 6 (string-length SIGMA)))  "lambda"]
      "LAMBDA"
    ]
    [[string=? (substring SIGMA 0 (min 6 (string-length SIGMA)))  "define"]
      "DEFINE"
    ]
    ; [[or (= 1 (string-length SIGMA) ) (= 1 (findNextStateType(char->integer (string-ref SIGMA 1)))) (= 2 (findNextStateType(char->integer (string-ref SIGMA 1)))) (= 4 (findNextStateType(char->integer (string-ref SIGMA 1))))]
    ;   "CHAR"
    ; ]
    [else
    "IDENTIFIER"
    ]
  )
)

;The next three function are helpers for returning an error token. They split the line into two sides, and walk down them from the center until they are able return our error token.
(define (findLHSindexForErrorToken lhs lhsLength)
  (if (or (= 1 (findNextStateType(char->integer (string-ref lhs (- lhsLength 1))))) (= 2 (findNextStateType(char->integer (string-ref lhs (- lhsLength 1))))) (= 1 lhsLength))
    lhsLength
    (findLHSindexForErrorToken (substring lhs 0 (- lhsLength 1)) (- lhsLength 1))
  )
)
(define (findRHSindexForErrorToken rhs rhsLength) 
  (if (or (= 1 (findNextStateType(char->integer (string-ref rhs 1)))) (= 2 (findNextStateType(char->integer (string-ref rhs 1)))) (= 4 (findNextStateType(char->integer (string-ref rhs 1))))(= 1 rhsLength))
    (- rhsLength 1)
    (findRHSindexForErrorToken (substring rhs 1 rhsLength) (- rhsLength 1))
  )
)
(define (findBadToken lineContent strIndexOfIncident)
  ;Uses the dfa's helper functions to check for possbile state transitions, we scan outward left and right, from the index of the error incdient in order to find the two tokens spots to report them together. 
  (let ([leftHandSide (substring lineContent 0 (+ 1 strIndexOfIncident))]
        [rightHandSide (substring lineContent (+ 1 strIndexOfIncident) (string-length lineContent))]
        )
    (substring lineContent (findLHSindexForErrorToken leftHandSide (string-length leftHandSide)) (+ strIndexOfIncident 1 (- (string-length rightHandSide) (findRHSindexForErrorToken rightHandSide (string-length rightHandSide)))))
  ) 
)


(define (dfa lineNumber lineContent lineLength)
(if (= lineLength 0)
  #f
  (let loop ([strIndex 0] [currentState 0] [currentChar (char->integer (string-ref lineContent 0))])
    ;(println  "DFA STATE: "currentState ", onChar: " currentChar ", Line Length: " lineLength ", strIndex: " strIndex)
    (cond
      [(= currentState 0) ;;As this is the start state, the only work to be is to determine what state to transition to. Look at the current char, find the next state, transition to it and bring with it that char. 
       (if (= strIndex lineLength)
        (println "(SHOULD_NOT_SEE_THIS)") ;0 length line, return
        (loop strIndex (findNextStateType currentChar) currentChar));enter SCT State for First Character
      ]

      [(= currentState 1) ;STATE 1, The General SCT State
        ;do stuff
        (println (lookUpSCTToken currentChar)" "lineNumber":"(+ strIndex 1))
        ;now move on
        (if (= strIndex (- lineLength 1))
          #f
          (loop (+ strIndex 1) (findNextStateType (char->integer (string-ref lineContent (+ strIndex 1)))) (char->integer (string-ref lineContent (+ strIndex 1)))))
      ]

      [(= currentState 2) ;STATE 2, The SCT 'white'space State
        ;do no output!
        ;now move on
        (if (= strIndex (- lineLength 1))
          #f
          (loop (+ strIndex 1) (findNextStateType (char->integer (string-ref lineContent (+ strIndex 1)))) (char->integer (string-ref lineContent (+ strIndex 1)))))
      ]
;;;;;;;State 3, 33, and 333 are related. They are the mct states, while the 6 and 66 are the number states that are related.
      [(= currentState 3) ;STATE 3, The  MCT Transition State . The other mct state is a more restricted version of this. It is activated when starting with a digit. This starts with not a digit and may have digits within.
        ;do stuff
        (println (lookUpMCTToken (substring lineContent strIndex (- lineLength 1))) " "  lineNumber":"(+ strIndex 1)) ;prints token type, line number, then char number.
        ;now move on, but watch for the special restriction. MCT can not move to a new MCT. A new MCT can only be defined by going from number to word, not word to number; Loop threw the remainder of the string until you find a sct.
        (if (= strIndex (- lineLength 1))
          #f
          (if (string=? "STRING" (lookUpMCTToken (substring lineContent strIndex (- lineLength 1))))
            (loop (+ strIndex 1) 333 (char->integer (string-ref lineContent (+ strIndex 1))))
            (if (= 3 (findNextStateType (char->integer (string-ref lineContent (+ 1 strIndex))))) ;if next is still a mct, enter state 33
              (loop (+ strIndex 1) 33 (char->integer (string-ref lineContent (+ strIndex 1))))
              (loop (+ strIndex 1) (findNextStateType (char->integer (string-ref lineContent (+ strIndex 1)))) (char->integer (string-ref lineContent (+ strIndex 1))))
            )
          )    
        )
      ]

      [(= currentState 33) ;STATE 33, The  MCT cycle State. I consume the 2nd character to the last of a MCT.
        ;do no output! PRINTING the head of this token was enough. We now wait for our end.
        (if (= strIndex (- lineLength 1))
          #f
          (if (and (not (= 34 (char->integer (string-ref lineContent (+ strIndex 1))))) (or (= 3 (findNextStateType (char->integer (string-ref lineContent (+ 1 strIndex))))) (= 6 (findNextStateType (char->integer (string-ref lineContent (+ 1 strIndex))))))) ;if next is still a mct, enter state 33
            (loop (+ strIndex 1) 33 (char->integer (string-ref lineContent (+ strIndex 1))))
            (loop (+ strIndex 1) (findNextStateType (char->integer (string-ref lineContent (+ strIndex 1)))) (char->integer (string-ref lineContent (+ strIndex 1))))
          )
        )
      ]
      [(= currentState 333) ;STATE 333, The STRING MCT cycle State. I consume the string.
        ;do no output! PRINTING the head of this token was enough. We now wait for our end.   ;34=", 92=\
        (if (= strIndex (- lineLength 1))
          #f
          (if (and (= 34 (char->integer (string-ref lineContent (+ 1 strIndex)))) (not(= 92 (char->integer (string-ref lineContent strIndex))))); if next is still a s.mct, enter state 333
            (if (= 34 (char->integer (string-ref lineContent (+ 1 strIndex))))
              (if (> lineLength (+ strIndex 2 ))
                (loop (+ strIndex 2) (findNextStateType (char->integer (string-ref lineContent (+ strIndex 2)))) (char->integer (string-ref lineContent (+ strIndex 2))))
                #f
              )
              (loop (+ strIndex 1) (findNextStateType (char->integer (string-ref lineContent (+ strIndex 1)))) (char->integer (string-ref lineContent (+ strIndex 1))))
            )
            (loop (+ strIndex 1) 333 (char->integer (string-ref lineContent (+ strIndex 1))))
            )
        )
      ]

      [(= currentState 4) ;STATE 4, The  Comment  State
        ;now exits back to loadSourceCode function since all work is done once comment symbol is reached. Check if it's neccessary to check the comments for illegal characters. 
      ]

      [(= currentState 6) ;STATE 6, The  MCT number  State. . It is activated when starting with a digit. IT may not transition to not a number, (just a sct/eol). 
        ;do stuff
        (println "NUMBER "lineNumber":"(+ strIndex 1))
        ;now move on, but watch for the special restriction. MCT can not move to a new MCT. A new MCT can only be defined by going from number to word, not word to number; Loop threw the remainder of the string until you find a sct.
        (if (= strIndex (- lineLength 1))
          #f
          (if (= 6 (findNextStateType (char->integer (string-ref lineContent (+ 1 strIndex))))) ;if next is still a mct, enter state 66
            (loop (+ strIndex 1) 66 (char->integer (string-ref lineContent (+ strIndex 1))))
            (if (= 3 (findNextStateType (char->integer (string-ref lineContent (+ 1 strIndex)))))
              (begin
                (println "LEXICAL ERROR ["lineNumber":" (+ 1 strIndex) "]: Invalid token '" (findBadToken lineContent strIndex) "'")
                (exit 1))
              (loop (+ strIndex 1) (findNextStateType (char->integer (string-ref lineContent (+ strIndex 1)))) (char->integer (string-ref lineContent (+ strIndex 1)))))))
      ]

      [(= currentState 66) ;STATE 66, The  MCT cycle State. I consume the seceond character to the last of a MCT.
        ;do no output! PRINTING the head of this token was enough. We now wait for our end of token.
        (if (= strIndex (- lineLength 1))
          #f
          (if (= 6 (findNextStateType (char->integer (string-ref lineContent (+ 1 strIndex))))) ;if next is still a mct, enter state 66
            (loop (+ strIndex 1) 66 (char->integer (string-ref lineContent (+ strIndex 1))))
            (if (= 3 (findNextStateType (char->integer (string-ref lineContent (+ 1 strIndex)))))
              (begin
                (println "LEXICAL ERROR ["lineNumber":" (+ 1 strIndex) "]: Invalid token '" (findBadToken lineContent strIndex) "'")
                (exit 1))
              (loop (+ strIndex 1) (findNextStateType (char->integer (string-ref lineContent (+ strIndex 1)))) (char->integer (string-ref lineContent (+ strIndex 1)))))))
      ]
))))

(define (loadSourceCode filename)
  (call-with-input-file filename
    (lambda (file)
      (let loop ([count 0])
        (let ([line (get-line file)])
          (cond [(eof-object? line)
                 (println "Done Reading. There were " count " Lines.")
                 (exit 1)]
                [else
                  (println "Sending '"line"' to dfa")
                  (dfa (+ 1 count) line (string-length line))
                 (loop (+ count 1))])))))) 

;; The main function
(define (main)
  (let ([args (command-line)])
    (if (not (= (length args) 2))
        (usage)
        (loadSourceCode (cadr args))
    )
  )
)
;; Start the main function
(main)