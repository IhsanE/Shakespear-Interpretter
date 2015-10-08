#| Assignment 1 - Functional Shakespeare Interpreter

Read through the starter code carefully. In particular, look for:

- interpret: the main function used to drive the program.
  This is provided for you, and should not be changed.
- evaluate: this is the main function you'll need to change.
  Please put all helper functions you write below this one.
  Doing so will greatly help TAs when they are marking. :)
|#
#lang racket

; You are allowed to use all the string functions in this module.
; You may *not* import any other modules for this assignment.
(require racket/string)

; This exports the main driver function. Used for testing purposes.
; This is the only function you should export. Don't change this line!
(provide interpret)

;------------------------------------------------------------------------------
; Parsing constants
;------------------------------------------------------------------------------

; Sections dividers
(define personae "Dramatis personae")
(define settings "Settings")
(define finis "Finis")

; Comment lines
(define comments '("Act" "Scene"))

; List of all "bad words" in a definition
(define bad-words
  '("vile"
    "villainous"
    "wicked"
    "naughty"
    "blackhearted"
    "shameless"
    "scoundrelous"))

; Arithmetic
(define add "join'd with")
(define mult "entranc'd by")

; Self-reference keywords
(define self-refs
  '("I"
    "me"
    "Me"
    "myself"
    "Myself"))

; Function call
(define call "The song of")

; Function parameter name
(define param "Hamlet")

;------------------------------------------------------------------------------
; Interpreter driver
;------------------------------------------------------------------------------

#|
(interpret filename)
  filename: a string representing the path to a FunShake file

  Returns a list of numbers produced when evaluating the FunShake file.
  You can complete this assignment without modifying this function at all,
  but you may change the implementation if you like. Please note that you may
  not change the interface, as this is the function that will be autotested.
|#
(define (interpret filename)
  (let* ([contents (port->string (open-input-file filename))]
         [lines (map normalize-line (string-split contents "\n"))]
         ; Ignore title, empty, and comment lines
         [body (remove-empty-and-comments (rest lines))])
    (evaluate body)))

#|
(normalize-line str)
  str: the line string to normalize

  Remove trailing period and whitespace.
|#
(define (normalize-line str)
  (string-trim (string-normalize-spaces (string-trim str)) "."))

#|
(remove-empty-and-comments strings)
  strings: a list of strings

  Removes all empty strings and FunShake comment strings from 'strings'.
|#
(define (remove-empty-and-comments strings)
  (filter (lambda (s)
            (and
             (< 0 (string-length s))
             (not (ormap (lambda (comment) (prefix? comment s))
                         comments))))
          strings))

#|
(prefix? s1 s2)
  s1, s2: strings

  Returns whether 's1' is a prefix of 's2'.
|#
(define (prefix? s1 s2)
  (and (<= (string-length s1) (string-length s2))
       (equal? s1 (substring s2 0 (string-length s1)))))

;------------------------------------------------------------------------------
; Main evaluation (YOUR WORK GOES HERE)
;------------------------------------------------------------------------------

(define (get-dramatis-helper l acc)
  (if (string=? personae (first l))
      (get-dramatis-helper (rest l) acc)
      (if (string=? finis (first l))
          acc
          (get-dramatis-helper (rest l) (append acc (list (first l)))))))

(define (get-dramatis l)
  (get-dramatis-helper l '()))

(define (get-settings-helper l acc bool)
  (if (string=? settings (first l))
      (get-settings-helper (rest l) acc #t)
      (if (and (string=? finis (first l)) bool)
          acc
          (if (eq? #t bool)
              (get-settings-helper (rest l) (append acc (list (first l))) #t)
              (get-settings-helper (rest l) '() #f)))))

(define (get-settings l)
  (get-settings-helper l '() #f))

(define (get-finis-count l)
  (foldl (lambda (x y) (if (string=? finis x) (+ 1 y) y)) 0 l))

(define (get-dialogue l)
  (let* ([finis-count (get-finis-count body)])
   (get-dialogue-helper l finis-count 0))
)

#|
    Return true iff list contains s.
|#
(define (list-contains list s)
  (if (null? list) #f
      (if (string=? (first list) s) #t (list-contains (rest list) s))))

#|
    Return the number of bad words in desc.
    desc should be a list of words.
|#
(define (count-bad-words-helper desc)
  (if (null? desc) 0 
      (if (list-contains bad-words (first desc)) (+ 1 (count-bad-words-helper (rest desc)))
          (count-bad-words-helper (rest desc))))
  )

#|
    Given string 'desc' return the integer value of the description.
|#
(define (eval-description-helper desc)
  (if (> (count-bad-words-helper (string-split desc)) 0)
      (* -1 (* (expt 2 (count-bad-words-helper (string-split desc))) (length (string-split desc))))
      (length (string-split desc)))
  )

#|
    Return a list of lists, where each inner list is a pair of
    (<name>, <value>) for the Dramatis Personae section.
    d-list is a list of strings where each is a Dramatis Personae line.
|#
(define (parse-dramatis-bindings d-list)
  (map (lambda (d) (list (first (string-split d ","))
                         (eval-description-helper (string-join (rest (string-split d ",")) ""))))
       d-list)
  )

#|
    Return a list of lists, where each inner list is a pair of
    (<function name>, <expression>) for the settings section.
    s-list is a list of string where each is a Settings line.
|#
(define (parse-settings-bindings s-list)
  (map (lambda (s) (list (first (string-split s ",")) (rest (string-split s ",")))) s-list)
  )

#|
    Replace the first occurrence of 'Hamlet' in body with param.
|#
(define (replace-hamlet body param)
  (string-replace body "Hamlet" param)
  )

#|
    Return the body of the function called name.
    Assumes that a function called name exists.
    func-list is the list of functions given by (parse-settings-bindings).
|#
(define (func-search-helper name func-list)
  (if (string=? (first (first func-list)) name) (string-join (rest (first func-list)) "")
      (func-search-helper name (rest func-list)))
  )

#|
    Helper for evaluating functions. Return a string where the contents are the
    expression given by the function called name, with Hamlet replaced by param.
    Assumes that the function exists and is formatted properly.
    func-list is the list of functions given by (parse-settings-bindings).
|#
(define (func-parser name param func-list)
  (replace-hamlet (func-search-helper name func-list) param)
  )

#|
(evaluate body)
  body: a list of lines corresponding to the semantically meaningful text
  of a FunShake file.

  Returns a list of numbers produced when evaluating the FunShake file.
  This should be the main starting point of your work! Currently,
  it just outputs the semantically meaningful lines in the file.
|#
(define (evaluate body)
  ; TODO: Change this part!
  (get-settings body))
