#!/usr/bin/env racket
#lang racket/base
(require racket/match
         racket/promise
         racket/cmdline
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(define separator "\n")
(define lower #t)
(command-line
 #:once-any
 [("-s" "--space") "Ouput token separator is space" (set! separator " ")]
 [("-n" "--newline") "Ouput token separator is newline" (set! separator "\n")]
 #:once-any
 [("-l" "--lower-case") "Lowercase output tokens" (set! lower #t)]
 [("-c" "--case-sensitive") "Case sensitive output tokens" (set! lower #f)])

(define emit
  (lambda (token tail)
    (begin (cond [(string? token)
                  (fprintf (current-output-port)
                           "~a~a"
                           token
                           separator)])
           (force tail))))

(define (normalize type token)
  (match type
    ['TERM (if lower
               (string-downcase token)
               token)]
    ['MISC token]
    ['WHITESPACE 'nop]
    ['EOF 'nop]
    [_ token]))

(define tokenizer
  (lexer
   [(:+ whitespace)
    ; =>
    (emit (normalize 'WHITESPACE lexeme)
          (delay (tokenizer input-port)))]
   [(:+ (:or (char-range #\a #\z) (char-range #\A #\Z)))
    ; =>
    (emit (normalize 'TERM lexeme)
          (delay (tokenizer input-port)))]
   [(:: (:? (:or #\- #\+))
        (:* (char-range #\0 #\9))
        (:? #\.)
        (:+ (char-range #\0 #\9))
        (:? (:: (:or #\e #\E)
                (:? (:or #\- #\+))
                (:+ (char-range #\0 #\9)))))
    ; =>
    (emit (normalize 'NUMBER lexeme)
          (delay (tokenizer input-port)))]
   [(:+ (:& punctuation
            (:~ #\-)))
    ; =>
    (emit (normalize 'PUNCT lexeme)
          (delay (tokenizer input-port)))]
   [any-char
    ; =>
    (emit (normalize 'MISC lexeme)
          (delay (tokenizer input-port)))]
   [(eof)
    ; =>
    (emit (normalize 'EOF lexeme)
          (void))]))

(tokenizer (current-input-port))
