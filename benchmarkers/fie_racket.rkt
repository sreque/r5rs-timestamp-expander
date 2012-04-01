#lang r6rs
(import 
  (rnrs) 
  (only (scheme) printf fprintf format pretty-print directory-exists? file-exists? compose void void?)
  (only (racket) in-directory for match-define define-values collect-garbage time-apply add1 sub1 null find-relative-path simplify-path path->complete-path current-command-line-arguments current-seconds  define-namespace-anchor namespace-anchor->namespace current-namespace sequence->list path->string)
  (only (racket mpair) list->mlist)
  (rnrs mutable-pairs (6))
  #;(rename (rnrs eval (6)) (eval orig-eval))
  ;(only (racket) eval)
  ;(rename (only (racket) eval) (eval racket-eval))
  (rename (only (r5rs) eval interaction-environment) (eval r5rs-eval))
  (srfi :19))

(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))
(current-namespace ns)
  
#;(define (eval e)
  (printf "eval'ing: ~a\n" e)
  (racket-eval e ns))
(define (eval e)
  #;(printf "eval'ing: ~a\n" e)
  (r5rs-eval e (interaction-environment)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(load "compat-mzscheme.scm")
;;;===============================================================================
;;;
;;; MzScheme compatibility file:
;;;
;;; Uncomment the appropriate LOAD command in macros-core.scm
;;;
;;;===============================================================================

;; A number converted to string that uniquely identifies this run in the universe

(define (ex:unique-token) (number->string (current-seconds)))

;; The letrec black hole and corresponding setter.

(define ex:undefined (letrec ((x y) (y #f)) x))
(define ex:undefined-set! 'set!)

;; Single-character symbol prefixes.
;; No builtins may start with one of these.
;; If they do, select different values here.

(define ex:guid-prefix "&")
(define ex:free-prefix "~")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(load "runtime.scm")
;;; 
;;; Runtime include file:
;;; Contains the minimal set of binding necessary
;;; for running a fully expanded program.
;;;

(define ex:unspecified (if #f #f #f))

(define (ex:make-library name envs exports imports builds visiter invoker build)
  (list name envs exports imports builds visiter invoker build #f #f))

(define (ex:library-name     lib) (car lib))
(define (ex:library-envs     lib) (cadr lib))
(define (ex:library-exports  lib) (caddr lib))
(define (ex:library-imports  lib) (cadddr lib))
(define (ex:library-builds   lib) (car (cddddr lib)))
(define (ex:library-visiter  lib) (car (cdr (cddddr lib))))
(define (ex:library-invoker  lib) (car (cdr (cdr (cddddr lib)))))
(define (ex:library-build    lib) (car (cdr (cdr (cdr (cddddr lib))))))
(define (ex:library-visited? lib) (car (cdr (cdr (cdr (cdr (cddddr lib)))))))
(define (ex:library-invoked? lib) (car (cdr (cdr (cdr (cdr (cdr (cddddr lib))))))))

(define (list-set xs idx x)
  (let loop ([i 0] [rem xs])
    (if (>= i idx)
      (cons x (rem xs))
      (cons (car rem) (loop (+ i 1) (cdr rem))))))

(define (ex:library-visited?-set! lib b) (set-car! (cdr (cdr (cdr (cdr (cddddr lib))))) b))
(define (ex:library-invoked?-set! lib b) (set-car! (cdr (cdr (cdr (cdr (cdr (cddddr lib)))))) b))

(define (ex:library-invoked?-set lib b) (list-set lib 9 b))
(define (ex:library-visited?-set lib b) (list-set lib 8 b))

(define ex:imported '())
(define (ex:import-libraries-for imports builds phase importer run-or-expand)
  (define (import-libraries imports builds phase)
    (for-each (lambda (import build)
                (let ((name   (car import))
                      (levels (cdr import)))
                  (for-each (lambda (level)
                              (import-library name build (+ phase level)))
                            levels)))
              imports
              builds)
    (values))
  (define (import-library name build phase)
    (if (not (member (cons name (cons phase run-or-expand)) ex:imported))
        (let ((library (ex:lookup-library name)))
          (or (not build)
              (eq? build (ex:library-build library))
              (assertion-violation 
               'import "Client was expanded against a different build of this library" name))
          (import-libraries (ex:library-imports library) 
                            (ex:library-builds library)
                            phase)
          (importer library phase ex:imported)
          (set! ex:imported (cons (cons name (cons phase run-or-expand)) ex:imported)))))
  (import-libraries imports builds phase))

(define (ex:import-libraries-for-run imports builds phase)
  (ex:import-libraries-for imports 
                           builds
                           phase 
                           (lambda (library phase imported)
                             (if (and (= phase 0)
                                      (not (ex:library-invoked? library)))
                                 (begin 
                                   ((ex:library-invoker library))
                                   (ex:library-invoked?-set! library #t))))
                           'run))

(define ex:register-library! #f)
(define ex:lookup-library    #f)
(let ((table '()))
  (set! ex:register-library! 
        (lambda (library)
          (set! table (cons library table))
          (set! ex:imported (filter (lambda (entry)
                                      (not (equal? (ex:library-name library) 
                                                   (car entry))))
                                    ex:imported))))
  (set! ex:lookup-library 
        (lambda (name)
          (let ((library (assoc name table)))
            (if library
                library
                (assertion-violation 'lookup-library "Library not loaded" name)
                ;; AUTOMATIC LOADING ON IMPORT OF LIBRARIES:
                ;; Instead of assertion-violation, something like the following
                ;; can be used to load libraries automatically
                ;;(begin
                ;;  (ex:load (library-name->filename name)) 
                ;;  (ex:lookup-library name))
                )))))

;; Only instantiate part of the bootstrap library 
;; that would be needed for invocation at runtime.

(ex:register-library! 
 (let ((error (lambda () 
                (assertion-violation 
                 'runtime.scm
                 "Attempt to use runtime instance of (core primitive-macros) for expansion.  Make sure expander.scm is loaded after runtime.scm."))))
   (ex:make-library
    '(core primitive-macros)
    ;; envs
    error
    ;; exports
    '()
    ;; imported-libraries
    '()
    ;; builds
    '()
    ;; visiter
    error
    ;; invoker
    (lambda () (values))
    ;; build
    'system)))
;end "runtime.scm")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(load "expander.scm")
;;;=================================================================================
;;;
;;; R6RS Macros and R6RS libraries:
;;;
;;;   Copyright (c) 2006 Andre van Tonder
;;;
;;;   Copyright statement at http://srfi.schemers.org/srfi-process.html
;;;
;;;=================================================================================
;;;
;;;=================================================================================
;;;
;;; PORTING COMMENTS:
;;;
;;;=================================================================================
;;;
;;; The file compat-*.scm has to be loaded before loading this expander.
;;;
;;; Compat-*.scm should supply whatever is missing from your implementation of
;;; the following.
;;;
;;; NOTE: A purely r5rs approximation is provided that can be used
;;;       as a customization template.
;;;
;;;  - Procedures assertion-violation, memp, filter, for-all, pretty-print,
;;;    file-exists? and delete-file.
;;;  - Procedures make-record-type-descriptor, make-record-constructor-descriptor,
;;;    record-constructor, record-predicate and record-accessor.
;;;  - Procedure (ex:unique-token) that provides a numeric GUID string once per run.
;;;  - Single-character string ex:guid-prefix.  No builtin may start with this.
;;;  - Single-character string ex:free-prefix.  No builtin may start with this.
;;;  - Value ex:undefined representing the letrec black hole value.
;;;  - Symbol ex:undefined-set! representing the corresponding setter.
;;;
;;; HOOKS:
;;; ------
;;;
;;; For compiler and REPL integration, see the procedures
;;;
;;;   - ex:repl              : Use this as REPL evaluator.  See description below.
;;;   - ex:expand-file       : Use this to expand a file containing libraries and/or
;;;                            toplevel programs before loading into an r5rs-type system
;;;                            or feeding result to an r5rs-type compiler.
;;;                            Suitable for separate compilation.
;;;   - ex:run-r6rs-sequence : Evaluates a sequence of forms of the format
;;;                            <library>* | <library>* <toplevel program>.
;;;                            The <toplevel program> environment is separate from the 
;;;                            interactive REPL environment and does not persist
;;;                            between invocations of run-r6rs-sequence.  
;;;                            For importing and evaluating stuff in the persistent 
;;;                            interactive environment, ex:REPL should be used instead.
;;;   - ex:run-r6rs-program  : Same as ex:run-r6rs-sequence, except that it reads the 
;;;                            input from a file.
;;;   - ex:expand-r5rs-file  : For expanding r5rs-like toplevel files in a given environment.
;;;                            Mainly provided so this expander can expand itself, but may
;;;                            have other uses.  See the documentation below where the
;;;                            procedure is defined.  See also the note below on
;;;                            metacircularity.
;;;
;;; COMPILATION:
;;; ------------
;;;
;;; Example compilation scripts can be seen in examples.scm.
;;; The expander expands everything to r5rs toplevel definitions
;;; and expressions, so the expanded code should be compilable
;;; with an r5rs compiler.
;;;
;;; REPL:
;;; -----
;;;
;;; Example REPL interaction can be seen in examples.scm.
;;;
;;; The REPL goes beyond r6rs to allow incremental development in
;;; a toplevel environment.
;;; The developer can freely change, replace and make new toplevel
;;; definitions, evaluate toplevel expressions, enter libraries and
;;; <toplevel programs> at the prompt, as well as import libraries
;;; into the toplevel environment.
;;;    
;;; EX:REPL evaluates a sequence of library definitions, commands, and top-level 
;;; import forms in the interactive environment.  The semantics for 
;;; evaluating libraries in and importing bindings into the interactive 
;;; environment is consistent with the ERR5RS proposal at
;;; http://scheme-punks.cyber-rush.org/wiki/index.php?title=ERR5RS:Libraries.
;;; Bindings in the interactive environment persist between invocations 
;;; of REPL. 
;;;
;;; An example session where I do all these things is in examples.scm.
;;; All an integrator would need to do is to automate the call to
;;; ex:repl in the development system so users don't have to type
;;; (ex:repl '( <code> )) at each prompt.
;;;
;;; FORMAT OF EXPANDED CODE:
;;; ------------------------
;;;
;;; We expand internal and library definitions, as well as letrec and letrec*
;;; completely to lambda and set! (or more accurately, whatever ex:undefined-set!
;;; is set to).  This seems to be the preferred input format for Larceny.
;;; It would be very easy to abstract or change, but we don't bother for now
;;; until implementors other than Larceny show a serious interest.
;;;
;;; METACIRCULARITY AND BOOTSTRAPPING:
;;; ----------------------------------
;;;
;;; This section is mostly of interest for r5rs non-compliant systems.
;;;
;;; The expander relies on r5rs (or r6rs) syntax-rules and letrec-syntax
;;; and should run in a correct r5rs system, but if you don't have 
;;; r5rs macros, you may bootstrap it by expanding the expander itself
;;; first on an R5RS system.
;;; Here is how to do it:
;;;
;;;   (load "compat-mzscheme.scm")   ; for example bootstrapping from mzscheme 
;;;   (load "runtime.scm")
;;;   (load "expander.scm")
;;;   (ex:expand-file "standard-libraries.scm" "standard-libraries.exp")
;;;   (ex:expand-r5rs-file "expander.scm" "expander.exp" (ex:environment '(rnrs base)))
;;; 
;;; The expanded (.exp) files are vanilla Scheme and can then be run on the target
;;; system as follows:
;;;
;;;   (load "compat-chez.scm")       ; for example
;;;   (load "runtime.scm")
;;;   (load "standard-libraries.exp")
;;;   (load "expander.exp")
;;;
;;; SIZE OF OBJECT CODE:
;;; --------------------
;;;
;;; The minimal runtime prerequisites has been separated into a small
;;; include file runtime.scm, which is all that needs to be present for
;;; executing an expanded program that does not contain runtime
;;; uses the exports of (rnrs syntax-case) or (rnrs eval).
;;; See examples.scm for demonstrations of this.
;;;
;;; Expanded libraries may contain identifier environment information
;;; and visit code that could adversely affect the runtime binary size.
;;; This is not a big problem, for several reasons:
;;; First, note that this information is only present in libraries that
;;; define macros.
;;; Second, the size of the environments saved in the object code can
;;; usually be reduced dramatically by using 'only' imports.
;;; Third, the environments, as well as the visit code, can be discarded
;;; completely from the runtime image of a fully expanded program not
;;; using (rnrs syntax-case) or (rnrs eval) at runtime.  It is very
;;; easy to write a little build script that does this.
;;;
;;; The only reason for including this information now in the object code
;;; of a library is to support separate compilation, so one can expand a
;;; library in one session and use macros from the /expanded/ library to
;;; expand another library or program in a new session.  The customization
;;; to get rid of separate compilation, if desired, would be trivial.

;;=================================================================================
;;
;; IMPORTS:
;;
;;=================================================================================
;;
;; The include file runtime.scm has to be loaded before loading this expander
;;
;;=================================================================================
;;
;; EXPORTS:
;;
;;=================================================================================

;; Direct exports:

(define ex:make-variable-transformer #f)
(define ex:identifier?               #f)
(define ex:bound-identifier=?        #f)
(define ex:free-identifier=?         #f)
(define ex:generate-temporaries      #f)
(define ex:datum->syntax             #f)
(define ex:syntax->datum             #f)
(define ex:environment               #f)
(define ex:environment-bindings      #f)
(define ex:eval                      #f)
(define ex:load                      #f)
(define ex:syntax-violation          #f)
(define ex:with-toplevel-parameters  #f)

;; System exports:

(define ex:expand-file               #f)
(define ex:repl                      #f)
(define ex:expand-r5rs-file          #f)
(define ex:run-r6rs-sequence         #f)
(define ex:run-r6rs-program          #f)

;; Indirect exports:

(define ex:invalid-form              #f)
(define ex:register-macro!           #f)
(define ex:syntax-rename             #f)
(define ex:map-while                 #f)
(define ex:dotted-length             #f)
(define ex:dotted-butlast            #f)
(define ex:dotted-last               #f)
(define ex:uncompress                #f)
(define ex:free=?                    #f)
(define ex:read-file                 #f)
(define ex:normalize                 #f)
(define ex:expand-toplevel-sequence  #f)

(letrec-syntax
    ;; Not everyone has the same parameter API:

    ((fluid-let
      (syntax-rules ()
        ((fluid-let () be ...)
         (begin be ...))
        ((fluid-let ((p0 e0) (p e) ...) be ...)
         (let ((saved p0))
           (set! p0 e0)
           (call-with-values (lambda ()
                               (fluid-let ((p e) ...) be ...))
             (lambda results
               (set! p0 saved)
               (apply values results)))))))

     ;; A trivial but extremely useful s-expression matcher.
     ;; Implements a subset of Wright's matcher's patterns.
     ;; Includes additional (syntax id) pattern that matches
     ;; if input is identifier? and free=? to 'id.

     (match
      (syntax-rules ()
        ((match (op arg ...) clause ...)
         (let ((x (op arg ...)))
           (match x clause ...)))
        ((match x)
         (ex:invalid-form x))
        ((match x (pat e ...) clause ...)
         (matcher "base" pat "done" x (e ...) (lambda () (match x clause ...))))))

     (matcher
      (syntax-rules (- ___ ? syntax)
        ((matcher "base" () k arg ...)
         (matcher k (lambda (x sk fk) (if (null? x) (sk) (fk))) () arg ...))
        ((matcher "base" - k arg ...)
         (matcher k (lambda (x sk fk) (sk)) () arg ...))
        ((matcher "base" (syntax id) k arg ...)
         (matcher k
                  (lambda (x sk fk)
                    (if (ex:free=? x 'id) (sk) (fk)))
                  ()
                  arg ...))
        ((matcher "base" (? pred? p) k arg ...)
         (matcher "base" p "predicate" pred? k arg ...))
        ((matcher "predicate" code vars pred? k arg ...)
         (matcher k
                  (lambda (x sk fk)
                    (if (pred? x)
                        (code x sk fk)
                        (fk)))
                  vars
                  arg ...))
        ((matcher "base" (p1 ___ tailp ...) k arg ...)
         (matcher "base" p1 "ellipses" (tailp ...) k arg ...))
        ((matcher "ellipses" code vars (tailp ...) k arg ...)
         (matcher k
                  (lambda (x sk fk)
                    (let loop ((x x)
                               (result '()))
                      (define (match-tail)
                        (match x
                          ((tailp ...)
                           (apply sk (if (null? result)
                                         (map (lambda (ignore) '()) 'vars)
                                         (apply map list (reverse result)))))
                          (- (fk))))
                      (cond ((null? x) (match-tail))
                            ((pair? x)
                             (code (car x)
                                   (lambda car-vars
                                     (loop (cdr x) (cons car-vars result)))
                                   match-tail))
                            (else (fk)))))
                  vars
                  arg ...))
        ((matcher "base" (p1 . p2) k arg ...)
         (matcher "base" p1 "pair" p2 k arg ...))
        ((matcher "pair" car-code car-vars p2 k arg ...)
         (matcher "base" p2 "pair-done" car-code car-vars k arg ...))
        ((matcher "pair-done" cdr-code (cdr-var ...) car-code (car-var ...) k arg ...)
         (matcher k
                  (lambda (x sk fk)
                    (if (pair? x)
                        (car-code (car x)
                                  (lambda (car-var ...)
                                    (cdr-code (cdr x)
                                              (lambda (cdr-var ...)
                                                (sk car-var ... cdr-var ...))
                                              fk))
                                  fk)
                        (fk)))
                  (car-var ... cdr-var ...)
                  arg ...))
        ((matcher "base" #(p ___) k arg ...)
         (matcher "base" (p ___) "vector" k arg ...))
        ((matcher "vector" list-code vars k arg ...)
         (matcher k
                  (lambda (x sk fk)
                    (if (vector? x)
                        (list-code (vector->list x)
                                   sk
                                   fk)
                        (fk)))
                  vars
                  arg ...))
        ((matcher "base" id k arg ...)
         (matcher k (lambda (x sk fk) (sk x)) (id) arg ...))
        ((matcher "done" code vars x (e ...) fk)
         (code x (lambda vars e ...) fk)))))

  (let* (;;==========================================================================
         ;;
         ;; Dynamic parameters:
         ;;
         ;;==========================================================================

         ;; toplevel REPL bindings to be initialized later
         (*toplevel-env*     #f)
         ;; current lexical environment to be initialized later
         (*usage-env*        #f)
         ;; current phase
         (*phase*            0)
         ;; current color for painting identifiers upon renaming to be initialized
         (*color*            #f)
         ;; global table mapping <binding name> of keyword to <macro> object
         (*macro-table*      '())
         ;; maps <symbolic key> of reflected environment to actual <environment>
         (*env-table*        '())
         ;; current library name as list of symbols or '() for toplevel
         (*current-library*  '())
         ;; car of this records bindings already referenced in current body
         ;; for detecting when later definitions may violate lexical scope
         (*used*             (list '()))
         ;; history trace for error reporting
         (*trace*            '())
         ;; whether expanded library introduces identifiers via syntax
         ;; expressions - if not, save lots of space by not including
         ;; env-table in object code
         (*syntax-reflected* #f)

         ;;==========================================================================
         ;;
         ;; Identifiers:
         ;;
         ;;==========================================================================

         ;; <name>             ::= <symbol>
         ;; <colors>           ::= (<color> ...)
         ;; <transformer-envs> ::= (<env> ...)
         ;; <displacement>     ::= <integer>
         ;; <maybe-library>    ::= (<symbol> ...) | #f
         ;;
         ;; where
         ;;   <name>             : The symbolic name of the identifier in the source.
         ;;   <colors>           : Each time an introduced identifier is renamed, a fresh
         ;;                        color gets prepended to its <colors>.
         ;;   <transformer-envs> : List of reflected transformer environments.
         ;;                        The environment (env-reify (car <transformer-envs>)) was the
         ;;                        usage environment valid during expansion of any (syntax id)
         ;;                        expression whose evaluation introduced this identifier, while
         ;;                        (cdr <transformer-envs>) are in turn the reflected
         ;;                        <transformer-envs> of the original id.
         ;;   <displacement>     : Integer that keeps track of shifts in phases
         ;;                        between transformer and usage sites of identifier.
         ;;   <maybe-library>    : Library name if identifier was introduced by evaluation of
         ;;                        a (syntax ...) expression, otherwise #f.
         ;;                        The empty name '() is used for toplevel.

         (:identifier
          (make-record-type-descriptor 'identifier #f #f #f #f
                                       '#((immutable name)
                                          (immutable colors)
                                          (immutable transformer-envs)
                                          (immutable displacement)
                                          (immutable maybe-library))))
         (make-identifier
          (record-constructor (make-record-constructor-descriptor :identifier #f #f))))

    ;; We sequenced stuff in the let* above because r5rs internal
    ;; definitions use letrec semantics and cannot be used for sequencing.

    (define identifier?         (record-predicate :identifier))
    (define id-name             (record-accessor :identifier 0))
    (define id-colors           (record-accessor :identifier 1))
    (define id-transformer-envs (record-accessor :identifier 2))
    (define id-displacement     (record-accessor :identifier 3))
    (define id-maybe-library    (record-accessor :identifier 4))

    (define (id-library id)
      (or (id-maybe-library id)
          *current-library*))

    (define (bound-identifier=? x y)
      (check x identifier? 'bound-identifier=?)
      (check y identifier? 'bound-identifier=?)
      (and (eq? (id-name x)
                (id-name y))
           (equal? (id-colors x)
                   (id-colors y))))

    ;; As required by r6rs, when this returns, the result is #t
    ;; if and only if the two identifiers resolve to the same binding.
    ;; It also treats unbound identifiers specially.
    ;; As allowed by R6RS, included phase checking of arguments.
    ;; An out of phase error is raised if the comparison succeeds but
    ;; either argument is out of phase.  This is sufficient to ensure
    ;; that literals such as ... in syntax-case are used in the correct phase.
    ;; For more dicussion on this choice, see the readme and the examples file.

    (define (free-identifier=? x y)
      (check x identifier? 'free-identifier=?)
      (check y identifier? 'free-identifier=?)
      (let  ((bx (binding x))
             (by (binding y)))
        (let ((result (if bx
                          (and by
                               (eq? (binding-name bx)
                                    (binding-name by)))
                          (and (not by)
                               (eq? (id-name x)
                                    (id-name y))))))
          (and result
               bx
               (begin (check-binding-level x bx)
                      (check-binding-level y by)))
          ;; A later definition in the same body can only change
          ;; #t to #f, so only record usage in that case.
          (and result
               (register-use! x bx)
               (register-use! y by))
          result)))

    ;; For internal use

    (define (free=? x symbol)
      (and (identifier? x)
           (let  ((bx (binding x)))
             (let ((result
                    (and bx
                         (eq? (binding-name bx) symbol))))
               (and result
                    bx
                    (check-binding-level x bx))
               (and result
                    (register-use! x bx))
               result))))

    ;;==========================================================================
    ;;
    ;; Infrastructure for generated names:
    ;;
    ;;==========================================================================

    ;; Generate-guid returns a fresh symbol that has a globally
    ;; unique external representation and is read-write invariant.
    ;; Your local gensym will probably not satisfy both conditions.
    ;; Prefix makes it disjoint from all builtins.
    ;; Uniqueness is important for incremental and separate expansion.

    (define generate-guid
      (let ((token (ex:unique-token))
            (ticks 0))
        (lambda (symbol)
          (set! ticks (+ ticks 1))
          (string->symbol
           (string-append ex:guid-prefix
                          (symbol->string symbol)
                          "~"
                          token
                          "~"
                          (number->string ticks))))))

    ;; Used to generate user program toplevel names.
    ;; Prefix makes it disjoint from all builtins.
    ;; Prefix makes it disjoint from output of generate-guid.
    ;; Must be read-write invariant.

    (define (make-free-name symbol)
      (string->symbol (string-append ex:free-prefix (symbol->string symbol))))

    ;;=========================================================================
    ;;
    ;; Colors to paint identifiers with:
    ;;
    ;;=========================================================================

    ;; Returns <color> ::= globally unique symbol

    (define (generate-color)
      (generate-guid 'c))

    ;;=========================================================================
    ;;
    ;; Bindings:
    ;;
    ;;=========================================================================

    ;; <binding> ::= (variable         <binding-name> (<level> ...) <mutable?>  <library-name>)
    ;;            |  (macro            <binding-name> (<level> ...) #f          <library-name>)
    ;;            |  (pattern-variable <binding-name> (<level> ...) <dimension> <library-name>)
    ;;            |  #f  (out of context binding from another library)
    ;; <mutable> ::= #t | #f
    ;; <dimension> ::= 0 | 1 | 2 | ...
    ;; <binding-name> ::= <symbol> uniquely identifying binding.
    ;; <binding-name> is used for free-identifier=? comparison.
    ;; For variable and pattern variable bindings, it is the same
    ;; as the symbol emitted for the binding in the object code.
    ;; For macro bindings, it is the key for looking up the transformer
    ;; in the global macro table.

    (define (make-binding type name levels content library)
      (list type name levels content library))

    (define (binding-type b)           (car b))
    (define (binding-name b)           (cadr b))
    (define (binding-levels b)         (caddr b))
    (define (binding-mutable? b)       (cadddr b))
    (define (binding-dimension b)      (cadddr b))
    (define (binding-library b)        (car (cddddr b)))
    (define (binding-mutable-set! b x) (set-car! (cdddr b) x))

    ;; Looks up binding first in usage environment and
    ;; then in attached transformer environments.
    ;; Toplevel forward references are treated specially.
    ;; Returns <binding> | #f if unbound.

    (define (binding id)
      (let ((name (id-name id)))
        (let loop ((env    *usage-env*)
                   (envs   (id-transformer-envs id))
                   (colors (id-colors id)))
          (or (env-lookup (cons name colors) env)
              (and (pair? envs)
                   (loop (env-reify (car envs))
                         (cdr envs)
                         (cdr colors)))))))

    ;;=========================================================================
    ;;
    ;; Mapping in environment: ((<name> <color> ...) . <binding>)
    ;;
    ;;=========================================================================

    ;; Generates a local mapping at the current meta-level
    ;; that can be added to the usage environment.

    (define (make-local-mapping type id content)
      (cons (cons (id-name id)
                  (id-colors id))
            (make-binding type
                          (generate-guid (id-name id))
                          (list (source-level id))
                          content
                          *current-library*)))

    ;; Toplevel binding forms use as binding name the free name
    ;; so that source-level forward references will work in REPL.
    ;; If identifier is macro-generated, bind it with a fresh name.
    ;; This ensures that generated toplevel defines are not visible
    ;; from toplevel source code, thus approximating the behaviour
    ;; of generated internal definitions.

    (define (make-toplevel-mapping type id content)
      (if (null? (id-colors id))
          (cons (cons (id-name id)
                      (id-colors id))
                (make-binding type
                              (make-free-name (id-name id))
                              '(0)
                              content
                              *current-library*))
          (make-local-mapping type id content)))

    ;;=========================================================================
    ;;
    ;; Infrastructure for binding levels:
    ;;
    ;;=========================================================================

    (define (source-level id)
      (- *phase* (id-displacement id)))

    (define (check-binding-level id binding)
      (if binding
          (or (memv (source-level id)
                    (binding-levels binding))
              (syntax-violation
               "invalid reference"
               (string-append "Attempt to use binding of " (symbol->string (id-name id))
                              " in library (" (list->string (id-library id) " ")
                              ") at invalid level " (number->string (source-level id))
                              ".  Binding is only available at levels: "
                              (list->string (binding-levels binding) " "))
               id))
          (or (and (null? (id-library id))
                   (= *phase* 0))
              (syntax-violation
               "invalid reference"
               (string-append "No binding available for " (symbol->string (id-name id))
                              " in library (" (list->string (id-library id) " ") ")")

               id))))

    ;;=========================================================================
    ;;
    ;; Environments:
    ;;
    ;;=========================================================================

    ;; An environment is a list of frames.
    ;;
    ;;   <environment> ::= (<frame> ...)
    ;;   <frame>       ::= (list ((<key> . <value>) ...))
    ;;
    ;; Keys must be comparable with equal? and unique in each frame.
    ;; Frames can be added, or the leftmost frame can be destructively
    ;; updated in the case of binding constructs such as bodies where
    ;; definitions are incrementally discovered.

    (define (make-null-env) '())
    (define (make-unit-env) (env-extend '() (make-null-env)))

    ;; Adds a new frame containing mappings to env.

    (define (env-extend mappings env)
      (cons (list mappings) env))

    ;; Destructively extends the leftmost frame in env.

    (define (env-extend! mappings env)
      (let ((frame (car env)))
        (set-car! frame (append mappings (car frame)))))

    ;; Returns <object> | #f

    (define (env-lookup key env)
      (and (pair? env)
           (or (let ((probe (assoc key (caar env))))
                 (and probe
                      (or (cdr probe)
                          (syntax-violation
                           #f "Out of context reference to identifier" (car key)))))
               (env-lookup key (cdr env)))))

    ;; Is id already bound in leftmost frame?

    (define (duplicate? id env)
      (assoc (cons (id-name id)
                   (id-colors id))
             (caar env)))

    ;; Returns a single-symbol <key> representing an
    ;; environment that can be included in object code.

    (define (env-reflect env)
      (cond ((and (not (null? *env-table*))      ; +++
                  (eq? env (cdar *env-table*)))  ; +++
             (caar *env-table*))                 ; +++
            (else
             (let ((key (generate-guid 'env)))
               (set! *env-table*
                     (cons (cons key env)
                           *env-table*))
               key))))

    ;; The inverse of the above.

    (define (env-reify key-or-env)
      (if (symbol? key-or-env)
          (cdr (assq key-or-env *env-table*))
          key-or-env))

    ;; This makes a much smaller external representation of an
    ;; environment table by factoring shared structure.

    (define (compress env-table)
      (let ((frame-table '())
            (count 0))
        (for-each (lambda (entry)
                    (for-each (lambda (frame)
                                (if (not (assq frame frame-table))
                                    (begin
                                      (set! frame-table (cons (cons frame count) frame-table))
                                      (set! count (+ 1 count)))))
                              (cdr entry)))
                  env-table)
        (cons (map (lambda (env-entry)
                     (cons (car env-entry)
                           (map (lambda (frame)
                                  (cdr (assq frame frame-table)))
                                (cdr env-entry))))
                   env-table)
              (map (lambda (frame-entry)
                     (cons (cdr frame-entry)
                           (list (map (lambda (mapping)
                                        (cons (car mapping)
                                              (let ((binding (cdr mapping)))
                                                (case (binding-type binding)
                                                  ;; Pattern variable bindings can never be
                                                  ;; used in client, so don't waste space.
                                                  ;; Should really do the same with all local
                                                  ;; bindings, but there are usually much less
                                                  ;; of them, so don't bother for now.
                                                  ((pattern-variable) #f) ; +++
                                                  (else binding)))))
                                      (caar frame-entry)))))
                   frame-table))))

    (define (uncompress compressed-env-table)
      (map (lambda (env-entry)
             (cons (car env-entry)
                   (map (lambda (frame-abbrev)
                          (cdr (assv frame-abbrev (cdr compressed-env-table))))
                        (cdr env-entry))))
           (car compressed-env-table)))

    ;;=========================================================================
    ;;
    ;; Syntax-reflect and syntax-rename:
    ;;
    ;; This is the basic building block of the implicit renaming mechanism for
    ;; maintaining hygiene.  Syntax-reflect generates the expanded code for
    ;; (syntax id), including the expand-time environment in the
    ;; external representation.  It expands to syntax-rename, which performs
    ;; the implicit renaming when this expanded code is eventually run.
    ;; The displacement computations calculate the difference between the
    ;; usage phase and the transformer phase.
    ;;
    ;;=========================================================================

    (define (syntax-reflect id)
      (set! *syntax-reflected* #t)
      `(ex:syntax-rename ',(id-name id)
                         ',(id-colors id)
                         ',(cons (env-reflect *usage-env*)
                                 (id-transformer-envs id))
                         ,(- (- *phase* (id-displacement id)) 1)
                         ',(id-library id)))

    (define (syntax-rename name colors transformer-envs transformer-phase source-library)
      (make-identifier name
                       (cons *color* colors)
                       transformer-envs
                       (- *phase* transformer-phase)
                       source-library))

    ;;=====================================================================
    ;;
    ;; Capture and sexp <-> syntax conversions:
    ;;
    ;;=====================================================================

    (define (datum->syntax tid datum)
      (check tid identifier? 'datum->syntax)
      (sexp-map (lambda (leaf)
                  (cond ((symbol? leaf)
                         (make-identifier leaf
                                          (id-colors tid)
                                          (id-transformer-envs tid)
                                          (id-displacement tid)
                                          (id-maybe-library tid)))
                        (else leaf)))
                datum))

    (define (syntax->datum exp)
      (sexp-map (lambda (leaf)
                  (cond ((identifier? leaf) (id-name leaf))
                        ((symbol? leaf)
                         (assertion-violation 'syntax->datum "A symbol is not a valid syntax object" leaf))
                        (else leaf)))
                exp))

    ;; Fresh identifiers:

    (define (generate-temporaries ls)
      (check ls list? 'generate-temporaries)
      (map (lambda (ignore)
             (make-identifier 'temp
                              (list (generate-color))
                              (list (make-null-env))
                              *phase*
                              #f))
           ls))

    ;; For use internally as in the explicit renaming system.

    (define (rename type symbol)
      (make-identifier symbol
                       (list *color*)
                       (list (env-extend
                              (list (cons (cons symbol '())
                                          (make-binding type symbol '(0) #f '())))
                              (make-null-env)))
                       *phase*
                       #f))

    ;;=========================================================================
    ;;
    ;; Macro objects:
    ;;
    ;;=========================================================================

    ;; Expanders are system macros that fully expand
    ;; their arguments to core Scheme, while
    ;; transformers and variable transformers are
    ;; user macros.

    ;; <type> ::= expander | transformer | variable-transformer

    (define (make-macro type proc)
      (list type proc))
    (define macro-type car)
    (define macro-proc cadr)

    (define (make-expander proc)             (make-macro 'expander proc))
    (define (make-transformer proc)          (make-macro 'transformer proc))
    (define (make-variable-transformer proc) (make-macro 'variable-transformer proc))

    (define (make-user-macro procedure-or-macro)
      (if (procedure? procedure-or-macro)
          (make-transformer procedure-or-macro)
          procedure-or-macro))

    ;; Returns <macro>.

    (define (binding->macro binding t)
      (cond ((assq (binding-name binding) *macro-table*) => cdr)
            (else
             (syntax-violation
              #f "Reference to macro keyword out of context" t))))

    ;; Registering macro.

    (define (register-macro! binding-name procedure-or-macro)
      (set! *macro-table* (cons (cons binding-name (make-user-macro procedure-or-macro))
                                *macro-table*)))

    ;; Calls a macro with a new color.

    (define (invoke-macro macro t)
      (set! *color* (generate-color))
      ((macro-proc macro) t))

    ;;=========================================================================
    ;;
    ;; Expander dispatch:
    ;;
    ;;=========================================================================

    (define (expand t)
      (fluid-let ((*trace* (cons t *trace*)))
        (let ((binding (operator-binding t)))
          (cond (binding (case (binding-type binding)
                           ((macro)
                            (let ((macro (binding->macro binding t)))
                              (let ((expanded-once (invoke-macro macro t)))
                                (case (macro-type macro)
                                  ((expander) expanded-once)
                                  (else
                                   (expand expanded-once))))))
                           ((variable)
                            (check-implicit-import-of-mutable binding t)
                            (if (list? t)
                                (cons (binding-name binding)
                                      (map expand (cdr t)))
                                (binding-name binding)))
                           ((pattern-variable)
                            (syntax-violation #f "Pattern variable used outside syntax template" t))))
                ((list? t)       (map expand t))
                ((identifier? t) (make-free-name (id-name t)))
                ((pair? t)       (syntax-violation #f "Invalid procedure call syntax" t))
                ((symbol? t)     (syntax-violation #f "Symbol may not appear in syntax object" t))
                (else t)))))

    ;; Only expands while t is a user macro invocation.
    ;; Used by expand-lambda to detect internal definitions.

    (define (head-expand t)
      (fluid-let ((*trace* (cons t *trace*)))
        (let ((binding (operator-binding t)))
          (cond (binding (case (binding-type binding)
                           ((macro)
                            (let ((macro (binding->macro binding t)))
                              (case (macro-type macro)
                                ((expander) (values t binding))
                                (else
                                 (head-expand (invoke-macro macro t))))))
                           (else (values t binding))))
                (else (values t binding))))))

    ;; Returns binding of identifier in operator position | #f if none.
    ;; Singleton identifiers are also considered operators here for
    ;; the purpose of discovering identifier macros and variables.
    ;; Checks level and registers as a use.

    (define (operator-binding t)
      (let ((operator (if (pair? t) (car t) t)))
        (and (identifier? operator)
             (let ((binding (binding operator)))
               (check-binding-level operator binding)
               (register-use! operator binding)
               binding))))

    ;; We cannot implicitly import a mutable variable.

    (define (check-implicit-import-of-mutable binding t)
      (or (equal? (binding-library binding) *current-library*)
          (not (binding-mutable? binding))
          (syntax-violation
           #f
           (string-append "Attempt to implicitly import variable that is mutable in library ("
                          (list->string (binding-library binding) " ") ")")
           t)))

    ;;=========================================================================
    ;;
    ;; Quote, if, set!, expression begin, expression let[rec]-syntax, and, or:
    ;;
    ;;=========================================================================

    (define (expand-quote exp)
      (match exp
        ((- datum) (syntax->datum exp))))

    (define (expand-if exp)
      (match exp
        ((- e1 e2 e3) `(if ,(expand e1) ,(expand e2) ,(expand e3)))
        ((- e1 e2)    `(if ,(expand e1) ,(expand e2)))))

    (define (expand-set! exp)
      (match exp
        ((- (? identifier? id) e)
         (let ((binding (binding id)))
           (check-binding-level id binding)
           (register-use! id binding)
           (case (binding-type binding)
             ((macro)
              (let ((macro (binding->macro binding id)))
                (case (macro-type macro)
                  ((variable-transformer)
                   (expand (invoke-macro macro exp)))
                  (else
                   (syntax-violation
                    'set! "Keyword being set! is not a variable transformer" exp id)))))
             ((variable)
              (or (eq? (binding-library binding) *current-library*)
                  (syntax-violation
                   'set! "Directly or indirectly imported variable cannot be assigned" exp id))
              (binding-mutable-set! binding #t)
              `(set! ,(binding-name binding)
                     ,(expand e)))
             ((pattern-variable)
              (syntax-violation 'set! "Pattern variable used outside syntax template" exp id)))))))

    ;; Expression begin.

    (define (expand-begin exp)
      (match exp
        ((- exps ___)
         (scan-sequence 'expression-sequence
                        #f
                        exps
                        (lambda (forms no-syntax-definitions no-bound-variables)
                          `(begin ,@(map cdr forms)))))))

    ;; Expression let(rec)-syntax:

    (define (expand-local-syntax exp)
      (expand-begin `(,(rename 'macro 'begin) ,exp)))

    ;; Define and and or as primitives  so we can import them into the repl
    ;; toplevel without spoiling the and and or of the library language.

    (define (expand-and exp)
      (match exp
        ((and) #t)
        ((and e) (expand e))
        ((and e es ___)
         `(if ,(expand e)
              ,(expand `(,and ,@es))
              #f))))

    (define (expand-or exp)
      (match exp
        ((or) #t)
        ((or e) (expand e))
        ((or e es ___)
         `(let ((x ,(expand e)))
            (if x x ,(expand `(,or ,@es)))))))

    ;;=========================================================================
    ;;
    ;; Lambda:
    ;;
    ;;=========================================================================

    (define (expand-lambda exp)
      (match exp
        ((- (? formals? formals) body ___)
         (fluid-let ((*usage-env*
                      (env-extend (map (lambda (formal)
                                         (make-local-mapping 'variable formal #f))
                                       (flatten formals))
                                  *usage-env*)))
           (let ((formals (dotted-map (lambda (formal) (binding-name (binding formal))) formals)))
             ;; Scan-sequence expects the caller to have prepared
             ;; the frame to which to destructively add bindings.
             ;; Lambda bodies need a fresh frame.
             (fluid-let ((*usage-env* (env-extend '() *usage-env*)))
               (scan-sequence 'lambda
                              make-local-mapping
                              body
                              (lambda (forms syntax-definitions bound-variables)
                                `(lambda ,formals
                                   ,@(if (null? bound-variables)                ; +++
                                         (emit-body forms ex:undefined-set!)    ; +++
                                         `(((lambda ,bound-variables
                                              ,@(emit-body forms ex:undefined-set!))
                                            ,@(map (lambda (ignore) `ex:undefined)
                                                   bound-variables)))))))))))))

    (define (formals? s)
      (or (null? s)
          (identifier? s)
          (and (pair? s)
               (identifier? (car s))
               (formals? (cdr s))
               (not (dotted-memp (lambda (x)
                                   (bound-identifier=? x (car s)))
                                 (cdr s))))))

    ;;=========================================================================
    ;;
    ;; Bodies and sequences:
    ;;
    ;;=========================================================================

    ;; R6RS splicing of internal let-syntax and letrec-syntax
    ;; requires that we remember the bindings visible in each
    ;; form for later expansion of deferred right hand sides
    ;; and expressions.  This is done by attaching
    ;; the environment to the expression.
    ;; We call the resulting data structure a wrap.
    ;; Wraps are only used internally in processing of bodies,
    ;; and are never seen by user macros.

    (define (make-wrap env exp)
      (cons env exp))
    (define wrap-env car)
    (define wrap-exp cdr)

    ;; The continuation k is evaluated in the body environment.  This is
    ;; used for example by expand-library to obtain the correct bindings of
    ;; exported identifiers.
    ;;
    ;; <body-type> ::= toplevel | library | program | lambda | expression-sequence
    ;;
    ;; All but TOPLEVEL are as in r6rs.
    ;; TOPLEVEL is meant for the REPL.
    ;; At TOPLEVEL, we may have a sequence of expressions, definitions, macros,
    ;; import declarations, libraries and programs wrapped in (program ---).
    ;; Redefinitions are allowed at toplevel.

    (define (scan-sequence body-type make-map body-forms k)

      ;; Each <form> ::= (<symbol | #f> #t <wrap>)   (deferred rhs)
      ;;              |  (<symbol | #f> #f <s-expr>) (undeferred rhs)
      ;; Returns ((<symbol | #f> . <s-expr>) ...)

      (define (expand-deferred forms)
        (map (lambda (form)
               (cons (car form)
                     (let ((deferred? (cadr form))
                           (exp       (caddr form)))
                       (if deferred?
                           (fluid-let ((*usage-env* (wrap-env exp)))
                             (expand (wrap-exp exp)))
                           exp))))
             forms))

      (let ((common-env *usage-env*))

        ;; Add new frame for keeping track of bindings used
        ;; so we can detect redefinitions violating lexical scope.
        (add-fresh-used-frame!)

        (let loop ((ws (map (lambda (e) (make-wrap common-env e))
                            body-forms))
                   (forms           '())
                   (syntax-defs     '())
                   (bound-variables '()))
          (cond
           ((null? ws)
            (check-expression-body body-type forms body-forms)
            ;; Add denotations used in this frame to those of parent.
            ;; This is just for the optional reporting of shadowing errors.
            (merge-used-with-parent-frame!)
            (k (reverse (expand-deferred forms))
               (reverse syntax-defs)
               bound-variables))
           (else
            (fluid-let ((*usage-env* (wrap-env (car ws))))
              (call-with-values
                  (lambda () (head-expand (wrap-exp (car ws))))
                (lambda (form operator-binding)
                  (let ((type (and operator-binding (binding-name operator-binding))))
                    (check-expression-sequence body-type type form)
                    (check-toplevel            body-type type form)
                    (case type
                      ((import)
                       (match form
                         ((- specs ___)
                          (call-with-values
                              (lambda () (scan-imports specs))
                            (lambda (imported-libraries imports)
                              (import-libraries-for-expand imported-libraries (map not imported-libraries) 0)
                              (env-import! (car form) imports common-env)
                              (loop (cdr ws)
                                    (cons (list #f #f `(ex:import-libraries-for-run
                                                        ',imported-libraries
                                                        ',(current-builds imported-libraries)
                                                        0))
                                          forms)
                                    syntax-defs
                                    bound-variables))))))
                      ((program)
                       (loop (cdr ws)
                             (cons (list #f #f (expand-program form)) forms)
                             syntax-defs
                             bound-variables))
                      ((library)
                       (loop (cdr ws)
                             (cons (list #f #f (expand-library form)) forms)
                             syntax-defs
                             bound-variables))
                      ((define)
                       (call-with-values
                           (lambda () (parse-definition form #f))
                         (lambda (id rhs)
                           (check-valid-definition id common-env body-type form forms type)
                           (env-extend! (list (make-map 'variable id #f)) common-env)
                           (loop (cdr ws)
                                 (cons (list (binding-name (binding id))
                                             #t
                                             (make-wrap *usage-env* rhs))
                                       forms)
                                 syntax-defs
                                 (cons (binding-name (binding id)) bound-variables)))))
                      ((define-syntax)
                       (call-with-values
                           (lambda () (parse-definition form #t))
                         (lambda (id rhs)
                           (check-valid-definition id common-env body-type form forms type)
                           (let ((mapping (make-map 'macro id #f)))
                             (env-extend! (list mapping) common-env)
                             (let ((rhs (fluid-let ((*phase* (+ 1 *phase*)))
                                          (expand rhs))))
                               (register-macro! (binding-name (cdr mapping)) (make-user-macro (eval rhs)))
                               (loop (cdr ws)
                                     forms
                                     (cons (cons (binding-name (binding id)) rhs) syntax-defs)
                                     bound-variables))))))
                      ((begin)
                       (or (list? form)
                           (invalid-form form))
                       (loop (append (map (lambda (exp)
                                            (make-wrap *usage-env* exp))
                                          (cdr form))
                                     (cdr ws))
                             forms
                             syntax-defs
                             bound-variables))
                      ((let-syntax letrec-syntax)
                       (call-with-values
                           (lambda () (parse-local-syntax form))
                         (lambda (formals rhs body)
                           (let* ((original-env *usage-env*)
                                  (usage-diff   (map (lambda (formal)
                                                       (make-local-mapping 'macro formal #f))
                                                     formals))
                                  (extended-env (env-extend usage-diff original-env))
                                  (rhs-expanded
                                   (fluid-let ((*phase* (+ 1 *phase*))
                                               (*usage-env*
                                                (case type
                                                  ((let-syntax)    original-env)
                                                  ((letrec-syntax) extended-env))))
                                     (map expand rhs)))
                                  (macros (map (lambda (e) (eval e)) rhs-expanded)))
                             (for-each (lambda (mapping macro)
                                         (register-macro! (binding-name (cdr mapping)) (make-user-macro macro)))
                                       usage-diff
                                       macros)
                             (loop (append (map (lambda (form) (make-wrap extended-env form))
                                                body)
                                           (cdr ws))
                                   forms
                                   syntax-defs
                                   bound-variables)))))
                      (else
                       (loop (cdr ws)
                             (cons (list #f #t (make-wrap *usage-env* form))
                                   forms)
                             syntax-defs
                             bound-variables))))))))))))

    (define (emit-body body-forms define-or-set)
      (map (lambda (body-form)
             (if (symbol? (car body-form))
                 `(,define-or-set ,(car body-form) ,(cdr body-form))
                 (cdr body-form)))
           body-forms))

    (define (parse-definition exp syntax-def?)
      (match exp
        ((- (? identifier? id))
         (values id (rename 'variable 'ex:unspecified)))
        ((- (? identifier? id) e)
         (values id e))
        ((- ((? identifier? id) . (? formals? formals)) body ___)
         (and syntax-def?
              (invalid-form exp))
         (values id `(,(rename 'macro 'lambda) ,formals ,@body)))))

    (define (parse-local-syntax t)
      (match t
        ((- ((x e) ___) body ___)
         (or (formals? x)
             (invalid-form t))
         (values x e body))))

    (define (check-expression-sequence body-type type form)
      (and (eq? body-type 'expression-sequence)
           (memq type '(import program library define define-syntax))
           (syntax-violation type "Invalid form in expression sequence" form)))

    (define (check-toplevel body-type type form)
      (and (not (eq? body-type 'toplevel))
           (memq type '(import program library))
           (syntax-violation type "Expression may only occur at toplevel" form)))

    (define (check-valid-definition id common-env body-type form forms type)
      (and (not (eq? body-type 'toplevel))
           (duplicate? id common-env)
           (syntax-violation type "Redefinition of identifier in body" form id))
      (check-used id body-type form)
      (and (not (memq body-type `(toplevel program)))
           (not (null? forms))
           (not (symbol? (car (car forms))))
           (syntax-violation type "Definitions may not follow expressions in a body" form)))

    (define (check-expression-body body-type forms body-forms)
      (and (eq? body-type 'lambda)
           (or (null? forms)
               (symbol? (caar forms)))
           (syntax-violation body-type "Body must be nonempty and end with an expression" body-forms)))

    ;;=========================================================================
    ;;
    ;; Syntax-case:
    ;;
    ;;=========================================================================

    (define (expand-syntax-case exp)
      (define (literal? x)
        (and (identifier? x)
             (not (or (free=? x '_)
                      (free=? x '...)))))
      (match exp
        ((- e ((? literal? literals) ___) clauses ___)
         (let ((input (generate-guid 'input)))
           `(let ((,input ,(expand e)))
              ,(process-clauses clauses input literals))))))

    (define (process-clauses clauses input literals)

      (define (literal? pattern)
        (and (identifier? pattern)
             (memp (lambda (x)
                     (bound-identifier=? x pattern))
                   literals)))

      (define (process-match input pattern sk fk)
        (if (not (symbol? input))
            (let ((temp (generate-guid 'temp)))
              `(let ((,temp ,input))
                 ,(process-match temp pattern sk fk)))
            (match pattern
              ((syntax _)         sk)
              ((syntax ...)       (syntax-violation 'syntax-case "Invalid use of ellipses" pattern))
              (()                 `(if (null? ,input) ,sk ,fk))
              ((? literal? id)    `(if (and (ex:identifier? ,input)
                                            (ex:free-identifier=? ,input ,(syntax-reflect id)))
                                       ,sk
                                       ,fk))
              ((? identifier? id) `(let ((,(binding-name (binding id)) ,input)) ,sk))
              ((p (syntax ...))
               (let ((mapped-pvars (map (lambda (pvar) (binding-name (binding pvar)))
                                        (map car (pattern-vars p 0)))))
                 (if (and (identifier? p)                                   ; +++
                          (= (length mapped-pvars) 1))                      ; +++
                     `(if (list? ,input)                                    ; +++
                          (let ((,(car mapped-pvars) ,input))               ; +++
                            ,sk)                                            ; +++
                          ,fk)                                              ; +++
                     (let ((columns (generate-guid 'cols))
                           (rest    (generate-guid 'rest)))
                       `(ex:map-while (lambda (,input)
                                        ,(process-match input
                                                        p
                                                        `(list ,@mapped-pvars)
                                                        #f))
                                      ,input
                                      (lambda (,columns ,rest)
                                        (if (null? ,rest)
                                            (apply (lambda ,mapped-pvars ,sk)
                                                   (if (null? ,columns)
                                                       ',(map (lambda (ignore) '()) mapped-pvars)
                                                       (apply map list ,columns)))
                                            ,fk)))))))
              ((p (syntax ...) . tail)
               (let ((tail-length (dotted-length tail)))
                 `(if (>= (ex:dotted-length ,input) ,tail-length)
                      ,(process-match `(ex:dotted-butlast ,input ,tail-length)
                                      `(,p ,(cadr pattern))
                                      (process-match `(ex:dotted-last ,input ,tail-length)
                                                     tail
                                                     sk
                                                     fk)
                                      fk)
                      ,fk)))
              ((p1 . p2)
               `(if (pair? ,input)
                    ,(process-match `(car ,input)
                                    p1
                                    (process-match `(cdr ,input) p2 sk fk)
                                    fk)
                    ,fk))
              (#(ps ___)
               `(if (vector? ,input)
                    ,(process-match `(vector->list ,input)
                                    ps
                                    sk
                                    fk)
                    ,fk))
              ((? symbol? -)
               (syntax-violation 'syntax-case "Symbol object may not appear in pattern" pattern))
              (other
               `(if (equal? ,input ',other) ,sk ,fk)))))

      (define (pattern-vars pattern level)
        (match pattern
          ((p (syntax ...) . tail) (append (pattern-vars p (+ level 1))
                                           (pattern-vars tail level)))
          ((p1 . p2)               (append (pattern-vars p1 level)
                                           (pattern-vars p2 level)))
          (#(ps ___)               (pattern-vars ps level))
          ((syntax ...)            '())
          ((syntax _)              '())
          ((? literal? -)          '())
          ((? identifier? id)      (list (cons id level)))
          (-                       '())))

      (define (process-clause clause input fk)
        (match clause
          ((pattern . rest)
           (let ((pvars    (pattern-vars pattern 0)))
             (check-set? (map car pvars)
                         bound-identifier=?
                         (lambda (dup)
                           (syntax-violation 'syntax-case "Repeated pattern variable" clause dup)))
             (let ((mappings (map (lambda (pvar)
                                    (make-local-mapping 'pattern-variable (car pvar) (cdr pvar)))
                                  pvars)))
               (fluid-let ((*usage-env* (env-extend mappings *usage-env*)))
                 (process-match input
                                pattern
                                (match rest
                                  ((template)
                                   (expand template))
                                  ((fender template)
                                   `(if ,(expand fender)
                                        ,(expand template)
                                        ,fk))
                                  (- (syntax-violation 'syntax-case "Invalid clause" clause)))
                                fk)))))))

      ;; process-clauses

      (match clauses
        (()
         `(ex:invalid-form ,input))
        ((clause clauses ___)
         (let ((fail  (generate-guid 'fail)))
           `(let ((,fail (lambda () ,(process-clauses clauses input literals))))
              ,(process-clause clause input `(,fail)))))))

    ;;=========================================================================
    ;;
    ;; Syntax:
    ;;
    ;;=========================================================================

    (define (expand-syntax form)
      (match form
        ((- template)
         (process-template template 0 #f))))

    (define (process-template template dim ellipses-quoted?)
      (match template
        ((syntax ...)
         (if (not ellipses-quoted?)
             (syntax-violation 'syntax "Invalid occurrence of ellipses in syntax template" template))
         (syntax-reflect template))
        ((? identifier? id)
         (let ((binding (binding id)))
           (cond ((and binding
                       (eq? (binding-type binding) 'pattern-variable)
                       (binding-dimension binding))
                  => (lambda (pdim)
                       (if (<= pdim dim)
                           (begin
                             (check-binding-level id binding)
                             (register-use! id binding)
                             (binding-name binding))
                           (syntax-violation 'syntax "Template dimension error (too few ...'s?)" id))))
                 (else
                  (syntax-reflect id)))))
        (((syntax ...) p)
         (process-template p dim #t))
        ((? (lambda (_) (not ellipses-quoted?))
            (t (syntax ...) . tail))
         (let* ((head (segment-head template)) 
                (vars
                 (map (lambda (mapping)
                        (let ((id      (car mapping))
                              (binding (cdr mapping)))
                          (check-binding-level id binding)
                          (register-use! id binding)
                          (binding-name binding)))
                      (free-meta-variables head (+ dim 1) '() 0))))
           (if (null? vars)
               (syntax-violation 'syntax "Too many ...'s" template)
               (let* ((x (process-template head (+ dim 1) ellipses-quoted?))
                      (gen (if (equal? (list x) vars)   ; +++
                               x                        ; +++
                               (if (= (length vars) 1) 
                                   `(map (lambda ,vars ,x)
                                         ,@vars)
                                   `(if (= ,@(map (lambda (var) 
                                                    `(length ,var))
                                                  vars))
                                        (map (lambda ,vars ,x)
                                             ,@vars)
                                        (ex:syntax-violation 
                                         'syntax 
                                         "Pattern variables denoting lists of unequal length preceding ellipses"
                                         ',(syntax->datum template) 
                                         (list ,@vars))))))
                      (gen (if (> (segment-depth template) 1)
                               `(apply append ,gen)
                               gen)))
                 (if (null? (segment-tail template))   ; +++
                     gen                               ; +++
                     `(append ,gen ,(process-template (segment-tail template) dim ellipses-quoted?)))))))
        ((t1 . t2)
         `(cons ,(process-template t1 dim ellipses-quoted?)
                ,(process-template t2 dim ellipses-quoted?)))
        (#(ts ___)
         `(list->vector ,(process-template ts dim ellipses-quoted?)))
        (other
         `(quote ,(expand other)))))
    
    (define (free-meta-variables template dim free deeper)
      (match template
        ((? identifier? id)
         (if (memp (lambda (x) (bound-identifier=? (car x) id)) free)
             free
             (let ((binding (binding id)))
               (if (and binding
                        (eq? (binding-type binding) 'pattern-variable)
                        (let ((pdim (binding-dimension binding)))
                          (and (> pdim 0) 
                               (not (>= deeper pdim))
                               (<= (- pdim deeper) 
                                   dim))))
                   (cons (cons id binding) free)
                   free))))
        ((t (syntax ...) . rest)
         (free-meta-variables t 
                              dim 
                              (free-meta-variables (segment-tail template) dim free deeper)
                              (+ deeper (segment-depth template))))  
        ((t1 . t2)
         (free-meta-variables t1 dim (free-meta-variables t2 dim free deeper) deeper))
        (#(ts ___) 
         (free-meta-variables ts dim free deeper))
        (- free)))
 
    ;; Count the number of `...'s in PATTERN.

    (define (segment-depth pattern)
      (match pattern
        ((p (syntax ...) . rest)
         (+ 1 (segment-depth (cdr pattern))))
        (- 0)))
      
    ;; All but the last ellipses
    
    (define (segment-head pattern)
      (let ((head
             (let recur ((pattern pattern))
               (match pattern
                 ((h (syntax ...) (syntax ...) . rest)
                  (cons h (recur (cdr pattern))))
                 ((h (syntax ...) . rest)
                  (list h))))))
        (match head 
          ((h (syntax ...) . rest)
           head)
          (- (car head)))))   

    ;; Get whatever is after the `...'s in PATTERN.

    (define (segment-tail pattern)
      (let loop ((pattern (cdr pattern)))
        (match pattern
          (((syntax ...) . tail)
           (loop tail))
          (- pattern))))

    ;;=========================================================================
    ;;
    ;; Detecting violations of lexical scope.
    ;;
    ;;=========================================================================

    ;; This is r6rs-optional.
    ;; For avoiding giving lexically invalid semantics to body
    ;; sequences according to the following semantics described in r6rs:
    ;; A definition in the sequence of forms must not define any
    ;; identifier whose binding is used to determine the meaning of the
    ;; undeferred portions of the definition or any definition that precedes
    ;; it in the sequence of forms.
    ;; This implementation treats a possble violation of the restriction
    ;; as a syntax violation.

    ;; The parameter *used* keeps track of bindings used so we can
    ;; detect redefinitions violating lexical scope in body sequences.
    ;; The car of *used* contains bindings used in current frame.

    (define (add-fresh-used-frame!)
      (set! *used* (cons '() *used*)))

    (define (register-use! id binding)
      (set! *used* (cons (cons (cons id binding)
                               (car *used*))
                         (cdr *used*))))

    (define (merge-used-with-parent-frame!)
      (set! *used* (cons (append (car  *used*)
                                 (cadr *used*))
                         (cddr *used*))))

    (define (check-used id body-type form)
      (and (not (eq? body-type 'toplevel))
           ;; The car contains bindings for current frame and nested frames
           (let* ((already-used (car *used*))
                  ;; This destructively changes *used* and must follow previous
                  (binding (binding id)))
             (if (memp (lambda (mapping)
                         (and (eq? binding (cdr mapping))
                              (bound-identifier=? id (car mapping))))
                       already-used)
                 (syntax-violation
                  'definition
                  "Definition of identifier that may have already affected meaning of undeferred portions of body"
                  form
                  id)))))

    ;;==========================================================================
    ;;
    ;; Libraries:
    ;;
    ;;==========================================================================

    (define (expand-program t)
      (match t
        ((program import-clause forms ___)
         (expand-library-or-program
          `(,program (,(datum->syntax program (generate-guid 'program)))
                     (,(datum->syntax program 'export))
                     ,import-clause
                     ,@forms)
          'program))))

    (define (expand-library t)
      (expand-library-or-program t 'library))

    ;; <library-type> ::= library | program

    (define (expand-library-or-program t library-type)
      (match t
        ((keyword name ((syntax export) sets ___) ((syntax import) specs ___) body-forms ___)
         (let ((name (syntax->datum (scan-library-name name))))
           (let ((exports (scan-exports sets)))
             (call-with-values
                 (lambda () (scan-imports specs))
               (lambda (imported-libraries imports)
                 (fluid-let ((*usage-env*        (make-unit-env))
                             (*current-library*  name)
                             (*syntax-reflected* #f))       ; +++ space

                   (import-libraries-for-expand imported-libraries (map not imported-libraries) 0)
                   (env-import! keyword imports *usage-env*)

                   (let ((initial-env-table *env-table*))   ; +++ space
                     (scan-sequence library-type
                                    make-local-mapping
                                    body-forms
                                    (lambda (forms syntax-definitions bound-variables)
                                      (let* ((exports
                                              (map (lambda (mapping)
                                                     (cons (id-name (car mapping))
                                                           (let ((binding (binding (cadr mapping))))
                                                             (or binding
                                                                 (syntax-violation
                                                                  'library "Unbound export" t (cadr mapping)))
                                                             (if (binding-mutable? binding)
                                                                 (syntax-violation
                                                                  'library "Attempt to export mutable variable" t (cadr mapping)))
                                                             binding)))
                                                   exports))
                                             (expanded-library
                                              (case library-type
                                                ((program)
                                                 `(begin
                                                    (ex:import-libraries-for-run ',imported-libraries
                                                                                 ',(current-builds imported-libraries)
                                                                                 0)
                                                    ,@(emit-body forms 'define)))
                                                ((library)
                                                 `(begin
                                                    ,@(map (lambda (var)
                                                             `(define ,var ex:unspecified))
                                                           bound-variables)
                                                    (ex:register-library!
                                                     (ex:make-library
                                                      ',name
                                                      ;; Store as thunk so that it is not unnecesarily
                                                      ;; uncompressed at runtime
                                                      (lambda ()
                                                        ,(if *syntax-reflected*                     ; +++ space
                                                             `(ex:uncompress                        ; +++ space
                                                               ',(compress (drop-tail
                                                                            *env-table*
                                                                            initial-env-table)))
                                                             `'()))                                 ; +++ space
                                                      ',exports
                                                      ',imported-libraries
                                                      ',(current-builds imported-libraries)
                                                      ;; visit
                                                      (lambda ()
                                                        ,@(map (lambda (def)
                                                                 `(ex:register-macro! ',(car def) ,(cdr def)))
                                                               syntax-definitions)
                                                        (values))
                                                      ;; invoke
                                                      (lambda ()
                                                        ,@(map (lambda (var)
                                                                 `(set! ,var ex:undefined))
                                                               bound-variables)
                                                        ,@(emit-body forms ex:undefined-set!)
                                                        (values))
                                                      ;; build
                                                      ',(generate-guid 'build)))
                                                    (values))))))

                                        ;; Register library for any further expansion.
                                        (if (eq? library-type 'library)
                                            (eval expanded-library))

                                        expanded-library))))))))))))

    (define (env-import! keyword imports env)
      (env-extend! (map (lambda (import)
                          (cons (cons (car import)
                                      (id-colors keyword))
                                (cdr import)))
                        imports)
                   env))

    (define (current-builds imported-libraries)
      (map (lambda (lib-entry)
             (ex:library-build (ex:lookup-library (car lib-entry))))
           imported-libraries))

    (define (import-libraries-for-expand imports builds phase)
      (ex:import-libraries-for
       imports
       builds
       phase
       (lambda (library phase imported)
         (if (and (>= phase 0)
                  (not (ex:library-visited? library)))
             (begin
               (set! *env-table* (append ((ex:library-envs library)) *env-table*))
               ((ex:library-visiter library))
               (ex:library-visited?-set! library #t)))
         (if (and (>= phase 1)
                  (not (ex:library-invoked? library)))
             (begin 
               ((ex:library-invoker library))
               (ex:library-invoked?-set! library #t))))
       'expand))

    ;; Returns ((<rename-identifier> <identifier> <level> ...) ...)

    (define (scan-exports sets)
      (let ((exports (apply append (map scan-export-set sets))))
        (check-set? exports
                    (lambda (x y)
                      (eq? (id-name (car x))
                           (id-name (car y))))
                    (lambda (dup) (syntax-violation 'export "Duplicate export" sets dup)))
        exports))

    (define (scan-export-set set)
      (match set
        ((? identifier? x)
         `((,x ,x 0)))
        (((syntax rename) ((? identifier? xs) (? identifier? ys)) ___)
         (map (lambda (x y) `(,y ,x 0)) xs ys))
        (- (syntax-violation 'export "Invalid export set" set))))

    ;; Returns
    ;;    (values ((<library reference> <level> ...) ....)
    ;;            ((<local name> . <binding>) ...))
    ;; with no repeats.

    (define (scan-imports specs)
      (let loop ((specs specs)
                 (imported-libraries '())
                 (imports '()))
        (if (null? specs)
            (values imported-libraries (unify-imports imports))
            (call-with-values
                (lambda () (scan-import-spec (car specs)))
              (lambda (library-ref levels more-imports)
                (loop (cdr specs)
                      ;; library-ref = #f if primitives spec
                      (if library-ref
                          (cons (cons library-ref levels)
                                imported-libraries)
                          imported-libraries)
                      (append more-imports imports)))))))

    ;; Returns (values <library reference> | #f
    ;;                 (<level> ...)
    ;;                 ((<local name> . <binding>) ...)
    ;; where <level> ::= <integer>
    ;; #f is returned for library name in case of primitives.

    (define (scan-import-spec spec)

      (call-with-values
          (lambda () (scan-levels spec))
        (lambda (levels import-set)
          (let loop ((import-set import-set)
                     (adjuster (lambda (set) set)))

            (define (check-presence names mappings from)
              (for-each (lambda (name)
                          (or (assq name mappings)
                              (syntax-violation from
                                                (string-append "Identifier not in set: "
                                                               (list->string (map car mappings) " "))
                                                import-set
                                                name)))
                        names))

            (match import-set
              (((syntax primitives) (? identifier? xs) ___)
               (values #f
                       levels
                       (map (lambda (mapping)
                              (cons (car mapping) (make-binding 'variable (cdr mapping) levels #f '())))
                            (adjuster (map (lambda (name) (cons name name))
                                           (syntax->datum xs))))))
              (((syntax only) set (? identifier? xs) ___)
               (let ((args (syntax->datum xs)))
                 (loop set
                       (compose adjuster (lambda (mappings)
                                           (check-presence args mappings 'only)
                                           (filter (lambda (mapping)
                                                     (memq (car mapping) args))
                                                   mappings))))))
              (((syntax except) set (? identifier? xs) ___)
               (let ((args (syntax->datum xs)))
                 (loop set
                       (compose adjuster (lambda (mappings)
                                           (check-presence args mappings 'except)
                                           (filter (lambda (mapping)
                                                     (not (memq (car mapping) args)))
                                                   mappings))))))
              (((syntax prefix) set (? identifier? pre))
               (loop set
                     (compose adjuster (lambda (mappings)
                                         (map (lambda (mapping)
                                                (cons (string->symbol
                                                       (string-append
                                                        (symbol->string (syntax->datum pre))
                                                        (symbol->string (car mapping))))
                                                      (cdr mapping)))
                                              mappings)))))
              (((syntax rename) set ((? identifier? xs) (? identifier? ys)) ___)
               (let ((args (syntax->datum (cddr import-set))))
                 (loop set
                       (compose adjuster
                                (lambda (mappings)
                                  (check-presence (map car args) mappings 'rename)
                                  (map (lambda (mapping)
                                         (cons (cond ((assq (car mapping) args) => cadr)
                                                     (else (car mapping)))
                                               (cdr mapping)))
                                       mappings))))))
              (((syntax primitives) . -) (invalid-form import-set))
              (((syntax only)       . -) (invalid-form import-set))
              (((syntax except)     . -) (invalid-form import-set))
              (((syntax prefix)     . -) (invalid-form import-set))
              (((syntax rename)     . -) (invalid-form import-set))
              (-
               (let ((library-ref (library-ref import-set)))
                 (if library-ref
                     (let* ((library (ex:lookup-library (syntax->datum library-ref)))
                            (exports (ex:library-exports library))
                            (imports
                             (map (lambda (mapping)
                                    (cons (car mapping)
                                          (let ((binding (cdr (assq (cdr mapping) exports))))
                                            (make-binding (binding-type binding)
                                                          (binding-name binding)
                                                          (compose-levels levels (binding-levels binding))
                                                          (binding-mutable? binding)
                                                          (binding-library binding)))))
                                  (adjuster (map (lambda (name) (cons name name))
                                                 (map car exports))))))
                       (values (syntax->datum library-ref)
                               levels
                               imports))
                     (syntax-violation 'import "Invalid import set" import-set)))))))))

    (define (scan-levels spec)
      (match spec
        (((syntax for) set levels ___)
         (let ((levels
                (map (lambda (level)
                       (match level
                         ((syntax run)                   0)
                         ((syntax expand)                1)
                         (((syntax meta) (? integer? n)) n)
                         (- (syntax-violation 'for "Invalid level in for spec" spec level))))
                     levels)))
           (check-set? levels = (lambda (dup) (syntax-violation 'for "Repeated level in for spec" spec dup)))
           (values levels set)))
        (- (values '(0) spec))))

    (define (compose-levels levels levels*)
      (apply unionv
             (map (lambda (level)
                    (map (lambda (level*)
                           (+ level level*))
                         levels*))
                  levels)))

    ;; Argument is of the form ((<local name> <binding>) ...)
    ;; where combinations (<local name> (binding-name <binding>)) may be repeated.
    ;; Return value is of the same format but with no repeats and
    ;; where union of (binding-levels <binding>)s has been taken for any original repeats.
    ;; An error is signaled if same <local name> occurs with <binding>s
    ;; whose (binding-name <binding>)s are different.

    (define (unify-imports imports)
      (let ((seen '()))
        (let loop ((imports imports))
          (if (null? imports)
              seen
              (let* ((mapping (car imports))
                     (probe (assq (car mapping) seen)))
                (if probe
                    (begin
                      (or (eq? (binding-name (cdr mapping))
                               (binding-name (cdr probe)))
                          (syntax-violation
                           'import
                           (string-append "Different bindings for identifier imported from libraries ("
                                          (list->string (binding-library (cdr mapping)) " ")
                                          ") and ("
                                          (list->string (binding-library (cdr probe)) " ") ")")
                           (car mapping)))
                      (set-cdr! probe
                                (make-binding (binding-type (cdr probe))
                                              (binding-name (cdr probe))
                                              (unionv (binding-levels (cdr probe))
                                                      (binding-levels (cdr mapping)))
                                              (binding-mutable? (cdr probe))
                                              (binding-library (cdr probe)))))
                    (set! seen (cons mapping seen)))
                (loop (cdr imports)))))))

    (define (scan-library-name e)
      (library-ref-helper e version?))

    (define (library-ref e)
      (library-ref-helper
       (match e
         (((syntax library) name) name)
         (((syntax library) . -)  (invalid-form e))
         (- e))
       version-reference?))

    (define (library-ref-helper e version?)
      (match e
        (((? identifier? ids) ___)                ids)
        (((? identifier? ids) ___ (? version? -)) ids)
        (- (syntax-violation 'library "Invalid library reference" e))))

    (define (version? e)
      (and (list? e)
           (for-all subversion? e)))

    (define (subversion? x)
      (and (integer? x)
           (>= x 0)))

    (define (version-reference? e)
      (match e
        (((syntax and) (? version-reference? -) ___) #t)
        (((syntax or)  (? version-reference? -) ___) #t)
        (((syntax not) (? version-reference? -))     #t)
        (((? subversion-reference? -) ___)           #t)
        (-                                           #f)))

    (define (subversion-reference? e)
      (or (subversion? e)
          (subversion-condition? e)))

    (define (subversion-condition? e)
      (match e
        (((syntax >=)  (? subversion? -))               #t)
        (((syntax <=)  (? subversion? -))               #t)
        (((syntax not) (? subversion? -))               #t)
        (((syntax and) (? subversion-reference? -) ___) #t)
        (((syntax or)  (? subversion-reference? -) ___) #t)
        (-                                              #f)))

    ;;==========================================================================
    ;;
    ;; Debugging facilities:
    ;;
    ;;==========================================================================

    (define (syntax-violation who message form . maybe-subform)
      (newline (current-error-port))
      (display "Syntax violation: " (current-error-port))
      (let ((who (if who
                     who
                     (cond ((identifier? form)
                            (syntax->datum form))
                           ((and (list? form)
                                 (identifier? (car form)))
                            (syntax->datum (car form)))
                           (else ""))))
            (subform (cond ((null? maybe-subform) #f)
                           ((and (pair? maybe-subform)
                                 (null? (cdr maybe-subform)))
                            (car maybe-subform))
                           (else (assertion-violation 'syntax-violation
                                                      "Invalid subform in syntax violation"
                                                      maybe-subform)))))
        (display who (current-error-port))
        (newline (current-error-port))
        (newline (current-error-port))
        (display message (current-error-port))
        (newline (current-error-port))
        (newline (current-error-port))
        (if subform
            (begin (display "Subform: " (current-error-port))
                   (pretty-print (syntax-debug subform) (current-error-port))
                   (newline (current-error-port))))
        (display "Form: " (current-error-port))
        (pretty-print (syntax-debug form) (current-error-port))
        (newline (current-error-port))
        (display "Trace: " (current-error-port))
        (newline (current-error-port))
        (newline (current-error-port))
        (for-each (lambda (exp)
                    (display "  " (current-error-port))
                    (pretty-print (syntax-debug exp) (current-error-port))
                    (newline (current-error-port)))
                  *trace*)
        (error 'syntax-violation "Integrate with host error handling here")))

    (define (syntax-debug exp)
      (sexp-map (lambda (leaf)
                  (cond ((identifier? leaf)
                         (id-name leaf))
                        (else leaf)))
                exp))

    ;;==========================================================================
    ;;
    ;;  Eval and environment:
    ;;
    ;;==========================================================================

    (define eval-template
      (make-identifier 'eval-template
                       '()
                       '()
                       0
                       `(anonymous)))

    (define (make-r6rs-environment imported-libraries env)
      (cons imported-libraries env))
    (define r6rs-environment-imported-libraries car)
    (define r6rs-environment-env                cdr)

    (define (environment . import-specs)
      (fluid-let ((*usage-env* (make-unit-env)))
        (env-import! eval-template (make-library-language) *usage-env*)
        (call-with-values
            (lambda () 
              (fluid-let ((*phase* 0))
                (scan-imports
                 (map (lambda (spec)
                        (datum->syntax eval-template spec))
                      import-specs))))
          (lambda (imported-libraries imports)
            (make-r6rs-environment imported-libraries
                                   (let ((env (make-unit-env)))
                                     (env-import! eval-template imports env)
                                     env))))))

    (define (r6rs-eval exp env)
      (fluid-let ((*usage-env* (r6rs-environment-env env)))
        (let ((exp (datum->syntax eval-template exp))
              (imported-libraries (r6rs-environment-imported-libraries env)))
          (import-libraries-for-expand (r6rs-environment-imported-libraries env) (map not imported-libraries) 0)
          (ex:import-libraries-for-run (r6rs-environment-imported-libraries env) (map not imported-libraries) 0)
          (eval (expand-begin
                 ;; wrap in expression begin so no definition can occur as required by r6rs
                 `(,(rename 'macro 'begin) ,exp))))))

    ;;==========================================================================
    ;;
    ;; Library reflection:
    ;;
    ;;=========================================================================

    (define (environment-bindings r6rs-env)
      (map format-mapping
           (caar (r6rs-environment-env r6rs-env))))

    (define (format-mapping mapping)
      `((name ,(caar mapping))
        (type ,(binding-type (cdr mapping)))
        (from ,(binding-library (cdr mapping)))
        (levels ,(binding-levels (cdr mapping)))))

    ;;=====================================================================
    ;;
    ;; Utilities:
    ;;
    ;;=====================================================================

    (define (flatten l)
      (cond ((null? l) l)
            ((pair? l) (cons (car l)
                             (flatten (cdr l))))
            (else (list l))))

    (define (sexp-map f s)
      (cond ((null? s) '())
            ((pair? s) (cons (sexp-map f (car s))
                             (sexp-map f (cdr s))))
            ((vector? s)
             (apply vector (sexp-map f (vector->list s))))
            (else (f s))))

    (define (dotted-memp proc ls)
      (cond ((null? ls) #f)
            ((pair? ls) (if (proc (car ls))
                            ls
                            (dotted-memp proc (cdr ls))))
            (else (and (proc ls)
                       ls))))

    (define (dotted-map f lst)
      (cond ((null? lst) '())
            ((pair? lst) (cons (f (car lst))
                               (dotted-map f (cdr lst))))
            (else (f lst))))

    ;; Returns 0 also for non-list a la SRFI-1 protest.

    (define (dotted-length dl)
      (cond ((null? dl) 0)
            ((pair? dl) (+ 1 (dotted-length (cdr dl))))
            (else 0)))

    (define (dotted-butlast ls n)
      (let recurse ((ls ls)
                    (length-left (dotted-length ls)))
        (cond ((< length-left n) (assertion-violation 'dotted-butlast "List too short" ls n))
              ((= length-left n) '())
              (else
               (cons (car ls)
                     (recurse (cdr ls)
                              (- length-left 1)))))))

    (define (dotted-last ls n)
      (let recurse ((ls ls)
                    (length-left (dotted-length ls)))
        (cond ((< length-left n) (assertion-violation 'dotted-last "List too short" ls n))
              ((= length-left n) ls)
              (else
               (recurse (cdr ls)
                        (- length-left 1))))))

    (define (map-while f lst k)
      (cond ((null? lst) (k '() '()))
            ((pair? lst)
             (let ((head (f (car lst))))
               (if head
                   (map-while f
                              (cdr lst)
                              (lambda (answer rest)
                                (k (cons head answer)
                                   rest)))
                   (k '() lst))))
            (else  (k '() lst))))

    (define (check-set? ls = fail)
      (or (null? ls)
          (if (memp (lambda (x)
                      (= x (car ls)))
                    (cdr ls))
              (fail (car ls))
              (check-set? (cdr ls) = fail))))

    (define (unionv . sets)
      (cond ((null? sets) '())
            ((null? (car sets))
             (apply unionv (cdr sets)))
            (else
             (let ((rest (apply unionv
                                (cdr (car sets))
                                (cdr sets))))
               (if (memv (car (car sets)) rest)
                   rest
                   (cons (car (car sets)) rest))))))
    
    (define (drop-tail list tail)
      (cond ((null? list)    '())
            ((eq? list tail) '())
            (else
             (cons (car list)
                   (drop-tail (cdr list) tail)))))

    (define (list->string e separator)
      (define (tostring x)
        (cond ((symbol? x)
               (symbol->string x))
              ((number? x)
               (number->string x))
              (else
               (assertion-violation 'list->string "Invalid argument" e))))
      (if (null? e)
          ""
          (string-append
           (tostring (car e))
           (apply string-append
                  (map (lambda (x)
                         (string-append separator (tostring x)))
                       (cdr e))))))

    (define (compose f g)
      (lambda (x) (f (g x))))

    (define (check x p? from)
      (or (p? x)
          (syntax-violation from "Invalid argument" x)))

    (define (invalid-form exp)
      (syntax-violation #f "Invalid form" exp))

    ;;============================================================================
    ;;
    ;; REPL integration:
    ;;
    ;;============================================================================

    ;; Evaluates a sequence of library definitions, commands, and top-level 
    ;; import forms in the interactive environment.  The semantics for 
    ;; evaluating libraries in and importing bindings into the interactive 
    ;; environment is consistent with the ERR5RS proposal at
    ;; http://scheme-punks.cyber-rush.org/wiki/index.php?title=ERR5RS:Libraries.
    ;; Bindings in the interactive environment persist between invocations 
    ;; of REPL.
    
    (define (repl exps)
      (with-toplevel-parameters
       (lambda ()
         (for-each (lambda (exp)
                     (for-each (lambda (exp)
                                 (for-each (lambda (result)
                                             (display result)
                                             (newline))
                                           (call-with-values
                                            (lambda ()
                                              (eval exp))
                                            list)))
                               (expand-toplevel-sequence (list exp))))
                   exps))))
    
    ;; Evaluates a sequence of forms of the format
    ;; <library>* | <library>* <toplevel program>.
    ;; The <toplevel program> environment is separate from the 
    ;; interactive REPL environment and does not persist
    ;; between invocations of run-r6rs-sequence.  
    ;; For importing and evaluating stuff in the persistent 
    ;; interactive environment, see REPL above.
    
    (define (run-r6rs-sequence forms)
      (with-toplevel-parameters
       (lambda ()
         (for-each (lambda (exp) (eval exp))
                   (expand-toplevel-sequence (normalize forms))))))
    
    (define (run-r6rs-program filename)
      (run-r6rs-sequence (read-file filename)))

    ;; Puts parameters to a consistent state for the toplevel
    ;; Old state is restored afterwards so that things will be
    ;; reentrant. 

    (define with-toplevel-parameters
      (lambda (thunk)
        (fluid-let ((*trace*            '())
                    (*current-library*  '())
                    (*phase*            0)
                    (*used*             (list '()))
                    (*color*            (generate-color))
                    (*usage-env*        *toplevel-env*)
                    (*syntax-reflected* #f))
          (thunk))))
    
    (define (expand-toplevel-sequence forms)
      (scan-sequence 'toplevel
                     make-toplevel-mapping
                     (source->syntax forms)
                     (lambda (forms syntax-definitions bound-variables)
                       (emit-body forms 'define))))

    ;; ERR5RS load:
    ;; We take some care to make this reentrant so that 
    ;; it can be used to recursively load libraries while
    ;; expanding a client library or program.
    
    (define (r6rs-load filename)
      (with-toplevel-parameters
       (lambda ()
         (for-each (lambda (exp)
                     (for-each (lambda (exp)
                                 (eval exp))
                               (expand-toplevel-sequence (list exp))))
                   (read-file filename)))))
      
    ;; This may be used as a front end for the compiler.
    ;; It expands a file consisting of a possibly empty sequence
    ;; of libraries optionally followed by a <toplevel program>.
    ;; The result is a sequence of vanilla r5rs-like toplevel
    ;; definitions and expressions.

    (define (expand-file filename target-filename)
      (with-toplevel-parameters
       (lambda ()
         (write-file (expand-toplevel-sequence (normalize (read-file filename)))
                     target-filename))))

    ;; This approximates the common r5rs behaviour of
    ;; expanding a toplevel file but treating unbound identifiers
    ;; as bare symbols that may refer to variables in the built-in toplevel
    ;; environment.  The environment argument should import at least the
    ;; macros necessary to expand the file.
    ;; This is provided mainly to be able to self-expand this expander
    ;; metacircularly (see the relevant note at the top of this file).
    ;; In contrast, expand-file strictly isolates a <toplevel program>
    ;; environment from the builtin environment and strictly disallows
    ;; unbound identifiers.
    ;; The resulting file will need the include file runtime.scm
    ;; and the appropriate libraries that constitute the env argument
    ;; to be preloaded before it can be run.

    (define (expand-r5rs-file filename target-filename r6rs-env)
      (with-toplevel-parameters
       (lambda ()
         (fluid-let ((make-free-name (lambda (symbol) symbol))
                     (*usage-env*    (r6rs-environment-env r6rs-env))
                     (*macro-table*  *macro-table*))
           (let ((imported-libraries (r6rs-environment-imported-libraries r6rs-env)))
             (import-libraries-for-expand (r6rs-environment-imported-libraries r6rs-env) (map not imported-libraries) 0)
             (write-file (cons `(ex:import-libraries-for-run ',(r6rs-environment-imported-libraries r6rs-env)
                                                             ',(current-builds imported-libraries)
                                                             0)
                               (expand-toplevel-sequence (read-file filename)))
                         target-filename))))))
       
    ;; Keeps (<library> ...) the same.
    ;; Converts (<library> ... . <toplevel program>)
    ;; to (<library> ... (program . <toplevel program>))

    (define (normalize exps)
      (define (error)
        (let ((newline (string #\newline)))
          (syntax-violation
           'expand-file
           (string-append
            "File should be of the form:" newline
            "      <library>*" newline
            "    | <library>* <toplevel program>")
           exps)))
      (let loop ((exps exps)
                 (normalized '()))
        (if (null? exps)
            (reverse normalized)
            (if (pair? (car exps))
                (case (caar exps)
                  ((library)
                   (loop (cdr exps)
                         (cons (car exps) normalized)))
                  ((import)
                   (loop '()
                         (cons (cons 'program exps)
                               normalized)))
                  (else (error)))
                (error)))))

    (define (read-file fn)
      (let ((p (open-input-file fn)))
        (let f ((x (read p)))
          (if (eof-object? x)
              (begin (close-input-port p) '())
              (cons x
                    (f (read p)))))))

    (define (write-file exps fn)
      (if (file-exists? fn)
          (delete-file fn))
      (let ((p (open-output-file fn)))
        (for-each (lambda (exp)
                    (write exp p)
                    (newline p))
                  exps)
        (close-output-port p)))

    ;;==========================================================================
    ;;
    ;; Toplevel bootstrap:
    ;;
    ;;==========================================================================

    (define toplevel-template
      (make-identifier 'toplevel-template
                       '()
                       '()
                       0
                       #f))

    (define (source->syntax datum)
      (datum->syntax toplevel-template datum))

    ;;===================================================================
    ;;
    ;; Language for bootstrapping the REPL session and (environment ---):
    ;;
    ;;===================================================================

    (define library-language-names
      `(program library export import for run expand meta only
                except prefix rename primitives >= <= and or not))

    (define (make-library-language)
      (map (lambda (name)
             (cons name (make-binding 'macro name '(0) #f '())))
           library-language-names))

    ;;===================================================================
    ;;
    ;; Bootstrap library containing macros defined in this expander.
    ;;
    ;;===================================================================

    (ex:register-library!
     (let ((primitive-macro-mapping
            `((lambda        . ,expand-lambda)
              (if            . ,expand-if)
              (set!          . ,expand-set!)
              (begin         . ,expand-begin)
              (syntax        . ,expand-syntax)
              (quote         . ,expand-quote)
              (let-syntax    . ,expand-local-syntax)
              (letrec-syntax . ,expand-local-syntax)
              (syntax-case   . ,expand-syntax-case)
              (and           . ,expand-and)
              (or            . ,expand-or)
              (define        . ,invalid-form)
              (define-syntax . ,invalid-form)
              (_             . ,invalid-form)
              (...           . ,invalid-form))))
       (ex:make-library
        '(core primitive-macros)
        ;; envs
        (lambda () '())
        ;; exports
        (map (lambda (mapping)
               (cons (car mapping) (make-binding 'macro (car mapping) '(0) #f '())))
             primitive-macro-mapping)
        ;; imported-libraries
        '()
        ;; builds
        '()
        ;; visit
        (lambda ()
          (for-each (lambda (mapping)
                      (register-macro! (car mapping) (make-expander (cdr mapping))))
                    primitive-macro-mapping)
          (values))
        ;; invoke
        (lambda () (values))
        ;; build
        'system)))

    ;; Initial environments:

    (set! *toplevel-env* (make-unit-env))
    (set! *usage-env*    *toplevel-env*)

    ;; Import only the minimal library language into the toplevel:

    (env-import! toplevel-template (make-library-language) *toplevel-env*)
    (register-macro! 'library (make-expander invalid-form))
    (register-macro! 'program (make-expander invalid-form))
    (register-macro! 'import  (make-expander invalid-form))

    ;;==========================================================================
    ;;
    ;; Exports:
    ;;
    ;;==========================================================================

    (set! ex:make-variable-transformer make-variable-transformer)
    (set! ex:identifier?               identifier?)
    (set! ex:bound-identifier=?        bound-identifier=?)
    (set! ex:free-identifier=?         free-identifier=?)
    (set! ex:generate-temporaries      generate-temporaries)
    (set! ex:datum->syntax             datum->syntax)
    (set! ex:syntax->datum             syntax->datum)
    (set! ex:environment               environment)
    (set! ex:environment-bindings      environment-bindings)
    (set! ex:eval                      r6rs-eval)
    (set! ex:load                      r6rs-load)
    (set! ex:syntax-violation          syntax-violation)
    
    (set! ex:expand-file               expand-file)
    (set! ex:repl                      repl)
    (set! ex:expand-r5rs-file          expand-r5rs-file)
    (set! ex:run-r6rs-sequence         run-r6rs-sequence)
    (set! ex:run-r6rs-program          run-r6rs-program)

    (set! ex:invalid-form              invalid-form)
    (set! ex:register-macro!           register-macro!)
    (set! ex:syntax-rename             syntax-rename)
    (set! ex:map-while                 map-while)
    (set! ex:dotted-length             dotted-length)
    (set! ex:dotted-butlast            dotted-butlast)
    (set! ex:dotted-last               dotted-last)
    (set! ex:uncompress                uncompress)
    (set! ex:free=?                    free=?)
    (set! ex:with-toplevel-parameters  with-toplevel-parameters)
    (set! ex:read-file                 read-file)
    (set! ex:normalize                 normalize)
    (set! ex:expand-toplevel-sequence  expand-toplevel-sequence)

    ) ; let
  ) ; letrec-syntax


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;end "expander.scm")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(load "standard-libraries.exp")
(begin (ex:register-library! (ex:make-library (quote (core primitives)) (lambda () (quote ())) (quote ((begin macro begin (0) #f ()) (if macro if (0) #f ()) (lambda macro lambda (0) #f ()) (quote macro quote (0) #f ()) (set! macro set! (0) #f ()) (and macro and (0) #f ()) (or macro or (0) #f ()) (define macro define (0) #f ()) (define-syntax macro define-syntax (0) #f ()) (let-syntax macro let-syntax (0) #f ()) (letrec-syntax macro letrec-syntax (0) #f ()) (_ macro _ (0) #f ()) (... macro ... (0) #f ()) (syntax macro syntax (0) #f ()) (syntax-case macro syntax-case (0) #f ()) (make-variable-transformer variable ex:make-variable-transformer (0) #f ()) (identifier? variable ex:identifier? (0) #f ()) (bound-identifier=? variable ex:bound-identifier=? (0) #f ()) (free-identifier=? variable ex:free-identifier=? (0) #f ()) (generate-temporaries variable ex:generate-temporaries (0) #f ()) (datum->syntax variable ex:datum->syntax (0) #f ()) (syntax->datum variable ex:syntax->datum (0) #f ()) (syntax-violation variable ex:syntax-violation (0) #f ()) (environment variable ex:environment (0) #f ()) (environment-bindings variable ex:environment-bindings (0) #f ()) (eval variable ex:eval (0) #f ()) (undefined variable ex:undefined (0) #f ()))) (quote (((core primitive-macros) 0))) (quote (system)) (lambda () (values)) (lambda () (values)) (quote &build~164~2))) (values))
(begin (ex:register-library! (ex:make-library (quote (core with-syntax)) (lambda () (ex:uncompress (quote (((&env~164~50 0 1 2 3) (&env~164~34 4 1 2 3) (&env~164~16 5 1 2 3)) (5 (((out) . #f) ((in) . #f) ((e1) . #f) ((e2) . #f))) (4 (((out) . #f) ((in) . #f) ((e1) . #f) ((e2) . #f))) (3 (((with-syntax) macro &with-syntax~164~3 (0) #f (core with-syntax)) ((undefined) variable ex:undefined (0 1) #f ()) ((eval) variable ex:eval (0 1) #f ()) ((environment-bindings) variable ex:environment-bindings (0 1) #f ()) ((environment) variable ex:environment (0 1) #f ()) ((syntax-violation) variable ex:syntax-violation (0 1) #f ()) ((syntax->datum) variable ex:syntax->datum (0 1) #f ()) ((datum->syntax) variable ex:datum->syntax (0 1) #f ()) ((generate-temporaries) variable ex:generate-temporaries (0 1) #f ()) ((free-identifier=?) variable ex:free-identifier=? (0 1) #f ()) ((bound-identifier=?) variable ex:bound-identifier=? (0 1) #f ()) ((identifier?) variable ex:identifier? (0 1) #f ()) ((make-variable-transformer) variable ex:make-variable-transformer (0 1) #f ()) ((syntax-case) macro syntax-case (0 1) #f ()) ((syntax) macro syntax (0 1) #f ()) ((...) macro ... (0 1) #f ()) ((_) macro _ (0 1) #f ()) ((letrec-syntax) macro letrec-syntax (0 1) #f ()) ((let-syntax) macro let-syntax (0 1) #f ()) ((define-syntax) macro define-syntax (0 1) #f ()) ((define) macro define (0 1) #f ()) ((or) macro or (0 1) #f ()) ((and) macro and (0 1) #f ()) ((set!) macro set! (0 1) #f ()) ((quote) macro quote (0 1) #f ()) ((lambda) macro lambda (0 1) #f ()) ((if) macro if (0 1) #f ()) ((begin) macro begin (0 1) #f ()) ((list) variable list (0) #f ()))) (2 (((x) variable &x~164~5 (1) #f (core with-syntax)))) (1 ()) (0 (((e1) . #f) ((e2) . #f))))))) (quote ((with-syntax macro &with-syntax~164~3 (0) #f (core with-syntax)))) (quote (((core primitives) 0 1))) (quote (&build~164~2)) (lambda () (ex:register-macro! (quote &with-syntax~164~3) (lambda (&x~164~5) (let ((&input~164~7 &x~164~5)) (let ((&fail~164~8 (lambda () (let ((&fail~164~9 (lambda () (let ((&fail~164~10 (lambda () (ex:invalid-form &input~164~7)))) (if (pair? &input~164~7) (let ((&temp~164~28 (car &input~164~7))) (let ((&temp~164~17 (cdr &input~164~7))) (if (pair? &temp~164~17) (let ((&temp~164~21 (car &temp~164~17))) (ex:map-while (lambda (&temp~164~21) (if (pair? &temp~164~21) (let ((&temp~164~27 (car &temp~164~21))) (let ((&out~164~14 &temp~164~27)) (let ((&temp~164~24 (cdr &temp~164~21))) (if (pair? &temp~164~24) (let ((&temp~164~26 (car &temp~164~24))) (let ((&in~164~13 &temp~164~26)) (let ((&temp~164~25 (cdr &temp~164~24))) (if (null? &temp~164~25) (list &out~164~14 &in~164~13) #f)))) #f)))) #f)) &temp~164~21 (lambda (&cols~164~22 &rest~164~23) (if (null? &rest~164~23) (apply (lambda (&out~164~14 &in~164~13) (let ((&temp~164~18 (cdr &temp~164~17))) (if (pair? &temp~164~18) (let ((&temp~164~20 (car &temp~164~18))) (let ((&e1~164~12 &temp~164~20)) (let ((&temp~164~19 (cdr &temp~164~18))) (if (list? &temp~164~19) (let ((&e2~164~11 &temp~164~19)) (cons (ex:syntax-rename (quote syntax-case) (quote ()) (quote (&env~164~16)) 0 (quote (core with-syntax))) (cons (cons (ex:syntax-rename (quote list) (quote ()) (quote (&env~164~16)) 0 (quote (core with-syntax))) &in~164~13) (cons (quote ()) (cons (cons &out~164~14 (cons (cons (ex:syntax-rename (quote begin) (quote ()) (quote (&env~164~16)) 0 (quote (core with-syntax))) (cons &e1~164~12 &e2~164~11)) (quote ()))) (quote ())))))) (&fail~164~10))))) (&fail~164~10)))) (if (null? &cols~164~22) (quote (() ())) (apply map list &cols~164~22))) (&fail~164~10))))) (&fail~164~10)))) (&fail~164~10)))))) (if (pair? &input~164~7) (let ((&temp~164~46 (car &input~164~7))) (let ((&temp~164~35 (cdr &input~164~7))) (if (pair? &temp~164~35) (let ((&temp~164~39 (car &temp~164~35))) (if (pair? &temp~164~39) (let ((&temp~164~41 (car &temp~164~39))) (if (pair? &temp~164~41) (let ((&temp~164~45 (car &temp~164~41))) (let ((&out~164~32 &temp~164~45)) (let ((&temp~164~42 (cdr &temp~164~41))) (if (pair? &temp~164~42) (let ((&temp~164~44 (car &temp~164~42))) (let ((&in~164~31 &temp~164~44)) (let ((&temp~164~43 (cdr &temp~164~42))) (if (null? &temp~164~43) (let ((&temp~164~40 (cdr &temp~164~39))) (if (null? &temp~164~40) (let ((&temp~164~36 (cdr &temp~164~35))) (if (pair? &temp~164~36) (let ((&temp~164~38 (car &temp~164~36))) (let ((&e1~164~30 &temp~164~38)) (let ((&temp~164~37 (cdr &temp~164~36))) (if (list? &temp~164~37) (let ((&e2~164~29 &temp~164~37)) (cons (ex:syntax-rename (quote syntax-case) (quote ()) (quote (&env~164~34)) 0 (quote (core with-syntax))) (cons &in~164~31 (cons (quote ()) (cons (cons &out~164~32 (cons (cons (ex:syntax-rename (quote begin) (quote ()) (quote (&env~164~34)) 0 (quote (core with-syntax))) (cons &e1~164~30 &e2~164~29)) (quote ()))) (quote ())))))) (&fail~164~9))))) (&fail~164~9))) (&fail~164~9))) (&fail~164~9))))) (&fail~164~9))))) (&fail~164~9))) (&fail~164~9))) (&fail~164~9)))) (&fail~164~9)))))) (if (pair? &input~164~7) (let ((&temp~164~56 (car &input~164~7))) (let ((&temp~164~51 (cdr &input~164~7))) (if (pair? &temp~164~51) (let ((&temp~164~55 (car &temp~164~51))) (if (null? &temp~164~55) (let ((&temp~164~52 (cdr &temp~164~51))) (if (pair? &temp~164~52) (let ((&temp~164~54 (car &temp~164~52))) (let ((&e1~164~48 &temp~164~54)) (let ((&temp~164~53 (cdr &temp~164~52))) (if (list? &temp~164~53) (let ((&e2~164~47 &temp~164~53)) (cons (ex:syntax-rename (quote begin) (quote ()) (quote (&env~164~50)) 0 (quote (core with-syntax))) (cons &e1~164~48 &e2~164~47))) (&fail~164~8))))) (&fail~164~8))) (&fail~164~8))) (&fail~164~8)))) (&fail~164~8)))))) (values)) (lambda () (values)) (quote &build~164~57))) (values))
(begin (ex:register-library! (ex:make-library (quote (core syntax-rules)) (lambda () (ex:uncompress (quote (((&env~164~93 0 1 2 3 4) (&env~164~73 5 6 7 2 3 4)) (7 (((y) variable &y~164~63 (1) #f (core syntax-rules)))) (6 ()) (5 (((keyword) . #f) ((pattern) . #f) ((template) . #f))) (4 (((syntax-rules) macro &syntax-rules~164~58 (0) #f (core syntax-rules)) ((undefined) variable ex:undefined (1 0) #f ()) ((eval) variable ex:eval (1 0) #f ()) ((environment-bindings) variable ex:environment-bindings (1 0) #f ()) ((environment) variable ex:environment (1 0) #f ()) ((syntax-violation) variable ex:syntax-violation (1 0) #f ()) ((syntax->datum) variable ex:syntax->datum (1 0) #f ()) ((datum->syntax) variable ex:datum->syntax (1 0) #f ()) ((generate-temporaries) variable ex:generate-temporaries (1 0) #f ()) ((free-identifier=?) variable ex:free-identifier=? (1 0) #f ()) ((bound-identifier=?) variable ex:bound-identifier=? (1 0) #f ()) ((identifier?) variable ex:identifier? (1 0) #f ()) ((make-variable-transformer) variable ex:make-variable-transformer (1 0) #f ()) ((syntax-case) macro syntax-case (1 0) #f ()) ((syntax) macro syntax (1 0) #f ()) ((...) macro ... (1 0) #f ()) ((_) macro _ (1 0) #f ()) ((letrec-syntax) macro letrec-syntax (1 0) #f ()) ((let-syntax) macro let-syntax (1 0) #f ()) ((define-syntax) macro define-syntax (1 0) #f ()) ((define) macro define (1 0) #f ()) ((or) macro or (1 0) #f ()) ((and) macro and (1 0) #f ()) ((set!) macro set! (1 0) #f ()) ((quote) macro quote (1 0) #f ()) ((lambda) macro lambda (1 0) #f ()) ((if) macro if (1 0) #f ()) ((begin) macro begin (1 0) #f ()) ((with-syntax) macro &with-syntax~164~3 (1) #f (core with-syntax)) ((map) variable map (1) #f ()) ((for-all) variable for-all (1) #f ()))) (3 (((x) variable &x~164~60 (1) #f (core syntax-rules)))) (2 (((clause) variable &clause~164~61 (1) #f (core syntax-rules)))) (1 (((k) . #f) ((cl) . #f))) (0 (((cl) . #f))))))) (quote ((syntax-rules macro &syntax-rules~164~58 (0) #f (core syntax-rules)))) (quote (((core with-syntax) 1) ((core primitives) 1 0))) (quote (&build~164~57 &build~164~2)) (lambda () (ex:register-macro! (quote &syntax-rules~164~58) (lambda (&x~164~60) ((lambda (&clause~164~61) (set! &clause~164~61 (lambda (&y~164~63) (let ((&input~164~65 &y~164~63)) (let ((&fail~164~66 (lambda () (let ((&fail~164~67 (lambda () (ex:invalid-form &input~164~65)))) (ex:syntax-violation (quote syntax-rules) "Invalid expression" &x~164~60))))) (if (pair? &input~164~65) (let ((&temp~164~77 (car &input~164~65))) (if (pair? &temp~164~77) (let ((&temp~164~79 (car &temp~164~77))) (let ((&keyword~164~71 &temp~164~79)) (let ((&temp~164~78 (cdr &temp~164~77))) (let ((&pattern~164~70 &temp~164~78)) (let ((&temp~164~74 (cdr &input~164~65))) (if (pair? &temp~164~74) (let ((&temp~164~76 (car &temp~164~74))) (let ((&template~164~69 &temp~164~76)) (let ((&temp~164~75 (cdr &temp~164~74))) (if (null? &temp~164~75) (cons (cons (ex:syntax-rename (quote dummy) (quote ()) (quote (&env~164~73)) 0 (quote (core syntax-rules))) &pattern~164~70) (cons (cons (ex:syntax-rename (quote syntax) (quote ()) (quote (&env~164~73)) 0 (quote (core syntax-rules))) (cons &template~164~69 (quote ()))) (quote ()))) (&fail~164~66))))) (&fail~164~66))))))) (&fail~164~66))) (&fail~164~66)))))) (let ((&input~164~81 &x~164~60)) (let ((&fail~164~82 (lambda () (ex:invalid-form &input~164~81)))) (if (pair? &input~164~81) (let ((&temp~164~98 (car &input~164~81))) (let ((&temp~164~95 (cdr &input~164~81))) (if (pair? &temp~164~95) (let ((&temp~164~97 (car &temp~164~95))) (if (list? &temp~164~97) (let ((&k~164~84 &temp~164~97)) (let ((&temp~164~96 (cdr &temp~164~95))) (if (list? &temp~164~96) (let ((&cl~164~83 &temp~164~96)) (if (for-all ex:identifier? &k~164~84) (let ((&input~164~87 (map &clause~164~61 &cl~164~83))) (let ((&fail~164~89 (lambda () (ex:invalid-form &input~164~87)))) (if (list? &input~164~87) (let ((&cl~164~90 &input~164~87)) (begin (cons (ex:syntax-rename (quote lambda) (quote ()) (quote (&env~164~93)) 0 (quote (core syntax-rules))) (cons (cons (ex:syntax-rename (quote x) (quote ()) (quote (&env~164~93)) 0 (quote (core syntax-rules))) (quote ())) (cons (cons (ex:syntax-rename (quote syntax-case) (quote ()) (quote (&env~164~93)) 0 (quote (core syntax-rules))) (cons (ex:syntax-rename (quote x) (quote ()) (quote (&env~164~93)) 0 (quote (core syntax-rules))) (cons &k~164~84 &cl~164~90))) (quote ())))))) (&fail~164~89)))) (&fail~164~82))) (&fail~164~82)))) (&fail~164~82))) (&fail~164~82)))) (&fail~164~82))))) ex:undefined))) (values)) (lambda () (values)) (quote &build~164~99))) (values))
(begin (ex:register-library! (ex:make-library (quote (core let)) (lambda () (ex:uncompress (quote (((&env~164~190 0 1 2 3) (&env~164~166 4 5 6 7 3) (&env~164~134 8 9 10 3) (&env~164~113 11 9 10 3)) (11 (((f) . #f) ((x) . #f) ((v) . #f) ((e1) . #f) ((e2) . #f))) (10 (((x) variable &x~164~102 (1) #f (core let)))) (9 ()) (8 (((x) . #f) ((v) . #f) ((e1) . #f) ((e2) . #f))) (7 (((x) variable &x~164~150 (1) #f (core let)))) (6 ()) (5 (((i) . #f) ((v) . #f) ((e1) . #f) ((e2) . #f))) (4 (((t) . #f))) (3 (((letrec*) macro &letrec*~164~179 (0) #f (core let)) ((letrec) macro &letrec~164~148 (0) #f (core let)) ((let) macro &let~164~100 (0) #f (core let)) ((undefined) variable ex:undefined (1 0) #f ()) ((eval) variable ex:eval (1 0) #f ()) ((environment-bindings) variable ex:environment-bindings (1 0) #f ()) ((environment) variable ex:environment (1 0) #f ()) ((syntax-violation) variable ex:syntax-violation (1 0) #f ()) ((syntax->datum) variable ex:syntax->datum (1 0) #f ()) ((datum->syntax) variable ex:datum->syntax (1 0) #f ()) ((generate-temporaries) variable ex:generate-temporaries (1 0) #f ()) ((free-identifier=?) variable ex:free-identifier=? (1 0) #f ()) ((bound-identifier=?) variable ex:bound-identifier=? (1 0) #f ()) ((identifier?) variable ex:identifier? (1 0) #f ()) ((make-variable-transformer) variable ex:make-variable-transformer (1 0) #f ()) ((syntax-case) macro syntax-case (1 0) #f ()) ((syntax) macro syntax (1 0) #f ()) ((...) macro ... (1 0) #f ()) ((_) macro _ (1 0) #f ()) ((letrec-syntax) macro letrec-syntax (1 0) #f ()) ((let-syntax) macro let-syntax (1 0) #f ()) ((define-syntax) macro define-syntax (1 0) #f ()) ((define) macro define (1 0) #f ()) ((or) macro or (1 0) #f ()) ((and) macro and (1 0) #f ()) ((set!) macro set! (1 0) #f ()) ((quote) macro quote (1 0) #f ()) ((lambda) macro lambda (1 0) #f ()) ((if) macro if (1 0) #f ()) ((begin) macro begin (1 0) #f ()) ((with-syntax) macro &with-syntax~164~3 (1) #f (core with-syntax)) ((for-all) variable for-all (1) #f ()))) (2 (((x) variable &x~164~181 (1) #f (core let)))) (1 ()) (0 (((i) . #f) ((v) . #f) ((e1) . #f) ((e2) . #f))))))) (quote ((let macro &let~164~100 (0) #f (core let)) (letrec macro &letrec~164~148 (0) #f (core let)) (letrec* macro &letrec*~164~179 (0) #f (core let)))) (quote (((core with-syntax) 1) ((core primitives) 1 0))) (quote (&build~164~57 &build~164~2)) (lambda () (ex:register-macro! (quote &let~164~100) (lambda (&x~164~102) (let ((&input~164~104 &x~164~102)) (let ((&fail~164~105 (lambda () (let ((&fail~164~106 (lambda () (ex:invalid-form &input~164~104)))) (if (pair? &input~164~104) (let ((&temp~164~128 (car &input~164~104))) (let ((&temp~164~115 (cdr &input~164~104))) (if (pair? &temp~164~115) (let ((&temp~164~127 (car &temp~164~115))) (let ((&f~164~111 &temp~164~127)) (let ((&temp~164~116 (cdr &temp~164~115))) (if (pair? &temp~164~116) (let ((&temp~164~120 (car &temp~164~116))) (ex:map-while (lambda (&temp~164~120) (if (pair? &temp~164~120) (let ((&temp~164~126 (car &temp~164~120))) (let ((&x~164~110 &temp~164~126)) (let ((&temp~164~123 (cdr &temp~164~120))) (if (pair? &temp~164~123) (let ((&temp~164~125 (car &temp~164~123))) (let ((&v~164~109 &temp~164~125)) (let ((&temp~164~124 (cdr &temp~164~123))) (if (null? &temp~164~124) (list &x~164~110 &v~164~109) #f)))) #f)))) #f)) &temp~164~120 (lambda (&cols~164~121 &rest~164~122) (if (null? &rest~164~122) (apply (lambda (&x~164~110 &v~164~109) (let ((&temp~164~117 (cdr &temp~164~116))) (if (pair? &temp~164~117) (let ((&temp~164~119 (car &temp~164~117))) (let ((&e1~164~108 &temp~164~119)) (let ((&temp~164~118 (cdr &temp~164~117))) (if (list? &temp~164~118) (let ((&e2~164~107 &temp~164~118)) (if (for-all ex:identifier? (cons &f~164~111 &x~164~110)) (cons (cons (ex:syntax-rename (quote letrec) (quote ()) (quote (&env~164~113)) 0 (quote (core let))) (cons (cons (cons &f~164~111 (cons (cons (ex:syntax-rename (quote lambda) (quote ()) (quote (&env~164~113)) 0 (quote (core let))) (cons &x~164~110 (cons &e1~164~108 &e2~164~107))) (quote ()))) (quote ())) (cons &f~164~111 (quote ())))) &v~164~109) (&fail~164~106))) (&fail~164~106))))) (&fail~164~106)))) (if (null? &cols~164~121) (quote (() ())) (apply map list &cols~164~121))) (&fail~164~106))))) (&fail~164~106))))) (&fail~164~106)))) (&fail~164~106)))))) (if (pair? &input~164~104) (let ((&temp~164~147 (car &input~164~104))) (let ((&temp~164~136 (cdr &input~164~104))) (if (pair? &temp~164~136) (let ((&temp~164~140 (car &temp~164~136))) (ex:map-while (lambda (&temp~164~140) (if (pair? &temp~164~140) (let ((&temp~164~146 (car &temp~164~140))) (let ((&x~164~132 &temp~164~146)) (let ((&temp~164~143 (cdr &temp~164~140))) (if (pair? &temp~164~143) (let ((&temp~164~145 (car &temp~164~143))) (let ((&v~164~131 &temp~164~145)) (let ((&temp~164~144 (cdr &temp~164~143))) (if (null? &temp~164~144) (list &x~164~132 &v~164~131) #f)))) #f)))) #f)) &temp~164~140 (lambda (&cols~164~141 &rest~164~142) (if (null? &rest~164~142) (apply (lambda (&x~164~132 &v~164~131) (let ((&temp~164~137 (cdr &temp~164~136))) (if (pair? &temp~164~137) (let ((&temp~164~139 (car &temp~164~137))) (let ((&e1~164~130 &temp~164~139)) (let ((&temp~164~138 (cdr &temp~164~137))) (if (list? &temp~164~138) (let ((&e2~164~129 &temp~164~138)) (if (for-all ex:identifier? &x~164~132) (cons (cons (ex:syntax-rename (quote lambda) (quote ()) (quote (&env~164~134)) 0 (quote (core let))) (cons &x~164~132 (cons &e1~164~130 &e2~164~129))) &v~164~131) (&fail~164~105))) (&fail~164~105))))) (&fail~164~105)))) (if (null? &cols~164~141) (quote (() ())) (apply map list &cols~164~141))) (&fail~164~105))))) (&fail~164~105)))) (&fail~164~105)))))) (ex:register-macro! (quote &letrec~164~148) (lambda (&x~164~150) (let ((&input~164~152 &x~164~150)) (let ((&fail~164~153 (lambda () (ex:invalid-form &input~164~152)))) (if (pair? &input~164~152) (let ((&temp~164~178 (car &input~164~152))) (let ((&temp~164~167 (cdr &input~164~152))) (if (pair? &temp~164~167) (let ((&temp~164~171 (car &temp~164~167))) (ex:map-while (lambda (&temp~164~171) (if (pair? &temp~164~171) (let ((&temp~164~177 (car &temp~164~171))) (let ((&i~164~157 &temp~164~177)) (let ((&temp~164~174 (cdr &temp~164~171))) (if (pair? &temp~164~174) (let ((&temp~164~176 (car &temp~164~174))) (let ((&v~164~156 &temp~164~176)) (let ((&temp~164~175 (cdr &temp~164~174))) (if (null? &temp~164~175) (list &i~164~157 &v~164~156) #f)))) #f)))) #f)) &temp~164~171 (lambda (&cols~164~172 &rest~164~173) (if (null? &rest~164~173) (apply (lambda (&i~164~157 &v~164~156) (let ((&temp~164~168 (cdr &temp~164~167))) (if (pair? &temp~164~168) (let ((&temp~164~170 (car &temp~164~168))) (let ((&e1~164~155 &temp~164~170)) (let ((&temp~164~169 (cdr &temp~164~168))) (if (list? &temp~164~169) (let ((&e2~164~154 &temp~164~169)) (let ((&input~164~160 (ex:generate-temporaries &i~164~157))) (let ((&fail~164~162 (lambda () (ex:invalid-form &input~164~160)))) (if (list? &input~164~160) (let ((&t~164~163 &input~164~160)) (begin (cons (ex:syntax-rename (quote let) (quote ()) (quote (&env~164~166)) 0 (quote (core let))) (cons (map (lambda (&i~164~157) (cons &i~164~157 (cons (ex:syntax-rename (quote undefined) (quote ()) (quote (&env~164~166)) 0 (quote (core let))) (quote ())))) &i~164~157) (cons (cons (ex:syntax-rename (quote let) (quote ()) (quote (&env~164~166)) 0 (quote (core let))) (cons (if (= (length &t~164~163) (length &v~164~156)) (map (lambda (&t~164~163 &v~164~156) (cons &t~164~163 (cons &v~164~156 (quote ())))) &t~164~163 &v~164~156) (ex:syntax-violation (quote syntax) "Pattern variables denoting lists of unequal length preceding ellipses" (quote ((t v) ...)) (list &t~164~163 &v~164~156))) (append (if (= (length &i~164~157) (length &t~164~163)) (map (lambda (&i~164~157 &t~164~163) (cons (ex:syntax-rename (quote set!) (quote ()) (quote (&env~164~166)) 0 (quote (core let))) (cons &i~164~157 (cons &t~164~163 (quote ()))))) &i~164~157 &t~164~163) (ex:syntax-violation (quote syntax) "Pattern variables denoting lists of unequal length preceding ellipses" (quote ((set! i t) ... (let () e1 e2 ...))) (list &i~164~157 &t~164~163))) (cons (cons (ex:syntax-rename (quote let) (quote ()) (quote (&env~164~166)) 0 (quote (core let))) (cons (quote ()) (cons &e1~164~155 &e2~164~154))) (quote ()))))) (quote ())))))) (&fail~164~162))))) (&fail~164~153))))) (&fail~164~153)))) (if (null? &cols~164~172) (quote (() ())) (apply map list &cols~164~172))) (&fail~164~153))))) (&fail~164~153)))) (&fail~164~153)))))) (ex:register-macro! (quote &letrec*~164~179) (lambda (&x~164~181) (let ((&input~164~183 &x~164~181)) (let ((&fail~164~184 (lambda () (ex:invalid-form &input~164~183)))) (if (pair? &input~164~183) (let ((&temp~164~202 (car &input~164~183))) (let ((&temp~164~191 (cdr &input~164~183))) (if (pair? &temp~164~191) (let ((&temp~164~195 (car &temp~164~191))) (ex:map-while (lambda (&temp~164~195) (if (pair? &temp~164~195) (let ((&temp~164~201 (car &temp~164~195))) (let ((&i~164~188 &temp~164~201)) (let ((&temp~164~198 (cdr &temp~164~195))) (if (pair? &temp~164~198) (let ((&temp~164~200 (car &temp~164~198))) (let ((&v~164~187 &temp~164~200)) (let ((&temp~164~199 (cdr &temp~164~198))) (if (null? &temp~164~199) (list &i~164~188 &v~164~187) #f)))) #f)))) #f)) &temp~164~195 (lambda (&cols~164~196 &rest~164~197) (if (null? &rest~164~197) (apply (lambda (&i~164~188 &v~164~187) (let ((&temp~164~192 (cdr &temp~164~191))) (if (pair? &temp~164~192) (let ((&temp~164~194 (car &temp~164~192))) (let ((&e1~164~186 &temp~164~194)) (let ((&temp~164~193 (cdr &temp~164~192))) (if (list? &temp~164~193) (let ((&e2~164~185 &temp~164~193)) (cons (ex:syntax-rename (quote let) (quote ()) (quote (&env~164~190)) 0 (quote (core let))) (cons (quote ()) (append (if (= (length &i~164~188) (length &v~164~187)) (map (lambda (&i~164~188 &v~164~187) (cons (ex:syntax-rename (quote define) (quote ()) (quote (&env~164~190)) 0 (quote (core let))) (cons &i~164~188 (cons &v~164~187 (quote ()))))) &i~164~188 &v~164~187) (ex:syntax-violation (quote syntax) "Pattern variables denoting lists of unequal length preceding ellipses" (quote ((define i v) ... (let () e1 e2 ...))) (list &i~164~188 &v~164~187))) (cons (cons (ex:syntax-rename (quote let) (quote ()) (quote (&env~164~190)) 0 (quote (core let))) (cons (quote ()) (cons &e1~164~186 &e2~164~185))) (quote ())))))) (&fail~164~184))))) (&fail~164~184)))) (if (null? &cols~164~196) (quote (() ())) (apply map list &cols~164~196))) (&fail~164~184))))) (&fail~164~184)))) (&fail~164~184)))))) (values)) (lambda () (values)) (quote &build~164~203))) (values))
(begin (ex:register-library! (ex:make-library (quote (core derived)) (lambda () (ex:uncompress (quote (((&env~164~474 0 1 2 3 4) (&env~164~460 5 6 7 8 9 1 2 3 4) (&env~164~452 10 6 7 8 9 1 2 3 4) (&env~164~439 11 12 6 7 8 9 1 2 3 4) (&env~164~387 13 14 15 16 17 18 19 20 21 4) (&env~164~381 22 14 15 16 17 18 19 20 21 4) (&env~164~372 23 14 15 16 17 18 19 20 21 4) (&env~164~364 24 14 15 16 17 18 19 20 21 4) (&env~164~347 25 26 27 15 16 17 18 19 20 21 4) (&env~164~338 28 26 27 15 16 17 18 19 20 21 4) (&env~164~330 29 26 27 15 16 17 18 19 20 21 4) (&env~164~278 30 31 32 4) (&env~164~250 33 34 35 36 37 38 31 32 4) (&env~164~240 39 40 34 35 36 37 38 31 32 4)) (40 (((x) . #f) ((v) . #f) ((rest) . #f))) (39 (((body) . #f))) (38 (((x) . #f) ((v) . #f) ((e1) . #f) ((e2) . #f))) (37 (((f) variable &f~164~221 (1) #t (core derived)))) (36 ()) (35 (((bindings) variable &bindings~164~224 (1) #f (core derived)))) (34 ()) (33 (((x) . #f) ((v) . #f))) (32 (((x) variable &x~164~206 (1) #f (core derived)))) (31 ()) (30 (((e1) . #f) ((e2) . #f))) (29 (((e0) . #f) ((e1) . #f) ((e2) . #f))) (28 (((e0) . #f) ((e1) . #f))) (27 (((c2) . #f) ((c3) . #f))) (26 (((rest) . #f))) (25 (((e0) . #f))) (24 (((e0) . #f) ((e1) . #f) ((e2) . #f))) (23 (((e0) . #f) ((e1) . #f))) (22 (((e0) . #f))) (21 (((x) variable &x~164~287 (1) #f (core derived)))) (20 ()) (19 (((c1) . #f) ((c2) . #f))) (18 (((f) variable &f~164~300 (1) #t (core derived)))) (17 ()) (16 (((c1) variable &c1~164~304 (1) #f (core derived)) ((c2*) variable &c2*~164~303 (1) #f (core derived)))) (15 ()) (14 ()) (13 (((e1) . #f) ((e2) . #f))) (12 (((rest) . #f))) (11 (((k) . #f) ((e1) . #f) ((e2) . #f))) (10 (((k) . #f) ((e1) . #f) ((e2) . #f))) (9 (((f) variable &f~164~420 (1) #t (core derived)))) (8 ()) (7 (((c1) variable &c1~164~424 (1) #f (core derived)) ((cmore) variable &cmore~164~423 (1) #f (core derived)))) (6 ()) (5 (((e1) . #f) ((e2) . #f))) (4 (((else) macro &else~164~485 (0) #f (core derived)) ((=>) macro &=>~164~481 (0) #f (core derived)) ((case) macro &case~164~401 (0) #f (core derived)) ((cond) macro &cond~164~285 (0) #f (core derived)) ((let*) macro &let*~164~204 (0) #f (core derived)) ((undefined) variable ex:undefined (1 0) #f ()) ((eval) variable ex:eval (1 0) #f ()) ((environment-bindings) variable ex:environment-bindings (1 0) #f ()) ((environment) variable ex:environment (1 0) #f ()) ((syntax-violation) variable ex:syntax-violation (1 0) #f ()) ((syntax->datum) variable ex:syntax->datum (1 0) #f ()) ((datum->syntax) variable ex:datum->syntax (1 0) #f ()) ((generate-temporaries) variable ex:generate-temporaries (1 0) #f ()) ((free-identifier=?) variable ex:free-identifier=? (1 0) #f ()) ((bound-identifier=?) variable ex:bound-identifier=? (1 0) #f ()) ((identifier?) variable ex:identifier? (1 0) #f ()) ((make-variable-transformer) variable ex:make-variable-transformer (1 0) #f ()) ((syntax-case) macro syntax-case (1 0) #f ()) ((syntax) macro syntax (1 0) #f ()) ((...) macro ... (1 0) #f ()) ((_) macro _ (1 0) #f ()) ((letrec-syntax) macro letrec-syntax (1 0) #f ()) ((let-syntax) macro let-syntax (1 0) #f ()) ((define-syntax) macro define-syntax (1 0) #f ()) ((define) macro define (1 0) #f ()) ((or) macro or (1 0) #f ()) ((and) macro and (1 0) #f ()) ((set!) macro set! (1 0) #f ()) ((quote) macro quote (1 0) #f ()) ((lambda) macro lambda (1 0) #f ()) ((if) macro if (1 0) #f ()) ((begin) macro begin (1 0) #f ()) ((letrec*) macro &letrec*~164~179 (1 0) #f (core let)) ((letrec) macro &letrec~164~148 (1 0) #f (core let)) ((let) macro &let~164~100 (1 0) #f (core let)) ((with-syntax) macro &with-syntax~164~3 (1) #f (core with-syntax)) ((syntax-rules) macro &syntax-rules~164~58 (1) #f (core syntax-rules)) ((cdr) variable cdr (1 0) #f ()) ((car) variable car (1 0) #f ()) ((memv) variable memv (1 0) #f ()) ((null?) variable null? (1 0) #f ()) ((for-all) variable for-all (1 0) #f ()))) (3 (((x) variable &x~164~403 (1) #f (core derived)))) (2 ()) (1 (((e) . #f) ((c1) . #f) ((c2) . #f))) (0 (((body) . #f))))))) (quote ((let* macro &let*~164~204 (0) #f (core derived)) (cond macro &cond~164~285 (0) #f (core derived)) (case macro &case~164~401 (0) #f (core derived)) (else macro &else~164~485 (0) #f (core derived)) (=> macro &=>~164~481 (0) #f (core derived)))) (quote (((core syntax-rules) 1) ((core with-syntax) 1) ((core let) 1 0) ((core primitives) 1 0))) (quote (&build~164~99 &build~164~57 &build~164~203 &build~164~2)) (lambda () (ex:register-macro! (quote &let*~164~204) (lambda (&x~164~206) (let ((&input~164~208 &x~164~206)) (let ((&fail~164~209 (lambda () (let ((&fail~164~210 (lambda () (ex:invalid-form &input~164~208)))) (if (pair? &input~164~208) (let ((&temp~164~274 (car &input~164~208))) (let ((&temp~164~263 (cdr &input~164~208))) (if (pair? &temp~164~263) (let ((&temp~164~267 (car &temp~164~263))) (ex:map-while (lambda (&temp~164~267) (if (pair? &temp~164~267) (let ((&temp~164~273 (car &temp~164~267))) (let ((&x~164~214 &temp~164~273)) (let ((&temp~164~270 (cdr &temp~164~267))) (if (pair? &temp~164~270) (let ((&temp~164~272 (car &temp~164~270))) (let ((&v~164~213 &temp~164~272)) (let ((&temp~164~271 (cdr &temp~164~270))) (if (null? &temp~164~271) (list &x~164~214 &v~164~213) #f)))) #f)))) #f)) &temp~164~267 (lambda (&cols~164~268 &rest~164~269) (if (null? &rest~164~269) (apply (lambda (&x~164~214 &v~164~213) (let ((&temp~164~264 (cdr &temp~164~263))) (if (pair? &temp~164~264) (let ((&temp~164~266 (car &temp~164~264))) (let ((&e1~164~212 &temp~164~266)) (let ((&temp~164~265 (cdr &temp~164~264))) (if (list? &temp~164~265) (let ((&e2~164~211 &temp~164~265)) (if (for-all ex:identifier? &x~164~214) (((lambda (&f~164~221) ((lambda (&temp~164~258) (set! &f~164~221 &temp~164~258) ((lambda () &f~164~221))) (lambda (&bindings~164~224) (let ((&input~164~226 &bindings~164~224)) (let ((&fail~164~227 (lambda () (let ((&fail~164~228 (lambda () (ex:invalid-form &input~164~226)))) (if (pair? &input~164~226) (let ((&temp~164~242 (car &input~164~226))) (if (pair? &temp~164~242) (let ((&temp~164~246 (car &temp~164~242))) (let ((&x~164~231 &temp~164~246)) (let ((&temp~164~243 (cdr &temp~164~242))) (if (pair? &temp~164~243) (let ((&temp~164~245 (car &temp~164~243))) (let ((&v~164~230 &temp~164~245)) (let ((&temp~164~244 (cdr &temp~164~243))) (if (null? &temp~164~244) (let ((&temp~164~241 (cdr &input~164~226))) (let ((&rest~164~229 &temp~164~241)) (let ((&input~164~234 (&f~164~221 &rest~164~229))) (let ((&fail~164~236 (lambda () (ex:invalid-form &input~164~234)))) (let ((&body~164~237 &input~164~234)) (begin (cons (ex:syntax-rename (quote let) (quote ()) (quote (&env~164~240)) 0 (quote (core derived))) (cons (cons (cons &x~164~231 (cons &v~164~230 (quote ()))) (quote ())) (cons &body~164~237 (quote ())))))))))) (&fail~164~228))))) (&fail~164~228))))) (&fail~164~228))) (&fail~164~228)))))) (if (pair? &input~164~226) (let ((&temp~164~252 (car &input~164~226))) (if (pair? &temp~164~252) (let ((&temp~164~256 (car &temp~164~252))) (let ((&x~164~248 &temp~164~256)) (let ((&temp~164~253 (cdr &temp~164~252))) (if (pair? &temp~164~253) (let ((&temp~164~255 (car &temp~164~253))) (let ((&v~164~247 &temp~164~255)) (let ((&temp~164~254 (cdr &temp~164~253))) (if (null? &temp~164~254) (let ((&temp~164~251 (cdr &input~164~226))) (if (null? &temp~164~251) (cons (ex:syntax-rename (quote let) (quote ()) (quote (&env~164~250)) 0 (quote (core derived))) (cons (cons (cons &x~164~248 (cons &v~164~247 (quote ()))) (quote ())) (cons &e1~164~212 &e2~164~211))) (&fail~164~227))) (&fail~164~227))))) (&fail~164~227))))) (&fail~164~227))) (&fail~164~227))))))) ex:undefined) (if (= (length &x~164~214) (length &v~164~213)) (map (lambda (&x~164~214 &v~164~213) (cons &x~164~214 (cons &v~164~213 (quote ())))) &x~164~214 &v~164~213) (ex:syntax-violation (quote syntax) "Pattern variables denoting lists of unequal length preceding ellipses" (quote ((x v) ...)) (list &x~164~214 &v~164~213)))) (&fail~164~210))) (&fail~164~210))))) (&fail~164~210)))) (if (null? &cols~164~268) (quote (() ())) (apply map list &cols~164~268))) (&fail~164~210))))) (&fail~164~210)))) (&fail~164~210)))))) (if (pair? &input~164~208) (let ((&temp~164~284 (car &input~164~208))) (let ((&temp~164~279 (cdr &input~164~208))) (if (pair? &temp~164~279) (let ((&temp~164~283 (car &temp~164~279))) (if (null? &temp~164~283) (let ((&temp~164~280 (cdr &temp~164~279))) (if (pair? &temp~164~280) (let ((&temp~164~282 (car &temp~164~280))) (let ((&e1~164~276 &temp~164~282)) (let ((&temp~164~281 (cdr &temp~164~280))) (if (list? &temp~164~281) (let ((&e2~164~275 &temp~164~281)) (cons (ex:syntax-rename (quote let) (quote ()) (quote (&env~164~278)) 0 (quote (core derived))) (cons (quote ()) (cons &e1~164~276 &e2~164~275)))) (&fail~164~209))))) (&fail~164~209))) (&fail~164~209))) (&fail~164~209)))) (&fail~164~209)))))) (ex:register-macro! (quote &cond~164~285) (lambda (&x~164~287) (let ((&input~164~289 &x~164~287)) (let ((&fail~164~290 (lambda () (ex:invalid-form &input~164~289)))) (if (pair? &input~164~289) (let ((&temp~164~400 (car &input~164~289))) (let ((&temp~164~397 (cdr &input~164~289))) (if (pair? &temp~164~397) (let ((&temp~164~399 (car &temp~164~397))) (let ((&c1~164~292 &temp~164~399)) (let ((&temp~164~398 (cdr &temp~164~397))) (if (list? &temp~164~398) (let ((&c2~164~291 &temp~164~398)) (((lambda (&f~164~300) ((lambda (&temp~164~393) (set! &f~164~300 &temp~164~393) ((lambda () &f~164~300))) (lambda (&c1~164~304 &c2*~164~303) (let ((&input~164~306 &c2*~164~303)) (let ((&fail~164~307 (lambda () (let ((&fail~164~308 (lambda () (ex:invalid-form &input~164~306)))) (if (pair? &input~164~306) (let ((&temp~164~351 (car &input~164~306))) (let ((&c2~164~310 &temp~164~351)) (let ((&temp~164~350 (cdr &input~164~306))) (if (list? &temp~164~350) (let ((&c3~164~309 &temp~164~350)) (let ((&input~164~313 (&f~164~300 &c2~164~310 &c3~164~309))) (let ((&fail~164~316 (lambda () (ex:invalid-form &input~164~313)))) (let ((&rest~164~317 &input~164~313)) (begin (let ((&input~164~320 &c1~164~304)) (let ((&fail~164~321 (lambda () (let ((&fail~164~322 (lambda () (let ((&fail~164~323 (lambda () (let ((&fail~164~324 (lambda () (ex:invalid-form &input~164~320)))) (ex:syntax-violation (quote cond) "Invalid expression" &x~164~287))))) (if (pair? &input~164~320) (let ((&temp~164~334 (car &input~164~320))) (let ((&e0~164~328 &temp~164~334)) (let ((&temp~164~331 (cdr &input~164~320))) (if (pair? &temp~164~331) (let ((&temp~164~333 (car &temp~164~331))) (let ((&e1~164~327 &temp~164~333)) (let ((&temp~164~332 (cdr &temp~164~331))) (if (list? &temp~164~332) (let ((&e2~164~326 &temp~164~332)) (cons (ex:syntax-rename (quote if) (quote ()) (quote (&env~164~330)) 0 (quote (core derived))) (cons &e0~164~328 (cons (cons (ex:syntax-rename (quote begin) (quote ()) (quote (&env~164~330)) 0 (quote (core derived))) (cons &e1~164~327 &e2~164~326)) (cons &rest~164~317 (quote ())))))) (&fail~164~323))))) (&fail~164~323))))) (&fail~164~323)))))) (if (pair? &input~164~320) (let ((&temp~164~344 (car &input~164~320))) (let ((&e0~164~336 &temp~164~344)) (let ((&temp~164~339 (cdr &input~164~320))) (if (pair? &temp~164~339) (let ((&temp~164~343 (car &temp~164~339))) (if (and (ex:identifier? &temp~164~343) (ex:free-identifier=? &temp~164~343 (ex:syntax-rename (quote =>) (quote ()) (quote (&env~164~338)) 0 (quote (core derived))))) (let ((&temp~164~340 (cdr &temp~164~339))) (if (pair? &temp~164~340) (let ((&temp~164~342 (car &temp~164~340))) (let ((&e1~164~335 &temp~164~342)) (let ((&temp~164~341 (cdr &temp~164~340))) (if (null? &temp~164~341) (cons (ex:syntax-rename (quote let) (quote ()) (quote (&env~164~338)) 0 (quote (core derived))) (cons (cons (cons (ex:syntax-rename (quote t) (quote ()) (quote (&env~164~338)) 0 (quote (core derived))) (cons &e0~164~336 (quote ()))) (quote ())) (cons (cons (ex:syntax-rename (quote if) (quote ()) (quote (&env~164~338)) 0 (quote (core derived))) (cons (ex:syntax-rename (quote t) (quote ()) (quote (&env~164~338)) 0 (quote (core derived))) (cons (cons &e1~164~335 (cons (ex:syntax-rename (quote t) (quote ()) (quote (&env~164~338)) 0 (quote (core derived))) (quote ()))) (cons &rest~164~317 (quote ()))))) (quote ())))) (&fail~164~322))))) (&fail~164~322))) (&fail~164~322))) (&fail~164~322))))) (&fail~164~322)))))) (if (pair? &input~164~320) (let ((&temp~164~349 (car &input~164~320))) (let ((&e0~164~345 &temp~164~349)) (let ((&temp~164~348 (cdr &input~164~320))) (if (null? &temp~164~348) (cons (ex:syntax-rename (quote let) (quote ()) (quote (&env~164~347)) 0 (quote (core derived))) (cons (cons (cons (ex:syntax-rename (quote t) (quote ()) (quote (&env~164~347)) 0 (quote (core derived))) (cons &e0~164~345 (quote ()))) (quote ())) (cons (cons (ex:syntax-rename (quote if) (quote ()) (quote (&env~164~347)) 0 (quote (core derived))) (cons (ex:syntax-rename (quote t) (quote ()) (quote (&env~164~347)) 0 (quote (core derived))) (cons (ex:syntax-rename (quote t) (quote ()) (quote (&env~164~347)) 0 (quote (core derived))) (cons &rest~164~317 (quote ()))))) (quote ())))) (&fail~164~321))))) (&fail~164~321))))))))) (&fail~164~308))))) (&fail~164~308)))))) (if (null? &input~164~306) (let ((&input~164~353 &c1~164~304)) (let ((&fail~164~354 (lambda () (let ((&fail~164~355 (lambda () (let ((&fail~164~356 (lambda () (let ((&fail~164~357 (lambda () (let ((&fail~164~358 (lambda () (ex:invalid-form &input~164~353)))) (ex:syntax-violation (quote cond) "Invalid expression" &x~164~287))))) (if (pair? &input~164~353) (let ((&temp~164~368 (car &input~164~353))) (let ((&e0~164~362 &temp~164~368)) (let ((&temp~164~365 (cdr &input~164~353))) (if (pair? &temp~164~365) (let ((&temp~164~367 (car &temp~164~365))) (let ((&e1~164~361 &temp~164~367)) (let ((&temp~164~366 (cdr &temp~164~365))) (if (list? &temp~164~366) (let ((&e2~164~360 &temp~164~366)) (cons (ex:syntax-rename (quote if) (quote ()) (quote (&env~164~364)) 0 (quote (core derived))) (cons &e0~164~362 (cons (cons (ex:syntax-rename (quote begin) (quote ()) (quote (&env~164~364)) 0 (quote (core derived))) (cons &e1~164~361 &e2~164~360)) (quote ()))))) (&fail~164~357))))) (&fail~164~357))))) (&fail~164~357)))))) (if (pair? &input~164~353) (let ((&temp~164~378 (car &input~164~353))) (let ((&e0~164~370 &temp~164~378)) (let ((&temp~164~373 (cdr &input~164~353))) (if (pair? &temp~164~373) (let ((&temp~164~377 (car &temp~164~373))) (if (and (ex:identifier? &temp~164~377) (ex:free-identifier=? &temp~164~377 (ex:syntax-rename (quote =>) (quote ()) (quote (&env~164~372)) 0 (quote (core derived))))) (let ((&temp~164~374 (cdr &temp~164~373))) (if (pair? &temp~164~374) (let ((&temp~164~376 (car &temp~164~374))) (let ((&e1~164~369 &temp~164~376)) (let ((&temp~164~375 (cdr &temp~164~374))) (if (null? &temp~164~375) (cons (ex:syntax-rename (quote let) (quote ()) (quote (&env~164~372)) 0 (quote (core derived))) (cons (cons (cons (ex:syntax-rename (quote t) (quote ()) (quote (&env~164~372)) 0 (quote (core derived))) (cons &e0~164~370 (quote ()))) (quote ())) (cons (cons (ex:syntax-rename (quote if) (quote ()) (quote (&env~164~372)) 0 (quote (core derived))) (cons (ex:syntax-rename (quote t) (quote ()) (quote (&env~164~372)) 0 (quote (core derived))) (cons (cons &e1~164~369 (cons (ex:syntax-rename (quote t) (quote ()) (quote (&env~164~372)) 0 (quote (core derived))) (quote ()))) (quote ())))) (quote ())))) (&fail~164~356))))) (&fail~164~356))) (&fail~164~356))) (&fail~164~356))))) (&fail~164~356)))))) (if (pair? &input~164~353) (let ((&temp~164~383 (car &input~164~353))) (let ((&e0~164~379 &temp~164~383)) (let ((&temp~164~382 (cdr &input~164~353))) (if (null? &temp~164~382) (cons (ex:syntax-rename (quote let) (quote ()) (quote (&env~164~381)) 0 (quote (core derived))) (cons (cons (cons (ex:syntax-rename (quote t) (quote ()) (quote (&env~164~381)) 0 (quote (core derived))) (cons &e0~164~379 (quote ()))) (quote ())) (cons (cons (ex:syntax-rename (quote if) (quote ()) (quote (&env~164~381)) 0 (quote (core derived))) (cons (ex:syntax-rename (quote t) (quote ()) (quote (&env~164~381)) 0 (quote (core derived))) (cons (ex:syntax-rename (quote t) (quote ()) (quote (&env~164~381)) 0 (quote (core derived))) (quote ())))) (quote ())))) (&fail~164~355))))) (&fail~164~355)))))) (if (pair? &input~164~353) (let ((&temp~164~391 (car &input~164~353))) (if (and (ex:identifier? &temp~164~391) (ex:free-identifier=? &temp~164~391 (ex:syntax-rename (quote else) (quote ()) (quote (&env~164~387)) 0 (quote (core derived))))) (let ((&temp~164~388 (cdr &input~164~353))) (if (pair? &temp~164~388) (let ((&temp~164~390 (car &temp~164~388))) (let ((&e1~164~385 &temp~164~390)) (let ((&temp~164~389 (cdr &temp~164~388))) (if (list? &temp~164~389) (let ((&e2~164~384 &temp~164~389)) (cons (ex:syntax-rename (quote begin) (quote ()) (quote (&env~164~387)) 0 (quote (core derived))) (cons &e1~164~385 &e2~164~384))) (&fail~164~354))))) (&fail~164~354))) (&fail~164~354))) (&fail~164~354)))) (&fail~164~307))))))) ex:undefined) &c1~164~292 &c2~164~291)) (&fail~164~290))))) (&fail~164~290)))) (&fail~164~290)))))) (ex:register-macro! (quote &case~164~401) (lambda (&x~164~403) (let ((&input~164~405 &x~164~403)) (let ((&fail~164~406 (lambda () (ex:invalid-form &input~164~405)))) (if (pair? &input~164~405) (let ((&temp~164~480 (car &input~164~405))) (let ((&temp~164~475 (cdr &input~164~405))) (if (pair? &temp~164~475) (let ((&temp~164~479 (car &temp~164~475))) (let ((&e~164~409 &temp~164~479)) (let ((&temp~164~476 (cdr &temp~164~475))) (if (pair? &temp~164~476) (let ((&temp~164~478 (car &temp~164~476))) (let ((&c1~164~408 &temp~164~478)) (let ((&temp~164~477 (cdr &temp~164~476))) (if (list? &temp~164~477) (let ((&c2~164~407 &temp~164~477)) (let ((&input~164~412 (((lambda (&f~164~420) ((lambda (&temp~164~466) (set! &f~164~420 &temp~164~466) ((lambda () &f~164~420))) (lambda (&c1~164~424 &cmore~164~423) (if (null? &cmore~164~423) (let ((&input~164~445 &c1~164~424)) (let ((&fail~164~446 (lambda () (let ((&fail~164~447 (lambda () (ex:invalid-form &input~164~445)))) (if (pair? &input~164~445) (let ((&temp~164~456 (car &input~164~445))) (if (list? &temp~164~456) (let ((&k~164~450 &temp~164~456)) (let ((&temp~164~453 (cdr &input~164~445))) (if (pair? &temp~164~453) (let ((&temp~164~455 (car &temp~164~453))) (let ((&e1~164~449 &temp~164~455)) (let ((&temp~164~454 (cdr &temp~164~453))) (if (list? &temp~164~454) (let ((&e2~164~448 &temp~164~454)) (cons (ex:syntax-rename (quote if) (quote ()) (quote (&env~164~452)) 0 (quote (core derived))) (cons (cons (ex:syntax-rename (quote memv) (quote ()) (quote (&env~164~452)) 0 (quote (core derived))) (cons (ex:syntax-rename (quote t) (quote ()) (quote (&env~164~452)) 0 (quote (core derived))) (cons (cons (ex:syntax-rename (quote quote) (quote ()) (quote (&env~164~452)) 0 (quote (core derived))) (cons &k~164~450 (quote ()))) (quote ())))) (cons (cons (ex:syntax-rename (quote begin) (quote ()) (quote (&env~164~452)) 0 (quote (core derived))) (cons &e1~164~449 &e2~164~448)) (quote ()))))) (&fail~164~447))))) (&fail~164~447)))) (&fail~164~447))) (&fail~164~447)))))) (if (pair? &input~164~445) (let ((&temp~164~464 (car &input~164~445))) (if (and (ex:identifier? &temp~164~464) (ex:free-identifier=? &temp~164~464 (ex:syntax-rename (quote else) (quote ()) (quote (&env~164~460)) 0 (quote (core derived))))) (let ((&temp~164~461 (cdr &input~164~445))) (if (pair? &temp~164~461) (let ((&temp~164~463 (car &temp~164~461))) (let ((&e1~164~458 &temp~164~463)) (let ((&temp~164~462 (cdr &temp~164~461))) (if (list? &temp~164~462) (let ((&e2~164~457 &temp~164~462)) (cons (ex:syntax-rename (quote begin) (quote ()) (quote (&env~164~460)) 0 (quote (core derived))) (cons &e1~164~458 &e2~164~457))) (&fail~164~446))))) (&fail~164~446))) (&fail~164~446))) (&fail~164~446)))) (let ((&input~164~428 (&f~164~420 (car &cmore~164~423) (cdr &cmore~164~423)))) (let ((&fail~164~429 (lambda () (ex:invalid-form &input~164~428)))) (let ((&rest~164~430 &input~164~428)) (begin (let ((&input~164~433 &c1~164~424)) (let ((&fail~164~434 (lambda () (ex:invalid-form &input~164~433)))) (if (pair? &input~164~433) (let ((&temp~164~443 (car &input~164~433))) (if (list? &temp~164~443) (let ((&k~164~437 &temp~164~443)) (let ((&temp~164~440 (cdr &input~164~433))) (if (pair? &temp~164~440) (let ((&temp~164~442 (car &temp~164~440))) (let ((&e1~164~436 &temp~164~442)) (let ((&temp~164~441 (cdr &temp~164~440))) (if (list? &temp~164~441) (let ((&e2~164~435 &temp~164~441)) (cons (ex:syntax-rename (quote if) (quote ()) (quote (&env~164~439)) 0 (quote (core derived))) (cons (cons (ex:syntax-rename (quote memv) (quote ()) (quote (&env~164~439)) 0 (quote (core derived))) (cons (ex:syntax-rename (quote t) (quote ()) (quote (&env~164~439)) 0 (quote (core derived))) (cons (cons (ex:syntax-rename (quote quote) (quote ()) (quote (&env~164~439)) 0 (quote (core derived))) (cons &k~164~437 (quote ()))) (quote ())))) (cons (cons (ex:syntax-rename (quote begin) (quote ()) (quote (&env~164~439)) 0 (quote (core derived))) (cons &e1~164~436 &e2~164~435)) (cons &rest~164~430 (quote ())))))) (&fail~164~434))))) (&fail~164~434)))) (&fail~164~434))) (&fail~164~434)))))))))))) ex:undefined) &c1~164~408 &c2~164~407))) (let ((&fail~164~470 (lambda () (ex:invalid-form &input~164~412)))) (let ((&body~164~471 &input~164~412)) (begin (cons (ex:syntax-rename (quote let) (quote ()) (quote (&env~164~474)) 0 (quote (core derived))) (cons (cons (cons (ex:syntax-rename (quote t) (quote ()) (quote (&env~164~474)) 0 (quote (core derived))) (cons &e~164~409 (quote ()))) (quote ())) (cons &body~164~471 (quote ()))))))))) (&fail~164~406))))) (&fail~164~406))))) (&fail~164~406)))) (&fail~164~406)))))) (ex:register-macro! (quote &=>~164~481) (lambda (&x~164~483) (ex:syntax-violation (quote =>) "Invalid expression" &x~164~483))) (ex:register-macro! (quote &else~164~485) (lambda (&x~164~487) (ex:syntax-violation (quote else) "Invalid expression" &x~164~487))) (values)) (lambda () (values)) (quote &build~164~489))) (values))
(begin (ex:register-library! (ex:make-library (quote (core identifier-syntax)) (lambda () (ex:uncompress (quote (((&env~164~530 0 1 2 3) (&env~164~503 4 1 2 3)) (4 (((id) . #f) ((exp1) . #f) ((var) . #f) ((val) . #f) ((exp2) . #f))) (3 (((identifier-syntax) macro &identifier-syntax~164~490 (0) #f (core identifier-syntax)) ((undefined) variable ex:undefined (1 0 -1) #f ()) ((eval) variable ex:eval (1 0 -1) #f ()) ((environment-bindings) variable ex:environment-bindings (1 0 -1) #f ()) ((environment) variable ex:environment (1 0 -1) #f ()) ((syntax-violation) variable ex:syntax-violation (1 0 -1) #f ()) ((syntax->datum) variable ex:syntax->datum (1 0 -1) #f ()) ((datum->syntax) variable ex:datum->syntax (1 0 -1) #f ()) ((generate-temporaries) variable ex:generate-temporaries (1 0 -1) #f ()) ((free-identifier=?) variable ex:free-identifier=? (1 0 -1) #f ()) ((bound-identifier=?) variable ex:bound-identifier=? (1 0 -1) #f ()) ((identifier?) variable ex:identifier? (1 0 -1) #f ()) ((make-variable-transformer) variable ex:make-variable-transformer (1 0 -1) #f ()) ((syntax-case) macro syntax-case (1 0 -1) #f ()) ((syntax) macro syntax (1 0 -1) #f ()) ((...) macro ... (1 0 -1) #f ()) ((_) macro _ (1 0 -1) #f ()) ((letrec-syntax) macro letrec-syntax (1 0 -1) #f ()) ((let-syntax) macro let-syntax (1 0 -1) #f ()) ((define-syntax) macro define-syntax (1 0 -1) #f ()) ((define) macro define (1 0 -1) #f ()) ((or) macro or (1 0 -1) #f ()) ((and) macro and (1 0 -1) #f ()) ((set!) macro set! (1 0 -1) #f ()) ((quote) macro quote (1 0 -1) #f ()) ((lambda) macro lambda (1 0 -1) #f ()) ((if) macro if (1 0 -1) #f ()) ((begin) macro begin (1 0 -1) #f ()))) (2 (((x) variable &x~164~492 (1) #f (core identifier-syntax)))) (1 ()) (0 (((e) . #f))))))) (quote ((identifier-syntax macro &identifier-syntax~164~490 (0) #f (core identifier-syntax)))) (quote (((core primitives) 1 0 -1))) (quote (&build~164~2)) (lambda () (ex:register-macro! (quote &identifier-syntax~164~490) (lambda (&x~164~492) (let ((&input~164~494 &x~164~492)) (let ((&fail~164~495 (lambda () (let ((&fail~164~496 (lambda () (ex:invalid-form &input~164~494)))) (if (pair? &input~164~494) (let ((&temp~164~527 (car &input~164~494))) (let ((&temp~164~508 (cdr &input~164~494))) (if (pair? &temp~164~508) (let ((&temp~164~522 (car &temp~164~508))) (if (pair? &temp~164~522) (let ((&temp~164~526 (car &temp~164~522))) (let ((&id~164~501 &temp~164~526)) (let ((&temp~164~523 (cdr &temp~164~522))) (if (pair? &temp~164~523) (let ((&temp~164~525 (car &temp~164~523))) (let ((&exp1~164~500 &temp~164~525)) (let ((&temp~164~524 (cdr &temp~164~523))) (if (null? &temp~164~524) (let ((&temp~164~509 (cdr &temp~164~508))) (if (pair? &temp~164~509) (let ((&temp~164~511 (car &temp~164~509))) (if (pair? &temp~164~511) (let ((&temp~164~515 (car &temp~164~511))) (if (pair? &temp~164~515) (let ((&temp~164~521 (car &temp~164~515))) (if (and (ex:identifier? &temp~164~521) (ex:free-identifier=? &temp~164~521 (ex:syntax-rename (quote set!) (quote ()) (quote (&env~164~503)) 0 (quote (core identifier-syntax))))) (let ((&temp~164~516 (cdr &temp~164~515))) (if (pair? &temp~164~516) (let ((&temp~164~520 (car &temp~164~516))) (let ((&var~164~499 &temp~164~520)) (let ((&temp~164~517 (cdr &temp~164~516))) (if (pair? &temp~164~517) (let ((&temp~164~519 (car &temp~164~517))) (let ((&val~164~498 &temp~164~519)) (let ((&temp~164~518 (cdr &temp~164~517))) (if (null? &temp~164~518) (let ((&temp~164~512 (cdr &temp~164~511))) (if (pair? &temp~164~512) (let ((&temp~164~514 (car &temp~164~512))) (let ((&exp2~164~497 &temp~164~514)) (let ((&temp~164~513 (cdr &temp~164~512))) (if (null? &temp~164~513) (let ((&temp~164~510 (cdr &temp~164~509))) (if (null? &temp~164~510) (if (if (ex:identifier? &id~164~501) (ex:identifier? &var~164~499) #f) (cons (ex:syntax-rename (quote make-variable-transformer) (quote ()) (quote (&env~164~503)) 0 (quote (core identifier-syntax))) (cons (cons (ex:syntax-rename (quote lambda) (quote ()) (quote (&env~164~503)) 0 (quote (core identifier-syntax))) (cons (cons (ex:syntax-rename (quote x) (quote ()) (quote (&env~164~503)) 0 (quote (core identifier-syntax))) (quote ())) (cons (cons (ex:syntax-rename (quote syntax-case) (quote ()) (quote (&env~164~503)) 0 (quote (core identifier-syntax))) (cons (ex:syntax-rename (quote x) (quote ()) (quote (&env~164~503)) 0 (quote (core identifier-syntax))) (cons (cons (ex:syntax-rename (quote set!) (quote ()) (quote (&env~164~503)) 0 (quote (core identifier-syntax))) (quote ())) (cons (cons (cons (ex:syntax-rename (quote set!) (quote ()) (quote (&env~164~503)) 0 (quote (core identifier-syntax))) (cons &var~164~499 (cons &val~164~498 (quote ())))) (cons (cons (ex:syntax-rename (quote syntax) (quote ()) (quote (&env~164~503)) 0 (quote (core identifier-syntax))) (cons &exp2~164~497 (quote ()))) (quote ()))) (cons (cons (cons &id~164~501 (cons (ex:syntax-rename (quote x) (quote ()) (quote (&env~164~503)) 0 (quote (core identifier-syntax))) (cons (ex:syntax-rename (quote ...) (quote ()) (quote (&env~164~503)) 0 (quote (core identifier-syntax))) (quote ())))) (cons (cons (ex:syntax-rename (quote syntax) (quote ()) (quote (&env~164~503)) 0 (quote (core identifier-syntax))) (cons (cons &exp1~164~500 (cons (ex:syntax-rename (quote x) (quote ()) (quote (&env~164~503)) 0 (quote (core identifier-syntax))) (cons (ex:syntax-rename (quote ...) (quote ()) (quote (&env~164~503)) 0 (quote (core identifier-syntax))) (quote ())))) (quote ()))) (quote ()))) (cons (cons &id~164~501 (cons (cons (ex:syntax-rename (quote identifier?) (quote ()) (quote (&env~164~503)) 0 (quote (core identifier-syntax))) (cons (cons (ex:syntax-rename (quote syntax) (quote ()) (quote (&env~164~503)) 0 (quote (core identifier-syntax))) (cons &id~164~501 (quote ()))) (quote ()))) (cons (cons (ex:syntax-rename (quote syntax) (quote ()) (quote (&env~164~503)) 0 (quote (core identifier-syntax))) (cons &exp1~164~500 (quote ()))) (quote ())))) (quote ()))))))) (quote ())))) (quote ()))) (&fail~164~496)) (&fail~164~496))) (&fail~164~496))))) (&fail~164~496))) (&fail~164~496))))) (&fail~164~496))))) (&fail~164~496))) (&fail~164~496))) (&fail~164~496))) (&fail~164~496))) (&fail~164~496))) (&fail~164~496))))) (&fail~164~496))))) (&fail~164~496))) (&fail~164~496)))) (&fail~164~496)))))) (if (pair? &input~164~494) (let ((&temp~164~534 (car &input~164~494))) (let ((&temp~164~531 (cdr &input~164~494))) (if (pair? &temp~164~531) (let ((&temp~164~533 (car &temp~164~531))) (let ((&e~164~528 &temp~164~533)) (let ((&temp~164~532 (cdr &temp~164~531))) (if (null? &temp~164~532) (cons (ex:syntax-rename (quote lambda) (quote ()) (quote (&env~164~530)) 0 (quote (core identifier-syntax))) (cons (cons (ex:syntax-rename (quote x) (quote ()) (quote (&env~164~530)) 0 (quote (core identifier-syntax))) (quote ())) (cons (cons (ex:syntax-rename (quote syntax-case) (quote ()) (quote (&env~164~530)) 0 (quote (core identifier-syntax))) (cons (ex:syntax-rename (quote x) (quote ()) (quote (&env~164~530)) 0 (quote (core identifier-syntax))) (cons (quote ()) (cons (cons (ex:syntax-rename (quote id) (quote ()) (quote (&env~164~530)) 0 (quote (core identifier-syntax))) (cons (cons (ex:syntax-rename (quote identifier?) (quote ()) (quote (&env~164~530)) 0 (quote (core identifier-syntax))) (cons (cons (ex:syntax-rename (quote syntax) (quote ()) (quote (&env~164~530)) 0 (quote (core identifier-syntax))) (cons (ex:syntax-rename (quote id) (quote ()) (quote (&env~164~530)) 0 (quote (core identifier-syntax))) (quote ()))) (quote ()))) (cons (cons (ex:syntax-rename (quote syntax) (quote ()) (quote (&env~164~530)) 0 (quote (core identifier-syntax))) (cons &e~164~528 (quote ()))) (quote ())))) (cons (cons (cons (ex:syntax-rename (quote _) (quote ()) (quote (&env~164~530)) 0 (quote (core identifier-syntax))) (cons (ex:syntax-rename (quote x) (quote ()) (quote (&env~164~530)) 0 (quote (core identifier-syntax))) (cons (ex:syntax-rename (quote ...) (quote ()) (quote (&env~164~530)) 0 (quote (core identifier-syntax))) (quote ())))) (cons (cons (ex:syntax-rename (quote syntax) (quote ()) (quote (&env~164~530)) 0 (quote (core identifier-syntax))) (cons (cons &e~164~528 (cons (ex:syntax-rename (quote x) (quote ()) (quote (&env~164~530)) 0 (quote (core identifier-syntax))) (cons (ex:syntax-rename (quote ...) (quote ()) (quote (&env~164~530)) 0 (quote (core identifier-syntax))) (quote ())))) (quote ()))) (quote ()))) (quote ())))))) (quote ())))) (&fail~164~495))))) (&fail~164~495)))) (&fail~164~495)))))) (values)) (lambda () (values)) (quote &build~164~535))) (values))
(begin (ex:register-library! (ex:make-library (quote (core quasisyntax)) (lambda () (ex:uncompress (quote (((&env~164~745 0 1 2 3 4) (&env~164~731 5 6 7 2 3 4) (&env~164~703 8 6 7 2 3 4) (&env~164~687 9 6 7 2 3 4) (&env~164~661 10 6 7 2 3 4) (&env~164~642 11 10 6 7 2 3 4) (&env~164~618 12 6 7 2 3 4)) (12 (((k) . #f) ((r) . #f))) (11 (((r*) . #f) ((rep) . #f) ((t) . #f))) (10 (((e) . #f) ((r) . #f))) (9 (((e) . #f) ((r) . #f))) (8 (((e) . #f))) (7 (((x) variable &x~164~542 (1) #f (core quasisyntax)) ((level) variable &level~164~541 (1) #f (core quasisyntax)))) (6 ()) (5 (((e) . #f))) (4 (((unsyntax-splicing) macro &unsyntax-splicing~164~758 (0) #f (core quasisyntax)) ((unsyntax) macro &unsyntax~164~754 (0) #f (core quasisyntax)) ((quasisyntax) macro &quasisyntax~164~536 (0) #f (core quasisyntax)) ((undefined) variable ex:undefined (0 1) #f ()) ((eval) variable ex:eval (0 1) #f ()) ((environment-bindings) variable ex:environment-bindings (0 1) #f ()) ((environment) variable ex:environment (0 1) #f ()) ((syntax-violation) variable ex:syntax-violation (0 1) #f ()) ((syntax->datum) variable ex:syntax->datum (0 1) #f ()) ((datum->syntax) variable ex:datum->syntax (0 1) #f ()) ((generate-temporaries) variable ex:generate-temporaries (0 1) #f ()) ((free-identifier=?) variable ex:free-identifier=? (0 1) #f ()) ((bound-identifier=?) variable ex:bound-identifier=? (0 1) #f ()) ((identifier?) variable ex:identifier? (0 1) #f ()) ((make-variable-transformer) variable ex:make-variable-transformer (0 1) #f ()) ((syntax-case) macro syntax-case (0 1) #f ()) ((syntax) macro syntax (0 1) #f ()) ((...) macro ... (0 1) #f ()) ((_) macro _ (0 1) #f ()) ((letrec-syntax) macro letrec-syntax (0 1) #f ()) ((let-syntax) macro let-syntax (0 1) #f ()) ((define-syntax) macro define-syntax (0 1) #f ()) ((define) macro define (0 1) #f ()) ((or) macro or (0 1) #f ()) ((and) macro and (0 1) #f ()) ((set!) macro set! (0 1) #f ()) ((quote) macro quote (0 1) #f ()) ((lambda) macro lambda (0 1) #f ()) ((if) macro if (0 1) #f ()) ((begin) macro begin (0 1) #f ()) ((letrec*) macro &letrec*~164~179 (0 1) #f (core let)) ((letrec) macro &letrec~164~148 (0 1) #f (core let)) ((let) macro &let~164~100 (0 1) #f (core let)) ((=>) macro &=>~164~481 (0 1) #f (core derived)) ((else) macro &else~164~485 (0 1) #f (core derived)) ((case) macro &case~164~401 (0 1) #f (core derived)) ((cond) macro &cond~164~285 (0 1) #f (core derived)) ((let*) macro &let*~164~204 (0 1) #f (core derived)) ((with-syntax) macro &with-syntax~164~3 (0 1) #f (core with-syntax)) ((vector->list) variable vector->list (0 1) #f ()) ((-) variable - (0 1) #f ()) ((+) variable + (0 1) #f ()) ((>) variable > (0 1) #f ()) ((=) variable = (0 1) #f ()))) (3 (((e) variable &e~164~538 (1) #f (core quasisyntax)))) (2 (((expand) variable &expand~164~539 (1) #f (core quasisyntax)))) (1 (((template) . #f))) (0 (((template*) . #f) ((replacements) . #f))))))) (quote ((quasisyntax macro &quasisyntax~164~536 (0) #f (core quasisyntax)) (unsyntax macro &unsyntax~164~754 (0) #f (core quasisyntax)) (unsyntax-splicing macro &unsyntax-splicing~164~758 (0) #f (core quasisyntax)))) (quote (((core with-syntax) 0 1) ((core derived) 0 1) ((core let) 0 1) ((core primitives) 0 1))) (quote (&build~164~57 &build~164~489 &build~164~203 &build~164~2)) (lambda () (ex:register-macro! (quote &quasisyntax~164~536) (lambda (&e~164~538) ((lambda (&expand~164~539) (set! &expand~164~539 (lambda (&x~164~542 &level~164~541) (let ((&input~164~544 &x~164~542)) (let ((&fail~164~545 (lambda () (let ((&fail~164~546 (lambda () (let ((&fail~164~547 (lambda () (let ((&fail~164~548 (lambda () (let ((&fail~164~549 (lambda () (let ((&fail~164~550 (lambda () (let ((&fail~164~551 (lambda () (let ((&fail~164~552 (lambda () (ex:invalid-form &input~164~544)))) (let ((&other~164~553 &input~164~544)) (cons &other~164~553 (cons (quote ()) (quote ())))))))) (if (vector? &input~164~544) (let ((&temp~164~569 (vector->list &input~164~544))) (if (list? &temp~164~569) (let ((&e~164~555 &temp~164~569)) (let ((&input~164~558 (&expand~164~539 (vector->list (list->vector &e~164~555)) &level~164~541))) (let ((&fail~164~560 (lambda () (ex:invalid-form &input~164~558)))) (if (pair? &input~164~558) (let ((&temp~164~568 (car &input~164~558))) (if (list? &temp~164~568) (let ((&e*~164~562 &temp~164~568)) (let ((&temp~164~565 (cdr &input~164~558))) (if (pair? &temp~164~565) (let ((&temp~164~567 (car &temp~164~565))) (let ((&reps~164~561 &temp~164~567)) (let ((&temp~164~566 (cdr &temp~164~565))) (if (null? &temp~164~566) (begin (cons (list->vector &e*~164~562) (cons &reps~164~561 (quote ())))) (&fail~164~560))))) (&fail~164~560)))) (&fail~164~560))) (&fail~164~560))))) (&fail~164~551))) (&fail~164~551)))))) (if (pair? &input~164~544) (let ((&temp~164~597 (car &input~164~544))) (let ((&h~164~571 &temp~164~597)) (let ((&temp~164~596 (cdr &input~164~544))) (let ((&t~164~570 &temp~164~596)) (let ((&input~164~574 (list (&expand~164~539 &h~164~571 &level~164~541) (&expand~164~539 &t~164~570 &level~164~541)))) (let ((&fail~164~577 (lambda () (ex:invalid-form &input~164~574)))) (if (pair? &input~164~574) (let ((&temp~164~591 (car &input~164~574))) (if (pair? &temp~164~591) (let ((&temp~164~595 (car &temp~164~591))) (let ((&h*~164~581 &temp~164~595)) (let ((&temp~164~592 (cdr &temp~164~591))) (if (pair? &temp~164~592) (let ((&temp~164~594 (car &temp~164~592))) (if (list? &temp~164~594) (let ((&rep1~164~580 &temp~164~594)) (let ((&temp~164~593 (cdr &temp~164~592))) (if (null? &temp~164~593) (let ((&temp~164~584 (cdr &input~164~574))) (if (pair? &temp~164~584) (let ((&temp~164~586 (car &temp~164~584))) (if (pair? &temp~164~586) (let ((&temp~164~590 (car &temp~164~586))) (let ((&t*~164~579 &temp~164~590)) (let ((&temp~164~587 (cdr &temp~164~586))) (if (pair? &temp~164~587) (let ((&temp~164~589 (car &temp~164~587))) (if (list? &temp~164~589) (let ((&rep2~164~578 &temp~164~589)) (let ((&temp~164~588 (cdr &temp~164~587))) (if (null? &temp~164~588) (let ((&temp~164~585 (cdr &temp~164~584))) (if (null? &temp~164~585) (begin (cons (cons &h*~164~581 &t*~164~579) (cons (append &rep1~164~580 &rep2~164~578) (quote ())))) (&fail~164~577))) (&fail~164~577)))) (&fail~164~577))) (&fail~164~577))))) (&fail~164~577))) (&fail~164~577))) (&fail~164~577)))) (&fail~164~577))) (&fail~164~577))))) (&fail~164~577))) (&fail~164~577)))))))) (&fail~164~550)))))) (if (pair? &input~164~544) (let ((&temp~164~625 (car &input~164~544))) (let ((&k~164~599 &temp~164~625)) (let ((&temp~164~624 (cdr &input~164~544))) (let ((&r~164~598 &temp~164~624)) (if (if (> &level~164~541 0) (if (ex:identifier? &k~164~599) (let ((x (ex:free-identifier=? &k~164~599 (ex:syntax-rename (quote unsyntax) (quote ()) (quote (&env~164~618)) 0 (quote (core quasisyntax)))))) (if x x (ex:free-identifier=? &k~164~599 (ex:syntax-rename (quote unsyntax-splicing) (quote ()) (quote (&env~164~618)) 0 (quote (core quasisyntax)))))) #f) #f) (let ((&input~164~602 (&expand~164~539 &r~164~598 (- &level~164~541 1)))) (let ((&fail~164~604 (lambda () (ex:invalid-form &input~164~602)))) (if (pair? &input~164~602) (let ((&temp~164~612 (car &input~164~602))) (let ((&r*~164~606 &temp~164~612)) (let ((&temp~164~609 (cdr &input~164~602))) (if (pair? &temp~164~609) (let ((&temp~164~611 (car &temp~164~609))) (let ((&reps~164~605 &temp~164~611)) (let ((&temp~164~610 (cdr &temp~164~609))) (if (null? &temp~164~610) (begin (cons (cons &k~164~599 &r*~164~606) (cons &reps~164~605 (quote ())))) (&fail~164~604))))) (&fail~164~604))))) (&fail~164~604)))) (&fail~164~549)))))) (&fail~164~549)))))) (if (pair? &input~164~544) (let ((&temp~164~658 (car &input~164~544))) (if (pair? &temp~164~658) (let ((&temp~164~660 (car &temp~164~658))) (if (and (ex:identifier? &temp~164~660) (ex:free-identifier=? &temp~164~660 (ex:syntax-rename (quote unsyntax-splicing) (quote ()) (quote (&env~164~661)) 0 (quote (core quasisyntax))))) (let ((&temp~164~659 (cdr &temp~164~658))) (if (list? &temp~164~659) (let ((&e~164~627 &temp~164~659)) (let ((&temp~164~657 (cdr &input~164~544))) (let ((&r~164~626 &temp~164~657)) (if (= &level~164~541 0) (let ((&input~164~630 (list (&expand~164~539 &r~164~626 0) (ex:generate-temporaries &e~164~627)))) (let ((&fail~164~633 (lambda () (ex:invalid-form &input~164~630)))) (if (pair? &input~164~630) (let ((&temp~164~652 (car &input~164~630))) (if (pair? &temp~164~652) (let ((&temp~164~656 (car &temp~164~652))) (let ((&r*~164~636 &temp~164~656)) (let ((&temp~164~653 (cdr &temp~164~652))) (if (pair? &temp~164~653) (let ((&temp~164~655 (car &temp~164~653))) (if (list? &temp~164~655) (let ((&rep~164~635 &temp~164~655)) (let ((&temp~164~654 (cdr &temp~164~653))) (if (null? &temp~164~654) (let ((&temp~164~649 (cdr &input~164~630))) (if (pair? &temp~164~649) (let ((&temp~164~651 (car &temp~164~649))) (if (list? &temp~164~651) (let ((&t~164~634 &temp~164~651)) (let ((&temp~164~650 (cdr &temp~164~649))) (if (null? &temp~164~650) (begin (let ((&input~164~640 (map (lambda (&t~164~634) (cons &t~164~634 (cons (ex:syntax-rename (quote ...) (quote ()) (quote (&env~164~642)) 0 (quote (core quasisyntax))) (quote ())))) &t~164~634))) (let ((&fail~164~643 (lambda () (ex:invalid-form &input~164~640)))) (ex:map-while (lambda (&input~164~640) (if (list? &input~164~640) (let ((&t~164~644 &input~164~640)) (list &t~164~644)) #f)) &input~164~640 (lambda (&cols~164~647 &rest~164~648) (if (null? &rest~164~648) (apply (lambda (&t~164~644) (begin (cons (append (apply append &t~164~644) &r*~164~636) (cons (append (if (= (length &t~164~644) (length &e~164~627)) (map (lambda (&t~164~644 &e~164~627) (cons &t~164~644 (cons &e~164~627 (quote ())))) &t~164~644 &e~164~627) (ex:syntax-violation (quote syntax) "Pattern variables denoting lists of unequal length preceding ellipses" (quote (((t ...) e) ... rep ...)) (list &t~164~644 &e~164~627))) &rep~164~635) (quote ()))))) (if (null? &cols~164~647) (quote (())) (apply map list &cols~164~647))) (&fail~164~643))))))) (&fail~164~633)))) (&fail~164~633))) (&fail~164~633))) (&fail~164~633)))) (&fail~164~633))) (&fail~164~633))))) (&fail~164~633))) (&fail~164~633)))) (&fail~164~548))))) (&fail~164~548))) (&fail~164~548))) (&fail~164~548))) (&fail~164~548)))))) (if (pair? &input~164~544) (let ((&temp~164~684 (car &input~164~544))) (if (pair? &temp~164~684) (let ((&temp~164~686 (car &temp~164~684))) (if (and (ex:identifier? &temp~164~686) (ex:free-identifier=? &temp~164~686 (ex:syntax-rename (quote unsyntax) (quote ()) (quote (&env~164~687)) 0 (quote (core quasisyntax))))) (let ((&temp~164~685 (cdr &temp~164~684))) (if (list? &temp~164~685) (let ((&e~164~663 &temp~164~685)) (let ((&temp~164~683 (cdr &input~164~544))) (let ((&r~164~662 &temp~164~683)) (if (= &level~164~541 0) (let ((&input~164~666 (list (&expand~164~539 &r~164~662 0) (ex:generate-temporaries &e~164~663)))) (let ((&fail~164~669 (lambda () (ex:invalid-form &input~164~666)))) (if (pair? &input~164~666) (let ((&temp~164~678 (car &input~164~666))) (if (pair? &temp~164~678) (let ((&temp~164~682 (car &temp~164~678))) (let ((&r*~164~672 &temp~164~682)) (let ((&temp~164~679 (cdr &temp~164~678))) (if (pair? &temp~164~679) (let ((&temp~164~681 (car &temp~164~679))) (if (list? &temp~164~681) (let ((&rep~164~671 &temp~164~681)) (let ((&temp~164~680 (cdr &temp~164~679))) (if (null? &temp~164~680) (let ((&temp~164~675 (cdr &input~164~666))) (if (pair? &temp~164~675) (let ((&temp~164~677 (car &temp~164~675))) (if (list? &temp~164~677) (let ((&t~164~670 &temp~164~677)) (let ((&temp~164~676 (cdr &temp~164~675))) (if (null? &temp~164~676) (begin (cons (append &t~164~670 &r*~164~672) (cons (append (if (= (length &t~164~670) (length &e~164~663)) (map (lambda (&t~164~670 &e~164~663) (cons &t~164~670 (cons &e~164~663 (quote ())))) &t~164~670 &e~164~663) (ex:syntax-violation (quote syntax) "Pattern variables denoting lists of unequal length preceding ellipses" (quote ((t e) ... rep ...)) (list &t~164~670 &e~164~663))) &rep~164~671) (quote ())))) (&fail~164~669)))) (&fail~164~669))) (&fail~164~669))) (&fail~164~669)))) (&fail~164~669))) (&fail~164~669))))) (&fail~164~669))) (&fail~164~669)))) (&fail~164~547))))) (&fail~164~547))) (&fail~164~547))) (&fail~164~547))) (&fail~164~547)))))) (if (pair? &input~164~544) (let ((&temp~164~702 (car &input~164~544))) (if (and (ex:identifier? &temp~164~702) (ex:free-identifier=? &temp~164~702 (ex:syntax-rename (quote unsyntax) (quote ()) (quote (&env~164~703)) 0 (quote (core quasisyntax))))) (let ((&temp~164~699 (cdr &input~164~544))) (if (pair? &temp~164~699) (let ((&temp~164~701 (car &temp~164~699))) (let ((&e~164~688 &temp~164~701)) (let ((&temp~164~700 (cdr &temp~164~699))) (if (null? &temp~164~700) (if (= &level~164~541 0) (let ((&input~164~691 (ex:generate-temporaries (quote (t))))) (let ((&fail~164~693 (lambda () (ex:invalid-form &input~164~691)))) (if (pair? &input~164~691) (let ((&temp~164~698 (car &input~164~691))) (let ((&t~164~694 &temp~164~698)) (let ((&temp~164~697 (cdr &input~164~691))) (if (null? &temp~164~697) (begin (cons &t~164~694 (cons (cons (cons &t~164~694 (cons &e~164~688 (quote ()))) (quote ())) (quote ())))) (&fail~164~693))))) (&fail~164~693)))) (&fail~164~546)) (&fail~164~546))))) (&fail~164~546))) (&fail~164~546))) (&fail~164~546)))))) (if (pair? &input~164~544) (let ((&temp~164~730 (car &input~164~544))) (if (and (ex:identifier? &temp~164~730) (ex:free-identifier=? &temp~164~730 (ex:syntax-rename (quote quasisyntax) (quote ()) (quote (&env~164~731)) 0 (quote (core quasisyntax))))) (let ((&temp~164~727 (cdr &input~164~544))) (if (pair? &temp~164~727) (let ((&temp~164~729 (car &temp~164~727))) (let ((&e~164~704 &temp~164~729)) (let ((&temp~164~728 (cdr &temp~164~727))) (if (null? &temp~164~728) (let ((&input~164~707 (list &x~164~542 (&expand~164~539 &e~164~704 (+ &level~164~541 1))))) (let ((&fail~164~709 (lambda () (ex:invalid-form &input~164~707)))) (if (pair? &input~164~707) (let ((&temp~164~722 (car &input~164~707))) (if (pair? &temp~164~722) (let ((&temp~164~726 (car &temp~164~722))) (let ((&k~164~712 &temp~164~726)) (let ((&temp~164~723 (cdr &temp~164~722))) (if (pair? &temp~164~723) (let ((&temp~164~725 (car &temp~164~723))) (let ((&temp~164~724 (cdr &temp~164~723))) (if (null? &temp~164~724) (let ((&temp~164~715 (cdr &input~164~707))) (if (pair? &temp~164~715) (let ((&temp~164~717 (car &temp~164~715))) (if (pair? &temp~164~717) (let ((&temp~164~721 (car &temp~164~717))) (let ((&e*~164~711 &temp~164~721)) (let ((&temp~164~718 (cdr &temp~164~717))) (if (pair? &temp~164~718) (let ((&temp~164~720 (car &temp~164~718))) (let ((&reps~164~710 &temp~164~720)) (let ((&temp~164~719 (cdr &temp~164~718))) (if (null? &temp~164~719) (let ((&temp~164~716 (cdr &temp~164~715))) (if (null? &temp~164~716) (begin (cons (cons &k~164~712 (cons &e*~164~711 (quote ()))) (cons &reps~164~710 (quote ())))) (&fail~164~709))) (&fail~164~709))))) (&fail~164~709))))) (&fail~164~709))) (&fail~164~709))) (&fail~164~709)))) (&fail~164~709))))) (&fail~164~709))) (&fail~164~709)))) (&fail~164~545))))) (&fail~164~545))) (&fail~164~545))) (&fail~164~545)))))) (let ((&input~164~733 &e~164~538)) (let ((&fail~164~734 (lambda () (ex:invalid-form &input~164~733)))) (if (pair? &input~164~733) (let ((&temp~164~753 (car &input~164~733))) (let ((&temp~164~750 (cdr &input~164~733))) (if (pair? &temp~164~750) (let ((&temp~164~752 (car &temp~164~750))) (let ((&template~164~735 &temp~164~752)) (let ((&temp~164~751 (cdr &temp~164~750))) (if (null? &temp~164~751) (let ((&input~164~738 (&expand~164~539 &template~164~735 0))) (let ((&fail~164~740 (lambda () (ex:invalid-form &input~164~738)))) (if (pair? &input~164~738) (let ((&temp~164~749 (car &input~164~738))) (let ((&template*~164~742 &temp~164~749)) (let ((&temp~164~746 (cdr &input~164~738))) (if (pair? &temp~164~746) (let ((&temp~164~748 (car &temp~164~746))) (let ((&replacements~164~741 &temp~164~748)) (let ((&temp~164~747 (cdr &temp~164~746))) (if (null? &temp~164~747) (begin (cons (ex:syntax-rename (quote with-syntax) (quote ()) (quote (&env~164~745)) 0 (quote (core quasisyntax))) (cons &replacements~164~741 (cons (cons (ex:syntax-rename (quote syntax) (quote ()) (quote (&env~164~745)) 0 (quote (core quasisyntax))) (cons &template*~164~742 (quote ()))) (quote ()))))) (&fail~164~740))))) (&fail~164~740))))) (&fail~164~740)))) (&fail~164~734))))) (&fail~164~734)))) (&fail~164~734))))) ex:undefined))) (ex:register-macro! (quote &unsyntax~164~754) (lambda (&e~164~756) (ex:syntax-violation (quote unsyntax) "Invalid expression" &e~164~756))) (ex:register-macro! (quote &unsyntax-splicing~164~758) (lambda (&e~164~760) (ex:syntax-violation (quote unsyntax) "Invalid expression" &e~164~760))) (values)) (lambda () (values)) (quote &build~164~762))) (values))
(begin (ex:register-library! (ex:make-library (quote (core quasiquote)) (lambda () (ex:uncompress (quote (((&env~164~1181 0 1 2 3 4 5) (&env~164~1176 6 7 1 2 3 4 5) (&env~164~1148 8 9 10 11 12 13 1 2 3 4 5) (&env~164~1121 14 15 1 2 3 4 5) (&env~164~1107 16 17 1 2 3 4 5) (&env~164~1091 18 19 1 2 3 4 5) (&env~164~871 20 21 22 23 3 4 5) (&env~164~861 24 21 22 23 3 4 5) (&env~164~832 25 26 27 3 4 5) (&env~164~823 28 26 27 3 4 5) (&env~164~813 29 30 26 27 3 4 5) (&env~164~803 31 30 26 27 3 4 5)) (31 (((p) . #f))) (30 (((p) . #f) ((q) . #f))) (29 (((p) . #f))) (28 (((p) . #f))) (27 (((p) variable &p~164~775 (1) #f (core quasiquote)) ((lev) variable &lev~164~774 (1) #f (core quasiquote)))) (26 ()) (25 (((p) . #f))) (24 (((p) . #f))) (23 (((p) variable &p~164~840 (1) #f (core quasiquote)) ((lev) variable &lev~164~839 (1) #f (core quasiquote)))) (22 ()) (21 (((p) . #f) ((q) . #f))) (20 (((p) . #f))) (19 (((x) . #f))) (18 (((temp &c~164~1082) . #f))) (17 (((x) . #f))) (16 (((temp &c~164~1098) . #f))) (15 (((x) . #f))) (14 (((temp &c~164~1112) . #f))) (13 (((x) . #f) ((y) . #f))) (12 (((f) variable &f~164~1132 (1) #t (core quasiquote)))) (11 ()) (10 (((x*) variable &x*~164~1135 (1) #f (core quasiquote)))) (9 ()) (8 (((temp &c~164~1139) . #f) ((temp &c~164~1138) . #f))) (7 (((x) . #f))) (6 (((temp &c~164~1167) . #f))) (5 (((unquote-splicing) macro &unquote-splicing~164~1201 (0) #f (core quasiquote)) ((unquote) macro &unquote~164~1197 (0) #f (core quasiquote)) ((quasiquote) macro &quasiquote~164~763 (0) #f (core quasiquote)) ((undefined) variable ex:undefined (0 1) #f ()) ((eval) variable ex:eval (0 1) #f ()) ((environment-bindings) variable ex:environment-bindings (0 1) #f ()) ((environment) variable ex:environment (0 1) #f ()) ((syntax-violation) variable ex:syntax-violation (0 1) #f ()) ((syntax->datum) variable ex:syntax->datum (0 1) #f ()) ((datum->syntax) variable ex:datum->syntax (0 1) #f ()) ((generate-temporaries) variable ex:generate-temporaries (0 1) #f ()) ((free-identifier=?) variable ex:free-identifier=? (0 1) #f ()) ((bound-identifier=?) variable ex:bound-identifier=? (0 1) #f ()) ((identifier?) variable ex:identifier? (0 1) #f ()) ((make-variable-transformer) variable ex:make-variable-transformer (0 1) #f ()) ((syntax-case) macro syntax-case (0 1) #f ()) ((syntax) macro syntax (0 1) #f ()) ((...) macro ... (0 1) #f ()) ((_) macro _ (0 1) #f ()) ((letrec-syntax) macro letrec-syntax (0 1) #f ()) ((let-syntax) macro let-syntax (0 1) #f ()) ((define-syntax) macro define-syntax (0 1) #f ()) ((define) macro define (0 1) #f ()) ((or) macro or (0 1) #f ()) ((and) macro and (0 1) #f ()) ((set!) macro set! (0 1) #f ()) ((quote) macro quote (0 1) #f ()) ((lambda) macro lambda (0 1) #f ()) ((if) macro if (0 1) #f ()) ((begin) macro begin (0 1) #f ()) ((letrec*) macro &letrec*~164~179 (0 1) #f (core let)) ((letrec) macro &letrec~164~148 (0 1) #f (core let)) ((let) macro &let~164~100 (0 1) #f (core let)) ((=>) macro &=>~164~481 (0 1) #f (core derived)) ((else) macro &else~164~485 (0 1) #f (core derived)) ((case) macro &case~164~401 (0 1) #f (core derived)) ((cond) macro &cond~164~285 (0 1) #f (core derived)) ((let*) macro &let*~164~204 (0 1) #f (core derived)) ((with-syntax) macro &with-syntax~164~3 (1) #f (core with-syntax)) ((unsyntax-splicing) macro &unsyntax-splicing~164~758 (1) #f (core quasisyntax)) ((unsyntax) macro &unsyntax~164~754 (1) #f (core quasisyntax)) ((quasisyntax) macro &quasisyntax~164~536 (1) #f (core quasisyntax)) ((list->vector) variable list->vector (0 1) #f ()) ((vector) variable vector (0 1) #f ()) ((list) variable list (0 1) #f ()) ((map) variable map (0 1) #f ()) ((append) variable append (0 1) #f ()) ((cdr) variable cdr (0 1) #f ()) ((car) variable car (0 1) #f ()) ((cons) variable cons (0 1) #f ()) ((null?) variable null? (0 1) #f ()) ((-) variable - (0 1) #f ()) ((+) variable + (0 1) #f ()) ((=) variable = (0 1) #f ()))) (4 ()) (3 (((emit) variable &emit~164~772 (1) #f (core quasiquote)) ((quasivector) variable &quasivector~164~771 (1) #f (core quasiquote)) ((quasilist*) variable &quasilist*~164~770 (1) #f (core quasiquote)) ((quasiappend) variable &quasiappend~164~769 (1) #f (core quasiquote)) ((quasicons) variable &quasicons~164~768 (1) #f (core quasiquote)) ((vquasi) variable &vquasi~164~767 (1) #f (core quasiquote)) ((quasi) variable &quasi~164~766 (1) #f (core quasiquote)))) (2 (((x) variable &x~164~1064 (1) #f (core quasiquote)))) (1 ()) (0 (((x) . #f))))))) (quote ((quasiquote macro &quasiquote~164~763 (0) #f (core quasiquote)) (unquote macro &unquote~164~1197 (0) #f (core quasiquote)) (unquote-splicing macro &unquote-splicing~164~1201 (0) #f (core quasiquote)))) (quote (((core quasisyntax) 1) ((core with-syntax) 1) ((core derived) 0 1) ((core let) 0 1) ((core primitives) 0 1))) (quote (&build~164~762 &build~164~57 &build~164~489 &build~164~203 &build~164~2)) (lambda () (ex:register-macro! (quote &quasiquote~164~763) ((lambda () ((lambda (&emit~164~772 &quasivector~164~771 &quasilist*~164~770 &quasiappend~164~769 &quasicons~164~768 &vquasi~164~767 &quasi~164~766) (set! &quasi~164~766 (lambda (&p~164~775 &lev~164~774) (let ((&input~164~777 &p~164~775)) (let ((&fail~164~778 (lambda () (let ((&fail~164~779 (lambda () (let ((&fail~164~780 (lambda () (let ((&fail~164~781 (lambda () (let ((&fail~164~782 (lambda () (ex:invalid-form &input~164~777)))) (let ((&p~164~783 &input~164~777)) (cons (quote "quote") (cons &p~164~783 (quote ())))))))) (if (vector? &input~164~777) (let ((&temp~164~787 (vector->list &input~164~777))) (if (list? &temp~164~787) (let ((&x~164~785 &temp~164~787)) (&quasivector~164~771 (&vquasi~164~767 &x~164~785 &lev~164~774))) (&fail~164~781))) (&fail~164~781)))))) (if (pair? &input~164~777) (let ((&temp~164~819 (car &input~164~777))) (let ((&p~164~789 &temp~164~819)) (let ((&temp~164~818 (cdr &input~164~777))) (let ((&q~164~788 &temp~164~818)) (let ((&input~164~791 &p~164~789)) (let ((&fail~164~793 (lambda () (let ((&fail~164~794 (lambda () (let ((&fail~164~795 (lambda () (ex:invalid-form &input~164~791)))) (&quasicons~164~768 (&quasi~164~766 &p~164~789 &lev~164~774) (&quasi~164~766 &q~164~788 &lev~164~774)))))) (if (pair? &input~164~791) (let ((&temp~164~807 (car &input~164~791))) (if (and (ex:identifier? &temp~164~807) (ex:free-identifier=? &temp~164~807 (ex:syntax-rename (quote unquote-splicing) (quote ()) (quote (&env~164~803)) 0 (quote (core quasiquote))))) (let ((&temp~164~806 (cdr &input~164~791))) (if (list? &temp~164~806) (let ((&p~164~798 &temp~164~806)) (if (= &lev~164~774 0) (&quasiappend~164~769 (map (lambda (&p~164~798) (cons (quote "value") (cons &p~164~798 (quote ())))) &p~164~798) (&quasi~164~766 &q~164~788 &lev~164~774)) (&quasicons~164~768 (&quasicons~164~768 (cons (quote "quote") (cons (ex:syntax-rename (quote unquote-splicing) (quote ()) (quote (&env~164~803)) 0 (quote (core quasiquote))) (quote ()))) (&quasi~164~766 &p~164~798 (- &lev~164~774 1))) (&quasi~164~766 &q~164~788 &lev~164~774)))) (&fail~164~794))) (&fail~164~794))) (&fail~164~794)))))) (if (pair? &input~164~791) (let ((&temp~164~817 (car &input~164~791))) (if (and (ex:identifier? &temp~164~817) (ex:free-identifier=? &temp~164~817 (ex:syntax-rename (quote unquote) (quote ()) (quote (&env~164~813)) 0 (quote (core quasiquote))))) (let ((&temp~164~816 (cdr &input~164~791))) (if (list? &temp~164~816) (let ((&p~164~808 &temp~164~816)) (if (= &lev~164~774 0) (&quasilist*~164~770 (map (lambda (&p~164~808) (cons (quote "value") (cons &p~164~808 (quote ())))) &p~164~808) (&quasi~164~766 &q~164~788 &lev~164~774)) (&quasicons~164~768 (&quasicons~164~768 (cons (quote "quote") (cons (ex:syntax-rename (quote unquote) (quote ()) (quote (&env~164~813)) 0 (quote (core quasiquote))) (quote ()))) (&quasi~164~766 &p~164~808 (- &lev~164~774 1))) (&quasi~164~766 &q~164~788 &lev~164~774)))) (&fail~164~793))) (&fail~164~793))) (&fail~164~793)))))))) (&fail~164~780)))))) (if (pair? &input~164~777) (let ((&temp~164~827 (car &input~164~777))) (if (and (ex:identifier? &temp~164~827) (ex:free-identifier=? &temp~164~827 (ex:syntax-rename (quote quasiquote) (quote ()) (quote (&env~164~823)) 0 (quote (core quasiquote))))) (let ((&temp~164~824 (cdr &input~164~777))) (if (pair? &temp~164~824) (let ((&temp~164~826 (car &temp~164~824))) (let ((&p~164~820 &temp~164~826)) (let ((&temp~164~825 (cdr &temp~164~824))) (if (null? &temp~164~825) (&quasicons~164~768 (cons (quote "quote") (cons (ex:syntax-rename (quote quasiquote) (quote ()) (quote (&env~164~823)) 0 (quote (core quasiquote))) (quote ()))) (&quasi~164~766 (cons &p~164~820 (quote ())) (+ &lev~164~774 1))) (&fail~164~779))))) (&fail~164~779))) (&fail~164~779))) (&fail~164~779)))))) (if (pair? &input~164~777) (let ((&temp~164~837 (car &input~164~777))) (if (and (ex:identifier? &temp~164~837) (ex:free-identifier=? &temp~164~837 (ex:syntax-rename (quote unquote) (quote ()) (quote (&env~164~832)) 0 (quote (core quasiquote))))) (let ((&temp~164~834 (cdr &input~164~777))) (if (pair? &temp~164~834) (let ((&temp~164~836 (car &temp~164~834))) (let ((&p~164~828 &temp~164~836)) (let ((&temp~164~835 (cdr &temp~164~834))) (if (null? &temp~164~835) (if (= &lev~164~774 0) (cons (quote "value") (cons &p~164~828 (quote ()))) (&quasicons~164~768 (cons (quote "quote") (cons (ex:syntax-rename (quote unquote) (quote ()) (quote (&env~164~832)) 0 (quote (core quasiquote))) (quote ()))) (&quasi~164~766 (cons &p~164~828 (quote ())) (- &lev~164~774 1)))) (&fail~164~778))))) (&fail~164~778))) (&fail~164~778))) (&fail~164~778)))))) (set! &vquasi~164~767 (lambda (&p~164~840 &lev~164~839) (let ((&input~164~842 &p~164~840)) (let ((&fail~164~843 (lambda () (let ((&fail~164~844 (lambda () (ex:invalid-form &input~164~842)))) (if (null? &input~164~842) (cons (quote "quote") (cons (quote ()) (quote ()))) (&fail~164~844)))))) (if (pair? &input~164~842) (let ((&temp~164~877 (car &input~164~842))) (let ((&p~164~847 &temp~164~877)) (let ((&temp~164~876 (cdr &input~164~842))) (let ((&q~164~846 &temp~164~876)) (let ((&input~164~849 &p~164~847)) (let ((&fail~164~851 (lambda () (let ((&fail~164~852 (lambda () (let ((&fail~164~853 (lambda () (ex:invalid-form &input~164~849)))) (&quasicons~164~768 (&quasi~164~766 &p~164~847 &lev~164~839) (&vquasi~164~767 &q~164~846 &lev~164~839)))))) (if (pair? &input~164~849) (let ((&temp~164~865 (car &input~164~849))) (if (and (ex:identifier? &temp~164~865) (ex:free-identifier=? &temp~164~865 (ex:syntax-rename (quote unquote-splicing) (quote ()) (quote (&env~164~861)) 0 (quote (core quasiquote))))) (let ((&temp~164~864 (cdr &input~164~849))) (if (list? &temp~164~864) (let ((&p~164~856 &temp~164~864)) (if (= &lev~164~839 0) (&quasiappend~164~769 (map (lambda (&p~164~856) (cons (quote "value") (cons &p~164~856 (quote ())))) &p~164~856) (&vquasi~164~767 &q~164~846 &lev~164~839)) (&quasicons~164~768 (&quasicons~164~768 (cons (quote "quote") (cons (ex:syntax-rename (quote unquote-splicing) (quote ()) (quote (&env~164~861)) 0 (quote (core quasiquote))) (quote ()))) (&quasi~164~766 &p~164~856 (- &lev~164~839 1))) (&vquasi~164~767 &q~164~846 &lev~164~839)))) (&fail~164~852))) (&fail~164~852))) (&fail~164~852)))))) (if (pair? &input~164~849) (let ((&temp~164~875 (car &input~164~849))) (if (and (ex:identifier? &temp~164~875) (ex:free-identifier=? &temp~164~875 (ex:syntax-rename (quote unquote) (quote ()) (quote (&env~164~871)) 0 (quote (core quasiquote))))) (let ((&temp~164~874 (cdr &input~164~849))) (if (list? &temp~164~874) (let ((&p~164~866 &temp~164~874)) (if (= &lev~164~839 0) (&quasilist*~164~770 (map (lambda (&p~164~866) (cons (quote "value") (cons &p~164~866 (quote ())))) &p~164~866) (&vquasi~164~767 &q~164~846 &lev~164~839)) (&quasicons~164~768 (&quasicons~164~768 (cons (quote "quote") (cons (ex:syntax-rename (quote unquote) (quote ()) (quote (&env~164~871)) 0 (quote (core quasiquote))) (quote ()))) (&quasi~164~766 &p~164~866 (- &lev~164~839 1))) (&vquasi~164~767 &q~164~846 &lev~164~839)))) (&fail~164~851))) (&fail~164~851))) (&fail~164~851)))))))) (&fail~164~843)))))) (set! &quasicons~164~768 (lambda (&x~164~880 &y~164~879) (let ((&input~164~883 (list &x~164~880 &y~164~879))) (let ((&fail~164~884 (lambda () (ex:invalid-form &input~164~883)))) (if (pair? &input~164~883) (let ((&temp~164~927 (car &input~164~883))) (let ((&x~164~886 &temp~164~927)) (let ((&temp~164~924 (cdr &input~164~883))) (if (pair? &temp~164~924) (let ((&temp~164~926 (car &temp~164~924))) (let ((&y~164~885 &temp~164~926)) (let ((&temp~164~925 (cdr &temp~164~924))) (if (null? &temp~164~925) (begin (let ((&input~164~889 &y~164~885)) (let ((&fail~164~891 (lambda () (let ((&fail~164~892 (lambda () (let ((&fail~164~893 (lambda () (let ((&fail~164~894 (lambda () (ex:invalid-form &input~164~889)))) (cons (quote "list*") (cons &x~164~886 (cons &y~164~885 (quote ())))))))) (if (pair? &input~164~889) (let ((&temp~164~899 (car &input~164~889))) (if (equal? &temp~164~899 (quote "list*")) (let ((&temp~164~898 (cdr &input~164~889))) (let ((&stuff~164~896 &temp~164~898)) (cons (quote "list*") (cons &x~164~886 &stuff~164~896)))) (&fail~164~893))) (&fail~164~893)))))) (if (pair? &input~164~889) (let ((&temp~164~903 (car &input~164~889))) (if (equal? &temp~164~903 (quote "list")) (let ((&temp~164~902 (cdr &input~164~889))) (let ((&stuff~164~900 &temp~164~902)) (cons (quote "list") (cons &x~164~886 &stuff~164~900)))) (&fail~164~892))) (&fail~164~892)))))) (if (pair? &input~164~889) (let ((&temp~164~923 (car &input~164~889))) (if (equal? &temp~164~923 (quote "quote")) (let ((&temp~164~920 (cdr &input~164~889))) (if (pair? &temp~164~920) (let ((&temp~164~922 (car &temp~164~920))) (let ((&dy~164~904 &temp~164~922)) (let ((&temp~164~921 (cdr &temp~164~920))) (if (null? &temp~164~921) (let ((&input~164~906 &x~164~886)) (let ((&fail~164~908 (lambda () (let ((&fail~164~909 (lambda () (ex:invalid-form &input~164~906)))) (if (null? &dy~164~904) (cons (quote "list") (cons &x~164~886 (quote ()))) (cons (quote "list*") (cons &x~164~886 (cons &y~164~885 (quote ()))))))))) (if (pair? &input~164~906) (let ((&temp~164~919 (car &input~164~906))) (if (equal? &temp~164~919 (quote "quote")) (let ((&temp~164~916 (cdr &input~164~906))) (if (pair? &temp~164~916) (let ((&temp~164~918 (car &temp~164~916))) (let ((&dx~164~914 &temp~164~918)) (let ((&temp~164~917 (cdr &temp~164~916))) (if (null? &temp~164~917) (cons (quote "quote") (cons (cons &dx~164~914 &dy~164~904) (quote ()))) (&fail~164~908))))) (&fail~164~908))) (&fail~164~908))) (&fail~164~908)))) (&fail~164~891))))) (&fail~164~891))) (&fail~164~891))) (&fail~164~891))))) (&fail~164~884))))) (&fail~164~884))))) (&fail~164~884)))))) (set! &quasiappend~164~769 (lambda (&x~164~930 &y~164~929) (let ((&input~164~932 &y~164~929)) (let ((&fail~164~933 (lambda () (let ((&fail~164~934 (lambda () (ex:invalid-form &input~164~932)))) (if (null? &x~164~930) (begin &y~164~929) (begin (let ((&input~164~940 (list &x~164~930 &y~164~929))) (let ((&fail~164~941 (lambda () (ex:invalid-form &input~164~940)))) (if (pair? &input~164~940) (let ((&temp~164~949 (car &input~164~940))) (if (list? &temp~164~949) (let ((&p~164~943 &temp~164~949)) (let ((&temp~164~946 (cdr &input~164~940))) (if (pair? &temp~164~946) (let ((&temp~164~948 (car &temp~164~946))) (let ((&y~164~942 &temp~164~948)) (let ((&temp~164~947 (cdr &temp~164~946))) (if (null? &temp~164~947) (begin (cons (quote "append") (append &p~164~943 (cons &y~164~942 (quote ()))))) (&fail~164~941))))) (&fail~164~941)))) (&fail~164~941))) (&fail~164~941)))))))))) (if (pair? &input~164~932) (let ((&temp~164~968 (car &input~164~932))) (if (equal? &temp~164~968 (quote "quote")) (let ((&temp~164~965 (cdr &input~164~932))) (if (pair? &temp~164~965) (let ((&temp~164~967 (car &temp~164~965))) (if (null? &temp~164~967) (let ((&temp~164~966 (cdr &temp~164~965))) (if (null? &temp~164~966) (if (null? &x~164~930) (begin (cons (quote "quote") (cons (quote ()) (quote ())))) (if (null? (cdr &x~164~930)) (begin (car &x~164~930)) (begin (let ((&input~164~957 &x~164~930)) (let ((&fail~164~958 (lambda () (ex:invalid-form &input~164~957)))) (if (list? &input~164~957) (let ((&p~164~959 &input~164~957)) (begin (cons (quote "append") &p~164~959))) (&fail~164~958))))))) (&fail~164~933))) (&fail~164~933))) (&fail~164~933))) (&fail~164~933))) (&fail~164~933)))))) (set! &quasilist*~164~770 (lambda (&x~164~971 &y~164~970) (((lambda (&f~164~977) ((lambda (&temp~164~983) (set! &f~164~977 &temp~164~983) ((lambda () &f~164~977))) (lambda (&x~164~980) (if (null? &x~164~980) &y~164~970 (&quasicons~164~768 (car &x~164~980) (&f~164~977 (cdr &x~164~980))))))) ex:undefined) &x~164~971))) (set! &quasivector~164~771 (lambda (&x~164~988) (let ((&input~164~990 &x~164~988)) (let ((&fail~164~991 (lambda () (let ((&fail~164~992 (lambda () (ex:invalid-form &input~164~990)))) (((lambda (&f~164~1009) ((lambda (&temp~164~1053) (set! &f~164~1009 &temp~164~1053) ((lambda () &f~164~1009))) (lambda (&y~164~1013 &k~164~1012) (let ((&input~164~1015 &y~164~1013)) (let ((&fail~164~1016 (lambda () (let ((&fail~164~1017 (lambda () (let ((&fail~164~1018 (lambda () (let ((&fail~164~1019 (lambda () (ex:invalid-form &input~164~1015)))) (let ((&else~164~1020 &input~164~1015)) (let ((&input~164~1025 &x~164~988)) (let ((&fail~164~1026 (lambda () (ex:invalid-form &input~164~1025)))) (let ((&temp~164~1027 &input~164~1025)) (begin (cons (quote "list->vector") (cons &temp~164~1027 (quote ())))))))))))) (if (pair? &input~164~1015) (let ((&temp~164~1041 (car &input~164~1015))) (if (equal? &temp~164~1041 (quote "list*")) (let ((&temp~164~1036 (cdr &input~164~1015))) (if (>= (ex:dotted-length &temp~164~1036) 1) (let ((&temp~164~1040 (ex:dotted-butlast &temp~164~1036 1))) (if (list? &temp~164~1040) (let ((&y~164~1031 &temp~164~1040)) (let ((&temp~164~1037 (ex:dotted-last &temp~164~1036 1))) (if (pair? &temp~164~1037) (let ((&temp~164~1039 (car &temp~164~1037))) (let ((&z~164~1030 &temp~164~1039)) (let ((&temp~164~1038 (cdr &temp~164~1037))) (if (null? &temp~164~1038) (&f~164~1009 &z~164~1030 (lambda (&ls~164~1033) (&k~164~1012 (append &y~164~1031 &ls~164~1033)))) (&fail~164~1018))))) (&fail~164~1018)))) (&fail~164~1018))) (&fail~164~1018))) (&fail~164~1018))) (&fail~164~1018)))))) (if (pair? &input~164~1015) (let ((&temp~164~1045 (car &input~164~1015))) (if (equal? &temp~164~1045 (quote "list")) (let ((&temp~164~1044 (cdr &input~164~1015))) (if (list? &temp~164~1044) (let ((&y~164~1042 &temp~164~1044)) (&k~164~1012 &y~164~1042)) (&fail~164~1017))) (&fail~164~1017))) (&fail~164~1017)))))) (if (pair? &input~164~1015) (let ((&temp~164~1051 (car &input~164~1015))) (if (equal? &temp~164~1051 (quote "quote")) (let ((&temp~164~1048 (cdr &input~164~1015))) (if (pair? &temp~164~1048) (let ((&temp~164~1050 (car &temp~164~1048))) (if (list? &temp~164~1050) (let ((&y~164~1046 &temp~164~1050)) (let ((&temp~164~1049 (cdr &temp~164~1048))) (if (null? &temp~164~1049) (&k~164~1012 (map (lambda (&y~164~1046) (cons (quote "quote") (cons &y~164~1046 (quote ())))) &y~164~1046)) (&fail~164~1016)))) (&fail~164~1016))) (&fail~164~1016))) (&fail~164~1016))) (&fail~164~1016))))))) ex:undefined) &x~164~988 (lambda (&ls~164~995) (let ((&input~164~1000 &ls~164~995)) (let ((&fail~164~1001 (lambda () (ex:invalid-form &input~164~1000)))) (if (list? &input~164~1000) (let ((&temp~164~1002 &input~164~1000)) (begin (cons (quote "vector") &temp~164~1002))) (&fail~164~1001)))))))))) (if (pair? &input~164~990) (let ((&temp~164~1062 (car &input~164~990))) (if (equal? &temp~164~1062 (quote "quote")) (let ((&temp~164~1059 (cdr &input~164~990))) (if (pair? &temp~164~1059) (let ((&temp~164~1061 (car &temp~164~1059))) (if (list? &temp~164~1061) (let ((&x~164~1057 &temp~164~1061)) (let ((&temp~164~1060 (cdr &temp~164~1059))) (if (null? &temp~164~1060) (cons (quote "quote") (cons (list->vector &x~164~1057) (quote ()))) (&fail~164~991)))) (&fail~164~991))) (&fail~164~991))) (&fail~164~991))) (&fail~164~991)))))) (set! &emit~164~772 (lambda (&x~164~1064) (let ((&input~164~1066 &x~164~1064)) (let ((&fail~164~1067 (lambda () (let ((&fail~164~1068 (lambda () (let ((&fail~164~1069 (lambda () (let ((&fail~164~1070 (lambda () (let ((&fail~164~1071 (lambda () (let ((&fail~164~1072 (lambda () (let ((&fail~164~1073 (lambda () (ex:invalid-form &input~164~1066)))) (if (pair? &input~164~1066) (let ((&temp~164~1079 (car &input~164~1066))) (if (equal? &temp~164~1079 (quote "value")) (let ((&temp~164~1076 (cdr &input~164~1066))) (if (pair? &temp~164~1076) (let ((&temp~164~1078 (car &temp~164~1076))) (let ((&x~164~1074 &temp~164~1078)) (let ((&temp~164~1077 (cdr &temp~164~1076))) (if (null? &temp~164~1077) &x~164~1074 (&fail~164~1073))))) (&fail~164~1073))) (&fail~164~1073))) (&fail~164~1073)))))) (if (pair? &input~164~1066) (let ((&temp~164~1095 (car &input~164~1066))) (if (equal? &temp~164~1095 (quote "list->vector")) (let ((&temp~164~1092 (cdr &input~164~1066))) (if (pair? &temp~164~1092) (let ((&temp~164~1094 (car &temp~164~1092))) (let ((&x~164~1080 &temp~164~1094)) (let ((&temp~164~1093 (cdr &temp~164~1092))) (if (null? &temp~164~1093) (let ((&input~164~1085 (&emit~164~772 &x~164~1080))) (let ((&fail~164~1087 (lambda () (ex:invalid-form &input~164~1085)))) (let ((&temp~164~1088 &input~164~1085)) (begin (cons (ex:syntax-rename (quote list->vector) (quote ()) (quote (&env~164~1091)) 0 (quote (core quasiquote))) (cons &temp~164~1088 (quote ()))))))) (&fail~164~1072))))) (&fail~164~1072))) (&fail~164~1072))) (&fail~164~1072)))))) (if (pair? &input~164~1066) (let ((&temp~164~1109 (car &input~164~1066))) (if (equal? &temp~164~1109 (quote "vector")) (let ((&temp~164~1108 (cdr &input~164~1066))) (if (list? &temp~164~1108) (let ((&x~164~1096 &temp~164~1108)) (let ((&input~164~1101 (map &emit~164~772 &x~164~1096))) (let ((&fail~164~1103 (lambda () (ex:invalid-form &input~164~1101)))) (if (list? &input~164~1101) (let ((&temp~164~1104 &input~164~1101)) (begin (cons (ex:syntax-rename (quote vector) (quote ()) (quote (&env~164~1107)) 0 (quote (core quasiquote))) &temp~164~1104))) (&fail~164~1103))))) (&fail~164~1071))) (&fail~164~1071))) (&fail~164~1071)))))) (if (pair? &input~164~1066) (let ((&temp~164~1123 (car &input~164~1066))) (if (equal? &temp~164~1123 (quote "append")) (let ((&temp~164~1122 (cdr &input~164~1066))) (if (list? &temp~164~1122) (let ((&x~164~1110 &temp~164~1122)) (let ((&input~164~1115 (map &emit~164~772 &x~164~1110))) (let ((&fail~164~1117 (lambda () (ex:invalid-form &input~164~1115)))) (if (list? &input~164~1115) (let ((&temp~164~1118 &input~164~1115)) (begin (cons (ex:syntax-rename (quote append) (quote ()) (quote (&env~164~1121)) 0 (quote (core quasiquote))) &temp~164~1118))) (&fail~164~1117))))) (&fail~164~1070))) (&fail~164~1070))) (&fail~164~1070)))))) (if (pair? &input~164~1066) (let ((&temp~164~1164 (car &input~164~1066))) (if (equal? &temp~164~1164 (quote "list*")) (let ((&temp~164~1159 (cdr &input~164~1066))) (if (>= (ex:dotted-length &temp~164~1159) 1) (let ((&temp~164~1163 (ex:dotted-butlast &temp~164~1159 1))) (if (list? &temp~164~1163) (let ((&x~164~1125 &temp~164~1163)) (let ((&temp~164~1160 (ex:dotted-last &temp~164~1159 1))) (if (pair? &temp~164~1160) (let ((&temp~164~1162 (car &temp~164~1160))) (let ((&y~164~1124 &temp~164~1162)) (let ((&temp~164~1161 (cdr &temp~164~1160))) (if (null? &temp~164~1161) (((lambda (&f~164~1132) ((lambda (&temp~164~1155) (set! &f~164~1132 &temp~164~1155) ((lambda () &f~164~1132))) (lambda (&x*~164~1135) (if (null? &x*~164~1135) (&emit~164~772 &y~164~1124) (let ((&input~164~1142 (list (&emit~164~772 (car &x*~164~1135)) (&f~164~1132 (cdr &x*~164~1135))))) (let ((&fail~164~1143 (lambda () (ex:invalid-form &input~164~1142)))) (if (pair? &input~164~1142) (let ((&temp~164~1152 (car &input~164~1142))) (let ((&temp~164~1145 &temp~164~1152)) (let ((&temp~164~1149 (cdr &input~164~1142))) (if (pair? &temp~164~1149) (let ((&temp~164~1151 (car &temp~164~1149))) (let ((&temp~164~1144 &temp~164~1151)) (let ((&temp~164~1150 (cdr &temp~164~1149))) (if (null? &temp~164~1150) (begin (cons (ex:syntax-rename (quote cons) (quote ()) (quote (&env~164~1148)) 0 (quote (core quasiquote))) (cons &temp~164~1145 (cons &temp~164~1144 (quote ()))))) (&fail~164~1143))))) (&fail~164~1143))))) (&fail~164~1143)))))))) ex:undefined) &x~164~1125) (&fail~164~1069))))) (&fail~164~1069)))) (&fail~164~1069))) (&fail~164~1069))) (&fail~164~1069))) (&fail~164~1069)))))) (if (pair? &input~164~1066) (let ((&temp~164~1178 (car &input~164~1066))) (if (equal? &temp~164~1178 (quote "list")) (let ((&temp~164~1177 (cdr &input~164~1066))) (if (list? &temp~164~1177) (let ((&x~164~1165 &temp~164~1177)) (let ((&input~164~1170 (map &emit~164~772 &x~164~1165))) (let ((&fail~164~1172 (lambda () (ex:invalid-form &input~164~1170)))) (if (list? &input~164~1170) (let ((&temp~164~1173 &input~164~1170)) (begin (cons (ex:syntax-rename (quote list) (quote ()) (quote (&env~164~1176)) 0 (quote (core quasiquote))) &temp~164~1173))) (&fail~164~1172))))) (&fail~164~1068))) (&fail~164~1068))) (&fail~164~1068)))))) (if (pair? &input~164~1066) (let ((&temp~164~1185 (car &input~164~1066))) (if (equal? &temp~164~1185 (quote "quote")) (let ((&temp~164~1182 (cdr &input~164~1066))) (if (pair? &temp~164~1182) (let ((&temp~164~1184 (car &temp~164~1182))) (let ((&x~164~1179 &temp~164~1184)) (let ((&temp~164~1183 (cdr &temp~164~1182))) (if (null? &temp~164~1183) (cons (ex:syntax-rename (quote quote) (quote ()) (quote (&env~164~1181)) 0 (quote (core quasiquote))) (cons &x~164~1179 (quote ()))) (&fail~164~1067))))) (&fail~164~1067))) (&fail~164~1067))) (&fail~164~1067)))))) (lambda (&x~164~1187) (let ((&input~164~1189 &x~164~1187)) (let ((&fail~164~1190 (lambda () (ex:invalid-form &input~164~1189)))) (if (pair? &input~164~1189) (let ((&temp~164~1196 (car &input~164~1189))) (let ((&temp~164~1193 (cdr &input~164~1189))) (if (pair? &temp~164~1193) (let ((&temp~164~1195 (car &temp~164~1193))) (let ((&e~164~1191 &temp~164~1195)) (let ((&temp~164~1194 (cdr &temp~164~1193))) (if (null? &temp~164~1194) (&emit~164~772 (&quasi~164~766 &e~164~1191 0)) (&fail~164~1190))))) (&fail~164~1190)))) (&fail~164~1190)))))) ex:undefined ex:undefined ex:undefined ex:undefined ex:undefined ex:undefined ex:undefined)))) (ex:register-macro! (quote &unquote~164~1197) (lambda (&e~164~1199) (ex:syntax-violation (quote unquote) "Invalid expression" &e~164~1199))) (ex:register-macro! (quote &unquote-splicing~164~1201) (lambda (&e~164~1203) (ex:syntax-violation (quote unquote-splicing) "Invalid expression" &e~164~1203))) (values)) (lambda () (values)) (quote &build~164~1205))) (values))
(begin (ex:register-library! (ex:make-library (quote (core let-values)) (lambda () (ex:uncompress (quote (((&env~164~1373 0 1 2 3) (&env~164~1360 4 1 2 3) (&env~164~1339 5 6 7 3) (&env~164~1323 8 6 7 3) (&env~164~1302 9 6 7 3) (&env~164~1278 10 6 7 3) (&env~164~1252 11 6 7 3) (&env~164~1226 12 6 7 3)) (12 (((dummy &c~164~1207) . #f) ((?a) . #f) ((?e0) . #f) ((?arg) . #f) ((?bindings) . #f) ((?tmp) . #f) ((?body) . #f))) (11 (((dummy &c~164~1207) . #f) ((?a) . #f) ((?b) . #f) ((?e0) . #f) ((?arg) . #f) ((?bindings) . #f) ((?tmp) . #f) ((?body) . #f))) (10 (((dummy &c~164~1207) . #f) ((?e0) . #f) ((?args) . #f) ((?bindings) . #f) ((?tmps) . #f) ((?body) . #f))) (9 (((dummy &c~164~1207) . #f) ((?b0) . #f) ((?e0) . #f) ((?binding) . #f) ((?tmps) . #f) ((?body) . #f))) (8 (((dummy &c~164~1207) . #f) ((?tmps) . #f) ((?body) . #f))) (7 (((x &c~164~1207) variable &x~164~1209 (0) #f (core let-values)))) (6 ()) (5 (((dummy &c~164~1207) . #f) ((?binding) . #f) ((?body0) . #f) ((?body1) . #f))) (4 (((dummy &c~164~1347) . #f) ((?binding0) . #f) ((?binding1) . #f) ((?body0) . #f) ((?body1) . #f))) (3 (((let*-values) macro &let*-values~164~1346 (0) #f (core let-values)) ((let-values) macro &let-values~164~1206 (0) #f (core let-values)) ((undefined) variable ex:undefined (1 0) #f ()) ((eval) variable ex:eval (1 0) #f ()) ((environment-bindings) variable ex:environment-bindings (1 0) #f ()) ((environment) variable ex:environment (1 0) #f ()) ((syntax-violation) variable ex:syntax-violation (1 0) #f ()) ((syntax->datum) variable ex:syntax->datum (1 0) #f ()) ((datum->syntax) variable ex:datum->syntax (1 0) #f ()) ((generate-temporaries) variable ex:generate-temporaries (1 0) #f ()) ((free-identifier=?) variable ex:free-identifier=? (1 0) #f ()) ((bound-identifier=?) variable ex:bound-identifier=? (1 0) #f ()) ((identifier?) variable ex:identifier? (1 0) #f ()) ((make-variable-transformer) variable ex:make-variable-transformer (1 0) #f ()) ((syntax-case) macro syntax-case (1 0) #f ()) ((syntax) macro syntax (1 0) #f ()) ((...) macro ... (1 0) #f ()) ((_) macro _ (1 0) #f ()) ((letrec-syntax) macro letrec-syntax (1 0) #f ()) ((let-syntax) macro let-syntax (1 0) #f ()) ((define-syntax) macro define-syntax (1 0) #f ()) ((define) macro define (1 0) #f ()) ((or) macro or (1 0) #f ()) ((and) macro and (1 0) #f ()) ((set!) macro set! (1 0) #f ()) ((quote) macro quote (1 0) #f ()) ((lambda) macro lambda (1 0) #f ()) ((if) macro if (1 0) #f ()) ((begin) macro begin (1 0) #f ()) ((syntax-rules) macro &syntax-rules~164~58 (1) #f (core syntax-rules)) ((letrec*) macro &letrec*~164~179 (0) #f (core let)) ((letrec) macro &letrec~164~148 (0) #f (core let)) ((let) macro &let~164~100 (0) #f (core let)) ((call-with-values) variable call-with-values (0) #f ()))) (2 (((x &c~164~1347) variable &x~164~1349 (0) #f (core let-values)))) (1 ()) (0 (((dummy &c~164~1347) . #f) ((?body0) . #f) ((?body1) . #f))))))) (quote ((let-values macro &let-values~164~1206 (0) #f (core let-values)) (let*-values macro &let*-values~164~1346 (0) #f (core let-values)))) (quote (((core let) 0) ((core syntax-rules) 1) ((core primitives) 1 0))) (quote (&build~164~203 &build~164~99 &build~164~2)) (lambda () (ex:register-macro! (quote &let-values~164~1206) (lambda (&x~164~1209) (let ((&input~164~1211 &x~164~1209)) (let ((&fail~164~1212 (lambda () (let ((&fail~164~1213 (lambda () (let ((&fail~164~1214 (lambda () (let ((&fail~164~1215 (lambda () (let ((&fail~164~1216 (lambda () (let ((&fail~164~1217 (lambda () (ex:invalid-form &input~164~1211)))) (if (pair? &input~164~1211) (let ((&temp~164~1242 (car &input~164~1211))) (let ((&dummy~164~1224 &temp~164~1242)) (let ((&temp~164~1227 (cdr &input~164~1211))) (if (pair? &temp~164~1227) (let ((&temp~164~1241 (car &temp~164~1227))) (if (equal? &temp~164~1241 (quote "mktmp")) (let ((&temp~164~1228 (cdr &temp~164~1227))) (if (pair? &temp~164~1228) (let ((&temp~164~1240 (car &temp~164~1228))) (let ((&?a~164~1223 &temp~164~1240)) (let ((&temp~164~1229 (cdr &temp~164~1228))) (if (pair? &temp~164~1229) (let ((&temp~164~1239 (car &temp~164~1229))) (let ((&?e0~164~1222 &temp~164~1239)) (let ((&temp~164~1230 (cdr &temp~164~1229))) (if (pair? &temp~164~1230) (let ((&temp~164~1238 (car &temp~164~1230))) (if (list? &temp~164~1238) (let ((&?arg~164~1221 &temp~164~1238)) (let ((&temp~164~1231 (cdr &temp~164~1230))) (if (pair? &temp~164~1231) (let ((&temp~164~1237 (car &temp~164~1231))) (let ((&?bindings~164~1220 &temp~164~1237)) (let ((&temp~164~1232 (cdr &temp~164~1231))) (if (pair? &temp~164~1232) (let ((&temp~164~1236 (car &temp~164~1232))) (if (list? &temp~164~1236) (let ((&?tmp~164~1219 &temp~164~1236)) (let ((&temp~164~1233 (cdr &temp~164~1232))) (if (pair? &temp~164~1233) (let ((&temp~164~1235 (car &temp~164~1233))) (let ((&?body~164~1218 &temp~164~1235)) (let ((&temp~164~1234 (cdr &temp~164~1233))) (if (null? &temp~164~1234) (cons (ex:syntax-rename (quote call-with-values) (quote ()) (quote (&env~164~1226)) 0 (quote (core let-values))) (cons (cons (ex:syntax-rename (quote lambda) (quote ()) (quote (&env~164~1226)) 0 (quote (core let-values))) (cons (quote ()) (cons &?e0~164~1222 (quote ())))) (cons (cons (ex:syntax-rename (quote lambda) (quote ()) (quote (&env~164~1226)) 0 (quote (core let-values))) (cons (append &?arg~164~1221 (ex:syntax-rename (quote x) (quote ()) (quote (&env~164~1226)) 0 (quote (core let-values)))) (cons (cons (ex:syntax-rename (quote let-values) (quote ()) (quote (&env~164~1226)) 0 (quote (core let-values))) (cons (quote "bind") (cons &?bindings~164~1220 (cons (append &?tmp~164~1219 (cons (cons &?a~164~1223 (cons (ex:syntax-rename (quote x) (quote ()) (quote (&env~164~1226)) 0 (quote (core let-values))) (quote ()))) (quote ()))) (cons &?body~164~1218 (quote ())))))) (quote ())))) (quote ())))) (&fail~164~1217))))) (&fail~164~1217)))) (&fail~164~1217))) (&fail~164~1217))))) (&fail~164~1217)))) (&fail~164~1217))) (&fail~164~1217))))) (&fail~164~1217))))) (&fail~164~1217))) (&fail~164~1217))) (&fail~164~1217))))) (&fail~164~1217)))))) (if (pair? &input~164~1211) (let ((&temp~164~1270 (car &input~164~1211))) (let ((&dummy~164~1250 &temp~164~1270)) (let ((&temp~164~1253 (cdr &input~164~1211))) (if (pair? &temp~164~1253) (let ((&temp~164~1269 (car &temp~164~1253))) (if (equal? &temp~164~1269 (quote "mktmp")) (let ((&temp~164~1254 (cdr &temp~164~1253))) (if (pair? &temp~164~1254) (let ((&temp~164~1266 (car &temp~164~1254))) (if (pair? &temp~164~1266) (let ((&temp~164~1268 (car &temp~164~1266))) (let ((&?a~164~1249 &temp~164~1268)) (let ((&temp~164~1267 (cdr &temp~164~1266))) (let ((&?b~164~1248 &temp~164~1267)) (let ((&temp~164~1255 (cdr &temp~164~1254))) (if (pair? &temp~164~1255) (let ((&temp~164~1265 (car &temp~164~1255))) (let ((&?e0~164~1247 &temp~164~1265)) (let ((&temp~164~1256 (cdr &temp~164~1255))) (if (pair? &temp~164~1256) (let ((&temp~164~1264 (car &temp~164~1256))) (if (list? &temp~164~1264) (let ((&?arg~164~1246 &temp~164~1264)) (let ((&temp~164~1257 (cdr &temp~164~1256))) (if (pair? &temp~164~1257) (let ((&temp~164~1263 (car &temp~164~1257))) (let ((&?bindings~164~1245 &temp~164~1263)) (let ((&temp~164~1258 (cdr &temp~164~1257))) (if (pair? &temp~164~1258) (let ((&temp~164~1262 (car &temp~164~1258))) (if (list? &temp~164~1262) (let ((&?tmp~164~1244 &temp~164~1262)) (let ((&temp~164~1259 (cdr &temp~164~1258))) (if (pair? &temp~164~1259) (let ((&temp~164~1261 (car &temp~164~1259))) (let ((&?body~164~1243 &temp~164~1261)) (let ((&temp~164~1260 (cdr &temp~164~1259))) (if (null? &temp~164~1260) (cons (ex:syntax-rename (quote let-values) (quote ()) (quote (&env~164~1252)) 0 (quote (core let-values))) (cons (quote "mktmp") (cons &?b~164~1248 (cons &?e0~164~1247 (cons (append &?arg~164~1246 (cons (ex:syntax-rename (quote x) (quote ()) (quote (&env~164~1252)) 0 (quote (core let-values))) (quote ()))) (cons &?bindings~164~1245 (cons (append &?tmp~164~1244 (cons (cons &?a~164~1249 (cons (ex:syntax-rename (quote x) (quote ()) (quote (&env~164~1252)) 0 (quote (core let-values))) (quote ()))) (quote ()))) (cons &?body~164~1243 (quote ()))))))))) (&fail~164~1216))))) (&fail~164~1216)))) (&fail~164~1216))) (&fail~164~1216))))) (&fail~164~1216)))) (&fail~164~1216))) (&fail~164~1216))))) (&fail~164~1216))))))) (&fail~164~1216))) (&fail~164~1216))) (&fail~164~1216))) (&fail~164~1216))))) (&fail~164~1216)))))) (if (pair? &input~164~1211) (let ((&temp~164~1294 (car &input~164~1211))) (let ((&dummy~164~1276 &temp~164~1294)) (let ((&temp~164~1279 (cdr &input~164~1211))) (if (pair? &temp~164~1279) (let ((&temp~164~1293 (car &temp~164~1279))) (if (equal? &temp~164~1293 (quote "mktmp")) (let ((&temp~164~1280 (cdr &temp~164~1279))) (if (pair? &temp~164~1280) (let ((&temp~164~1292 (car &temp~164~1280))) (if (null? &temp~164~1292) (let ((&temp~164~1281 (cdr &temp~164~1280))) (if (pair? &temp~164~1281) (let ((&temp~164~1291 (car &temp~164~1281))) (let ((&?e0~164~1275 &temp~164~1291)) (let ((&temp~164~1282 (cdr &temp~164~1281))) (if (pair? &temp~164~1282) (let ((&temp~164~1290 (car &temp~164~1282))) (let ((&?args~164~1274 &temp~164~1290)) (let ((&temp~164~1283 (cdr &temp~164~1282))) (if (pair? &temp~164~1283) (let ((&temp~164~1289 (car &temp~164~1283))) (let ((&?bindings~164~1273 &temp~164~1289)) (let ((&temp~164~1284 (cdr &temp~164~1283))) (if (pair? &temp~164~1284) (let ((&temp~164~1288 (car &temp~164~1284))) (let ((&?tmps~164~1272 &temp~164~1288)) (let ((&temp~164~1285 (cdr &temp~164~1284))) (if (pair? &temp~164~1285) (let ((&temp~164~1287 (car &temp~164~1285))) (let ((&?body~164~1271 &temp~164~1287)) (let ((&temp~164~1286 (cdr &temp~164~1285))) (if (null? &temp~164~1286) (cons (ex:syntax-rename (quote call-with-values) (quote ()) (quote (&env~164~1278)) 0 (quote (core let-values))) (cons (cons (ex:syntax-rename (quote lambda) (quote ()) (quote (&env~164~1278)) 0 (quote (core let-values))) (cons (quote ()) (cons &?e0~164~1275 (quote ())))) (cons (cons (ex:syntax-rename (quote lambda) (quote ()) (quote (&env~164~1278)) 0 (quote (core let-values))) (cons &?args~164~1274 (cons (cons (ex:syntax-rename (quote let-values) (quote ()) (quote (&env~164~1278)) 0 (quote (core let-values))) (cons (quote "bind") (cons &?bindings~164~1273 (cons &?tmps~164~1272 (cons &?body~164~1271 (quote ())))))) (quote ())))) (quote ())))) (&fail~164~1215))))) (&fail~164~1215))))) (&fail~164~1215))))) (&fail~164~1215))))) (&fail~164~1215))))) (&fail~164~1215))) (&fail~164~1215))) (&fail~164~1215))) (&fail~164~1215))) (&fail~164~1215))))) (&fail~164~1215)))))) (if (pair? &input~164~1211) (let ((&temp~164~1318 (car &input~164~1211))) (let ((&dummy~164~1300 &temp~164~1318)) (let ((&temp~164~1303 (cdr &input~164~1211))) (if (pair? &temp~164~1303) (let ((&temp~164~1317 (car &temp~164~1303))) (if (equal? &temp~164~1317 (quote "bind")) (let ((&temp~164~1304 (cdr &temp~164~1303))) (if (pair? &temp~164~1304) (let ((&temp~164~1310 (car &temp~164~1304))) (if (pair? &temp~164~1310) (let ((&temp~164~1312 (car &temp~164~1310))) (if (pair? &temp~164~1312) (let ((&temp~164~1316 (car &temp~164~1312))) (let ((&?b0~164~1299 &temp~164~1316)) (let ((&temp~164~1313 (cdr &temp~164~1312))) (if (pair? &temp~164~1313) (let ((&temp~164~1315 (car &temp~164~1313))) (let ((&?e0~164~1298 &temp~164~1315)) (let ((&temp~164~1314 (cdr &temp~164~1313))) (if (null? &temp~164~1314) (let ((&temp~164~1311 (cdr &temp~164~1310))) (if (list? &temp~164~1311) (let ((&?binding~164~1297 &temp~164~1311)) (let ((&temp~164~1305 (cdr &temp~164~1304))) (if (pair? &temp~164~1305) (let ((&temp~164~1309 (car &temp~164~1305))) (let ((&?tmps~164~1296 &temp~164~1309)) (let ((&temp~164~1306 (cdr &temp~164~1305))) (if (pair? &temp~164~1306) (let ((&temp~164~1308 (car &temp~164~1306))) (let ((&?body~164~1295 &temp~164~1308)) (let ((&temp~164~1307 (cdr &temp~164~1306))) (if (null? &temp~164~1307) (cons (ex:syntax-rename (quote let-values) (quote ()) (quote (&env~164~1302)) 0 (quote (core let-values))) (cons (quote "mktmp") (cons &?b0~164~1299 (cons &?e0~164~1298 (cons (quote ()) (cons &?binding~164~1297 (cons &?tmps~164~1296 (cons &?body~164~1295 (quote ()))))))))) (&fail~164~1214))))) (&fail~164~1214))))) (&fail~164~1214)))) (&fail~164~1214))) (&fail~164~1214))))) (&fail~164~1214))))) (&fail~164~1214))) (&fail~164~1214))) (&fail~164~1214))) (&fail~164~1214))) (&fail~164~1214))))) (&fail~164~1214)))))) (if (pair? &input~164~1211) (let ((&temp~164~1333 (car &input~164~1211))) (let ((&dummy~164~1321 &temp~164~1333)) (let ((&temp~164~1324 (cdr &input~164~1211))) (if (pair? &temp~164~1324) (let ((&temp~164~1332 (car &temp~164~1324))) (if (equal? &temp~164~1332 (quote "bind")) (let ((&temp~164~1325 (cdr &temp~164~1324))) (if (pair? &temp~164~1325) (let ((&temp~164~1331 (car &temp~164~1325))) (if (null? &temp~164~1331) (let ((&temp~164~1326 (cdr &temp~164~1325))) (if (pair? &temp~164~1326) (let ((&temp~164~1330 (car &temp~164~1326))) (let ((&?tmps~164~1320 &temp~164~1330)) (let ((&temp~164~1327 (cdr &temp~164~1326))) (if (pair? &temp~164~1327) (let ((&temp~164~1329 (car &temp~164~1327))) (let ((&?body~164~1319 &temp~164~1329)) (let ((&temp~164~1328 (cdr &temp~164~1327))) (if (null? &temp~164~1328) (cons (ex:syntax-rename (quote let) (quote ()) (quote (&env~164~1323)) 0 (quote (core let-values))) (cons &?tmps~164~1320 (cons &?body~164~1319 (quote ())))) (&fail~164~1213))))) (&fail~164~1213))))) (&fail~164~1213))) (&fail~164~1213))) (&fail~164~1213))) (&fail~164~1213))) (&fail~164~1213))))) (&fail~164~1213)))))) (if (pair? &input~164~1211) (let ((&temp~164~1345 (car &input~164~1211))) (let ((&dummy~164~1337 &temp~164~1345)) (let ((&temp~164~1340 (cdr &input~164~1211))) (if (pair? &temp~164~1340) (let ((&temp~164~1344 (car &temp~164~1340))) (if (list? &temp~164~1344) (let ((&?binding~164~1336 &temp~164~1344)) (let ((&temp~164~1341 (cdr &temp~164~1340))) (if (pair? &temp~164~1341) (let ((&temp~164~1343 (car &temp~164~1341))) (let ((&?body0~164~1335 &temp~164~1343)) (let ((&temp~164~1342 (cdr &temp~164~1341))) (if (list? &temp~164~1342) (let ((&?body1~164~1334 &temp~164~1342)) (cons (ex:syntax-rename (quote let-values) (quote ()) (quote (&env~164~1339)) 0 (quote (core let-values))) (cons (quote "bind") (cons &?binding~164~1336 (cons (quote ()) (cons (cons (ex:syntax-rename (quote begin) (quote ()) (quote (&env~164~1339)) 0 (quote (core let-values))) (cons &?body0~164~1335 &?body1~164~1334)) (quote ()))))))) (&fail~164~1212))))) (&fail~164~1212)))) (&fail~164~1212))) (&fail~164~1212))))) (&fail~164~1212)))))) (ex:register-macro! (quote &let*-values~164~1346) (lambda (&x~164~1349) (let ((&input~164~1351 &x~164~1349)) (let ((&fail~164~1352 (lambda () (let ((&fail~164~1353 (lambda () (ex:invalid-form &input~164~1351)))) (if (pair? &input~164~1351) (let ((&temp~164~1368 (car &input~164~1351))) (let ((&dummy~164~1358 &temp~164~1368)) (let ((&temp~164~1361 (cdr &input~164~1351))) (if (pair? &temp~164~1361) (let ((&temp~164~1365 (car &temp~164~1361))) (if (pair? &temp~164~1365) (let ((&temp~164~1367 (car &temp~164~1365))) (let ((&?binding0~164~1357 &temp~164~1367)) (let ((&temp~164~1366 (cdr &temp~164~1365))) (if (list? &temp~164~1366) (let ((&?binding1~164~1356 &temp~164~1366)) (let ((&temp~164~1362 (cdr &temp~164~1361))) (if (pair? &temp~164~1362) (let ((&temp~164~1364 (car &temp~164~1362))) (let ((&?body0~164~1355 &temp~164~1364)) (let ((&temp~164~1363 (cdr &temp~164~1362))) (if (list? &temp~164~1363) (let ((&?body1~164~1354 &temp~164~1363)) (cons (ex:syntax-rename (quote let-values) (quote ()) (quote (&env~164~1360)) 0 (quote (core let-values))) (cons (cons &?binding0~164~1357 (quote ())) (cons (cons (ex:syntax-rename (quote let*-values) (quote ()) (quote (&env~164~1360)) 0 (quote (core let-values))) (cons &?binding1~164~1356 (cons &?body0~164~1355 &?body1~164~1354))) (quote ()))))) (&fail~164~1353))))) (&fail~164~1353)))) (&fail~164~1353))))) (&fail~164~1353))) (&fail~164~1353))))) (&fail~164~1353)))))) (if (pair? &input~164~1351) (let ((&temp~164~1379 (car &input~164~1351))) (let ((&dummy~164~1371 &temp~164~1379)) (let ((&temp~164~1374 (cdr &input~164~1351))) (if (pair? &temp~164~1374) (let ((&temp~164~1378 (car &temp~164~1374))) (if (null? &temp~164~1378) (let ((&temp~164~1375 (cdr &temp~164~1374))) (if (pair? &temp~164~1375) (let ((&temp~164~1377 (car &temp~164~1375))) (let ((&?body0~164~1370 &temp~164~1377)) (let ((&temp~164~1376 (cdr &temp~164~1375))) (if (list? &temp~164~1376) (let ((&?body1~164~1369 &temp~164~1376)) (cons (ex:syntax-rename (quote begin) (quote ()) (quote (&env~164~1373)) 0 (quote (core let-values))) (cons &?body0~164~1370 &?body1~164~1369))) (&fail~164~1352))))) (&fail~164~1352))) (&fail~164~1352))) (&fail~164~1352))))) (&fail~164~1352)))))) (values)) (lambda () (values)) (quote &build~164~1380))) (values))
(begin (ex:register-library! (ex:make-library (quote (rnrs control)) (lambda () (ex:uncompress (quote (((&env~164~1598 0 1 2 3) (&env~164~1581 4 1 2 3) (&env~164~1556 5 1 2 3) (&env~164~1533 6 1 2 3) (&env~164~1506 7 8 9 3) (&env~164~1492 10 8 9 3) (&env~164~1464 11 12 13 14 15 3) (&env~164~1460 16 12 13 14 15 3) (&env~164~1412 17 18 19 3) (&env~164~1393 20 21 22 3)) (22 (((x &c~164~1382) variable &x~164~1384 (0) #f (rnrs control)))) (21 ()) (20 (((dummy &c~164~1382) . #f) ((test) . #f) ((result1) . #f) ((result2) . #f))) (19 (((x &c~164~1401) variable &x~164~1403 (0) #f (rnrs control)))) (18 ()) (17 (((dummy &c~164~1401) . #f) ((test) . #f) ((result1) . #f) ((result2) . #f))) (16 (((e1) . #f) ((e2) . #f))) (15 (((orig-x) variable &orig-x~164~1421 (1) #f (rnrs control)))) (14 ()) (13 (((var) . #f) ((init) . #f) ((step) . #f) ((e0) . #f) ((e1) . #f) ((c) . #f))) (12 (((step) . #f))) (11 ()) (10 (((dummy &c~164~1480) . #f) ((fmls) . #f) ((b1) . #f) ((b2) . #f))) (9 (((x &c~164~1480) variable &x~164~1482 (0) #f (rnrs control)))) (8 ()) (7 (((dummy &c~164~1480) . #f) ((fmls) . #f) ((b1) . #f) ((b2) . #f))) (6 (((dummy &c~164~1516) . #f) ((args) . #f) ((n) . #f) ((r) . #f) ((b1) . #f) ((b2) . #f) ((more) . #f))) (5 (((dummy &c~164~1516) . #f) ((args) . #f) ((n) . #f) ((x1) . #f) ((x2) . #f) ((r) . #f) ((b1) . #f) ((b2) . #f) ((more) . #f))) (4 (((dummy &c~164~1516) . #f) ((args) . #f) ((n) . #f) ((x) . #f) ((b1) . #f) ((b2) . #f) ((more) . #f))) (3 (((case-lambda-help) macro &case-lambda-help~164~1515 (0) #f (rnrs control)) ((case-lambda) macro &case-lambda~164~1479 (0) #f (rnrs control)) ((do) macro &do~164~1419 (0) #f (rnrs control)) ((unless) macro &unless~164~1400 (0) #f (rnrs control)) ((when) macro &when~164~1381 (0) #f (rnrs control)) ((undefined) variable ex:undefined (1 0) #f ()) ((eval) variable ex:eval (1 0) #f ()) ((environment-bindings) variable ex:environment-bindings (1 0) #f ()) ((environment) variable ex:environment (1 0) #f ()) ((syntax-violation) variable ex:syntax-violation (1 0) #f ()) ((syntax->datum) variable ex:syntax->datum (1 0) #f ()) ((datum->syntax) variable ex:datum->syntax (1 0) #f ()) ((generate-temporaries) variable ex:generate-temporaries (1 0) #f ()) ((free-identifier=?) variable ex:free-identifier=? (1 0) #f ()) ((bound-identifier=?) variable ex:bound-identifier=? (1 0) #f ()) ((identifier?) variable ex:identifier? (1 0) #f ()) ((make-variable-transformer) variable ex:make-variable-transformer (1 0) #f ()) ((syntax-case) macro syntax-case (1 0) #f ()) ((syntax) macro syntax (1 0) #f ()) ((...) macro ... (1 0) #f ()) ((_) macro _ (1 0) #f ()) ((letrec-syntax) macro letrec-syntax (1 0) #f ()) ((let-syntax) macro let-syntax (1 0) #f ()) ((define-syntax) macro define-syntax (1 0) #f ()) ((define) macro define (1 0) #f ()) ((or) macro or (1 0) #f ()) ((and) macro and (1 0) #f ()) ((set!) macro set! (1 0) #f ()) ((quote) macro quote (1 0) #f ()) ((lambda) macro lambda (1 0) #f ()) ((if) macro if (1 0) #f ()) ((begin) macro begin (1 0) #f ()) ((letrec*) macro &letrec*~164~179 (1 0) #f (core let)) ((letrec) macro &letrec~164~148 (1 0) #f (core let)) ((let) macro &let~164~100 (1 0) #f (core let)) ((with-syntax) macro &with-syntax~164~3 (1) #f (core with-syntax)) ((syntax-rules) macro &syntax-rules~164~58 (1) #f (core syntax-rules)) ((apply) variable apply (1 0) #f ()) ((>=) variable >= (1 0) #f ()) ((=) variable = (1 0) #f ()) ((assertion-violation) variable assertion-violation (1 0) #f ()) ((length) variable length (1 0) #f ()) ((map) variable map (1 0) #f ()) ((not) variable not (1 0) #f ()))) (2 (((x &c~164~1516) variable &x~164~1518 (0) #f (rnrs control)))) (1 ()) (0 (((dummy &c~164~1516) . #f) ((args) . #f) ((n) . #f))))))) (quote ((when macro &when~164~1381 (0) #f (rnrs control)) (unless macro &unless~164~1400 (0) #f (rnrs control)) (do macro &do~164~1419 (0) #f (rnrs control)) (case-lambda macro &case-lambda~164~1479 (0) #f (rnrs control)))) (quote (((core syntax-rules) 1) ((core with-syntax) 1) ((core let) 1 0) ((core primitives) 1 0))) (quote (&build~164~99 &build~164~57 &build~164~203 &build~164~2)) (lambda () (ex:register-macro! (quote &when~164~1381) (lambda (&x~164~1384) (let ((&input~164~1386 &x~164~1384)) (let ((&fail~164~1387 (lambda () (ex:invalid-form &input~164~1386)))) (if (pair? &input~164~1386) (let ((&temp~164~1399 (car &input~164~1386))) (let ((&dummy~164~1391 &temp~164~1399)) (let ((&temp~164~1394 (cdr &input~164~1386))) (if (pair? &temp~164~1394) (let ((&temp~164~1398 (car &temp~164~1394))) (let ((&test~164~1390 &temp~164~1398)) (let ((&temp~164~1395 (cdr &temp~164~1394))) (if (pair? &temp~164~1395) (let ((&temp~164~1397 (car &temp~164~1395))) (let ((&result1~164~1389 &temp~164~1397)) (let ((&temp~164~1396 (cdr &temp~164~1395))) (if (list? &temp~164~1396) (let ((&result2~164~1388 &temp~164~1396)) (cons (ex:syntax-rename (quote if) (quote ()) (quote (&env~164~1393)) 0 (quote (rnrs control))) (cons &test~164~1390 (cons (cons (ex:syntax-rename (quote begin) (quote ()) (quote (&env~164~1393)) 0 (quote (rnrs control))) (cons &result1~164~1389 &result2~164~1388)) (quote ()))))) (&fail~164~1387))))) (&fail~164~1387))))) (&fail~164~1387))))) (&fail~164~1387)))))) (ex:register-macro! (quote &unless~164~1400) (lambda (&x~164~1403) (let ((&input~164~1405 &x~164~1403)) (let ((&fail~164~1406 (lambda () (ex:invalid-form &input~164~1405)))) (if (pair? &input~164~1405) (let ((&temp~164~1418 (car &input~164~1405))) (let ((&dummy~164~1410 &temp~164~1418)) (let ((&temp~164~1413 (cdr &input~164~1405))) (if (pair? &temp~164~1413) (let ((&temp~164~1417 (car &temp~164~1413))) (let ((&test~164~1409 &temp~164~1417)) (let ((&temp~164~1414 (cdr &temp~164~1413))) (if (pair? &temp~164~1414) (let ((&temp~164~1416 (car &temp~164~1414))) (let ((&result1~164~1408 &temp~164~1416)) (let ((&temp~164~1415 (cdr &temp~164~1414))) (if (list? &temp~164~1415) (let ((&result2~164~1407 &temp~164~1415)) (cons (ex:syntax-rename (quote if) (quote ()) (quote (&env~164~1412)) 0 (quote (rnrs control))) (cons (cons (ex:syntax-rename (quote not) (quote ()) (quote (&env~164~1412)) 0 (quote (rnrs control))) (cons &test~164~1409 (quote ()))) (cons (cons (ex:syntax-rename (quote begin) (quote ()) (quote (&env~164~1412)) 0 (quote (rnrs control))) (cons &result1~164~1408 &result2~164~1407)) (quote ()))))) (&fail~164~1406))))) (&fail~164~1406))))) (&fail~164~1406))))) (&fail~164~1406)))))) (ex:register-macro! (quote &do~164~1419) (lambda (&orig-x~164~1421) (let ((&input~164~1423 &orig-x~164~1421)) (let ((&fail~164~1424 (lambda () (ex:invalid-form &input~164~1423)))) (if (pair? &input~164~1423) (let ((&temp~164~1478 (car &input~164~1423))) (let ((&temp~164~1465 (cdr &input~164~1423))) (if (pair? &temp~164~1465) (let ((&temp~164~1471 (car &temp~164~1465))) (ex:map-while (lambda (&temp~164~1471) (if (pair? &temp~164~1471) (let ((&temp~164~1477 (car &temp~164~1471))) (let ((&var~164~1430 &temp~164~1477)) (let ((&temp~164~1474 (cdr &temp~164~1471))) (if (pair? &temp~164~1474) (let ((&temp~164~1476 (car &temp~164~1474))) (let ((&init~164~1429 &temp~164~1476)) (let ((&temp~164~1475 (cdr &temp~164~1474))) (let ((&step~164~1428 &temp~164~1475)) (list &var~164~1430 &init~164~1429 &step~164~1428))))) #f)))) #f)) &temp~164~1471 (lambda (&cols~164~1472 &rest~164~1473) (if (null? &rest~164~1473) (apply (lambda (&var~164~1430 &init~164~1429 &step~164~1428) (let ((&temp~164~1466 (cdr &temp~164~1465))) (if (pair? &temp~164~1466) (let ((&temp~164~1468 (car &temp~164~1466))) (if (pair? &temp~164~1468) (let ((&temp~164~1470 (car &temp~164~1468))) (let ((&e0~164~1427 &temp~164~1470)) (let ((&temp~164~1469 (cdr &temp~164~1468))) (if (list? &temp~164~1469) (let ((&e1~164~1426 &temp~164~1469)) (let ((&temp~164~1467 (cdr &temp~164~1466))) (if (list? &temp~164~1467) (let ((&c~164~1425 &temp~164~1467)) (let ((&input~164~1433 (map (lambda (&v~164~1438 &s~164~1437) (let ((&input~164~1440 &s~164~1437)) (let ((&fail~164~1441 (lambda () (let ((&fail~164~1442 (lambda () (let ((&fail~164~1443 (lambda () (ex:invalid-form &input~164~1440)))) (ex:syntax-violation (quote do) "Invalid step" &orig-x~164~1421 &s~164~1437))))) (if (pair? &input~164~1440) (let ((&temp~164~1448 (car &input~164~1440))) (let ((&e~164~1445 &temp~164~1448)) (let ((&temp~164~1447 (cdr &input~164~1440))) (if (null? &temp~164~1447) &e~164~1445 (&fail~164~1442))))) (&fail~164~1442)))))) (if (null? &input~164~1440) &v~164~1438 (&fail~164~1441))))) &var~164~1430 &step~164~1428))) (let ((&fail~164~1449 (lambda () (ex:invalid-form &input~164~1433)))) (if (list? &input~164~1433) (let ((&step~164~1450 &input~164~1433)) (begin (let ((&input~164~1453 &e1~164~1426)) (let ((&fail~164~1455 (lambda () (let ((&fail~164~1456 (lambda () (ex:invalid-form &input~164~1453)))) (if (pair? &input~164~1453) (let ((&temp~164~1462 (car &input~164~1453))) (let ((&e1~164~1458 &temp~164~1462)) (let ((&temp~164~1461 (cdr &input~164~1453))) (if (list? &temp~164~1461) (let ((&e2~164~1457 &temp~164~1461)) (cons (ex:syntax-rename (quote let) (quote ()) (quote (&env~164~1460)) 0 (quote (rnrs control))) (cons (ex:syntax-rename (quote do) (quote ()) (quote (&env~164~1460)) 0 (quote (rnrs control))) (cons (if (= (length &var~164~1430) (length &init~164~1429)) (map (lambda (&var~164~1430 &init~164~1429) (cons &var~164~1430 (cons &init~164~1429 (quote ())))) &var~164~1430 &init~164~1429) (ex:syntax-violation (quote syntax) "Pattern variables denoting lists of unequal length preceding ellipses" (quote ((var init) ...)) (list &var~164~1430 &init~164~1429))) (cons (cons (ex:syntax-rename (quote if) (quote ()) (quote (&env~164~1460)) 0 (quote (rnrs control))) (cons &e0~164~1427 (cons (cons (ex:syntax-rename (quote begin) (quote ()) (quote (&env~164~1460)) 0 (quote (rnrs control))) (cons &e1~164~1458 &e2~164~1457)) (cons (cons (ex:syntax-rename (quote begin) (quote ()) (quote (&env~164~1460)) 0 (quote (rnrs control))) (append &c~164~1425 (cons (cons (ex:syntax-rename (quote do) (quote ()) (quote (&env~164~1460)) 0 (quote (rnrs control))) &step~164~1450) (quote ())))) (quote ()))))) (quote ())))))) (&fail~164~1456))))) (&fail~164~1456)))))) (if (null? &input~164~1453) (cons (ex:syntax-rename (quote let) (quote ()) (quote (&env~164~1464)) 0 (quote (rnrs control))) (cons (ex:syntax-rename (quote do) (quote ()) (quote (&env~164~1464)) 0 (quote (rnrs control))) (cons (if (= (length &var~164~1430) (length &init~164~1429)) (map (lambda (&var~164~1430 &init~164~1429) (cons &var~164~1430 (cons &init~164~1429 (quote ())))) &var~164~1430 &init~164~1429) (ex:syntax-violation (quote syntax) "Pattern variables denoting lists of unequal length preceding ellipses" (quote ((var init) ...)) (list &var~164~1430 &init~164~1429))) (cons (cons (ex:syntax-rename (quote if) (quote ()) (quote (&env~164~1464)) 0 (quote (rnrs control))) (cons (cons (ex:syntax-rename (quote not) (quote ()) (quote (&env~164~1464)) 0 (quote (rnrs control))) (cons &e0~164~1427 (quote ()))) (cons (cons (ex:syntax-rename (quote begin) (quote ()) (quote (&env~164~1464)) 0 (quote (rnrs control))) (append &c~164~1425 (cons (cons (ex:syntax-rename (quote do) (quote ()) (quote (&env~164~1464)) 0 (quote (rnrs control))) &step~164~1450) (quote ())))) (quote ())))) (quote ()))))) (&fail~164~1455)))))) (&fail~164~1449))))) (&fail~164~1424)))) (&fail~164~1424))))) (&fail~164~1424))) (&fail~164~1424)))) (if (null? &cols~164~1472) (quote (() () ())) (apply map list &cols~164~1472))) (&fail~164~1424))))) (&fail~164~1424)))) (&fail~164~1424)))))) (ex:register-macro! (quote &case-lambda~164~1479) (lambda (&x~164~1482) (let ((&input~164~1484 &x~164~1482)) (let ((&fail~164~1485 (lambda () (let ((&fail~164~1486 (lambda () (ex:invalid-form &input~164~1484)))) (if (pair? &input~164~1484) (let ((&temp~164~1500 (car &input~164~1484))) (let ((&dummy~164~1490 &temp~164~1500)) (let ((&temp~164~1493 (cdr &input~164~1484))) (ex:map-while (lambda (&temp~164~1493) (if (pair? &temp~164~1493) (let ((&temp~164~1499 (car &temp~164~1493))) (let ((&fmls~164~1489 &temp~164~1499)) (let ((&temp~164~1496 (cdr &temp~164~1493))) (if (pair? &temp~164~1496) (let ((&temp~164~1498 (car &temp~164~1496))) (let ((&b1~164~1488 &temp~164~1498)) (let ((&temp~164~1497 (cdr &temp~164~1496))) (if (list? &temp~164~1497) (let ((&b2~164~1487 &temp~164~1497)) (list &fmls~164~1489 &b1~164~1488 &b2~164~1487)) #f)))) #f)))) #f)) &temp~164~1493 (lambda (&cols~164~1494 &rest~164~1495) (if (null? &rest~164~1495) (apply (lambda (&fmls~164~1489 &b1~164~1488 &b2~164~1487) (cons (ex:syntax-rename (quote lambda) (quote ()) (quote (&env~164~1492)) 0 (quote (rnrs control))) (cons (ex:syntax-rename (quote args) (quote ()) (quote (&env~164~1492)) 0 (quote (rnrs control))) (cons (cons (ex:syntax-rename (quote let) (quote ()) (quote (&env~164~1492)) 0 (quote (rnrs control))) (cons (cons (cons (ex:syntax-rename (quote n) (quote ()) (quote (&env~164~1492)) 0 (quote (rnrs control))) (cons (cons (ex:syntax-rename (quote length) (quote ()) (quote (&env~164~1492)) 0 (quote (rnrs control))) (cons (ex:syntax-rename (quote args) (quote ()) (quote (&env~164~1492)) 0 (quote (rnrs control))) (quote ()))) (quote ()))) (quote ())) (cons (cons (ex:syntax-rename (quote case-lambda-help) (quote ()) (quote (&env~164~1492)) 0 (quote (rnrs control))) (cons (ex:syntax-rename (quote args) (quote ()) (quote (&env~164~1492)) 0 (quote (rnrs control))) (cons (ex:syntax-rename (quote n) (quote ()) (quote (&env~164~1492)) 0 (quote (rnrs control))) (if (= (length &fmls~164~1489) (length &b1~164~1488) (length &b2~164~1487)) (map (lambda (&fmls~164~1489 &b1~164~1488 &b2~164~1487) (cons &fmls~164~1489 (cons &b1~164~1488 &b2~164~1487))) &fmls~164~1489 &b1~164~1488 &b2~164~1487) (ex:syntax-violation (quote syntax) "Pattern variables denoting lists of unequal length preceding ellipses" (quote ((fmls b1 b2 ...) ...)) (list &fmls~164~1489 &b1~164~1488 &b2~164~1487)))))) (quote ())))) (quote ()))))) (if (null? &cols~164~1494) (quote (() () ())) (apply map list &cols~164~1494))) (&fail~164~1486))))))) (&fail~164~1486)))))) (if (pair? &input~164~1484) (let ((&temp~164~1514 (car &input~164~1484))) (let ((&dummy~164~1504 &temp~164~1514)) (let ((&temp~164~1507 (cdr &input~164~1484))) (if (pair? &temp~164~1507) (let ((&temp~164~1509 (car &temp~164~1507))) (if (pair? &temp~164~1509) (let ((&temp~164~1513 (car &temp~164~1509))) (let ((&fmls~164~1503 &temp~164~1513)) (let ((&temp~164~1510 (cdr &temp~164~1509))) (if (pair? &temp~164~1510) (let ((&temp~164~1512 (car &temp~164~1510))) (let ((&b1~164~1502 &temp~164~1512)) (let ((&temp~164~1511 (cdr &temp~164~1510))) (if (list? &temp~164~1511) (let ((&b2~164~1501 &temp~164~1511)) (let ((&temp~164~1508 (cdr &temp~164~1507))) (if (null? &temp~164~1508) (cons (ex:syntax-rename (quote lambda) (quote ()) (quote (&env~164~1506)) 0 (quote (rnrs control))) (cons &fmls~164~1503 (cons &b1~164~1502 &b2~164~1501))) (&fail~164~1485)))) (&fail~164~1485))))) (&fail~164~1485))))) (&fail~164~1485))) (&fail~164~1485))))) (&fail~164~1485)))))) (ex:register-macro! (quote &case-lambda-help~164~1515) (lambda (&x~164~1518) (let ((&input~164~1520 &x~164~1518)) (let ((&fail~164~1521 (lambda () (let ((&fail~164~1522 (lambda () (let ((&fail~164~1523 (lambda () (let ((&fail~164~1524 (lambda () (ex:invalid-form &input~164~1520)))) (if (pair? &input~164~1520) (let ((&temp~164~1545 (car &input~164~1520))) (let ((&dummy~164~1531 &temp~164~1545)) (let ((&temp~164~1534 (cdr &input~164~1520))) (if (pair? &temp~164~1534) (let ((&temp~164~1544 (car &temp~164~1534))) (let ((&args~164~1530 &temp~164~1544)) (let ((&temp~164~1535 (cdr &temp~164~1534))) (if (pair? &temp~164~1535) (let ((&temp~164~1543 (car &temp~164~1535))) (let ((&n~164~1529 &temp~164~1543)) (let ((&temp~164~1536 (cdr &temp~164~1535))) (if (pair? &temp~164~1536) (let ((&temp~164~1538 (car &temp~164~1536))) (if (pair? &temp~164~1538) (let ((&temp~164~1542 (car &temp~164~1538))) (let ((&r~164~1528 &temp~164~1542)) (let ((&temp~164~1539 (cdr &temp~164~1538))) (if (pair? &temp~164~1539) (let ((&temp~164~1541 (car &temp~164~1539))) (let ((&b1~164~1527 &temp~164~1541)) (let ((&temp~164~1540 (cdr &temp~164~1539))) (if (list? &temp~164~1540) (let ((&b2~164~1526 &temp~164~1540)) (let ((&temp~164~1537 (cdr &temp~164~1536))) (if (list? &temp~164~1537) (let ((&more~164~1525 &temp~164~1537)) (cons (ex:syntax-rename (quote apply) (quote ()) (quote (&env~164~1533)) 0 (quote (rnrs control))) (cons (cons (ex:syntax-rename (quote lambda) (quote ()) (quote (&env~164~1533)) 0 (quote (rnrs control))) (cons &r~164~1528 (cons &b1~164~1527 &b2~164~1526))) (cons &args~164~1530 (quote ()))))) (&fail~164~1524)))) (&fail~164~1524))))) (&fail~164~1524))))) (&fail~164~1524))) (&fail~164~1524))))) (&fail~164~1524))))) (&fail~164~1524))))) (&fail~164~1524)))))) (if (pair? &input~164~1520) (let ((&temp~164~1572 (car &input~164~1520))) (let ((&dummy~164~1554 &temp~164~1572)) (let ((&temp~164~1557 (cdr &input~164~1520))) (if (pair? &temp~164~1557) (let ((&temp~164~1571 (car &temp~164~1557))) (let ((&args~164~1553 &temp~164~1571)) (let ((&temp~164~1558 (cdr &temp~164~1557))) (if (pair? &temp~164~1558) (let ((&temp~164~1570 (car &temp~164~1558))) (let ((&n~164~1552 &temp~164~1570)) (let ((&temp~164~1559 (cdr &temp~164~1558))) (if (pair? &temp~164~1559) (let ((&temp~164~1561 (car &temp~164~1559))) (if (pair? &temp~164~1561) (let ((&temp~164~1565 (car &temp~164~1561))) (if (pair? &temp~164~1565) (let ((&temp~164~1569 (car &temp~164~1565))) (let ((&x1~164~1551 &temp~164~1569)) (let ((&temp~164~1566 (cdr &temp~164~1565))) (if (>= (ex:dotted-length &temp~164~1566) 0) (let ((&temp~164~1568 (ex:dotted-butlast &temp~164~1566 0))) (if (list? &temp~164~1568) (let ((&x2~164~1550 &temp~164~1568)) (let ((&temp~164~1567 (ex:dotted-last &temp~164~1566 0))) (let ((&r~164~1549 &temp~164~1567)) (let ((&temp~164~1562 (cdr &temp~164~1561))) (if (pair? &temp~164~1562) (let ((&temp~164~1564 (car &temp~164~1562))) (let ((&b1~164~1548 &temp~164~1564)) (let ((&temp~164~1563 (cdr &temp~164~1562))) (if (list? &temp~164~1563) (let ((&b2~164~1547 &temp~164~1563)) (let ((&temp~164~1560 (cdr &temp~164~1559))) (if (list? &temp~164~1560) (let ((&more~164~1546 &temp~164~1560)) (cons (ex:syntax-rename (quote if) (quote ()) (quote (&env~164~1556)) 0 (quote (rnrs control))) (cons (cons (ex:syntax-rename (quote >=) (quote ()) (quote (&env~164~1556)) 0 (quote (rnrs control))) (cons &n~164~1552 (cons (cons (ex:syntax-rename (quote length) (quote ()) (quote (&env~164~1556)) 0 (quote (rnrs control))) (cons (cons (ex:syntax-rename (quote quote) (quote ()) (quote (&env~164~1556)) 0 (quote (rnrs control))) (cons (cons &x1~164~1551 &x2~164~1550) (quote ()))) (quote ()))) (quote ())))) (cons (cons (ex:syntax-rename (quote apply) (quote ()) (quote (&env~164~1556)) 0 (quote (rnrs control))) (cons (cons (ex:syntax-rename (quote lambda) (quote ()) (quote (&env~164~1556)) 0 (quote (rnrs control))) (cons (cons &x1~164~1551 (append &x2~164~1550 &r~164~1549)) (cons &b1~164~1548 &b2~164~1547))) (cons &args~164~1553 (quote ())))) (cons (cons (ex:syntax-rename (quote case-lambda-help) (quote ()) (quote (&env~164~1556)) 0 (quote (rnrs control))) (cons &args~164~1553 (cons &n~164~1552 &more~164~1546))) (quote ())))))) (&fail~164~1523)))) (&fail~164~1523))))) (&fail~164~1523)))))) (&fail~164~1523))) (&fail~164~1523))))) (&fail~164~1523))) (&fail~164~1523))) (&fail~164~1523))))) (&fail~164~1523))))) (&fail~164~1523))))) (&fail~164~1523)))))) (if (pair? &input~164~1520) (let ((&temp~164~1593 (car &input~164~1520))) (let ((&dummy~164~1579 &temp~164~1593)) (let ((&temp~164~1582 (cdr &input~164~1520))) (if (pair? &temp~164~1582) (let ((&temp~164~1592 (car &temp~164~1582))) (let ((&args~164~1578 &temp~164~1592)) (let ((&temp~164~1583 (cdr &temp~164~1582))) (if (pair? &temp~164~1583) (let ((&temp~164~1591 (car &temp~164~1583))) (let ((&n~164~1577 &temp~164~1591)) (let ((&temp~164~1584 (cdr &temp~164~1583))) (if (pair? &temp~164~1584) (let ((&temp~164~1586 (car &temp~164~1584))) (if (pair? &temp~164~1586) (let ((&temp~164~1590 (car &temp~164~1586))) (if (list? &temp~164~1590) (let ((&x~164~1576 &temp~164~1590)) (let ((&temp~164~1587 (cdr &temp~164~1586))) (if (pair? &temp~164~1587) (let ((&temp~164~1589 (car &temp~164~1587))) (let ((&b1~164~1575 &temp~164~1589)) (let ((&temp~164~1588 (cdr &temp~164~1587))) (if (list? &temp~164~1588) (let ((&b2~164~1574 &temp~164~1588)) (let ((&temp~164~1585 (cdr &temp~164~1584))) (if (list? &temp~164~1585) (let ((&more~164~1573 &temp~164~1585)) (cons (ex:syntax-rename (quote if) (quote ()) (quote (&env~164~1581)) 0 (quote (rnrs control))) (cons (cons (ex:syntax-rename (quote =) (quote ()) (quote (&env~164~1581)) 0 (quote (rnrs control))) (cons &n~164~1577 (cons (cons (ex:syntax-rename (quote length) (quote ()) (quote (&env~164~1581)) 0 (quote (rnrs control))) (cons (cons (ex:syntax-rename (quote quote) (quote ()) (quote (&env~164~1581)) 0 (quote (rnrs control))) (cons &x~164~1576 (quote ()))) (quote ()))) (quote ())))) (cons (cons (ex:syntax-rename (quote apply) (quote ()) (quote (&env~164~1581)) 0 (quote (rnrs control))) (cons (cons (ex:syntax-rename (quote lambda) (quote ()) (quote (&env~164~1581)) 0 (quote (rnrs control))) (cons &x~164~1576 (cons &b1~164~1575 &b2~164~1574))) (cons &args~164~1578 (quote ())))) (cons (cons (ex:syntax-rename (quote case-lambda-help) (quote ()) (quote (&env~164~1581)) 0 (quote (rnrs control))) (cons &args~164~1578 (cons &n~164~1577 &more~164~1573))) (quote ())))))) (&fail~164~1522)))) (&fail~164~1522))))) (&fail~164~1522)))) (&fail~164~1522))) (&fail~164~1522))) (&fail~164~1522))))) (&fail~164~1522))))) (&fail~164~1522))))) (&fail~164~1522)))))) (if (pair? &input~164~1520) (let ((&temp~164~1604 (car &input~164~1520))) (let ((&dummy~164~1596 &temp~164~1604)) (let ((&temp~164~1599 (cdr &input~164~1520))) (if (pair? &temp~164~1599) (let ((&temp~164~1603 (car &temp~164~1599))) (let ((&args~164~1595 &temp~164~1603)) (let ((&temp~164~1600 (cdr &temp~164~1599))) (if (pair? &temp~164~1600) (let ((&temp~164~1602 (car &temp~164~1600))) (let ((&n~164~1594 &temp~164~1602)) (let ((&temp~164~1601 (cdr &temp~164~1600))) (if (null? &temp~164~1601) (cons (ex:syntax-rename (quote assertion-violation) (quote ()) (quote (&env~164~1598)) 0 (quote (rnrs control))) (cons (quote #f) (cons (quote "unexpected number of arguments") (quote ())))) (&fail~164~1521))))) (&fail~164~1521))))) (&fail~164~1521))))) (&fail~164~1521)))))) (values)) (lambda () (values)) (quote &build~164~1605))) (values))
(begin (ex:register-library! (ex:make-library (quote (rnrs lists)) (lambda () (quote ())) (quote ((find variable find (0) #f ()) (for-all variable for-all (0) #f ()) (exists variable exists (0) #f ()) (filter variable filter (0) #f ()) (partition variable partition (0) #f ()) (fold-left variable fold-left (0) #f ()) (fold-right variable fold-right (0) #f ()) (remp variable remp (0) #f ()) (remove variable remove (0) #f ()) (remq variable remq (0) #f ()) (remv variable remv (0) #f ()) (memp variable memp (0) #f ()) (member variable member (0) #f ()) (memv variable memv (0) #f ()) (memq variable memq (0) #f ()) (assp variable assp (0) #f ()) (assoc variable assoc (0) #f ()) (assv variable assv (0) #f ()) (assq variable assq (0) #f ()))) (quote ()) (quote ()) (lambda () (values)) (lambda () (values)) (quote &build~164~1606))) (values))
(begin (ex:register-library! (ex:make-library (quote (rnrs io simple)) (lambda () (quote ())) (quote ((call-with-input-file variable call-with-input-file (0) #f ()) (call-with-output-file variable call-with-output-file (0) #f ()) (close-input-port variable close-input-port (0) #f ()) (close-output-port variable close-output-port (0) #f ()) (current-input-port variable current-input-port (0) #f ()) (current-output-port variable current-output-port (0) #f ()) (display variable display (0) #f ()) (eof-object? variable eof-object? (0) #f ()) (newline variable newline (0) #f ()) (open-input-file variable open-input-file (0) #f ()) (open-output-file variable open-output-file (0) #f ()) (peek-char variable peek-char (0) #f ()) (read variable read (0) #f ()) (read-char variable read-char (0) #f ()) (with-input-from-file variable with-input-from-file (0) #f ()) (with-output-to-file variable with-output-to-file (0) #f ()) (write variable write (0) #f ()) (write-char variable write-char (0) #f ()))) (quote ()) (quote ()) (lambda () (values)) (lambda () (values)) (quote &build~164~1607))) (values))
(begin (ex:register-library! (ex:make-library (quote (rnrs unicode)) (lambda () (quote ())) (quote ((char-upcase variable char-upcase (0) #f ()) (char-downcase variable char-downcase (0) #f ()) (char-titlecase variable char-titlecase (0) #f ()) (char-foldcase variable char-foldcase (0) #f ()) (char-ci=? variable char-ci=? (0) #f ()) (char-ci<? variable char-ci<? (0) #f ()) (char-ci>? variable char-ci>? (0) #f ()) (char-ci<=? variable char-ci<=? (0) #f ()) (char-ci>=? variable char-ci>=? (0) #f ()) (char-alphabetic? variable char-alphabetic? (0) #f ()) (char-numeric? variable char-numeric? (0) #f ()) (char-whitespace? variable char-whitespace? (0) #f ()) (char-upper-case? variable char-upper-case? (0) #f ()) (char-lower-case? variable char-lower-case? (0) #f ()) (char-title-case? variable char-title-case? (0) #f ()) (char-general-category variable char-general-category (0) #f ()) (string-upcase variable string-upcase (0) #f ()) (string-downcase variable string-downcase (0) #f ()) (string-titlecase variable string-titlecase (0) #f ()) (string-foldcase variable string-foldcase (0) #f ()) (string-ci=? variable string-ci=? (0) #f ()) (string-ci<? variable string-ci<? (0) #f ()) (string-ci>? variable string-ci>? (0) #f ()) (string-ci<=? variable string-ci<=? (0) #f ()) (string-ci>=? variable string-ci>=? (0) #f ()) (string-normalize-nfd variable string-normalize-nfd (0) #f ()) (string-normalize-nfkd variable string-normalize-nfkd (0) #f ()) (string-normalize-nfc variable string-normalize-nfc (0) #f ()) (string-normalize-nfkc variable string-normalize-nfkc (0) #f ()))) (quote ()) (quote ()) (lambda () (values)) (lambda () (values)) (quote &build~164~1608))) (values))
(begin (ex:register-library! (ex:make-library (quote (rnrs sorting)) (lambda () (quote ())) (quote ((list-sort variable list-sort (0) #f ()) (vector-sort variable vector-sort (0) #f ()) (vector-sort! variable vector-sort! (0) #f ()))) (quote ()) (quote ()) (lambda () (values)) (lambda () (values)) (quote &build~164~1609))) (values))
(begin (ex:register-library! (ex:make-library (quote (rnrs records procedural)) (lambda () (quote ())) (quote ((make-record-type-descriptor variable make-record-type-descriptor (0) #f ()) (record-type-descriptor? variable record-type-descriptor? (0) #f ()) (make-record-constructor-descriptor variable make-record-constructor-descriptor (0) #f ()) (record-constructor variable record-constructor (0) #f ()) (record-predicate variable record-predicate (0) #f ()) (record-accessor variable record-accessor (0) #f ()) (record-mutator variable record-mutator (0) #f ()))) (quote ()) (quote ()) (lambda () (values)) (lambda () (values)) (quote &build~164~1610))) (values))
(begin (ex:register-library! (ex:make-library (quote (rnrs records inspection)) (lambda () (quote ())) (quote ((record? variable record? (0) #f ()) (record-rtd variable record-rtd (0) #f ()) (record-type-name variable record-type-name (0) #f ()) (record-type-parent variable record-type-parent (0) #f ()) (record-type-uid variable record-type-uid (0) #f ()) (record-type-generative? variable record-type-generative? (0) #f ()) (record-type-sealed? variable record-type-sealed? (0) #f ()) (record-type-opaque? variable record-type-opaque? (0) #f ()) (record-type-field-names variable record-type-field-names (0) #f ()) (record-field-mutable? variable record-field-mutable? (0) #f ()))) (quote ()) (quote ()) (lambda () (values)) (lambda () (values)) (quote &build~164~1611))) (values))
(begin (ex:register-library! (ex:make-library (quote (rnrs arithmetic fixnums)) (lambda () (quote ())) (quote ((fixnum? variable fixnum? (0) #f ()) (fixnum-width variable fixnum-width (0) #f ()) (least-fixnum variable least-fixnum (0) #f ()) (greatest-fixnum variable greatest-fixnum (0) #f ()) (fx=? variable fx=? (0) #f ()) (fx>? variable fx>? (0) #f ()) (fx<? variable fx<? (0) #f ()) (fx>=? variable fx>=? (0) #f ()) (fx<=? variable fx<=? (0) #f ()) (fxzero? variable fxzero? (0) #f ()) (fxpositive? variable fxpositive? (0) #f ()) (fxnegative? variable fxnegative? (0) #f ()) (fxodd? variable fxodd? (0) #f ()) (fxeven? variable fxeven? (0) #f ()) (fxmax variable fxmax (0) #f ()) (fxmin variable fxmin (0) #f ()) (fx+ variable fx+ (0) #f ()) (fx- variable fx- (0) #f ()) (fx* variable fx* (0) #f ()) (fxdiv-and-mod variable fxdiv-and-mod (0) #f ()) (fxdiv variable fxdiv (0) #f ()) (fxmod variable fxmod (0) #f ()) (fxdiv0-and-mod0 variable fxdiv0-and-mod0 (0) #f ()) (fxdiv0 variable fxdiv0 (0) #f ()) (fxmod0 variable fxmod0 (0) #f ()) (fx+/carry variable fx+/carry (0) #f ()) (fx-/carry variable fx-/carry (0) #f ()) (fx*/carry variable fx*/carry (0) #f ()) (fxnot variable fxnot (0) #f ()) (fxand variable fxand (0) #f ()) (fxior variable fxior (0) #f ()) (fxxor variable fxxor (0) #f ()) (fxif variable fxif (0) #f ()) (fxbit-count variable fxbit-count (0) #f ()) (fxlength variable fxlength (0) #f ()) (fxfirst-bit-set variable fxfirst-bit-set (0) #f ()) (fxbit-set? variable fxbit-set? (0) #f ()) (fxcopy-bit variable fxcopy-bit (0) #f ()) (fxbit-field variable fxbit-field (0) #f ()) (fxcopy-bit-field variable fxcopy-bit-field (0) #f ()) (fxrotate-bit-field variable fxrotate-bit-field (0) #f ()) (fxreverse-bit-field variable fxreverse-bit-field (0) #f ()) (fxarithmetic-shift variable fxarithmetic-shift (0) #f ()) (fxarithmetic-shift-left variable fxarithmetic-shift-left (0) #f ()) (fxarithmetic-shift-right variable fxarithmetic-shift-right (0) #f ()))) (quote ()) (quote ()) (lambda () (values)) (lambda () (values)) (quote &build~164~1612))) (values))
(begin (ex:register-library! (ex:make-library (quote (rnrs arithmetic flonums)) (lambda () (quote ())) (quote ((flonum? variable flonum? (0) #f ()) (real->flonum variable real->flonum (0) #f ()) (fl=? variable fl=? (0) #f ()) (fl<? variable fl<? (0) #f ()) (fl>? variable fl>? (0) #f ()) (fl<=? variable fl<=? (0) #f ()) (fl>=? variable fl>=? (0) #f ()) (flinteger? variable flinteger? (0) #f ()) (flzero? variable flzero? (0) #f ()) (flpositive? variable flpositive? (0) #f ()) (flnegative? variable flnegative? (0) #f ()) (flodd? variable flodd? (0) #f ()) (fleven? variable fleven? (0) #f ()) (flfinite? variable flfinite? (0) #f ()) (flinfinite? variable flinfinite? (0) #f ()) (flnan? variable flnan? (0) #f ()) (flmax variable flmax (0) #f ()) (flmin variable flmin (0) #f ()) (fl+ variable fl+ (0) #f ()) (fl* variable fl* (0) #f ()) (fl- variable fl- (0) #f ()) (fl/ variable fl/ (0) #f ()) (flabs variable flabs (0) #f ()) (fldiv-and-mod variable fldiv-and-mod (0) #f ()) (fldiv variable fldiv (0) #f ()) (flmod variable flmod (0) #f ()) (fldiv0-and-mod0 variable fldiv0-and-mod0 (0) #f ()) (fldiv0 variable fldiv0 (0) #f ()) (flmod0 variable flmod0 (0) #f ()) (flnumerator variable flnumerator (0) #f ()) (fldenominator variable fldenominator (0) #f ()) (flfloor variable flfloor (0) #f ()) (flceiling variable flceiling (0) #f ()) (fltruncate variable fltruncate (0) #f ()) (flround variable flround (0) #f ()) (flexp variable flexp (0) #f ()) (fllog variable fllog (0) #f ()) (flsin variable flsin (0) #f ()) (flcos variable flcos (0) #f ()) (fltan variable fltan (0) #f ()) (flasin variable flasin (0) #f ()) (flacos variable flacos (0) #f ()) (flatan variable flatan (0) #f ()) (flsqrt variable flsqrt (0) #f ()) (flexpt variable flexpt (0) #f ()) (fixnum->flonum variable fixnum->flonum (0) #f ()))) (quote ()) (quote ()) (lambda () (values)) (lambda () (values)) (quote &build~164~1613))) (values))
(begin (ex:register-library! (ex:make-library (quote (rnrs arithmetic bitwise)) (lambda () (quote ())) (quote ((bitwise-not variable bitwise-not (0) #f ()) (bitwise-and variable bitwise-and (0) #f ()) (bitwise-ior variable bitwise-ior (0) #f ()) (bitwise-xor variable bitwise-xor (0) #f ()) (bitwise-if variable bitwise-if (0) #f ()) (bitwise-bit-count variable bitwise-bit-count (0) #f ()) (bitwise-length variable bitwise-length (0) #f ()) (bitwise-first-bit-set variable bitwise-first-bit-set (0) #f ()) (bitwise-bit-set? variable bitwise-bit-set? (0) #f ()) (bitwise-copy-bit variable bitwise-copy-bit (0) #f ()) (bitwise-bit-field variable bitwise-bit-field (0) #f ()) (bitwise-copy-bit-field variable bitwise-copy-bit-field (0) #f ()) (bitwise-rotate-bit-field variable bitwise-rotate-bit-field (0) #f ()) (bitwise-reverse-bit-field variable bitwise-reverse-bit-field (0) #f ()) (bitwise-arithmetic-shift variable bitwise-arithmetic-shift (0) #f ()) (bitwise-arithmetic-shift-left variable bitwise-arithmetic-shift-left (0) #f ()) (bitwise-arithmetic-shift-right variable bitwise-arithmetic-shift-right (0) #f ()))) (quote ()) (quote ()) (lambda () (values)) (lambda () (values)) (quote &build~164~1614))) (values))
(begin (ex:register-library! (ex:make-library (quote (rnrs files)) (lambda () (quote ())) (quote ((file-exists? variable file-exists? (0) #f ()) (delete-file variable delete-file (0) #f ()))) (quote ()) (quote ()) (lambda () (values)) (lambda () (values)) (quote &build~164~1615))) (values))
(begin (ex:register-library! (ex:make-library (quote (rnrs syntax-case)) (lambda () (quote ())) (quote ((make-variable-transformer variable ex:make-variable-transformer (0) #f ()) (identifier? variable ex:identifier? (0) #f ()) (bound-identifier=? variable ex:bound-identifier=? (0) #f ()) (free-identifier=? variable ex:free-identifier=? (0) #f ()) (generate-temporaries variable ex:generate-temporaries (0) #f ()) (datum->syntax variable ex:datum->syntax (0) #f ()) (syntax->datum variable ex:syntax->datum (0) #f ()) (syntax-violation variable ex:syntax-violation (0) #f ()) (syntax macro syntax (0) #f ()) (syntax-case macro syntax-case (0) #f ()) (quasisyntax macro &quasisyntax~164~536 (0) #f (core quasisyntax)) (unsyntax macro &unsyntax~164~754 (0) #f (core quasisyntax)) (unsyntax-splicing macro &unsyntax-splicing~164~758 (0) #f (core quasisyntax)) (with-syntax macro &with-syntax~164~3 (0) #f (core with-syntax)) (_ macro _ (0) #f ()) (... macro ... (0) #f ()))) (quote (((core quasisyntax) 0) ((core with-syntax) 0) ((core primitives) 0))) (quote (&build~164~762 &build~164~57 &build~164~2)) (lambda () (values)) (lambda () (values)) (quote &build~164~1616))) (values))
(begin (ex:register-library! (ex:make-library (quote (rnrs base)) (lambda () (ex:uncompress (quote (((&env~164~1627 0 1 2 3)) (3 (((assert) macro &assert~164~1617 (0) #f (rnrs base)) ((undefined) variable ex:undefined (0) #f ()) ((eval) variable ex:eval (0) #f ()) ((environment-bindings) variable ex:environment-bindings (0) #f ()) ((environment) variable ex:environment (0) #f ()) ((syntax-violation) variable ex:syntax-violation (0) #f ()) ((syntax->datum) variable ex:syntax->datum (0) #f ()) ((datum->syntax) variable ex:datum->syntax (0) #f ()) ((generate-temporaries) variable ex:generate-temporaries (0) #f ()) ((free-identifier=?) variable ex:free-identifier=? (0) #f ()) ((bound-identifier=?) variable ex:bound-identifier=? (0) #f ()) ((identifier?) variable ex:identifier? (0) #f ()) ((make-variable-transformer) variable ex:make-variable-transformer (0) #f ()) ((syntax-case) macro syntax-case (0) #f ()) ((syntax) macro syntax (0) #f ()) ((letrec-syntax) macro letrec-syntax (0) #f ()) ((let-syntax) macro let-syntax (0) #f ()) ((define-syntax) macro define-syntax (0) #f ()) ((define) macro define (0) #f ()) ((or) macro or (0) #f ()) ((and) macro and (0) #f ()) ((quote) macro quote (0) #f ()) ((lambda) macro lambda (0) #f ()) ((if) macro if (0) #f ()) ((begin) macro begin (0) #f ()) ((letrec*) macro &letrec*~164~179 (0) #f (core let)) ((letrec) macro &letrec~164~148 (0) #f (core let)) ((let) macro &let~164~100 (0) #f (core let)) ((=>) macro &=>~164~481 (0) #f (core derived)) ((else) macro &else~164~485 (0) #f (core derived)) ((case) macro &case~164~401 (0) #f (core derived)) ((cond) macro &cond~164~285 (0) #f (core derived)) ((let*) macro &let*~164~204 (0) #f (core derived)) ((unquote-splicing) macro &unquote-splicing~164~1201 (0) #f (core quasiquote)) ((unquote) macro &unquote~164~1197 (0) #f (core quasiquote)) ((quasiquote) macro &quasiquote~164~763 (0) #f (core quasiquote)) ((let*-values) macro &let*-values~164~1346 (0) #f (core let-values)) ((let-values) macro &let-values~164~1206 (0) #f (core let-values)) ((syntax-rules) macro &syntax-rules~164~58 (1) #f (core syntax-rules)) ((identifier-syntax) macro &identifier-syntax~164~490 (1) #f (core identifier-syntax)) ((...) macro ... (1) #f ()) ((_) macro _ (1) #f ()) ((set!) macro set! (1 0) #f ()) ((call/cc) variable call/cc (0) #f ()) ((assertion-violation) variable assertion-violation (0) #f ()) ((error) variable error (0) #f ()) ((vector-for-each) variable vector-for-each (0) #f ()) ((vector-map) variable vector-map (0) #f ()) ((string-for-each) variable string-for-each (0) #f ()) ((symbol=?) variable symbol=? (0) #f ()) ((boolean=?) variable boolean=? (0) #f ()) ((exact-integer-sqrt) variable exact-integer-sqrt (0) #f ()) ((div0-and-mod0) variable div0-and-mod0 (0) #f ()) ((mod0) variable mod0 (0) #f ()) ((div0) variable div0 (0) #f ()) ((div-and-mod) variable div-and-mod (0) #f ()) ((mod) variable mod (0) #f ()) ((div) variable div (0) #f ()) ((nan?) variable nan? (0) #f ()) ((infinite?) variable infinite? (0) #f ()) ((finite?) variable finite? (0) #f ()) ((inexact) variable inexact (0) #f ()) ((exact) variable exact (0) #f ()) ((integer-valued?) variable integer-valued? (0) #f ()) ((rational-valued?) variable rational-valued? (0) #f ()) ((real-valued?) variable real-valued? (0) #f ()) ((zero?) variable zero? (0) #f ()) ((vector?) variable vector? (0) #f ()) ((vector-set!) variable vector-set! (0) #f ()) ((vector-ref) variable vector-ref (0) #f ()) ((vector-length) variable vector-length (0) #f ()) ((vector-fill!) variable vector-fill! (0) #f ()) ((vector->list) variable vector->list (0) #f ()) ((vector) variable vector (0) #f ()) ((values) variable values (0) #f ()) ((truncate) variable truncate (0) #f ()) ((tan) variable tan (0) #f ()) ((symbol?) variable symbol? (0) #f ()) ((symbol->string) variable symbol->string (0) #f ()) ((substring) variable substring (0) #f ()) ((string?) variable string? (0) #f ()) ((string>?) variable string>? (0) #f ()) ((string>=?) variable string>=? (0) #f ()) ((string=?) variable string=? (0) #f ()) ((string<?) variable string<? (0) #f ()) ((string<=?) variable string<=? (0) #f ()) ((string-ref) variable string-ref (0) #f ()) ((string-length) variable string-length (0) #f ()) ((string-copy) variable string-copy (0) #f ()) ((string-append) variable string-append (0) #f ()) ((string->symbol) variable string->symbol (0) #f ()) ((string->number) variable string->number (0) #f ()) ((string->list) variable string->list (0) #f ()) ((string) variable string (0) #f ()) ((sqrt) variable sqrt (0) #f ()) ((sin) variable sin (0) #f ()) ((round) variable round (0) #f ()) ((reverse) variable reverse (0) #f ()) ((real?) variable real? (0) #f ()) ((real-part) variable real-part (0) #f ()) ((rationalize) variable rationalize (0) #f ()) ((rational?) variable rational? (0) #f ()) ((procedure?) variable procedure? (0) #f ()) ((positive?) variable positive? (0) #f ()) ((pair?) variable pair? (0) #f ()) ((odd?) variable odd? (0) #f ()) ((numerator) variable numerator (0) #f ()) ((number?) variable number? (0) #f ()) ((number->string) variable number->string (0) #f ()) ((null?) variable null? (0) #f ()) ((not) variable not (0) #f ()) ((negative?) variable negative? (0) #f ()) ((min) variable min (0) #f ()) ((max) variable max (0) #f ()) ((map) variable map (0) #f ()) ((make-vector) variable make-vector (0) #f ()) ((make-string) variable make-string (0) #f ()) ((make-rectangular) variable make-rectangular (0) #f ()) ((make-polar) variable make-polar (0) #f ()) ((magnitude) variable magnitude (0) #f ()) ((log) variable log (0) #f ()) ((list?) variable list? (0) #f ()) ((list-tail) variable list-tail (0) #f ()) ((list-ref) variable list-ref (0) #f ()) ((list->vector) variable list->vector (0) #f ()) ((list->string) variable list->string (0) #f ()) ((list) variable list (0) #f ()) ((length) variable length (0) #f ()) ((lcm) variable lcm (0) #f ()) ((integer?) variable integer? (0) #f ()) ((integer->char) variable integer->char (0) #f ()) ((inexact?) variable inexact? (0) #f ()) ((imag-part) variable imag-part (0) #f ()) ((gcd) variable gcd (0) #f ()) ((for-each) variable for-each (0) #f ()) ((floor) variable floor (0) #f ()) ((expt) variable expt (0) #f ()) ((exp) variable exp (0) #f ()) ((exact?) variable exact? (0) #f ()) ((even?) variable even? (0) #f ()) ((eqv?) variable eqv? (0) #f ()) ((equal?) variable equal? (0) #f ()) ((eq?) variable eq? (0) #f ()) ((dynamic-wind) variable dynamic-wind (0) #f ()) ((denominator) variable denominator (0) #f ()) ((cos) variable cos (0) #f ()) ((cons) variable cons (0) #f ()) ((complex?) variable complex? (0) #f ()) ((char->integer) variable char->integer (0) #f ()) ((char?) variable char? (0) #f ()) ((ceiling) variable ceiling (0) #f ()) ((cddddr) variable cddddr (0) #f ()) ((cdddar) variable cdddar (0) #f ()) ((cddadr) variable cddadr (0) #f ()) ((cddaar) variable cddaar (0) #f ()) ((cdaddr) variable cdaddr (0) #f ()) ((cdadar) variable cdadar (0) #f ()) ((cdaadr) variable cdaadr (0) #f ()) ((cdaaar) variable cdaaar (0) #f ()) ((cadddr) variable cadddr (0) #f ()) ((caddar) variable caddar (0) #f ()) ((cadadr) variable cadadr (0) #f ()) ((cadaar) variable cadaar (0) #f ()) ((caaddr) variable caaddr (0) #f ()) ((caadar) variable caadar (0) #f ()) ((caaadr) variable caaadr (0) #f ()) ((caaaar) variable caaaar (0) #f ()) ((cdddr) variable cdddr (0) #f ()) ((cddar) variable cddar (0) #f ()) ((cdadr) variable cdadr (0) #f ()) ((cdaar) variable cdaar (0) #f ()) ((caddr) variable caddr (0) #f ()) ((cadar) variable cadar (0) #f ()) ((caadr) variable caadr (0) #f ()) ((caaar) variable caaar (0) #f ()) ((cddr) variable cddr (0) #f ()) ((cdar) variable cdar (0) #f ()) ((cadr) variable cadr (0) #f ()) ((caar) variable caar (0) #f ()) ((cdr) variable cdr (0) #f ()) ((car) variable car (0) #f ()) ((call-with-values) variable call-with-values (0) #f ()) ((call-with-current-continuation) variable call-with-current-continuation (0) #f ()) ((boolean?) variable boolean? (0) #f ()) ((atan) variable atan (0) #f ()) ((asin) variable asin (0) #f ()) ((apply) variable apply (0) #f ()) ((append) variable append (0) #f ()) ((acos) variable acos (0) #f ()) ((abs) variable abs (0) #f ()) ((>=) variable >= (0) #f ()) ((>) variable > (0) #f ()) ((=) variable = (0) #f ()) ((<=) variable <= (0) #f ()) ((<) variable < (0) #f ()) ((/) variable / (0) #f ()) ((-) variable - (0) #f ()) ((+) variable + (0) #f ()) ((*) variable * (0) #f ()))) (2 (((x &c~164~1618) variable &x~164~1620 (0) #f (rnrs base)))) (1 ()) (0 (((dummy &c~164~1618) . #f) ((expression) . #f))))))) (quote ((begin macro begin (0) #f ()) (if macro if (0) #f ()) (lambda macro lambda (0) #f ()) (quote macro quote (0) #f ()) (set! macro set! (1 0) #f ()) (and macro and (0) #f ()) (or macro or (0) #f ()) (define macro define (0) #f ()) (define-syntax macro define-syntax (0) #f ()) (let-syntax macro let-syntax (0) #f ()) (letrec-syntax macro letrec-syntax (0) #f ()) (_ macro _ (1) #f ()) (... macro ... (1) #f ()) (let macro &let~164~100 (0) #f (core let)) (let* macro &let*~164~204 (0) #f (core derived)) (letrec macro &letrec~164~148 (0) #f (core let)) (letrec* macro &letrec*~164~179 (0) #f (core let)) (let-values macro &let-values~164~1206 (0) #f (core let-values)) (let*-values macro &let*-values~164~1346 (0) #f (core let-values)) (case macro &case~164~401 (0) #f (core derived)) (cond macro &cond~164~285 (0) #f (core derived)) (else macro &else~164~485 (0) #f (core derived)) (=> macro &=>~164~481 (0) #f (core derived)) (assert macro &assert~164~1617 (0) #f (rnrs base)) (quasiquote macro &quasiquote~164~763 (0) #f (core quasiquote)) (unquote macro &unquote~164~1197 (0) #f (core quasiquote)) (unquote-splicing macro &unquote-splicing~164~1201 (0) #f (core quasiquote)) (syntax-rules macro &syntax-rules~164~58 (1) #f (core syntax-rules)) (identifier-syntax macro &identifier-syntax~164~490 (1) #f (core identifier-syntax)) (* variable * (0) #f ()) (+ variable + (0) #f ()) (- variable - (0) #f ()) (/ variable / (0) #f ()) (< variable < (0) #f ()) (<= variable <= (0) #f ()) (= variable = (0) #f ()) (> variable > (0) #f ()) (>= variable >= (0) #f ()) (abs variable abs (0) #f ()) (acos variable acos (0) #f ()) (append variable append (0) #f ()) (apply variable apply (0) #f ()) (asin variable asin (0) #f ()) (atan variable atan (0) #f ()) (boolean? variable boolean? (0) #f ()) (call-with-current-continuation variable call-with-current-continuation (0) #f ()) (call-with-values variable call-with-values (0) #f ()) (car variable car (0) #f ()) (cdr variable cdr (0) #f ()) (caar variable caar (0) #f ()) (cadr variable cadr (0) #f ()) (cdar variable cdar (0) #f ()) (cddr variable cddr (0) #f ()) (caaar variable caaar (0) #f ()) (caadr variable caadr (0) #f ()) (cadar variable cadar (0) #f ()) (caddr variable caddr (0) #f ()) (cdaar variable cdaar (0) #f ()) (cdadr variable cdadr (0) #f ()) (cddar variable cddar (0) #f ()) (cdddr variable cdddr (0) #f ()) (caaaar variable caaaar (0) #f ()) (caaadr variable caaadr (0) #f ()) (caadar variable caadar (0) #f ()) (caaddr variable caaddr (0) #f ()) (cadaar variable cadaar (0) #f ()) (cadadr variable cadadr (0) #f ()) (caddar variable caddar (0) #f ()) (cadddr variable cadddr (0) #f ()) (cdaaar variable cdaaar (0) #f ()) (cdaadr variable cdaadr (0) #f ()) (cdadar variable cdadar (0) #f ()) (cdaddr variable cdaddr (0) #f ()) (cddaar variable cddaar (0) #f ()) (cddadr variable cddadr (0) #f ()) (cdddar variable cdddar (0) #f ()) (cddddr variable cddddr (0) #f ()) (ceiling variable ceiling (0) #f ()) (char? variable char? (0) #f ()) (char->integer variable char->integer (0) #f ()) (complex? variable complex? (0) #f ()) (cons variable cons (0) #f ()) (cos variable cos (0) #f ()) (denominator variable denominator (0) #f ()) (dynamic-wind variable dynamic-wind (0) #f ()) (eq? variable eq? (0) #f ()) (equal? variable equal? (0) #f ()) (eqv? variable eqv? (0) #f ()) (even? variable even? (0) #f ()) (exact? variable exact? (0) #f ()) (exp variable exp (0) #f ()) (expt variable expt (0) #f ()) (floor variable floor (0) #f ()) (for-each variable for-each (0) #f ()) (gcd variable gcd (0) #f ()) (imag-part variable imag-part (0) #f ()) (inexact? variable inexact? (0) #f ()) (integer->char variable integer->char (0) #f ()) (integer? variable integer? (0) #f ()) (lcm variable lcm (0) #f ()) (length variable length (0) #f ()) (list variable list (0) #f ()) (list->string variable list->string (0) #f ()) (list->vector variable list->vector (0) #f ()) (list-ref variable list-ref (0) #f ()) (list-tail variable list-tail (0) #f ()) (list? variable list? (0) #f ()) (log variable log (0) #f ()) (magnitude variable magnitude (0) #f ()) (make-polar variable make-polar (0) #f ()) (make-rectangular variable make-rectangular (0) #f ()) (make-string variable make-string (0) #f ()) (make-vector variable make-vector (0) #f ()) (map variable map (0) #f ()) (max variable max (0) #f ()) (min variable min (0) #f ()) (negative? variable negative? (0) #f ()) (not variable not (0) #f ()) (null? variable null? (0) #f ()) (number->string variable number->string (0) #f ()) (number? variable number? (0) #f ()) (numerator variable numerator (0) #f ()) (odd? variable odd? (0) #f ()) (pair? variable pair? (0) #f ()) (positive? variable positive? (0) #f ()) (procedure? variable procedure? (0) #f ()) (rational? variable rational? (0) #f ()) (rationalize variable rationalize (0) #f ()) (real-part variable real-part (0) #f ()) (real? variable real? (0) #f ()) (reverse variable reverse (0) #f ()) (round variable round (0) #f ()) (sin variable sin (0) #f ()) (sqrt variable sqrt (0) #f ()) (string variable string (0) #f ()) (string->list variable string->list (0) #f ()) (string->number variable string->number (0) #f ()) (string->symbol variable string->symbol (0) #f ()) (string-append variable string-append (0) #f ()) (string-copy variable string-copy (0) #f ()) (string-length variable string-length (0) #f ()) (string-ref variable string-ref (0) #f ()) (string<=? variable string<=? (0) #f ()) (string<? variable string<? (0) #f ()) (string=? variable string=? (0) #f ()) (string>=? variable string>=? (0) #f ()) (string>? variable string>? (0) #f ()) (string? variable string? (0) #f ()) (substring variable substring (0) #f ()) (symbol->string variable symbol->string (0) #f ()) (symbol? variable symbol? (0) #f ()) (tan variable tan (0) #f ()) (truncate variable truncate (0) #f ()) (values variable values (0) #f ()) (vector variable vector (0) #f ()) (vector->list variable vector->list (0) #f ()) (vector-fill! variable vector-fill! (0) #f ()) (vector-length variable vector-length (0) #f ()) (vector-ref variable vector-ref (0) #f ()) (vector-set! variable vector-set! (0) #f ()) (vector? variable vector? (0) #f ()) (zero? variable zero? (0) #f ()) (real-valued? variable real-valued? (0) #f ()) (rational-valued? variable rational-valued? (0) #f ()) (integer-valued? variable integer-valued? (0) #f ()) (exact variable exact (0) #f ()) (inexact variable inexact (0) #f ()) (finite? variable finite? (0) #f ()) (infinite? variable infinite? (0) #f ()) (nan? variable nan? (0) #f ()) (div variable div (0) #f ()) (mod variable mod (0) #f ()) (div-and-mod variable div-and-mod (0) #f ()) (div0 variable div0 (0) #f ()) (mod0 variable mod0 (0) #f ()) (div0-and-mod0 variable div0-and-mod0 (0) #f ()) (exact-integer-sqrt variable exact-integer-sqrt (0) #f ()) (boolean=? variable boolean=? (0) #f ()) (symbol=? variable symbol=? (0) #f ()) (string-for-each variable string-for-each (0) #f ()) (vector-map variable vector-map (0) #f ()) (vector-for-each variable vector-for-each (0) #f ()) (error variable error (0) #f ()) (assertion-violation variable assertion-violation (0) #f ()) (call/cc variable call/cc (0) #f ()))) (quote (((core primitives) 1) ((core identifier-syntax) 1) ((core syntax-rules) 1) ((core let-values) 0) ((core quasiquote) 0) ((core derived) 0) ((core let) 0) ((core primitives) 0))) (quote (&build~164~2 &build~164~535 &build~164~99 &build~164~1380 &build~164~1205 &build~164~489 &build~164~203 &build~164~2)) (lambda () (ex:register-macro! (quote &assert~164~1617) (lambda (&x~164~1620) (let ((&input~164~1622 &x~164~1620)) (let ((&fail~164~1623 (lambda () (ex:invalid-form &input~164~1622)))) (if (pair? &input~164~1622) (let ((&temp~164~1631 (car &input~164~1622))) (let ((&dummy~164~1625 &temp~164~1631)) (let ((&temp~164~1628 (cdr &input~164~1622))) (if (pair? &temp~164~1628) (let ((&temp~164~1630 (car &temp~164~1628))) (let ((&expression~164~1624 &temp~164~1630)) (let ((&temp~164~1629 (cdr &temp~164~1628))) (if (null? &temp~164~1629) (cons (ex:syntax-rename (quote if) (quote ()) (quote (&env~164~1627)) 0 (quote (rnrs base))) (cons (cons (ex:syntax-rename (quote not) (quote ()) (quote (&env~164~1627)) 0 (quote (rnrs base))) (cons &expression~164~1624 (quote ()))) (cons (cons (ex:syntax-rename (quote assertion-violation) (quote ()) (quote (&env~164~1627)) 0 (quote (rnrs base))) (cons (quote #f) (cons (quote "assertion failed") (cons (cons (ex:syntax-rename (quote quote) (quote ()) (quote (&env~164~1627)) 0 (quote (rnrs base))) (cons &expression~164~1624 (quote ()))) (quote ()))))) (quote ())))) (&fail~164~1623))))) (&fail~164~1623))))) (&fail~164~1623)))))) (values)) (lambda () (values)) (quote &build~164~1632))) (values))
(begin (ex:register-library! (ex:make-library (quote (rnrs)) (lambda () (quote ())) (quote ((begin macro begin (0 1) #f ()) (if macro if (0 1) #f ()) (lambda macro lambda (0 1) #f ()) (quote macro quote (0 1) #f ()) (set! macro set! (0 2 1) #f ()) (and macro and (0 1) #f ()) (or macro or (0 1) #f ()) (define macro define (0 1) #f ()) (define-syntax macro define-syntax (0 1) #f ()) (let-syntax macro let-syntax (0 1) #f ()) (letrec-syntax macro letrec-syntax (0 1) #f ()) (_ macro _ (0 1) #f ()) (... macro ... (0 1) #f ()) (let macro &let~164~100 (0 1) #f (core let)) (let* macro &let*~164~204 (0 1) #f (core derived)) (letrec macro &letrec~164~148 (0 1) #f (core let)) (letrec* macro &letrec*~164~179 (0 1) #f (core let)) (let-values macro &let-values~164~1206 (0 1) #f (core let-values)) (let*-values macro &let*-values~164~1346 (0 1) #f (core let-values)) (case macro &case~164~401 (0 1) #f (core derived)) (cond macro &cond~164~285 (0 1) #f (core derived)) (else macro &else~164~485 (0 1) #f (core derived)) (=> macro &=>~164~481 (0 1) #f (core derived)) (assert macro &assert~164~1617 (0 1) #f (rnrs base)) (quasiquote macro &quasiquote~164~763 (0 1) #f (core quasiquote)) (unquote macro &unquote~164~1197 (0 1) #f (core quasiquote)) (unquote-splicing macro &unquote-splicing~164~1201 (0 1) #f (core quasiquote)) (syntax-rules macro &syntax-rules~164~58 (0 1) #f (core syntax-rules)) (identifier-syntax macro &identifier-syntax~164~490 (0 1) #f (core identifier-syntax)) (* variable * (0 1) #f ()) (+ variable + (0 1) #f ()) (- variable - (0 1) #f ()) (/ variable / (0 1) #f ()) (< variable < (0 1) #f ()) (<= variable <= (0 1) #f ()) (= variable = (0 1) #f ()) (> variable > (0 1) #f ()) (>= variable >= (0 1) #f ()) (abs variable abs (0 1) #f ()) (acos variable acos (0 1) #f ()) (append variable append (0 1) #f ()) (apply variable apply (0 1) #f ()) (asin variable asin (0 1) #f ()) (atan variable atan (0 1) #f ()) (boolean? variable boolean? (0 1) #f ()) (call-with-current-continuation variable call-with-current-continuation (0 1) #f ()) (call-with-values variable call-with-values (0 1) #f ()) (car variable car (0 1) #f ()) (cdr variable cdr (0 1) #f ()) (caar variable caar (0 1) #f ()) (cadr variable cadr (0 1) #f ()) (cdar variable cdar (0 1) #f ()) (cddr variable cddr (0 1) #f ()) (caaar variable caaar (0 1) #f ()) (caadr variable caadr (0 1) #f ()) (cadar variable cadar (0 1) #f ()) (caddr variable caddr (0 1) #f ()) (cdaar variable cdaar (0 1) #f ()) (cdadr variable cdadr (0 1) #f ()) (cddar variable cddar (0 1) #f ()) (cdddr variable cdddr (0 1) #f ()) (caaaar variable caaaar (0 1) #f ()) (caaadr variable caaadr (0 1) #f ()) (caadar variable caadar (0 1) #f ()) (caaddr variable caaddr (0 1) #f ()) (cadaar variable cadaar (0 1) #f ()) (cadadr variable cadadr (0 1) #f ()) (caddar variable caddar (0 1) #f ()) (cadddr variable cadddr (0 1) #f ()) (cdaaar variable cdaaar (0 1) #f ()) (cdaadr variable cdaadr (0 1) #f ()) (cdadar variable cdadar (0 1) #f ()) (cdaddr variable cdaddr (0 1) #f ()) (cddaar variable cddaar (0 1) #f ()) (cddadr variable cddadr (0 1) #f ()) (cdddar variable cdddar (0 1) #f ()) (cddddr variable cddddr (0 1) #f ()) (ceiling variable ceiling (0 1) #f ()) (char? variable char? (0 1) #f ()) (char->integer variable char->integer (0 1) #f ()) (complex? variable complex? (0 1) #f ()) (cons variable cons (0 1) #f ()) (cos variable cos (0 1) #f ()) (denominator variable denominator (0 1) #f ()) (dynamic-wind variable dynamic-wind (0 1) #f ()) (eq? variable eq? (0 1) #f ()) (equal? variable equal? (0 1) #f ()) (eqv? variable eqv? (0 1) #f ()) (even? variable even? (0 1) #f ()) (exact? variable exact? (0 1) #f ()) (exp variable exp (0 1) #f ()) (expt variable expt (0 1) #f ()) (floor variable floor (0 1) #f ()) (for-each variable for-each (0 1) #f ()) (gcd variable gcd (0 1) #f ()) (imag-part variable imag-part (0 1) #f ()) (inexact? variable inexact? (0 1) #f ()) (integer->char variable integer->char (0 1) #f ()) (integer? variable integer? (0 1) #f ()) (lcm variable lcm (0 1) #f ()) (length variable length (0 1) #f ()) (list variable list (0 1) #f ()) (list->string variable list->string (0 1) #f ()) (list->vector variable list->vector (0 1) #f ()) (list-ref variable list-ref (0 1) #f ()) (list-tail variable list-tail (0 1) #f ()) (list? variable list? (0 1) #f ()) (log variable log (0 1) #f ()) (magnitude variable magnitude (0 1) #f ()) (make-polar variable make-polar (0 1) #f ()) (make-rectangular variable make-rectangular (0 1) #f ()) (make-string variable make-string (0 1) #f ()) (make-vector variable make-vector (0 1) #f ()) (map variable map (0 1) #f ()) (max variable max (0 1) #f ()) (min variable min (0 1) #f ()) (negative? variable negative? (0 1) #f ()) (not variable not (0 1) #f ()) (null? variable null? (0 1) #f ()) (number->string variable number->string (0 1) #f ()) (number? variable number? (0 1) #f ()) (numerator variable numerator (0 1) #f ()) (odd? variable odd? (0 1) #f ()) (pair? variable pair? (0 1) #f ()) (positive? variable positive? (0 1) #f ()) (procedure? variable procedure? (0 1) #f ()) (rational? variable rational? (0 1) #f ()) (rationalize variable rationalize (0 1) #f ()) (real-part variable real-part (0 1) #f ()) (real? variable real? (0 1) #f ()) (reverse variable reverse (0 1) #f ()) (round variable round (0 1) #f ()) (sin variable sin (0 1) #f ()) (sqrt variable sqrt (0 1) #f ()) (string variable string (0 1) #f ()) (string->list variable string->list (0 1) #f ()) (string->number variable string->number (0 1) #f ()) (string->symbol variable string->symbol (0 1) #f ()) (string-append variable string-append (0 1) #f ()) (string-copy variable string-copy (0 1) #f ()) (string-length variable string-length (0 1) #f ()) (string-ref variable string-ref (0 1) #f ()) (string<=? variable string<=? (0 1) #f ()) (string<? variable string<? (0 1) #f ()) (string=? variable string=? (0 1) #f ()) (string>=? variable string>=? (0 1) #f ()) (string>? variable string>? (0 1) #f ()) (string? variable string? (0 1) #f ()) (substring variable substring (0 1) #f ()) (symbol->string variable symbol->string (0 1) #f ()) (symbol? variable symbol? (0 1) #f ()) (tan variable tan (0 1) #f ()) (truncate variable truncate (0 1) #f ()) (values variable values (0 1) #f ()) (vector variable vector (0 1) #f ()) (vector->list variable vector->list (0 1) #f ()) (vector-fill! variable vector-fill! (0 1) #f ()) (vector-length variable vector-length (0 1) #f ()) (vector-ref variable vector-ref (0 1) #f ()) (vector-set! variable vector-set! (0 1) #f ()) (vector? variable vector? (0 1) #f ()) (zero? variable zero? (0 1) #f ()) (real-valued? variable real-valued? (0 1) #f ()) (rational-valued? variable rational-valued? (0 1) #f ()) (integer-valued? variable integer-valued? (0 1) #f ()) (exact variable exact (0 1) #f ()) (inexact variable inexact (0 1) #f ()) (finite? variable finite? (0 1) #f ()) (infinite? variable infinite? (0 1) #f ()) (nan? variable nan? (0 1) #f ()) (div variable div (0 1) #f ()) (mod variable mod (0 1) #f ()) (div-and-mod variable div-and-mod (0 1) #f ()) (div0 variable div0 (0 1) #f ()) (mod0 variable mod0 (0 1) #f ()) (div0-and-mod0 variable div0-and-mod0 (0 1) #f ()) (exact-integer-sqrt variable exact-integer-sqrt (0 1) #f ()) (boolean=? variable boolean=? (0 1) #f ()) (symbol=? variable symbol=? (0 1) #f ()) (string-for-each variable string-for-each (0 1) #f ()) (vector-map variable vector-map (0 1) #f ()) (vector-for-each variable vector-for-each (0 1) #f ()) (error variable error (0 1) #f ()) (assertion-violation variable assertion-violation (0 1) #f ()) (call/cc variable call/cc (0 1) #f ()) (make-variable-transformer variable ex:make-variable-transformer (0 1) #f ()) (identifier? variable ex:identifier? (0 1) #f ()) (bound-identifier=? variable ex:bound-identifier=? (0 1) #f ()) (free-identifier=? variable ex:free-identifier=? (0 1) #f ()) (generate-temporaries variable ex:generate-temporaries (0 1) #f ()) (datum->syntax variable ex:datum->syntax (0 1) #f ()) (syntax->datum variable ex:syntax->datum (0 1) #f ()) (syntax-violation variable ex:syntax-violation (0 1) #f ()) (syntax macro syntax (0 1) #f ()) (syntax-case macro syntax-case (0 1) #f ()) (quasisyntax macro &quasisyntax~164~536 (0 1) #f (core quasisyntax)) (unsyntax macro &unsyntax~164~754 (0 1) #f (core quasisyntax)) (unsyntax-splicing macro &unsyntax-splicing~164~758 (0 1) #f (core quasisyntax)) (with-syntax macro &with-syntax~164~3 (0 1) #f (core with-syntax)) (when macro &when~164~1381 (0 1) #f (rnrs control)) (unless macro &unless~164~1400 (0 1) #f (rnrs control)) (do macro &do~164~1419 (0 1) #f (rnrs control)) (case-lambda macro &case-lambda~164~1479 (0 1) #f (rnrs control)) (find variable find (0 1) #f ()) (for-all variable for-all (0 1) #f ()) (exists variable exists (0 1) #f ()) (filter variable filter (0 1) #f ()) (partition variable partition (0 1) #f ()) (fold-left variable fold-left (0 1) #f ()) (fold-right variable fold-right (0 1) #f ()) (remp variable remp (0 1) #f ()) (remove variable remove (0 1) #f ()) (remq variable remq (0 1) #f ()) (remv variable remv (0 1) #f ()) (memp variable memp (0 1) #f ()) (member variable member (0 1) #f ()) (memv variable memv (0 1) #f ()) (memq variable memq (0 1) #f ()) (assp variable assp (0 1) #f ()) (assoc variable assoc (0 1) #f ()) (assv variable assv (0 1) #f ()) (assq variable assq (0 1) #f ()) (call-with-input-file variable call-with-input-file (0 1) #f ()) (call-with-output-file variable call-with-output-file (0 1) #f ()) (close-input-port variable close-input-port (0 1) #f ()) (close-output-port variable close-output-port (0 1) #f ()) (current-input-port variable current-input-port (0 1) #f ()) (current-output-port variable current-output-port (0 1) #f ()) (display variable display (0 1) #f ()) (eof-object? variable eof-object? (0 1) #f ()) (newline variable newline (0 1) #f ()) (open-input-file variable open-input-file (0 1) #f ()) (open-output-file variable open-output-file (0 1) #f ()) (peek-char variable peek-char (0 1) #f ()) (read variable read (0 1) #f ()) (read-char variable read-char (0 1) #f ()) (with-input-from-file variable with-input-from-file (0 1) #f ()) (with-output-to-file variable with-output-to-file (0 1) #f ()) (write variable write (0 1) #f ()) (write-char variable write-char (0 1) #f ()) (char-upcase variable char-upcase (0 1) #f ()) (char-downcase variable char-downcase (0 1) #f ()) (char-titlecase variable char-titlecase (0 1) #f ()) (char-foldcase variable char-foldcase (0 1) #f ()) (char-ci=? variable char-ci=? (0 1) #f ()) (char-ci<? variable char-ci<? (0 1) #f ()) (char-ci>? variable char-ci>? (0 1) #f ()) (char-ci<=? variable char-ci<=? (0 1) #f ()) (char-ci>=? variable char-ci>=? (0 1) #f ()) (char-alphabetic? variable char-alphabetic? (0 1) #f ()) (char-numeric? variable char-numeric? (0 1) #f ()) (char-whitespace? variable char-whitespace? (0 1) #f ()) (char-upper-case? variable char-upper-case? (0 1) #f ()) (char-lower-case? variable char-lower-case? (0 1) #f ()) (char-title-case? variable char-title-case? (0 1) #f ()) (char-general-category variable char-general-category (0 1) #f ()) (string-upcase variable string-upcase (0 1) #f ()) (string-downcase variable string-downcase (0 1) #f ()) (string-titlecase variable string-titlecase (0 1) #f ()) (string-foldcase variable string-foldcase (0 1) #f ()) (string-ci=? variable string-ci=? (0 1) #f ()) (string-ci<? variable string-ci<? (0 1) #f ()) (string-ci>? variable string-ci>? (0 1) #f ()) (string-ci<=? variable string-ci<=? (0 1) #f ()) (string-ci>=? variable string-ci>=? (0 1) #f ()) (string-normalize-nfd variable string-normalize-nfd (0 1) #f ()) (string-normalize-nfkd variable string-normalize-nfkd (0 1) #f ()) (string-normalize-nfc variable string-normalize-nfc (0 1) #f ()) (string-normalize-nfkc variable string-normalize-nfkc (0 1) #f ()) (list-sort variable list-sort (0 1) #f ()) (vector-sort variable vector-sort (0 1) #f ()) (vector-sort! variable vector-sort! (0 1) #f ()) (make-record-type-descriptor variable make-record-type-descriptor (0 1) #f ()) (record-type-descriptor? variable record-type-descriptor? (0 1) #f ()) (make-record-constructor-descriptor variable make-record-constructor-descriptor (0 1) #f ()) (record-constructor variable record-constructor (0 1) #f ()) (record-predicate variable record-predicate (0 1) #f ()) (record-accessor variable record-accessor (0 1) #f ()) (record-mutator variable record-mutator (0 1) #f ()) (record? variable record? (0 1) #f ()) (record-rtd variable record-rtd (0 1) #f ()) (record-type-name variable record-type-name (0 1) #f ()) (record-type-parent variable record-type-parent (0 1) #f ()) (record-type-uid variable record-type-uid (0 1) #f ()) (record-type-generative? variable record-type-generative? (0 1) #f ()) (record-type-sealed? variable record-type-sealed? (0 1) #f ()) (record-type-opaque? variable record-type-opaque? (0 1) #f ()) (record-type-field-names variable record-type-field-names (0 1) #f ()) (record-field-mutable? variable record-field-mutable? (0 1) #f ()) (fixnum? variable fixnum? (0 1) #f ()) (fixnum-width variable fixnum-width (0 1) #f ()) (least-fixnum variable least-fixnum (0 1) #f ()) (greatest-fixnum variable greatest-fixnum (0 1) #f ()) (fx=? variable fx=? (0 1) #f ()) (fx>? variable fx>? (0 1) #f ()) (fx<? variable fx<? (0 1) #f ()) (fx>=? variable fx>=? (0 1) #f ()) (fx<=? variable fx<=? (0 1) #f ()) (fxzero? variable fxzero? (0 1) #f ()) (fxpositive? variable fxpositive? (0 1) #f ()) (fxnegative? variable fxnegative? (0 1) #f ()) (fxodd? variable fxodd? (0 1) #f ()) (fxeven? variable fxeven? (0 1) #f ()) (fxmax variable fxmax (0 1) #f ()) (fxmin variable fxmin (0 1) #f ()) (fx+ variable fx+ (0 1) #f ()) (fx- variable fx- (0 1) #f ()) (fx* variable fx* (0 1) #f ()) (fxdiv-and-mod variable fxdiv-and-mod (0 1) #f ()) (fxdiv variable fxdiv (0 1) #f ()) (fxmod variable fxmod (0 1) #f ()) (fxdiv0-and-mod0 variable fxdiv0-and-mod0 (0 1) #f ()) (fxdiv0 variable fxdiv0 (0 1) #f ()) (fxmod0 variable fxmod0 (0 1) #f ()) (fx+/carry variable fx+/carry (0 1) #f ()) (fx-/carry variable fx-/carry (0 1) #f ()) (fx*/carry variable fx*/carry (0 1) #f ()) (fxnot variable fxnot (0 1) #f ()) (fxand variable fxand (0 1) #f ()) (fxior variable fxior (0 1) #f ()) (fxxor variable fxxor (0 1) #f ()) (fxif variable fxif (0 1) #f ()) (fxbit-count variable fxbit-count (0 1) #f ()) (fxlength variable fxlength (0 1) #f ()) (fxfirst-bit-set variable fxfirst-bit-set (0 1) #f ()) (fxbit-set? variable fxbit-set? (0 1) #f ()) (fxcopy-bit variable fxcopy-bit (0 1) #f ()) (fxbit-field variable fxbit-field (0 1) #f ()) (fxcopy-bit-field variable fxcopy-bit-field (0 1) #f ()) (fxrotate-bit-field variable fxrotate-bit-field (0 1) #f ()) (fxreverse-bit-field variable fxreverse-bit-field (0 1) #f ()) (fxarithmetic-shift variable fxarithmetic-shift (0 1) #f ()) (fxarithmetic-shift-left variable fxarithmetic-shift-left (0 1) #f ()) (fxarithmetic-shift-right variable fxarithmetic-shift-right (0 1) #f ()) (flonum? variable flonum? (0 1) #f ()) (real->flonum variable real->flonum (0 1) #f ()) (fl=? variable fl=? (0 1) #f ()) (fl<? variable fl<? (0 1) #f ()) (fl>? variable fl>? (0 1) #f ()) (fl<=? variable fl<=? (0 1) #f ()) (fl>=? variable fl>=? (0 1) #f ()) (flinteger? variable flinteger? (0 1) #f ()) (flzero? variable flzero? (0 1) #f ()) (flpositive? variable flpositive? (0 1) #f ()) (flnegative? variable flnegative? (0 1) #f ()) (flodd? variable flodd? (0 1) #f ()) (fleven? variable fleven? (0 1) #f ()) (flfinite? variable flfinite? (0 1) #f ()) (flinfinite? variable flinfinite? (0 1) #f ()) (flnan? variable flnan? (0 1) #f ()) (flmax variable flmax (0 1) #f ()) (flmin variable flmin (0 1) #f ()) (fl+ variable fl+ (0 1) #f ()) (fl* variable fl* (0 1) #f ()) (fl- variable fl- (0 1) #f ()) (fl/ variable fl/ (0 1) #f ()) (flabs variable flabs (0 1) #f ()) (fldiv-and-mod variable fldiv-and-mod (0 1) #f ()) (fldiv variable fldiv (0 1) #f ()) (flmod variable flmod (0 1) #f ()) (fldiv0-and-mod0 variable fldiv0-and-mod0 (0 1) #f ()) (fldiv0 variable fldiv0 (0 1) #f ()) (flmod0 variable flmod0 (0 1) #f ()) (flnumerator variable flnumerator (0 1) #f ()) (fldenominator variable fldenominator (0 1) #f ()) (flfloor variable flfloor (0 1) #f ()) (flceiling variable flceiling (0 1) #f ()) (fltruncate variable fltruncate (0 1) #f ()) (flround variable flround (0 1) #f ()) (flexp variable flexp (0 1) #f ()) (fllog variable fllog (0 1) #f ()) (flsin variable flsin (0 1) #f ()) (flcos variable flcos (0 1) #f ()) (fltan variable fltan (0 1) #f ()) (flasin variable flasin (0 1) #f ()) (flacos variable flacos (0 1) #f ()) (flatan variable flatan (0 1) #f ()) (flsqrt variable flsqrt (0 1) #f ()) (flexpt variable flexpt (0 1) #f ()) (fixnum->flonum variable fixnum->flonum (0 1) #f ()) (bitwise-not variable bitwise-not (0 1) #f ()) (bitwise-and variable bitwise-and (0 1) #f ()) (bitwise-ior variable bitwise-ior (0 1) #f ()) (bitwise-xor variable bitwise-xor (0 1) #f ()) (bitwise-if variable bitwise-if (0 1) #f ()) (bitwise-bit-count variable bitwise-bit-count (0 1) #f ()) (bitwise-length variable bitwise-length (0 1) #f ()) (bitwise-first-bit-set variable bitwise-first-bit-set (0 1) #f ()) (bitwise-bit-set? variable bitwise-bit-set? (0 1) #f ()) (bitwise-copy-bit variable bitwise-copy-bit (0 1) #f ()) (bitwise-bit-field variable bitwise-bit-field (0 1) #f ()) (bitwise-copy-bit-field variable bitwise-copy-bit-field (0 1) #f ()) (bitwise-rotate-bit-field variable bitwise-rotate-bit-field (0 1) #f ()) (bitwise-reverse-bit-field variable bitwise-reverse-bit-field (0 1) #f ()) (bitwise-arithmetic-shift variable bitwise-arithmetic-shift (0 1) #f ()) (bitwise-arithmetic-shift-left variable bitwise-arithmetic-shift-left (0 1) #f ()) (bitwise-arithmetic-shift-right variable bitwise-arithmetic-shift-right (0 1) #f ()) (file-exists? variable file-exists? (0 1) #f ()) (delete-file variable delete-file (0 1) #f ()))) (quote (((rnrs arithmetic bitwise) 0 1) ((rnrs arithmetic flonums) 0 1) ((rnrs arithmetic fixnums) 0 1) ((rnrs files) 0 1) ((rnrs records inspection) 0 1) ((rnrs records procedural) 0 1) ((rnrs sorting) 0 1) ((rnrs unicode) 0 1) ((rnrs io simple) 0 1) ((rnrs syntax-case) 0 1) ((rnrs lists) 0 1) ((rnrs control) 0 1) ((core identifier-syntax) 0 1) ((core syntax-rules) 0 1) ((rnrs base) 0 1) ((rnrs base) 0 1))) (quote (&build~164~1614 &build~164~1613 &build~164~1612 &build~164~1615 &build~164~1611 &build~164~1610 &build~164~1609 &build~164~1608 &build~164~1607 &build~164~1616 &build~164~1606 &build~164~1605 &build~164~535 &build~164~99 &build~164~1632 &build~164~1632)) (lambda () (values)) (lambda () (values)) (quote &build~164~1633))) (values))
(begin (ex:register-library! (ex:make-library (quote (rnrs mutable-pairs)) (lambda () (quote ())) (quote ((set-car! variable set-car! (0) #f ()) (set-cdr! variable set-cdr! (0) #f ()))) (quote ()) (quote ()) (lambda () (values)) (lambda () (values)) (quote &build~164~1634))) (values))
(begin (ex:register-library! (ex:make-library (quote (rnrs mutable-strings)) (lambda () (quote ())) (quote ((string-set! variable string-set! (0) #f ()) (string-fill! variable string-fill! (0) #f ()))) (quote ()) (quote ()) (lambda () (values)) (lambda () (values)) (quote &build~164~1635))) (values))
(begin (ex:register-library! (ex:make-library (quote (rnrs eval)) (lambda () (quote ())) (quote ((eval variable ex:eval (0) #f ()) (environment variable ex:environment (0) #f ()))) (quote (((core primitives) 0))) (quote (&build~164~2)) (lambda () (values)) (lambda () (values)) (quote &build~164~1636))) (values))
(begin (ex:register-library! (ex:make-library (quote (rnrs eval reflection)) (lambda () (quote ())) (quote ((environment-bindings variable ex:environment-bindings (0) #f ()))) (quote (((core primitives) 0))) (quote (&build~164~2)) (lambda () (values)) (lambda () (values)) (quote &build~164~1637))) (values))
(begin (define &make-promise~164~1656 ex:unspecified) (define &force~164~1640 ex:unspecified) (define &null-environment~164~1639 ex:unspecified) (define &scheme-report-environment~164~1638 ex:unspecified) (ex:register-library! (ex:make-library (quote (rnrs r5rs)) (lambda () (ex:uncompress (quote (((&env~164~1651 0 1 2 3)) (3 (((make-promise) variable &make-promise~164~1656 (0) #f (rnrs r5rs)) ((delay) macro &delay~164~1641 (0) #f (rnrs r5rs)) ((force) variable &force~164~1640 (0) #f (rnrs r5rs)) ((null-environment) variable &null-environment~164~1639 (0) #f (rnrs r5rs)) ((scheme-report-environment) variable &scheme-report-environment~164~1638 (0) #f (rnrs r5rs)) ((modulo) variable modulo (0) #f ()) ((remainder) variable remainder (0) #f ()) ((quotient) variable quotient (0) #f ()) ((inexact->exact) variable inexact->exact (0) #f ()) ((exact->inexact) variable exact->inexact (0) #f ()) ((environment) variable ex:environment (0) #f ()) ((eval) variable ex:eval (0) #f ()) ((call/cc) variable call/cc (0) #f ()) ((assertion-violation) variable assertion-violation (0) #f ()) ((error) variable error (0) #f ()) ((vector-for-each) variable vector-for-each (0) #f ()) ((vector-map) variable vector-map (0) #f ()) ((string-for-each) variable string-for-each (0) #f ()) ((symbol=?) variable symbol=? (0) #f ()) ((boolean=?) variable boolean=? (0) #f ()) ((exact-integer-sqrt) variable exact-integer-sqrt (0) #f ()) ((div0-and-mod0) variable div0-and-mod0 (0) #f ()) ((mod0) variable mod0 (0) #f ()) ((div0) variable div0 (0) #f ()) ((div-and-mod) variable div-and-mod (0) #f ()) ((mod) variable mod (0) #f ()) ((div) variable div (0) #f ()) ((nan?) variable nan? (0) #f ()) ((infinite?) variable infinite? (0) #f ()) ((finite?) variable finite? (0) #f ()) ((inexact) variable inexact (0) #f ()) ((exact) variable exact (0) #f ()) ((integer-valued?) variable integer-valued? (0) #f ()) ((rational-valued?) variable rational-valued? (0) #f ()) ((real-valued?) variable real-valued? (0) #f ()) ((zero?) variable zero? (0) #f ()) ((vector?) variable vector? (0) #f ()) ((vector-set!) variable vector-set! (0) #f ()) ((vector-ref) variable vector-ref (0) #f ()) ((vector-length) variable vector-length (0) #f ()) ((vector-fill!) variable vector-fill! (0) #f ()) ((vector->list) variable vector->list (0) #f ()) ((vector) variable vector (0) #f ()) ((values) variable values (0) #f ()) ((truncate) variable truncate (0) #f ()) ((tan) variable tan (0) #f ()) ((symbol?) variable symbol? (0) #f ()) ((symbol->string) variable symbol->string (0) #f ()) ((substring) variable substring (0) #f ()) ((string?) variable string? (0) #f ()) ((string>?) variable string>? (0) #f ()) ((string>=?) variable string>=? (0) #f ()) ((string=?) variable string=? (0) #f ()) ((string<?) variable string<? (0) #f ()) ((string<=?) variable string<=? (0) #f ()) ((string-ref) variable string-ref (0) #f ()) ((string-length) variable string-length (0) #f ()) ((string-copy) variable string-copy (0) #f ()) ((string-append) variable string-append (0) #f ()) ((string->symbol) variable string->symbol (0) #f ()) ((string->number) variable string->number (0) #f ()) ((string->list) variable string->list (0) #f ()) ((string) variable string (0) #f ()) ((sqrt) variable sqrt (0) #f ()) ((sin) variable sin (0) #f ()) ((round) variable round (0) #f ()) ((reverse) variable reverse (0) #f ()) ((real?) variable real? (0) #f ()) ((real-part) variable real-part (0) #f ()) ((rationalize) variable rationalize (0) #f ()) ((rational?) variable rational? (0) #f ()) ((procedure?) variable procedure? (0) #f ()) ((positive?) variable positive? (0) #f ()) ((pair?) variable pair? (0) #f ()) ((odd?) variable odd? (0) #f ()) ((numerator) variable numerator (0) #f ()) ((number?) variable number? (0) #f ()) ((number->string) variable number->string (0) #f ()) ((null?) variable null? (0) #f ()) ((not) variable not (0) #f ()) ((negative?) variable negative? (0) #f ()) ((min) variable min (0) #f ()) ((max) variable max (0) #f ()) ((map) variable map (0) #f ()) ((make-vector) variable make-vector (0) #f ()) ((make-string) variable make-string (0) #f ()) ((make-rectangular) variable make-rectangular (0) #f ()) ((make-polar) variable make-polar (0) #f ()) ((magnitude) variable magnitude (0) #f ()) ((log) variable log (0) #f ()) ((list?) variable list? (0) #f ()) ((list-tail) variable list-tail (0) #f ()) ((list-ref) variable list-ref (0) #f ()) ((list->vector) variable list->vector (0) #f ()) ((list->string) variable list->string (0) #f ()) ((list) variable list (0) #f ()) ((length) variable length (0) #f ()) ((lcm) variable lcm (0) #f ()) ((integer?) variable integer? (0) #f ()) ((integer->char) variable integer->char (0) #f ()) ((inexact?) variable inexact? (0) #f ()) ((imag-part) variable imag-part (0) #f ()) ((gcd) variable gcd (0) #f ()) ((for-each) variable for-each (0) #f ()) ((floor) variable floor (0) #f ()) ((expt) variable expt (0) #f ()) ((exp) variable exp (0) #f ()) ((exact?) variable exact? (0) #f ()) ((even?) variable even? (0) #f ()) ((eqv?) variable eqv? (0) #f ()) ((equal?) variable equal? (0) #f ()) ((eq?) variable eq? (0) #f ()) ((dynamic-wind) variable dynamic-wind (0) #f ()) ((denominator) variable denominator (0) #f ()) ((cos) variable cos (0) #f ()) ((cons) variable cons (0) #f ()) ((complex?) variable complex? (0) #f ()) ((char->integer) variable char->integer (0) #f ()) ((char?) variable char? (0) #f ()) ((ceiling) variable ceiling (0) #f ()) ((cddddr) variable cddddr (0) #f ()) ((cdddar) variable cdddar (0) #f ()) ((cddadr) variable cddadr (0) #f ()) ((cddaar) variable cddaar (0) #f ()) ((cdaddr) variable cdaddr (0) #f ()) ((cdadar) variable cdadar (0) #f ()) ((cdaadr) variable cdaadr (0) #f ()) ((cdaaar) variable cdaaar (0) #f ()) ((cadddr) variable cadddr (0) #f ()) ((caddar) variable caddar (0) #f ()) ((cadadr) variable cadadr (0) #f ()) ((cadaar) variable cadaar (0) #f ()) ((caaddr) variable caaddr (0) #f ()) ((caadar) variable caadar (0) #f ()) ((caaadr) variable caaadr (0) #f ()) ((caaaar) variable caaaar (0) #f ()) ((cdddr) variable cdddr (0) #f ()) ((cddar) variable cddar (0) #f ()) ((cdadr) variable cdadr (0) #f ()) ((cdaar) variable cdaar (0) #f ()) ((caddr) variable caddr (0) #f ()) ((cadar) variable cadar (0) #f ()) ((caadr) variable caadr (0) #f ()) ((caaar) variable caaar (0) #f ()) ((cddr) variable cddr (0) #f ()) ((cdar) variable cdar (0) #f ()) ((cadr) variable cadr (0) #f ()) ((caar) variable caar (0) #f ()) ((cdr) variable cdr (0) #f ()) ((car) variable car (0) #f ()) ((call-with-values) variable call-with-values (0) #f ()) ((call-with-current-continuation) variable call-with-current-continuation (0) #f ()) ((boolean?) variable boolean? (0) #f ()) ((atan) variable atan (0) #f ()) ((asin) variable asin (0) #f ()) ((apply) variable apply (0) #f ()) ((append) variable append (0) #f ()) ((acos) variable acos (0) #f ()) ((abs) variable abs (0) #f ()) ((>=) variable >= (0) #f ()) ((>) variable > (0) #f ()) ((=) variable = (0) #f ()) ((<=) variable <= (0) #f ()) ((<) variable < (0) #f ()) ((/) variable / (0) #f ()) ((-) variable - (0) #f ()) ((+) variable + (0) #f ()) ((*) variable * (0) #f ()) ((identifier-syntax) macro &identifier-syntax~164~490 (1) #f (core identifier-syntax)) ((syntax-rules) macro &syntax-rules~164~58 (1) #f (core syntax-rules)) ((unquote-splicing) macro &unquote-splicing~164~1201 (0) #f (core quasiquote)) ((unquote) macro &unquote~164~1197 (0) #f (core quasiquote)) ((quasiquote) macro &quasiquote~164~763 (0) #f (core quasiquote)) ((assert) macro &assert~164~1617 (0) #f (rnrs base)) ((=>) macro &=>~164~481 (0) #f (core derived)) ((else) macro &else~164~485 (0) #f (core derived)) ((cond) macro &cond~164~285 (0) #f (core derived)) ((case) macro &case~164~401 (0) #f (core derived)) ((let*-values) macro &let*-values~164~1346 (0) #f (core let-values)) ((let-values) macro &let-values~164~1206 (0) #f (core let-values)) ((letrec*) macro &letrec*~164~179 (0) #f (core let)) ((letrec) macro &letrec~164~148 (0) #f (core let)) ((let*) macro &let*~164~204 (0) #f (core derived)) ((let) macro &let~164~100 (0) #f (core let)) ((...) macro ... (1) #f ()) ((_) macro _ (1) #f ()) ((letrec-syntax) macro letrec-syntax (0) #f ()) ((let-syntax) macro let-syntax (0) #f ()) ((define-syntax) macro define-syntax (0) #f ()) ((define) macro define (0) #f ()) ((or) macro or (0) #f ()) ((and) macro and (0) #f ()) ((set!) macro set! (1 0) #f ()) ((quote) macro quote (0) #f ()) ((lambda) macro lambda (0) #f ()) ((if) macro if (0) #f ()) ((begin) macro begin (0) #f ()) ((case-lambda) macro &case-lambda~164~1479 (0) #f (rnrs control)) ((do) macro &do~164~1419 (0) #f (rnrs control)) ((unless) macro &unless~164~1400 (0) #f (rnrs control)) ((when) macro &when~164~1381 (0) #f (rnrs control)))) (2 (((x &c~164~1642) variable &x~164~1644 (0) #f (rnrs r5rs)))) (1 ()) (0 (((dummy &c~164~1642) . #f) ((expression) . #f))))))) (quote ((null-environment variable &null-environment~164~1639 (0) #f (rnrs r5rs)) (scheme-report-environment variable &scheme-report-environment~164~1638 (0) #f (rnrs r5rs)) (delay macro &delay~164~1641 (0) #f (rnrs r5rs)) (force variable &force~164~1640 (0) #f (rnrs r5rs)) (exact->inexact variable exact->inexact (0) #f ()) (inexact->exact variable inexact->exact (0) #f ()) (quotient variable quotient (0) #f ()) (remainder variable remainder (0) #f ()) (modulo variable modulo (0) #f ()))) (quote (((rnrs control) 0) ((rnrs base) 0) ((rnrs eval) 0))) (quote (&build~164~1605 &build~164~1632 &build~164~1636)) (lambda () (ex:register-macro! (quote &delay~164~1641) (lambda (&x~164~1644) (let ((&input~164~1646 &x~164~1644)) (let ((&fail~164~1647 (lambda () (ex:invalid-form &input~164~1646)))) (if (pair? &input~164~1646) (let ((&temp~164~1655 (car &input~164~1646))) (let ((&dummy~164~1649 &temp~164~1655)) (let ((&temp~164~1652 (cdr &input~164~1646))) (if (pair? &temp~164~1652) (let ((&temp~164~1654 (car &temp~164~1652))) (let ((&expression~164~1648 &temp~164~1654)) (let ((&temp~164~1653 (cdr &temp~164~1652))) (if (null? &temp~164~1653) (cons (ex:syntax-rename (quote make-promise) (quote ()) (quote (&env~164~1651)) 0 (quote (rnrs r5rs))) (cons (cons (ex:syntax-rename (quote lambda) (quote ()) (quote (&env~164~1651)) 0 (quote (rnrs r5rs))) (cons (quote ()) (cons &expression~164~1648 (quote ())))) (quote ()))) (&fail~164~1647))))) (&fail~164~1647))))) (&fail~164~1647)))))) (values)) (lambda () (set! &make-promise~164~1656 ex:undefined) (set! &force~164~1640 ex:undefined) (set! &null-environment~164~1639 ex:undefined) (set! &scheme-report-environment~164~1638 ex:undefined) (set! &scheme-report-environment~164~1638 (lambda (&n~164~1658) (if (not (= &n~164~1658 5)) (begin (assertion-violation (quote scheme-report-environment) "Argument should be 5" &n~164~1658))) (ex:environment (quote (r5rs))))) (set! &null-environment~164~1639 ((lambda (&null-env~164~1668) (lambda (&n~164~1670) (if (not (= &n~164~1670 5)) (begin (assertion-violation (quote scheme-report-environment) "Argument should be 5" &n~164~1670))) &null-env~164~1668)) (ex:environment (quote (only (rnrs base) begin if lambda quote set! and or define define-syntax let-syntax letrec-syntax let let* letrec case cond else => quasiquote unquote unquote-splicing syntax-rules ...)) (quote (only (rnrs control) do))))) (set! &force~164~1640 (lambda (&object~164~1676) (&object~164~1676))) (set! &make-promise~164~1656 (lambda (&proc~164~1678) ((lambda (&result-ready?~164~1682 &result~164~1681) (lambda () (if &result-ready?~164~1682 &result~164~1681 ((lambda (&x~164~1687) (if &result-ready?~164~1682 &result~164~1681 (begin (set! &result-ready?~164~1682 #t) (set! &result~164~1681 &x~164~1687) &result~164~1681))) (&proc~164~1678))))) #f #f))) (values)) (quote &build~164~1692))) (values))
(begin (ex:register-library! (ex:make-library (quote (rnrs load)) (lambda () (quote ())) (quote ((load variable ex:load (0) #f ()))) (quote ()) (quote ()) (lambda () (values)) (lambda () (values)) (quote &build~164~1693))) (values))
(begin (ex:register-library! (ex:make-library (quote (r5rs)) (lambda () (quote ())) (quote ((set! macro set! (0) #f ()) (begin macro begin (0) #f ()) (if macro if (0) #f ()) (lambda macro lambda (0) #f ()) (quote macro quote (0) #f ()) (and macro and (0) #f ()) (or macro or (0) #f ()) (define macro define (0) #f ()) (define-syntax macro define-syntax (0) #f ()) (let-syntax macro let-syntax (0) #f ()) (letrec-syntax macro letrec-syntax (0) #f ()) (... macro ... (1) #f ()) (let macro &let~164~100 (0) #f (core let)) (let* macro &let*~164~204 (0) #f (core derived)) (letrec macro &letrec~164~148 (0) #f (core let)) (case macro &case~164~401 (0) #f (core derived)) (cond macro &cond~164~285 (0) #f (core derived)) (else macro &else~164~485 (0) #f (core derived)) (=> macro &=>~164~481 (0) #f (core derived)) (quasiquote macro &quasiquote~164~763 (0) #f (core quasiquote)) (unquote macro &unquote~164~1197 (0) #f (core quasiquote)) (unquote-splicing macro &unquote-splicing~164~1201 (0) #f (core quasiquote)) (syntax-rules macro &syntax-rules~164~58 (1) #f (core syntax-rules)) (* variable * (0) #f ()) (+ variable + (0) #f ()) (- variable - (0) #f ()) (/ variable / (0) #f ()) (< variable < (0) #f ()) (<= variable <= (0) #f ()) (= variable = (0) #f ()) (> variable > (0) #f ()) (>= variable >= (0) #f ()) (abs variable abs (0) #f ()) (acos variable acos (0) #f ()) (append variable append (0) #f ()) (apply variable apply (0) #f ()) (asin variable asin (0) #f ()) (atan variable atan (0) #f ()) (boolean? variable boolean? (0) #f ()) (call-with-current-continuation variable call-with-current-continuation (0) #f ()) (call-with-values variable call-with-values (0) #f ()) (car variable car (0) #f ()) (cdr variable cdr (0) #f ()) (caar variable caar (0) #f ()) (cadr variable cadr (0) #f ()) (cdar variable cdar (0) #f ()) (cddr variable cddr (0) #f ()) (caaar variable caaar (0) #f ()) (caadr variable caadr (0) #f ()) (cadar variable cadar (0) #f ()) (caddr variable caddr (0) #f ()) (cdaar variable cdaar (0) #f ()) (cdadr variable cdadr (0) #f ()) (cddar variable cddar (0) #f ()) (cdddr variable cdddr (0) #f ()) (caaaar variable caaaar (0) #f ()) (caaadr variable caaadr (0) #f ()) (caadar variable caadar (0) #f ()) (caaddr variable caaddr (0) #f ()) (cadaar variable cadaar (0) #f ()) (cadadr variable cadadr (0) #f ()) (caddar variable caddar (0) #f ()) (cadddr variable cadddr (0) #f ()) (cdaaar variable cdaaar (0) #f ()) (cdaadr variable cdaadr (0) #f ()) (cdadar variable cdadar (0) #f ()) (cdaddr variable cdaddr (0) #f ()) (cddaar variable cddaar (0) #f ()) (cddadr variable cddadr (0) #f ()) (cdddar variable cdddar (0) #f ()) (cddddr variable cddddr (0) #f ()) (ceiling variable ceiling (0) #f ()) (char? variable char? (0) #f ()) (char->integer variable char->integer (0) #f ()) (complex? variable complex? (0) #f ()) (cons variable cons (0) #f ()) (cos variable cos (0) #f ()) (denominator variable denominator (0) #f ()) (dynamic-wind variable dynamic-wind (0) #f ()) (eq? variable eq? (0) #f ()) (equal? variable equal? (0) #f ()) (eqv? variable eqv? (0) #f ()) (even? variable even? (0) #f ()) (exact? variable exact? (0) #f ()) (exp variable exp (0) #f ()) (expt variable expt (0) #f ()) (floor variable floor (0) #f ()) (for-each variable for-each (0) #f ()) (gcd variable gcd (0) #f ()) (imag-part variable imag-part (0) #f ()) (inexact? variable inexact? (0) #f ()) (integer->char variable integer->char (0) #f ()) (integer? variable integer? (0) #f ()) (lcm variable lcm (0) #f ()) (length variable length (0) #f ()) (list variable list (0) #f ()) (list->string variable list->string (0) #f ()) (list->vector variable list->vector (0) #f ()) (list-ref variable list-ref (0) #f ()) (list-tail variable list-tail (0) #f ()) (list? variable list? (0) #f ()) (log variable log (0) #f ()) (magnitude variable magnitude (0) #f ()) (make-polar variable make-polar (0) #f ()) (make-rectangular variable make-rectangular (0) #f ()) (make-string variable make-string (0) #f ()) (make-vector variable make-vector (0) #f ()) (map variable map (0) #f ()) (max variable max (0) #f ()) (min variable min (0) #f ()) (negative? variable negative? (0) #f ()) (not variable not (0) #f ()) (null? variable null? (0) #f ()) (number->string variable number->string (0) #f ()) (number? variable number? (0) #f ()) (numerator variable numerator (0) #f ()) (odd? variable odd? (0) #f ()) (pair? variable pair? (0) #f ()) (positive? variable positive? (0) #f ()) (procedure? variable procedure? (0) #f ()) (rational? variable rational? (0) #f ()) (rationalize variable rationalize (0) #f ()) (real-part variable real-part (0) #f ()) (real? variable real? (0) #f ()) (reverse variable reverse (0) #f ()) (round variable round (0) #f ()) (sin variable sin (0) #f ()) (sqrt variable sqrt (0) #f ()) (string variable string (0) #f ()) (string->list variable string->list (0) #f ()) (string->number variable string->number (0) #f ()) (string->symbol variable string->symbol (0) #f ()) (string-append variable string-append (0) #f ()) (string-copy variable string-copy (0) #f ()) (string-length variable string-length (0) #f ()) (string-ref variable string-ref (0) #f ()) (string<=? variable string<=? (0) #f ()) (string<? variable string<? (0) #f ()) (string=? variable string=? (0) #f ()) (string>=? variable string>=? (0) #f ()) (string>? variable string>? (0) #f ()) (string? variable string? (0) #f ()) (substring variable substring (0) #f ()) (symbol->string variable symbol->string (0) #f ()) (symbol? variable symbol? (0) #f ()) (tan variable tan (0) #f ()) (truncate variable truncate (0) #f ()) (values variable values (0) #f ()) (vector variable vector (0) #f ()) (vector->list variable vector->list (0) #f ()) (vector-fill! variable vector-fill! (0) #f ()) (vector-length variable vector-length (0) #f ()) (vector-ref variable vector-ref (0) #f ()) (vector-set! variable vector-set! (0) #f ()) (vector? variable vector? (0) #f ()) (zero? variable zero? (0) #f ()) (eval variable ex:eval (0) #f ()) (load variable ex:load (0) #f ()) (do macro &do~164~1419 (0) #f (rnrs control)) (call-with-input-file variable call-with-input-file (0) #f ()) (call-with-output-file variable call-with-output-file (0) #f ()) (close-input-port variable close-input-port (0) #f ()) (close-output-port variable close-output-port (0) #f ()) (current-input-port variable current-input-port (0) #f ()) (current-output-port variable current-output-port (0) #f ()) (display variable display (0) #f ()) (eof-object? variable eof-object? (0) #f ()) (newline variable newline (0) #f ()) (open-input-file variable open-input-file (0) #f ()) (open-output-file variable open-output-file (0) #f ()) (peek-char variable peek-char (0) #f ()) (read variable read (0) #f ()) (read-char variable read-char (0) #f ()) (with-input-from-file variable with-input-from-file (0) #f ()) (with-output-to-file variable with-output-to-file (0) #f ()) (write variable write (0) #f ()) (write-char variable write-char (0) #f ()) (char-upcase variable char-upcase (0) #f ()) (char-downcase variable char-downcase (0) #f ()) (char-ci=? variable char-ci=? (0) #f ()) (char-ci<? variable char-ci<? (0) #f ()) (char-ci>? variable char-ci>? (0) #f ()) (char-ci<=? variable char-ci<=? (0) #f ()) (char-ci>=? variable char-ci>=? (0) #f ()) (char-alphabetic? variable char-alphabetic? (0) #f ()) (char-numeric? variable char-numeric? (0) #f ()) (char-whitespace? variable char-whitespace? (0) #f ()) (char-upper-case? variable char-upper-case? (0) #f ()) (char-lower-case? variable char-lower-case? (0) #f ()) (string-ci=? variable string-ci=? (0) #f ()) (string-ci<? variable string-ci<? (0) #f ()) (string-ci>? variable string-ci>? (0) #f ()) (string-ci<=? variable string-ci<=? (0) #f ()) (string-ci>=? variable string-ci>=? (0) #f ()) (set-car! variable set-car! (0) #f ()) (set-cdr! variable set-cdr! (0) #f ()) (assoc variable assoc (0) #f ()) (assv variable assv (0) #f ()) (assq variable assq (0) #f ()) (member variable member (0) #f ()) (memv variable memv (0) #f ()) (memq variable memq (0) #f ()) (string-set! variable string-set! (0) #f ()) (string-fill! variable string-fill! (0) #f ()) (null-environment variable &null-environment~164~1639 (0) #f (rnrs r5rs)) (scheme-report-environment variable &scheme-report-environment~164~1638 (0) #f (rnrs r5rs)) (delay macro &delay~164~1641 (0) #f (rnrs r5rs)) (force variable &force~164~1640 (0) #f (rnrs r5rs)) (exact->inexact variable exact->inexact (0) #f ()) (inexact->exact variable inexact->exact (0) #f ()) (quotient variable quotient (0) #f ()) (remainder variable remainder (0) #f ()) (modulo variable modulo (0) #f ()))) (quote (((rnrs r5rs) 0) ((rnrs mutable-strings) 0) ((rnrs lists) 0) ((rnrs mutable-pairs) 0) ((rnrs unicode) 0) ((rnrs io simple) 0) ((rnrs control) 0) ((rnrs load) 0) ((rnrs eval) 0) ((rnrs base) 0) ((core primitives) 0))) (quote (&build~164~1692 &build~164~1635 &build~164~1606 &build~164~1634 &build~164~1608 &build~164~1607 &build~164~1605 &build~164~1693 &build~164~1636 &build~164~1632 &build~164~2)) (lambda () (values)) (lambda () (values)) (quote &build~164~1694))) (values))
(begin (ex:register-library! (ex:make-library (quote (explicit-renaming helper)) (lambda () (ex:uncompress (quote (((&env~164~1704 0 1 2 3)) (3 (((er-transformer) macro &er-transformer~164~1695 (0) #f (explicit-renaming helper)) ((syntax-case) macro syntax-case (0 1) #f ()) ((syntax) macro syntax (0 1) #f ()) ((datum->syntax) variable ex:datum->syntax (0 1) #f ()) ((free-identifier=?) variable ex:free-identifier=? (0 1) #f ()) ((define-syntax) macro define-syntax (0 1) #f ()) ((lambda) macro lambda (0 1) #f ()))) (2 (((exp) variable &exp~164~1697 (1) #f (explicit-renaming helper)))) (1 ()) (0 (((k) . #f) ((proc) . #f))))))) (quote ((er-transformer macro &er-transformer~164~1695 (0) #f (explicit-renaming helper)))) (quote (((rnrs) 0))) (quote (&build~164~1633)) (lambda () (ex:register-macro! (quote &er-transformer~164~1695) (lambda (&exp~164~1697) (let ((&input~164~1699 &exp~164~1697)) (let ((&fail~164~1700 (lambda () (ex:invalid-form &input~164~1699)))) (if (pair? &input~164~1699) (let ((&temp~164~1708 (car &input~164~1699))) (let ((&k~164~1702 &temp~164~1708)) (let ((&temp~164~1705 (cdr &input~164~1699))) (if (pair? &temp~164~1705) (let ((&temp~164~1707 (car &temp~164~1705))) (let ((&proc~164~1701 &temp~164~1707)) (let ((&temp~164~1706 (cdr &temp~164~1705))) (if (null? &temp~164~1706) (cons (ex:syntax-rename (quote lambda) (quote ()) (quote (&env~164~1704)) 0 (quote (explicit-renaming helper))) (cons (cons (ex:syntax-rename (quote form) (quote ()) (quote (&env~164~1704)) 0 (quote (explicit-renaming helper))) (quote ())) (cons (cons &proc~164~1701 (cons (ex:syntax-rename (quote form) (quote ()) (quote (&env~164~1704)) 0 (quote (explicit-renaming helper))) (cons (cons (ex:syntax-rename (quote lambda) (quote ()) (quote (&env~164~1704)) 0 (quote (explicit-renaming helper))) (cons (cons (ex:syntax-rename (quote symbol) (quote ()) (quote (&env~164~1704)) 0 (quote (explicit-renaming helper))) (quote ())) (cons (cons (ex:syntax-rename (quote datum->syntax) (quote ()) (quote (&env~164~1704)) 0 (quote (explicit-renaming helper))) (cons (cons (ex:syntax-rename (quote syntax) (quote ()) (quote (&env~164~1704)) 0 (quote (explicit-renaming helper))) (cons &k~164~1702 (quote ()))) (cons (ex:syntax-rename (quote symbol) (quote ()) (quote (&env~164~1704)) 0 (quote (explicit-renaming helper))) (quote ())))) (quote ())))) (cons (ex:syntax-rename (quote free-identifier=?) (quote ()) (quote (&env~164~1704)) 0 (quote (explicit-renaming helper))) (quote ()))))) (quote ())))) (&fail~164~1700))))) (&fail~164~1700))))) (&fail~164~1700)))))) (values)) (lambda () (values)) (quote &build~164~1709))) (values))
(begin (ex:register-library! (ex:make-library (quote (explicit-renaming)) (lambda () (quote ())) (quote ((er-transformer macro &er-transformer~164~1695 (0) #f (explicit-renaming helper)) (identifier? variable ex:identifier? (0) #f ()) (bound-identifier=? variable ex:bound-identifier=? (0) #f ()) (datum->syntax variable ex:datum->syntax (0) #f ()))) (quote (((rnrs syntax-case) 0) ((explicit-renaming helper) 0))) (quote (&build~164~1616 &build~164~1709)) (lambda () (values)) (lambda () (values)) (quote &build~164~1710))) (values))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;end "standard-libraries.exp")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (my-expand-file filename)
  (ex:with-toplevel-parameters
   (lambda ()
     (ex:expand-toplevel-sequence 
       (ex:normalize 
         (cons '(import (rnrs) (rnrs r5rs))
           (ex:read-file filename)))))))

(define-syntax cfor
  (syntax-rules ()
    [(_ (var init cond incr) code ...)
     (let loop ([var init])
       (when cond
         (begin code ... (loop incr))))]))

(define cmdline-args (current-command-line-arguments))
(define arg-len (vector-length cmdline-args))

(when (not (eqv? 1 arg-len))
  (error (format "expected exactly one argument: got ~a" arg-len)))

(define benchmark-path (simplify-path (path->complete-path (vector-ref cmdline-args 0))))

(define (benchmark code warm-up-count)
  (cfor (i 0 (< i warm-up-count) (add1 i)) (code))
  (collect-garbage)
  (time-apply code null))

(define (string-ends-with? str sfx)
  (define sfx-len (string-length sfx))
  (define str-len (string-length str))
  (if (< str-len sfx-len)
    #f
    (let loop ((idx (- str-len 1)) 
               (sfx-idx (- sfx-len 1)))
      (if (< sfx-idx 0)
        #t
        (if (not (char=? (string-ref str idx) (string-ref sfx sfx-idx)))
          #f
          (loop (- idx 1) (- sfx-idx 1)))))))

(define sources
  (map (lambda (path) path #;(cons path (call-with-input-file path read-all)))
       (cond 
         [(directory-exists? benchmark-path)
          (filter
           (compose not void?)
           (map
             (lambda (path)
             
               (if (or
                    (string-ends-with? (path->string path) ".scm" )
                    (string-ends-with? (path->string path) ".rkt" ))
                 path          
                 (void))) 
             (list->mlist (sequence->list (in-directory benchmark-path)))))]
         [(file-exists? benchmark-path)
          (list benchmark-path)]
         [else
          (error(format "path does not refer to a file or directory: ~a" benchmark-path))])))

#;(printf "source files: ~a\n" sources)

(for ([path sources])
  (guard 
    (ex
      (#t
        (fprintf (current-error-port) "~a failed: ~a" path ex)))
  (define-values (expanded cpu real gc) (benchmark (lambda () (call-with-values (lambda () (my-expand-file path)) list)) 0))
  (printf "~a: real=~as cpu=~as gc=~as\n" (find-relative-path benchmark-path path) (/ real 1000.0) (/ cpu 1000.0) (/ gc 1000.0)))
  )

