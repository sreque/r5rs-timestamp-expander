(module macro_lambda_test racket
  (provide macro-lambda-test)
  (require racket rackunit
           "../clinger-rees-syntax-rules.rkt"
           "../clinger-rees-parser.rkt"
           "../clinger-rees-env.rkt"
           "test-utils.rkt")

(define program '(
;			Macro-lambda:
; A notation for a first-class parameterized future macro-expansion action
;
; 	      (??!lambda (bound-var ...) body)
;
; Here \texttt{bound-var} is a variable, \texttt{body} is an expression
; that may contain forms \texttt{(??! bound-var)}, and \texttt{??!lambda}
; is just a symbol. It must be stressed that \texttt{??!lambda} is not
; a syntactic or bound variable. Although the \texttt{??!lambda} form
; may look like a macro-application, it is not. This is a critical distinction:
; Hilsdale and Friedman \cite{Hilsdale} looked for a macro-level lambda
; as a macro, but did not succeed. Our macro-level abstractions are
; not macros themselves. The \texttt{??!lambda} form is first class:
; it can be passed to and returned from R5RS macros, and can be nested.
;
; The \texttt{??!lambda}-form is interpreted by a macro \texttt{??!apply}.
; To be more precise, we specify that the following macro-application
;	(??!apply (??!lambda (bound-var ...) body) arg ...)
;
; expands to \texttt{body}, with all non-shadowed instances of
; \texttt{(??! bound-var)} hygienically replaced by \texttt{arg}. In
; Scheme, question and exclamation marks are considered ordinary
; characters. Hence \texttt{??!lambda}, \texttt{??!apply} and
; \texttt{??!} are ordinary symbols -- albeit oddly looking, on
; purpose. A \texttt{??!lambda} form can have one or several bound
; variables; the corresponding \texttt{??!apply} form should have just
; as many arguments.
;
; The following is an implementation of \texttt{??!apply} that
; satisfies the specification above.
;
; Some parts in the ??!apply code below were inspired by
; substitute-fn macro by Al Petrofsky.
;
; Reference{Hilsdale}:
; * Erik Hilsdale and Daniel P. Friedman. "Writing macros in
;     continuation-passing style". Scheme and Functional Programming 2000.
;     September 2000.
; http://www.ccs.neu.edu/home/matthias/Scheme2000/hilsdale.ps

(define-syntax ??!apply
  (syntax-rules (??!lambda)
    ((_ (??!lambda (bound-var . other-bound-vars) body)
	oval . other-ovals)
     (letrec-syntax
	 ((subs
	   (syntax-rules (??! bound-var ??!lambda)
	     ((_ val k (??! bound-var))
	      (appl k val))
	     ; check if bound-var is shadowed in int-body
	     ((_ val k (??!lambda bvars int-body))
	      (subs-in-lambda val bvars (k bvars) int-body))
	     ((_ val k (x))	; optimize single-elem list substitution
	      (subs val (recon-pair val k ()) x))
	     ((_ val k (x . y))
	      (subs val (subsed-cdr val k x) y))
	     ((_ val k x)	; x is an id other than bound-var, or number&c
	      (appl k x))))
	  (subsed-cdr		; we've done the subs in the cdr of a pair
	   (syntax-rules ()     ; now do the subs in the car
	     ((_ val k x new-y)
	      (subs val (recon-pair val k new-y) x))))
	  (recon-pair		; reconstruct the pair out of substituted comp
	   (syntax-rules ()
	     ((_ val k new-y new-x)
	      (appl k (new-x . new-y)))))
	  (subs-in-lambda ; substitute inside the lambda form
	   (syntax-rules (bound-var)
	     ((_ val () kp  int-body)
	      (subs val (recon-l kp ()) int-body))
	      ; bound-var is shadowed in the int-body: no subs
             ((_ val (bound-var . obvars) (k bvars) int-body)
	      (appl k (??!lambda bvars int-body)))
             ((_ val (obvar . obvars) kp int-body)
	      (subs-in-lambda val obvars kp int-body))))
	  (recon-l	; reconstruct lambda from the substituted body
	   (syntax-rules ()
	     ((_ (k bvars) () result)
	      (appl k (??!lambda bvars result)))))
	  (appl		; apply the continuation
	   (syntax-rules ()	; add the result to the end of k
	     ((_ (a b c d) result)
	      (a b c d result))
	     ((_ (a b c) result)
	      (a b c result))))
	  (finish
	   (syntax-rules ()
	     ((_ () () exp)
	      exp)
	     ((_ rem-bvars rem-ovals exps)
	      (??!apply (??!lambda rem-bvars exps) . rem-ovals))))
	  )
       ; In the following, finish is the continuation...
       (subs oval (finish other-bound-vars other-ovals) body)))))

))

  (make-expand-test-defs)
  (define macro-lambda-test
    (test-suite 
     "macro lambda test"
     (expand-and-eval program)
     ; A few tests
     ; This test is due to Al Petrofsky
     (test-expand
      (??!apply (??!lambda (x) '(a . b)) foo)
      (a . b))
     ;===evals-to===> '(a . b)

     (test-expand (??!apply (??!lambda (x) 
                                (list (??!apply (??!lambda (x) (list '(??! x) 5 '(??! x))) (1 2))
                                      '(??! x))) (3 4))
                  (((1 2) 5 (1 2)) (3 4)))
     ;===expands-to===> (list (list '(1 2) 5 '(1 2)) '(3 4))
     ;===evals-to===> '(((1 2) 5 (1 2)) (3 4))

     ; Another test due to Al Petrofsky
     (test-expand
      (??!apply (??!lambda (x) (let (((??! x) 1)) (??! x))) foo)
      1)
     ;===expands-to===> ((lambda (foo) foo) 1)
     ;===evals-to===> 1

     ; Testing two-argument ??!lambda:
     (test-expand
      (??!apply (??!lambda (x k) (??!apply (??! k) (+ 1 (??! x))))
                4
                (??!lambda (x) (??! x)))
      5)
     ;===evals-to===> 5`
     )))