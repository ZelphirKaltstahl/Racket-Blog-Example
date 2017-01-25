#lang web-server/insta

; (require rackunit)
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(define nil '())
(define true #t)
(define false #f)
(custodian-limit-memory (current-custodian) MAX-BYTES)
; racket -l errortrace -t exercise-...
; (provide (all-defined-out))

(struct post (title body))
(struct blog (posts) #:mutable)

(define BLOG
  (blog (list (post "First Post!"
                    "Hey, this is my first post!")
              (post "Second Post!"
                    "Haha! Another one!")
              (post "Third Post!"
                    "Haha! <b>Another</b> one!"))))

;; Here the request starts to get handled.
(define (start request)
  (render-blog-page request))

;; Renders the blog page and specifies a handler for the form action.
(define (render-blog-page request)
  (define (response-generator embed/url)
    (response/xexpr
      `(html
         (head (title "My Racket Blog"))
         (body
           (h1 "My Racket Blog :)")
           (div ((class "content"))
             ,(render-posts))
           (form
             ((action ,(embed/url insert-post-handler)))
  
             (label ((for "title")) "Title:")
             (input ((name "title") (type "text") (id "title")))
             (br)
  
             (label ((for "body")) "Body:")
             (input ((name "body") (type "text") (id "body")))
             (br)
  
             (input ((type "submit"))))))))

  ;; render blog page with blog posts plus the new blog post from the request
  (define (insert-post-handler request)
    (blog-insert-post! (parse-post (request-bindings request)))
    (render-blog-page request))

  ;; ???
  (send/suspend/dispatch response-generator))

(define (render-posts)
  ;; This function uses the ,@ notation.
  ;; The backtick allows for S-Expressions to be evaluated inside the quoted expression.
  ;; , allows evaluating of an S-Expression inside the backtick quoted expression
  ;; , does however unquote the complete given S-Expression
  ;; We do not want to unquote the whole sub expressions though.
  ;; We only want to splice them into our code.
  ;; So we have to use ,@ instead.
  `(div ((class "posts-container"))
     ,@(map render-post (blog-posts BLOG))))

;; renders a post as HTML
(define (render-post a-post)
  `(div ((class "post"))
     (h1 ,(post-title a-post))
     (p ,(post-body a-post))))

;; checks if bindings contain the symbols of a post
(define (can-parse-post? bindings)
  (and
    (exists-binding? 'title bindings)
    (exists-binding? 'body bindings)))

;; creates a post from bindings
(define (parse-post bindings)
  (post
    (extract-binding/single 'title bindings)
    (extract-binding/single 'body bindings)))

;; ATTENTION: THIS FUNCTION HAS SIDE EFFECTS!!!
;; ALSO FUNCTIONS USING THIS FUNCTION HAVE SIDE EFFECTS!!!
(define (blog-insert-post! a-post)
  (set-blog-posts!
    BLOG
    (cons a-post (blog-posts BLOG))))


