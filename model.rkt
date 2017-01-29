#lang racket/base

; A blog is a (blog posts)
; where posts is a (listof post)
;
; mutable, as the state of our blog
; can be prefabricated, meaning persisted on disk before the program starts
(struct blog
  (home posts)
  #:mutable
  #:prefab)

; and post is a (post title body comments)
; where title is a string, body is a string,
; and comments is a (listof string)
(struct post
  (title body comments)
  #:mutable
  #:prefab)

(define (blog-insert-post! a-blog title body comments)
  (set-blog-posts!
    a-blog
    (cons (post title body comments)
          (blog-posts a-blog)))
  (save-blog! a-blog))

(define (post-insert-comment! a-blog a-post a-comment)
  (set-post-comments!
    a-post
    (append (post-comments a-post) (list a-comment)))
  (save-blog! a-blog))


(define (initialize-blog! home)
    ;; define an exception handler to handle cases where the log for the blog is missing
    ;; this will set default blog content
    (define (log-missing-exception-handler exn)
        (blog (path->string home)
              (list (post "First Post"
                          "This is my first post!"
                          (list "And my first comment!"))
                    (post "Second Post"
                          "My second post, as boring as the first one."
                          (list "There is a comment here too!")))))
    (define the-blog
        ;; if there is any exception use the exception handler log-missing-exception-handler
        (with-handlers ([exn? log-missing-exception-handler])
                       ;; read the file and the content is defined as the-blog
                       (with-input-from-file home read)))
    ;; set the home of the blog to the location read, which is either the original home or the defaul value
    (set-blog-home! the-blog (path->string home))
    ;; return the-blog
    the-blog)

(define (save-blog! a-blog)
    (define (write-to-database)
        (write a-blog))
    (with-output-to-file (blog-home a-blog)
                         write-to-database
                         ;; ensure that existing content on disk will be overwritten
                         #:exists 'replace))



(provide blog
         post
         initialize-blog!
         blog-insert-post!
         post-insert-comment!
         blog?
         blog-posts
         post?
         post-title
         post-body
         post-comments)
