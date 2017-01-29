#lang racket/base

(require racket/list
         db)

;; ========================
;; STRUCTS TO CONTAIN STATE
;; ========================
(struct blog (db))
(struct post (blog id))

;; INITIALIZE THE BLOG
(define (initialize-blog! home)
    (define db (sqlite3-connect #:database home #:mode 'create))
    (define the-blog (blog db))

    ;; unless the posts table already exists,
    ;; create it and insert posts.
    (unless
        (table-exists? db "posts")
        ;; execute a SQL query
        (query-exec
            db
            (string-append
                "CREATE TABLE posts "
                "(id INTEGER PRIMARY KEY, title TEXT, body TEXT)"))
    (blog-insert-post!
        the-blog
        "first post"
        "this is my first post")
    (blog-insert-post!
        the-blog
        "second post"
        "this is my second post"))

    ;; unless the comments table already exists,
    ;; create it and insert a post comment for the first post
    (unless
        (table-exists? db "comments")
        ;; execute SQL query
        (query-exec db "CREATE TABLE comments (pid INTEGER, content TEXT)")
        (post-insert-comment!
            the-blog
            (first (blog-posts the-blog))
            "first comment"))

    ;; return the initialized blog
    the-blog)

;; ================
;; GETTER FUNCTIONS
;; ================
(define (blog-posts a-blog)
    (define (id->post an-id)
        (post a-blog an-id))
    ;; for each result from the database apply id->post on it
    (map id->post
         ;; perform a query on the database connection of the blog and return a list
         ;; query-list can be used for queries that return a single column
         (query-list
             (blog-db a-blog)
             "SELECT id FROM posts")))

(define (post-title a-post)
    (query-value
        (blog-db (post-blog a-post))
        "SELECT title FROM posts WHERE id = ?"
        (post-id a-post)))

(define (post-body a-post)
    (query-value
        (blog-db (post-blog a-post))
        "SELECT body FROM posts WHERE id = ?"
        (post-id a-post)))

(define (post-comments a-post)
    (query-list
        (blog-db (post-blog a-post))
        "SELECT content FROM comments WHERE pid = ?"
        (post-id a-post)))

;; ================
;; INSERT FUNCTIONS
;; ================
(define (blog-insert-post! a-blog title body)
    (query-exec
        ;; get the db connection of blog
        (blog-db a-blog)
        "INSERT INTO posts (title, body) VALUES (?, ?)"
        title body))

(define (post-insert-comment! a-blog a-post a-comment)
    (query-exec
        (blog-db a-blog)
        "INSERT INTO comments (pid, content) VALUES (?, ?)"
        (post-id a-post)
        a-comment))

;; PROVIDE FUNCTION TO BLOG
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
