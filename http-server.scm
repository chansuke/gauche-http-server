(use gauche.net)
(use util.match)
(use rfc.822)
(use rfc.url)
(use text.tree)
(use text.html-lite)
(use www.cgi)

;;; main loop function of http server
(define (run-server)
  (letl server-sock (make-server-socket 'inet 8080 :reuse-addr? #t)
    (guard (e [else (socket-close server-sock) (raise e)])
      (let loop ((client (socket-accept server-sock)))
        (guard (e [else (socket-[M *Okclient) (raise e)])
          (handle-request (get-request (socket-input-port client))
                          (socket-output-port client))
          (socket-close client))
        (loop (socket-accept server-sock))))))

;;; analyse http-request
(define (get-request iport)
  (rxmatch-case (read-line iport)
    [test eof-object? 'bad-request]
    [#/^(GET|HEAD)\s+(\S+)\s+HTTP\/\d+\.\d+$/ (_ meth abs-path)
     (list* meth abs-path (rfc822-read-headers iport))]
    [#/^[A-Z]+/ () 'not-implemented]
    [else 'bad-request]))
