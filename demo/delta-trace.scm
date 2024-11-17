(let solve
  (lambda (a b c)
    (let d (trace sqrt (trace - (trace expt b 2) (trace * 4 (trace * a c))))
      (cons
        (trace / (trace - (trace - b) d) (trace * 2 a))
        (trace / (trace + (trace - b) d) (trace * 2 a)))))
  (let prompt
    (lambda (n)
      (begin
        (display (string-append "Enter a number for '" n "'...\n"))
        (trace string->number (trace read-line))))
    (let a (prompt "a")
      (let b (prompt "b")
        (let c (prompt "c")
          (let p (solve a b c)
            (trace display (trace string-append
              "Sol1 = " (trace number->string (trace car p))
              ", "
              "Sol2 = " (trace number->string (trace cdr p))
              "\n"))))))))