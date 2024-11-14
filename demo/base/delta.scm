(let solve
  (lambda (a b c)
    (let d (sqrt (- (expt b 2) (* 4 (* a c))))
      (cons
        (/ (- (- b) d) (* 2 a))
        (/ (+ (- b) d) (* 2 a)))))
  (let prompt
    (lambda (n)
      (begin
        (display (string-append "Enter a number for '" n "'...\n"))
        (string->number (read-line))))
    (let a (prompt "a")
      (let b (prompt "b")
        (let c (prompt "c")
          (let p (solve a b c)
            (display (string-append
              "Sol1 = " (number->string (car p))
              ", "
              "Sol2 = " (number->string (cdr p))
              "\n"))))))))