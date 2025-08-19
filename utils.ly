#(define voice_color_engraver
  (lambda (voice-color)
    (make-engraver
     (acknowledgers
      ((note-head-interface engraver grob source-engraver)
       (ly:grob-set-property! grob 'color voice-color))))))

#(define (get-all-grob-properties grob)
   "Return an alist of all properties set on a grob"
   (let* ((props '())
          (add-prop (lambda (key val)
                      (set! props (cons (cons key val) props)))))
     
     ;; We need to iterate through known property names
     ;; since there's no direct way to get all properties
     (let ((common-properties 
            '(stencil color thickness 
              X-offset Y-offset extra-offset 
              staff-position direction
              font-size font-name font-family
              break-alignment break-align-symbols
              spacing-wishes bounded-by-me
              axis-group-parent-X axis-group-parent-Y
              pure-Y-common pure-Y-offset-in-progress
              cross-staff self-alignment-X self-alignment-Y
              minimum-X-extent minimum-Y-extent
              skyline-horizontal-padding
              line-positions line-count
              when rank
              text markup)))
       
       (for-each 
         (lambda (prop-name)
           (let ((val (ly:grob-property grob prop-name)))
             (if (not (eq? val '()))  ; Only add if property is set
                 (add-prop prop-name val))))
         common-properties))
     
     ;; Return the collected properties
     (reverse props)))


%%% ---- slice-by-measures (supports end = -1, negatives count from end) ----

#(define (copy-with-elements m els)
  (let ((cp (ly:music-deep-copy m)))
    (ly:music-set-property! cp 'elements els)
    cp))

#(define (copy-with-element m el)
  (let ((cp (ly:music-deep-copy m)))
    (ly:music-set-property! cp 'element el)
    cp))

#(define (count-bars mus bar-in)
  (let ((name (ly:music-property mus 'name)))
    (cond
      ((eq? name 'SequentialMusic)
       (fold (lambda (e b) (count-bars e b)) bar-in
             (ly:music-property mus 'elements)))
      ((eq? name 'SimultaneousMusic)
       (fold max bar-in
             (map (lambda (e) (count-bars e bar-in))
                  (ly:music-property mus 'elements))))
      ((eq? name 'ContextSpeccedMusic)
       (count-bars (ly:music-property mus 'element) bar-in))
      ((or (eq? name 'BarCheck) (eq? name 'BarLine))
       (+ bar-in 1))
      (else bar-in))))

#(define (walk-slice start end m bar-in)
  (let* ((name (ly:music-property m 'name))
         (inside? (and (<= start bar-in)
                       (or (= end -1) (<= bar-in end)))))
    (cond
       ((eq? name 'SequentialMusic)
       (let loop ((xs (ly:music-property m 'elements)) (acc '()) (bar bar-in))
         (if (null? xs)
             (values (and (pair? acc) (copy-with-elements m (reverse acc))) bar)
             (call-with-values
               (lambda () (walk-slice start end (car xs) bar))
               (lambda (kept bar2)
                 (loop (cdr xs)
                       (if kept (cons kept acc) acc)
                       bar2))))))

       ((eq? name 'SimultaneousMusic)
       (let* ((els (ly:music-property m 'elements)))
         (let loop ((xs els) (acc '()) (bar-max bar-in))
           (if (null? xs)
               (values (and (pair? acc) (copy-with-elements m (reverse acc))) bar-max)
               (call-with-values
                 (lambda () (walk-slice start end (car xs) bar-in))
                 (lambda (kept bar2)
                   (loop (cdr xs)
                         (if kept (cons kept acc) acc)
                         (max bar-max bar2))))))))

       ((eq? name 'ContextSpeccedMusic)
       (call-with-values
         (lambda () (walk-slice start end (ly:music-property m 'element) bar-in))
         (lambda (kchild bar2)
           (values (and kchild (copy-with-element m kchild)) bar2))))

       ((or (eq? name 'BarCheck) (eq? name 'BarLine))
       (values (and inside? m) (+ bar-in 1)))

       (else
       (values (and inside? m) bar-in)))))

#(define (slice-by-measures start end mus)
  (let* ((total (count-bars mus 1))
         (zzz (display total))
          (real-start (if (< start 0) (+ total start +1) start))
          (real-end   (cond
                       ((= end -1) -1)                ; open-ended
                       ((< end 0) (+ total end +1))   ; negative index
                       (else end))))
    (call-with-values
      (lambda () (walk-slice real-start real-end mus 1))
      (lambda (kept _bar-out)
        (or kept (make-music 'SkipEvent))))))

#(define-public sliceMusic
  (define-music-function (start end music)
    (integer? integer? ly:music?)
    (slice-by-measures start end music)))


