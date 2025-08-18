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


