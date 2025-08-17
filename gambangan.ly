\version "2.24.4"

#(use-modules (srfi srfi-1))

#(define MEASURES_PER_SYSTEM 6)
#(define system-counter 1)
#(define notes-by-system (make-hash-table))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Tuning

#(ly:parser-set-note-names
   `((i . ,#{ ais #})
     (o . ,#{ b #})
     (e . ,#{ cis #})
     (u . ,#{ eis #})
     (a . ,#{ fis #})))

#(define reyong_notes_low
  (list 
    #{ e'  #}
    #{ u'  #}
    #{ a'  #}
    #{ i'  #}
    #{ o'  #}
    #{ e'' #}
    #{ u'' #}))

#(define reyong_notes
  (list
    #{ e'   #}
    #{ u'   #}
    #{ a'   #}
    #{ i'   #}
    #{ o'   #}
    #{ e''  #}
    #{ u''  #}
    #{ a''  #}
    #{ i''  #}
    #{ o''  #}
    #{ e''' #}
    #{ u''' #}))


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This engraver is attached to the Score context
autoBreakEngraver =
#(let ((count 0))
   (make-engraver
     (acknowledgers
       ((bar-line-interface engraver grob source-engraver)
        (set! count (+ count 1))
        (when (and (> MEASURES_PER_SYSTEM 0)
                   (zero? (modulo count MEASURES_PER_SYSTEM)))
          (let* ((ctx (ly:translator-context engraver))
                 (col (ly:context-property ctx 'currentCommandColumn)))
            (when (ly:grob? col)
              (ly:grob-set-property! col 'line-break-permission 'force))))))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Plotting the grid

#(define (square-color voice-id)
  (cond
    ((string=? voice-id "polos_low") (rgb-color 0.678 0.88 0.898))
    ((string=? voice-id "sangsih_low") (rgb-color 0.980 0.500 0.564))
    ((string=? voice-id "polos_hi") (rgb-color 0.678 0.88 0.898))
    ((string=? voice-id "sangsih_hi") (rgb-color 0.980 0.500 0.564))    
    (else (rgb-color 0.5 0.5 0.5))))


#(define (create-auto-grid grob)
  "Create a grid using notes collected for this specific system"
  (let* ((staff-space 1.0)
         (thickness 0.1)
         (pitches (length reyong_notes))
         (beats-per-measure 16)
         (cell-size staff-space)
	 (num-systems (hash-count (const #t) notes-by-system))
         (current-system (modulo system-counter num-systems))
         (system-start-measure (+ (* current-system MEASURES_PER_SYSTEM) 1))
         (grid-width (* MEASURES_PER_SYSTEM beats-per-measure cell-size))
         (grid-height (* pitches cell-size))
         (system-notes (hash-ref notes-by-system current-system '()))
         (stencils '()))
	     
    ; Increment counter for next system
    (set! system-counter (+ system-counter 1))
        
    ; Draw horizontal lines
    (do ((i 0 (1+ i)))
        ((> i pitches))
      (let* ((y-pos (* (- (/ pitches 2) i) cell-size))
             (line (ly:round-filled-box
                    (cons 0 grid-width)
                    (cons (- (* thickness 0.5)) (* thickness 0.5))
                    0)))
        (set! stencils
              (cons (stencil-with-color
	                (ly:stencil-translate-axis line y-pos Y)
		        (rgb-color 0.1 0.1 0.1))
                    stencils))))
    
    ; Draw vertical lines for beats
    (do ((i 0 (1+ i)))
        ((> i (* MEASURES_PER_SYSTEM beats-per-measure)))
      (let* ((x-pos (* i cell-size))
             (line-thickness (if (= (modulo i beats-per-measure) 0) 
                               (* thickness 2) 
                               thickness))
             (line (ly:round-filled-box
                    (cons (- (* line-thickness 0.5)) (* line-thickness 0.5))
                    (cons (- (/ grid-height 2)) (/ grid-height 2))
                    0)))
        (set! stencils
              (cons (stencil-with-color
	                (ly:stencil-translate-axis line x-pos X)
			(rgb-color 0.1 0.1 0.1))
                    stencils))))
    
    ; Fill cells for notes in this system
    (for-each
      (lambda (note-info)
        (let* ((measure-num (car note-info))
               (beat-in-measure (cadr note-info))
               (voice-id (caddr note-info))
	       (note-idx (cadddr note-info))
               (relative-measure (- measure-num system-start-measure))
               (relative-beat (+ (* relative-measure beats-per-measure) beat-in-measure))
               (x-pos (* relative-beat cell-size))
               (y-pos (* (- note-idx (/ (length reyong_notes) 2)) cell-size))
               (cell (ly:round-filled-box
                      (cons 0 cell-size)
                      (cons 0 cell-size)
                      0))
	       (cell (stencil-with-color cell (square-color voice-id)))
               (filled-cell (ly:stencil-translate-axis
                            (ly:stencil-translate-axis cell x-pos X)
                            y-pos Y)))
          (set! stencils (cons filled-cell stencils))))
      system-notes)
    
    (apply ly:stencil-add stencils)))


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Gather notes into a hash table

#(define (pitch->components p)
   (list (ly:pitch-notename p)
         (ly:pitch-alteration p)
         (ly:pitch-octave p)))

#(define reyong_notes_components
    (map pitch->components reyong_notes))

#(define reyong_note_idx
  (lambda (notename alteration octave)
    (list-index
      (lambda (x) (equal? x (list notename alteration octave)))
       reyong_notes_components)))

% It is attached to the Staff context. It is run in the music interpretation phase
note_collector_engraver =
#(make-engraver
  (acknowledgers
   ((note-head-interface engraver grob source-engraver)
    (let* ((note-event (ly:grob-property grob 'cause))
           (pitch (ly:event-property note-event 'pitch))
           (notename (ly:pitch-notename pitch))
	   (alteration (ly:pitch-alteration pitch))
           (octave (ly:pitch-octave pitch))
	   (note-index (reyong_note_idx notename alteration octave))
           (context (ly:translator-context engraver))
           (measure-num (ly:context-property context 'currentBarNumber 1))
           (moment (ly:context-current-moment context))
           (beat-pos (ly:moment-main moment))
           (beat-in-measure (modulo (inexact->exact (floor (* beat-pos 16))) 16))
           ; Calculate which system this note belongs to
           (system-num (quotient (- measure-num 1) MEASURES_PER_SYSTEM))
	   ; Get the voice id
	   (voice-context (ly:translator-context source-engraver))
	   (voice-id (ly:context-id voice-context))
	   ; All the info needed
	   (all-info (list measure-num beat-in-measure voice-id note-index)))
      
      ; Store note in the appropriate system
      (let* ((current-notes (hash-ref notes-by-system system-num '()))
             (new-note all-info))
	;(display "\n")
	;(display new-note)
	;(display "\n")
        (hash-set! notes-by-system system-num (cons new-note current-notes)))
      
      (ly:grob-set-property! grob 'stencil empty-stencil)))))

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


gridStaff = {
  \override Staff.StaffSymbol.stencil = #create-auto-grid
  \override Staff.StaffSymbol.line-count = #(length reyong_notes)
  \override Staff.StaffSymbol.staff-space = #1.0
  
  % Hide all traditional notation elements
  \override NoteHead.stencil = ##f
  \override Stem.stencil = ##f
  \override Beam.stencil = ##f
  \override Flag.stencil = ##f
  \override Rest.stencil = ##f
  \override Accidental.stencil = ##f
  \override Clef.stencil = ##f
  \override TimeSignature.stencil = ##f
  \override KeySignature.stencil = ##f
  \override BarLine.stencil = ##f
}

#(define voice_color_engraver
  (lambda (voice-color)
    (make-engraver
     (acknowledgers
      ((note-head-interface engraver grob source-engraver)
       (ly:grob-set-property! grob 'color voice-color))))))

polos_low = {
      \key a \major

      % measure 1
      r16    a'16	i'8
      a'16   i'8	        a'16
      i'8    		a'16	i'16
      r16    i'16	a'16	i'16    |

      % measure 2
      r16    a'16	i'8
      a'16   i'8		a'16
      i'8    		a'16	i'16
      r16    i'16	a'16	i'16    |
      
      % measure 3 (i'16)
      r16   u'16	a'8
      u'16   a'8		u'16
      a'8    		u'16	a'16
      r16    u'16	a'8             |
      
      % measure 4
      u'8    		a'16	u'16
      r16    a'16	u'8
      a'16   u'8		a'16
      u'8    		u'16 	a'16    |
      
      % measure 5
      r16    u'16	a'8
      u'16   a'8		u'16
      a'8    		u'16 	a'16
      i'16    a'16	a'16    i'16    |

      % measure 6
      r16    e'16	u'8
      e'16   u'8		e'16
      u'8               e'16    u'16                
      a'16   a'8                u'16    |

      % measure 7
      r8		i'16	u'16
      r8		i'16	u'16
      r8		i'16	u'16
      r8		i'16	u'16    |

      % measure 8
      r8		i'16	u'16
      r8		i'16	u'16
      r8		i'16	u'16
      r8		i'16	u'16    |

      % measure 9
      r16    i'16    u'8
      i'16   u'8		i'16
      u'8    	     i'16	u'16
      r16    i'16    u'8

      % measure 10
      r16    a'16    i'8
      a'16   i'8	        a'16
      i'8    	     a'16	i'16
      r16    a'8     		r16

      % measure 11
      r16    u'16    a'8
      a'16   a'16    i'16	a'16
      u'16   u'8    		u'16
      a'16   a'16     u'16	a'16

      % measure 12
      r16    u'16    a'8
      u'16   a'8		u'16
      a'8   	     u'16	a'16
      r16    a'16    u'16	a'16

      % measure 13
      r16    a'16    u'8
      a'16   u'8                a'16
      u'8    	     a'16	u'16
      r8	     u'16	a'16

      % measure 14
      a'8	     a'16	u'16
      r16    a'16    u'8
      a'16   a'16    u'8
      r8             u'16	u'16

      % measure 15
      r16	a'16	u'16	r16
      a'16	r16	u'16	r16
      a'16	a'16	u'16	r16
      a'8		u'8

      % measure 16
      r16	e'16	u'8
      e'16	u'8		e'16
      u'16	u'8		e'16
      u'8		e'8

      % measure 17
      r8		a'8
      r8		a'8      
      r8		a'8
      r8		a'8

      % measure 18
      r8		u'8
      r16	u'16	r8
      u'16	r8		u'16
      r8		u'8

      % measure 19
      u'16	a'16	r8
      u'16	r8		a'16
      a'16	a'8		u'16
      a'16	r16	a'16	r16

      % measure 20
      o'16	i'16	r16	i'16
      o'16	i'16	r16	i'16
      o'16	i'16	r16	i'16
      r8		i'16	a'16

      % measure 21
      r16	u'16	a'16	r16
      a'16	a'16	r16	a'16
      u'16	u'16	r16	u'16
      a'16	a'16	u'16	a'16

      % measure 22
      r16	u'16	a'16	r16
      u'16	a'16	r16	u'16
      a'16	r16	u'16	a'16
      r16	u'16	a'16	r16

      % measure 23
      u'16	r16	a'16	u'16
      a'16	r16	u'8
      a'16	u'16	a'16	a'16
      u'8		a'16	u'16

      % measure 24
      r16	a'16	u'8
      a'16	u'16	a'16	u'16
      r16	a'16	u'8
      a'16	a'16	u'8

      % measure 25
      a'16	u'16	a'16	u'16
      r16	a'16	u'8
      a'16	u'8		a'16
      u'16	r16	r8

      % measure 26
      r16	e'16	u'8
      e'16	u'8		e'16
      u'16	u'8		e'16
      u'8		e'8

      % measure 27
      e'8	a'8
      e'8	a'8
      e'8	a'8
      e'8	a'8

      % measure 28
      e'16	a'8		e'16
      a'8		e'16	a'16
      r16	e'16	a'8
      a'16      r8		r16

      % measure 29 (switch to Piano 2)
      u'16	u'16	   a'8
      a'8		   u'16	a'16
      r16       u'16	   a'16	r16
      a'16	r16	   a'16	r16

      % measure 30
      r16	a'16	i'8
      a'16	i'8		a'16
      i'8		a'16	i'16
      r8		i'16	a'16

      % measure 31
      r16	u'16	a'8
      a'16	a'16	u'16	a'16
      u'8		a'16	u'16
      a'8		u'16	a'16
}

sangsih_low = {
      \key a \major

      % measure 1
      o'8		e''16	o'16
      r16    e''16	o'8
      e''16  o'8		e''16
      o'16   e''8	     	e''16  |

      % measure 2
      o'8		e''16	o'16
      r16      e''16 	o'8
      e''16    o'8		e''16
      o'16     e''8        	e''16  |
      
      % measure 3 (e''8)
      r8		e''16	i'16
      r16      e''16	i'8
      e''16    i'8		e''16
      i'8      		e''16	i'16   |
      
      % measure 4
      r16      i'16 	e''8
      i'16     e''8		i'16
      e''8     		i'16	e''16
      r16      i'16     r8

      % measure 5
      i'8		e''16	i'16
      r16      e''16	i'8
      e''16    i'8              e''16
      r16      e''16     e''16  r16

      % measure 6
      a'8		i'16	a'16
      r16      i'16	a'8
      i'16     a'8		i'16
      r16      e''16	i'8

      % measure 7
      e''8		e''8
      e''8		e''8
      e''8		e''8
      e''8		e''8

      % measure 8
      e''8		e''8
      e''8		e''8
      e''8		e''8
      e''8		e''8

      % measure 9
      e''8		r16	e''16
      r8		e''8
      r16	e''16	r8
      e''16	r8		e''16

      % measure 10
      o'8		e''16	o'16
      r16	e''16	o'8
      e''16	o'8		e''16
      o'16	e''16	o'16	e''16

      % measure 11
      i'16	i'8		i'16
      e''16	e''8		e''16
      r16	i'16	e''8
      r16	i'8		e''16

      % measure 12
      i'8		e''16	i'16
      r16	e''16	i'8
      e''16     i'8             e''16	
      i'16	e''8		e''16

      % measure 13
      i'8		e''16	i'16
      r16	e''16	i'8
      e''16	i'8		e''16
      r16	e''16   i'16	e''16

      % measure 14
      e''16	i'16	e''8
      i'16	e''8		i'16
      e''16	e''8		i'16
      r16	i'16	e''16	e''16

      % measure 15
      r8		e''16	i'16
      e''16	i'8		i'16
      e''16	e''8		i'16
      e''16	o'8		i'16

      % measure 16
      a'8		i'16	a'16
      r16	i'16	a'8
      i'16	i'16	a'8
      i'16	a'8		i'16

      % measure 17
      e''8		i'16	u''16
      e''8		i'16	u''16
      e''8		i'16	u''16
      e''8		i'16	u''16

      % measure 18
      e''8		o'16	e''16
      r16	o'16	e''8
      o'16	e''8		o'16
      e''16	i'16	e''16	o'16

      % measure 19
      r16	e''16	i'8
      e''16	i'8		e''16
      e''16	e''8		i'16
      r16	e''16	r16	e''16

      % measure 20
      r16	e''16	o'16	r16
      r16	e''16	o'16	r16
      r16	e''16	o'16	r16
      r16	o'16	r8

      % measure 21
      i'16	i'16	r16	i'16
      e''16	e''16	i'16	e''16
      r16	i'16	e''16	r16
      r16	i'8		e''16

      % measure 22
      i'8		e''16	i'16
      r16	e''16	i'8
      e''16	i'16	e''8
      i'16	e''8		i'16

      % measure 23
      r16	i'16	e''8
      e''16	i'8		i'16
      e''16	i'16	e''16	e''16
      r16	i'16	e''8

      % measure 24
      i'16	e''8		i'16
      e''8		e''8
      i'16	e''8		i'16
      e''16	e''16	r16	i'16

      % measure 25
      e''8		e''8
      i'16      e''8		i'16
      e''8		i'16	e''16
      r16	e''16	o'16	i'16

      % measure 26
      a'8		i'16	a'16
      r16	i'16	a'8
      i'16	i'16	a'8
      i'16	a'8		o'16

      % measure 27
      r16	o'16	i'16	o'16
      r16	o'16	i'16	o'16
      r16	o'16	i'16	o'16
      r16	o'16	i'16	o'16

      % measure 28
      r16	i'16	o'8
      i'16	o'8		i'16
      o'8		i'16	o'16
      e''8		r8

      % measure 29 (switch to Piano 2)
      r8	   	e''16   i'16
      e''16	i'8		e''16
      i'8		e''16	i'16
      e''16	i'8		i'16

      % measure 30
      o'8		e''16	o'16
      r16	e''16	o'8
      e''16	o'8		e''16
      o'16	o'16	r8

      % measure 31
      i'16	r8		i'16
      e''16	e''16	i'16	e''16
      r16	i'16	e''8
      r16	i'8		r16	
}

polos_hi = {
      \key a \major

      % measure 1
      r16     a''16	i''8
      a''16   i''8	        a''16
      i''8    		a''16	i''16
      r16     i''16	a''16	i''16    |

      % measure 2
      r16    a''16	i''8
      a''16   i''8		a''16
      i''8    		a''16	i''16
      r16    i''16	a''16	i''16    |

      % measure 3 (i''16)
      r8		a''8
      u''16   a''8		u''16
      a''8    		u''16	a''16
      r16    u''16	a''8             |

      % measure 4
      u''8    		a''16	u''16
      r16    a''16	u''8
      a''16   u''8		a''16
      u''8    		u''16 	a''16    |

      % measure 5
      r16    u''16	a''8
      u''16   a''8		u''16
      a''8    		u''16 	a''16
      i''16    a''16	a''16    i''16    |

      % measure 6
      r16    e''16	u''8
      e''16   u''8		e''16
      u''8               e''16    u''16                
      a''16   a''8                u''16    |

      % measure 7
      r8		i''16	u''16
      r8		i''16	u''16
      r8		i''16	u''16
      r8		i''16	u''16    |

      % measure 8
      r8		i''16	u''16
      r8		i''16	u''16
      r8		i''16	u''16
      r8		i''16	u''16    |

      % measure 9
      r16    i''16    u''8
      i''16   u''8		i''16
      u''8    	     i''16	u''16
      r16    i''16    u''8

      % measure 10
      r16    a''16    i''8
      a''16   i''8	        a''16
      i''8    	     a''16	i''16
      r16    a''8     		r16

      % measure 11
      r16    u''16    a''8
      a''16   a''16    i''16	a''16
      u''16   u''8    		u''16
      a''16   a''16     u''16	a''16

      % measure 12
      r16    u''16    a''8
      u''16   a''8		u''16
      a''8   	     u''16	a''16
      r16    a''16    u''16	a''16

      % measure 13
      r16    a''16    u''8
      a''16   u''8                a''16
      u''8    	     a''16	u''16
      r8	     u''16	a''16

      % measure 14
      a''8	     a''16	u''16
      r16    a''16    u''8
      a''16   a''16    u''8
      r8             u''16	u''16

      % measure 15
      r16	a''16	u''16	r16
      a''16	r16	u''16	r16
      a''16	a''16	u''16	r16
      a''8		u''8

      % measure 16
      r16	e''16	u''8
      e''16	u''8		e''16
      u''16	u''8		e''16
      u''8		e''8
      
      % measure 17
      r8		a''8
      r8		a''8      
      r8		a''8
      r8		a''8

      % measure 18
      r8		u''8
      r16	u''16	r8
      u''16	r8		u''16
      r8		u''8

      % measure 19
      u''16	a''16	r8
      u''16	r8		a''16
      a''16	a''8		u''16
      a''16	r16	a''16	r16

      % measure 20
      o''16	i''16	r16	i''16
      o''16	i''16	r16	i''16
      o''16	i''16	r16	i''16
      r8		i''16	a''16

      % measure 21
      r16	u''16	a''16	r16
      a''16	a''16	r16	a''16
      u''16	u''16	r16	u''16
      a''16	a''16	u''16	a''16

      % measure 22
      r16	u''16	a''16	r16
      u''16	a''16	r16	u''16
      a''16	r16	u''16	a''16
      r16	u''16	a''16	r16

      % measure 23
      u''16	r16	a''16	u''16
      a''16	r16	u''8
      a''16	u''16	a''16	a''16
      u''8		a''16	u''16

      % measure 24
      r16	a''16	u''8
      a''16	u''16	a''16	u''16
      r16	a''16	u''8
      a''16	a''16	u''8

      % measure 25
      a''16	u''16	a''16	u''16
      r16	a''16	u''8
      a''16	u''8		a''16
      u''16	r16	r8

      % measure 26
      r16	e''16	u''8
      e''16	u''8		e''16
      u''16	u''8		e''16
      u''8		e''8

      % measure 27
      e''16	u''8		u''16
      e''16	u''8		u''16
      e''16	u''8		u''16
      e''16	u''8		u''16

      % measure 28
      e''16	a''8		e''16
      a''8		e''16	a''16
      r16	e''16	a''8
      a''16      r8		r16

      % measure 29 (switch to Piano 2)
      u''16	u''16	   a''8
      a''8		   u''16	a''16
      r16       u''16	   a''16	r16
      a''16	r16	   a''16	r16

      % measure 30
      r16	a''16	i''8
      a''16	i''8		a''16
      i''8		a''16	i''16
      r8		i''16	a''16

      % measure 31
      r16	u''16	a''8
      a''16	a''16	u''16	a''16
      u''8		a''16	u''16
      a''8		u''16	<a'' e'''>16
}

sangsih_hi = {
      \key a \major

      % measure 1
      o''8		e'''16	o''16
      r16    e'''16	o''8
      e'''16  o''8		e'''16
      o''16   e'''8	     	e'''16  |

      % measure 2
      o''8		e'''16	o''16
      r16      e'''16 	o''8
      e'''16    o''8		e'''16
      o''16     e'''8        	e'''16  |

      % measure 3 (e'''8)
      r8     		e'''16	i''16
      r16      e'''16	i''8
      e'''16    i''8		e'''16
      i''8      		e'''16	i''16   |

      % measure 4
      r16      i''16 	e'''8
      i''16     e'''8		i''16
      e'''8     		i''16	e'''16
      r16      i''16     r8

      % measure 5
      i''8		e'''16	i''16
      r16      e'''16	i''8
      e'''16    i''8              e'''16
      r16      e'''16     e'''16  r16

      % measure 6
      a''8		i''16	a''16
      r16      i''16	a''8
      i''16     a''8		i''16
      r16      e'''16	i''8

      % measure 7
      e'''8		e'''16	u'''16
      e'''8		e'''16   u'''16
      e'''8		e'''16   u'''16
      e'''8		e'''16   u'''16

      % measure 8
      e'''8		e'''16	u'''16
      e'''8		e'''16   u'''16
      e'''8		e'''16   u'''16
      e'''8		e'''16   u'''16

      % measure 9
      e'''8		u'''16	e'''16
      r16    u'''16	e'''8
      u'''16	e'''16	r16     u'''16
      e'''16	r16    u'''16		e'''16

      % measure 10
      o''8		e'''16	o''16
      r16	e'''16	o''8
      e'''16	o''8		e'''16
      o''16	e'''16	o''16	e'''16

      % measure 11
      i''16	i''8		i''16
      e'''16	e'''8		e'''16
      r16	i''16	e'''8
      r16	i''8		e'''16

      % measure 12
      i''8		e'''16	i''16
      r16	e'''16	i''8
      e'''16     i''8             e'''16	
      i''16	e'''8		e'''16

      % measure 13
      i''8		e'''16	i''16
      r16	e'''16	i''8
      e'''16	i''8		e'''16
      r16	e'''16   i''16	e'''16

      % measure 14
      e'''16	i''16	e'''8
      i''16	e'''8		i''16
      e'''16	e'''8		i''16
      r16	i''16	e'''16	e'''16

      % measure 15
      r8		e'''16	i''16
      e'''16	i''8		i''16
      e'''16	e'''8		i''16
      e'''16	o''8		i''16

      % measure 16
      a''8		i''16	a''16
      r16	i''16	a''8
      i''16	i''16	a''8
      i''16	a''8		i''16

      % measure 17
      e'''8		i''16	u'''16
      e'''8		i''16	u'''16
      e'''8		i''16	u'''16
      e'''8		i''16	u'''16      

      % measure 18
      e'''8		<o'' u'''>16	e'''16
      r16	<o'' u'''>16	e'''8
      <o'' u'''>16	e'''8		<o'' u'''>16
      e'''16	<i'' u'''>16	e'''16	o''16

      % measure 19
      r16	e'''16	i''8
      e'''16	i''8		e'''16
      e'''16	e'''8		i''16
      r16	<i'' u''>16	r16	<i'' u''>16

      % measure 20
      r16	e'''16	o''16	r16
      r16	e'''16	o''16	r16
      r16	e'''16	o''16	r16
      r16	o''16	r8

      % measure 21
      i''16	i''16	r16	i''16
      e'''16	e'''16	i''16	e'''16
      r16	i''16	e'''16	r16
      r16	i''8		e'''16

      % measure 22
      i''8		e'''16	i''16
      r16	e'''16	i''8
      e'''16	i''16	e'''8
      i''16	e'''8		i''16

      % measure 23
      r16	i''16	e'''8
      e'''16	i''8		i''16
      e'''16	i''16	e'''16	e'''16
      r16	i''16	e'''8

      % measure 24
      i''16	e'''8		i''16
      e'''8		e'''8
      i''16	e'''8		i''16
      e'''16	e'''16	r16	i''16

      % measure 25
      e'''8		e'''8
      i''16      e'''8		i''16
      e'''8		i''16	e'''16
      r16	e'''16	o''16	i''16

      % measure 26
      a''8		i''16	a''16
      r16	i''16	a''8
      i''16	i''16	a''8
      i''16	a''8		o''16

      % measure 27
      u'''16	o''16	i''16	o''16
      u'''16	o''16	i''16	o''16
      u'''16	o''16	i''16	o''16
      u'''16	o''16	i''16	o''16

      % measure 28
      u'''16	i''16	o''16	u'''16
      i''16	o''16	u'''16	i''16
      o''16	u'''16	i''16	o''16
      e'''8		r8

      % measure 29 (switch to Piano 2)
      r8	   	e'''16   i''16
      e'''16	i''8		e'''16
      i''8		e'''16	i''16
      e'''16	i''8		i''16

      % measure 30
      o''8		e'''16	o''16
      r16	e'''16	o''8
      e'''16	o''8		e'''16
      o''16	o''16	r8

      % measure 31
      i''16	r8		i''16
      e'''16	e'''16	i''16	e'''16
      r16	i''16	e'''8
      r16	i''8		r16	
}      


reyong_notes_display = {
    \key a \major
    e'4^\markup{\center-align{deng}}
    u'4^\markup{\center-align{dung}}
    a'4^\markup{\center-align{dang}}
    i'4^\markup{\center-align{ding}}
    o'4^\markup{\center-align{dong}}
    e''4^\markup{\center-align{e}}
    u''4^\markup{\center-align{u}}
    a''4^\markup{\center-align{a}}
    i''4^\markup{\center-align{i}}
    o''4^\markup{\center-align{o}}
    e'''4^\markup{\center-align{e}}
    u'''4^\markup{\center-align{u}}
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


\markup {
  \column {
    \vspace #1 
    \line { "Approximate" \italic "Pelog" "scale for the" \italic "Reyong" }
    \vspace #1
  }
}

\score {
    \new Staff \reyong_notes_display

    \layout {}
}

\markup {
  \column {
    \line { "Example of" \italic "Kotekan" "(interlocked melodies) from McPhee, C. (1940)" }
    \vspace #1
  }
}

\score {
    \new Staff \with {
        instrumentName = " S. + P."
    } {
    <<
	\new Voice = "polos_hi" \with {
	    \consists #(voice_color_engraver (rgb-color 0.0 0.0 0.0))
	} {
	  \stemUp \polos_hi
	}

	\new Voice = "sangsih_hi" \with {
	    \consists #(voice_color_engraver (rgb-color 0.0 0.0 0.0))
	} {
	  \stemDown \sangsih_hi
	}
   >>
   }
    \layout {
        %\context {
    	%    \Voice
    	%    \override Rest.transparent = ##t
        %}
    }
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Engrave in grid format


\markup {
  \column {
    \line { "The Reyong Kotekan in grid format" }
    \vspace #2
  }
}

\score {
  \new Staff \with {
      % Setting parameters for the Staff context
      % Importantly, it customises the Staff.StaffSymbol stencil to actually plot the grids
      
      \gridStaff
  }  {
      <<
          \new Voice = "polos_low" {
            \voiceOne
            \polos_low
          }
          \new Voice = "sangsih_low" {
            \voiceTwo
            \sangsih_low
          }
          \new Voice = "polos_hi" {
            \voiceThree
            \polos_hi
          }
          \new Voice = "sangsih_hi" {
            \voiceFour
            \sangsih_hi
          }	  
      >>
  }

  \layout {
      \context {
          % Add the autoBreakEngraver engraver to the Score context.
	  % It breaks the music into systems
          \Score
	  \consists \autoBreakEngraver
      }

      \context {
          % Add the note_collector_engraver to the Staff context
	  % It disables plotting traditional notes, and saves all pitch information in a hash table
          \Staff
          \consists \note_collector_engraver
	  \omit StaffSymbol

      }      
      indent = 0
  }

}
