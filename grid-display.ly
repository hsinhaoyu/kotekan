%%%%%%%%%%%%%%%%%%%%%%%%%%% Global variables

#(define system-counter 1)
#(define notes-by-system (make-hash-table))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% color maps

#(define (voice-color-func voice-id notename)
  (cond
    ((string=? voice-id "polos_low") (rgb-color 0.678 0.88 0.898))
    ((string=? voice-id "sangsih_low") (rgb-color 0.980 0.500 0.564))
    ((string=? voice-id "polos_hi") (rgb-color 0.678 0.88 0.898))
    ((string=? voice-id "sangsih_hi") (rgb-color 0.980 0.500 0.564))    
    (else (rgb-color 0.5 0.5 0.5))))

#(define (notename-color-func voice-id notename)
  (cond
    ((= notename 0) (rgb-color 1.0 0.0 0.0))
    ((= notename 1) (rgb-color 0.0 0.8 0.0))
    ((= notename 2) (rgb-color 0.0 0.0 1.0))
    ((= notename 3) (rgb-color 1.0 1.0 0.0))
    ((= notename 4) (rgb-color 1.0 0.6 0.0))
    ((= notename 5) (rgb-color 0.5 0.6 0.5))
    ((= notename 6) (rgb-color 0.6 0.4 0.2))    
    (else (rgb-color 0.5 0.5 0.5))))


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Plotting the grid

#(define (mk-create-grid scale-notes color-func)
  (lambda (grob)
    (let* ((staff-space 1.0)
           (thickness 0.1)
           (pitches (length scale-notes))
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
	       (notename (list-ref note-info 4))
	       (zz (display notename))
               (relative-measure (- measure-num system-start-measure))
               (relative-beat (+ (* relative-measure beats-per-measure) beat-in-measure))
               (x-pos (* relative-beat cell-size))
               (y-pos (* (- note-idx (/ (length scale-notes) 2)) cell-size))
               (cell (ly:round-filled-box
                      (cons 0 cell-size)
                      (cons 0 cell-size)
                      0))
	       (cell (stencil-with-color cell (color-func voice-id notename)))
               (filled-cell (ly:stencil-translate-axis
                            (ly:stencil-translate-axis cell x-pos X)
                            y-pos Y)))
          (set! stencils (cons filled-cell stencils))))
      system-notes)
    
    (apply ly:stencil-add stencils))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Gather notes into a hash table

#(define (pitch->components p)
   (list (ly:pitch-notename p)
         (ly:pitch-alteration p)
         (ly:pitch-octave p)))

#(define (mk-note-collector scale-notes)
  (let* ((scale-notes-components (map pitch->components scale-notes))
         (note->index (lambda (notename alteration octave)
                        (list-index
			  (lambda (x) (equal? x (list notename alteration octave)))
			  scale-notes-components))))
    (make-engraver
      (acknowledgers
        ((note-head-interface engraver grob source-engraver)
        (let* ((note-event (ly:grob-property grob 'cause))
               (pitch (ly:event-property note-event 'pitch))
               (notename (ly:pitch-notename pitch))
	       (alteration (ly:pitch-alteration pitch))
               (octave (ly:pitch-octave pitch))
	       (note-index (note->index notename alteration octave))
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
	       (all-info (list measure-num beat-in-measure voice-id note-index notename alteration octave))
	       (current-notes (hash-ref notes-by-system system-num '())))
	  (when note-index
	      (hash-set! notes-by-system system-num (cons all-info current-notes)))
	  (ly:grob-set-property! grob 'stencil empty-stencil)))))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gridStaffParams =
#(define-music-function (scale-notes color-func) (list? procedure?)
  #{
    \override Staff.StaffSymbol.stencil = #(mk-create-grid scale-notes color-func)
    \override Staff.StaffSymbol.line-count = #(length scale-notes)
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
  #})
