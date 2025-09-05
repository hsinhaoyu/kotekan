#(use-modules (srfi srfi-1))

%%%%%%%%%%%%%%%%%%%%%%%%%%% Global variables

#(define notes-by-system (make-hash-table))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This engraver is attached to the Score context
% This engraver can encounter the same bar number multiple times, because
% There might be more than one instruments.  Addressed by a hash table.

autoBreakEngraver =
#(let ((count 0)
       (seen-measures (make-hash-table)))
   (make-engraver
     (acknowledgers
       ((bar-line-interface engraver grob source-engraver)
        (let* ((ctx (ly:translator-context engraver))
               (measure-num (ly:context-property ctx 'currentBarNumber 1)))
          (unless (hash-ref seen-measures measure-num #f)
            (hash-set! seen-measures measure-num #t)
            (set! count (+ count 1))
            (when (and (> MEASURES_PER_SYSTEM 0)
                       (zero? (modulo count MEASURES_PER_SYSTEM)))
              (let ((col (ly:context-property ctx 'currentCommandColumn)))
                (when (ly:grob? col)
                  (ly:grob-set-property! col 'line-break-permission 'force))))))))))

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
%% Turns a grob into a stencil
#(define (mk-create-grid scale-notes color-func)
  (lambda (grob)
    (let* ((staff-space 0.9)
           (thickness 0.1)
           (pitches (length scale-notes))
           (beats-per-measure 16)
           (cell-size staff-space)
	   (current-system (hashq-ref staffsymbol-index-table grob #f))
           (system-start-measure (+ (* current-system MEASURES_PER_SYSTEM) 1))
           (grid-width (* MEASURES_PER_SYSTEM beats-per-measure cell-size))
           (grid-height (* pitches cell-size))
           (system-notes (hash-ref notes-by-system current-system '()))
           (stencils '()))

(display "\n")
(display grob)
(display color-func)
(display "\n")
        
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
		        (rgb-color 0.6 0.6 0.6))
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
			(rgb-color 0.6 0.6 0.6))
                    stencils))))
    
    ; Fill cells for notes in this system
    (for-each
      (lambda (note-info)
        (let* ((measure-num (car note-info))
               (beat-in-measure (cadr note-info))
               (voice-id (caddr note-info))
	       (note-idx (cadddr note-info))
	       (notename (list-ref note-info 4))
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

#(define get-init-bar-num
   (let ((bar-num-0 #f))
     (lambda (current-bar-number)
       (if bar-num-0
           bar-num-0
           (begin (set! bar-num-0 current-bar-number)
                  bar-num-0)))))

#(define (mk-note-collector scale-notes instrument-names)
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
	       (init-measure-num (get-init-bar-num measure-num))
	       (measure-num (1+ (- measure-num init-measure-num)))
               (moment (ly:context-current-moment context))
               (beat-pos (ly:moment-main moment))
               (beat-in-measure (modulo (inexact->exact (floor (* beat-pos 16))) 16))
               (system-num (quotient (- measure-num 1) MEASURES_PER_SYSTEM))
	       (voice-context (ly:translator-context source-engraver))
	       (voice-id (ly:context-id voice-context))

	       (staff-context (ly:translator-context engraver))
	       (instrument-name (ly:context-property staff-context 'instrumentName ""))
	       (instrument-idx (list-index (lambda (x) (string=? x instrument-name)) instrument-names))
	       (hash-idx (+ (* system-num (length instrument-names)) instrument-idx))
	       (zzzzz (format #t "measure n:~a\tsys n:~a\t~a\t~a\t~a \n" measure-num system-num instrument-name instrument-idx hash-idx))

	       (all-info (list measure-num beat-in-measure voice-id note-index notename alteration octave))
	       (current-notes (hash-ref notes-by-system hash-idx '())))

	  (when note-index
	      (hash-set! notes-by-system hash-idx (cons all-info current-notes)))
	  (ly:grob-set-property! grob 'stencil empty-stencil)))))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% system-index-table: map system to system index
% staffsymbol-index-table: map staffsymbol to system index
% in lilypond, a system can have multiple staffs 

#(define system-index-table (make-hash-table))
#(define s-counter -1)

#(define (ensure-system-index sys)
  (let ((idx (and sys (hashq-ref system-index-table sys #f))))
    (if idx
        idx
        (begin
          (set! s-counter (+ s-counter 1)) 
          (hashq-set! system-index-table sys s-counter)
          s-counter))))

% sys is a sys grob, which might have multiple instrumemts
#(define (system-after-line-breaking sys)
   (let ((idx (ensure-system-index sys)))
     (format #t "System finalized, index ~a\n" idx)))

#(define staffsymbol-index-table (make-hash-table))

#(define (staffsymbol-after-line-breaking ss)
   (let* ((sys (ly:grob-system ss))
          (idx (ensure-system-index sys)))
     (hashq-set! staffsymbol-index-table ss idx)
     (format #t "StaffSymbol finalized with system-index=~a\n" idx)))


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#(define (bn-x-offset-sp grob sp)
   (let* ((ctx (ly:grob-context grob))
          (bar (ly:context-property ctx 'currentBarNumber 0))
          (digits (string-length (number->string (max 1 bar)))))
     (+ 2.0 (* 0.25 digits))) )  

#(define (staff-left-x sys grob)
   (let* ((vagg  (ly:grob-parent grob Y))    
          (staff (and vagg (ly:grob-object vagg 'staff-symbol))))
     (cond
       ((and staff (ly:grob? staff))
        (car (ly:grob-extent staff sys X)))                  
       (else
        (car (ly:grob-extent sys sys X))))) )                

#(define (my-bar-number-stencil grob)
   (let* ((st     (ly:text-interface::print grob))           
          (sys    (ly:grob-system grob))
          (layout (and grob (ly:grob-layout grob)))
          (sp     (and layout (ly:output-def-lookup layout 'staff-space))))

(display grob)

     (if (and (ly:stencil? st) sys sp)
         (let* ((anchor-left (staff-left-x sys grob))        
                (bn-x        (ly:grob-relative-coordinate grob sys X))
                (target-x    (+ anchor-left (* sp (bn-x-offset-sp grob sp))))
                (delta       (- target-x bn-x)))
           (ly:stencil-translate-axis st delta X))
         st)))


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
