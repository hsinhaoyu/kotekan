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
#(define (mk-create-grid scale-notes color-func instrument-names instrument-idx)
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
           (composite-key (cons current-system instrument-idx))
           (system-notes (hash-ref notes-by-system composite-key '()))
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
	       (instrument-idx (or instrument-idx 0)) 

	       (composite-key (cons system-num instrument-idx))
	       (all-info (list measure-num beat-in-measure voice-id note-index notename alteration octave))
	       (current-notes (hash-ref notes-by-system composite-key '())))

	  (when note-index
	      (hash-set! notes-by-system composite-key (cons all-info current-notes)))
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

#(define MEASURES-PER-SYSTEM   6)     
#(define BEATS-PER-MEASURE    16)     
#(define CELL-SIZE           0.90)    
#(define FIRST-BAR-NUMBER    1)
#(define BAR-BOTTOM-PAD-SP   0.18)
#(define NUMBER-FONTSIZE    -1)

#(define (pair?* x) (and (pair? x) #t))

#(define (extent-left grob frame)
  (let ((e (and (ly:grob? grob) (ly:grob? frame) (ly:grob-extent grob frame X))))
    (and (pair?* e) (car e))))

#(define (extent-top  grob frame)
  (let ((e (and (ly:grob? grob) (ly:grob? frame) (ly:grob-extent grob frame Y))))
    (and (pair?* e) (cdr e))))

#(define (staff-space grob)
  (and (ly:grob-layout grob)
       (ly:output-def-lookup (ly:grob-layout grob) 'staff-space)))

#(define (mk-number-stencil grob str)
  (grob-interpret-markup grob (markup #:fontsize NUMBER-FONTSIZE str)))


#(define (translate-stencil st dx dy)
  (and (ly:stencil? st) (ly:stencil-translate st (cons dx dy)) st))

% return a grob
#(define (system-top-staffsymbol sys)
  (let* ((elts (ly:grob-object sys 'elements))
         (lst  (and (ly:grob-array? elts) (ly:grob-array->list elts))))
    (and (pair? lst)
         (let loop ((xs lst))
           (and (pair? xs)
                (let* ((g      (car xs))
                       (meta   (ly:grob-property g 'meta))
                       (staves (ly:grob-object g 'staves)))
                  (cond
                    ((eq? meta 'StaffSymbol) g)
                    ((ly:grob-array? staves)
                     (let* ((sl (ly:grob-array->list staves)))
                       (and (pair? sl) (car sl))))
                    (else (loop (cdr xs))))))))))


#(define system-counter -1)
#(define system-index (make-hash-table))
#(define (ensure-system-index sys)
  (let ((idx (hashq-ref system-index sys #f)))
    (if idx
        idx
        (begin
          (set! system-counter (+ system-counter 1))
          (hashq-set! system-index sys system-counter)
          system-counter))))

#(define (system-with-grid-nums sys)
  (let* ((top-staff (system-top-staffsymbol sys))
         (left-x    0)
         (sp        (staff-space sys))
         (system-extent (ly:grob-extent sys sys Y))
         (system-top    (if (pair? system-extent) (cdr system-extent) 0))
         (top-y         (+ system-top (* 0.5 sp))))
    (format #t "HERE ~a ~a ~a\n" left-x top-y sp)
    
    (if (or (not (number? left-x))
            (not (number? top-y))
            (not (number? sp)))
        (ly:make-stencil '() '(0 . 0) '(0 . 0))
        (let* ((sys-idx (ensure-system-index sys))
               (bn0     (+ FIRST-BAR-NUMBER (* sys-idx MEASURES-PER-SYSTEM)))
               (baseline (+ top-y (* BAR-BOTTOM-PAD-SP sp)))
               (nums
                (let loop ((i 0) (acc '()))
                  (if (>= i MEASURES-PER-SYSTEM)
                      acc
                      (let* ((bn  (+ bn0 i))
                             (x   (+ left-x (* i BEATS-PER-MEASURE CELL-SIZE)))
                             (st  (mk-number-stencil sys (number->string bn)))
                             (ext (and (ly:stencil? st) (ly:stencil-extent st Y)))
                             (ymin (and (pair?* ext) (car ext)))
                             (dy  (if (number? ymin) (- baseline ymin) baseline))
                             (placed (and (ly:stencil? st)
                                          (ly:stencil-translate st (cons x dy)))))
                        (loop (+ i 1) (cons placed acc)))))))
          (apply ly:stencil-add (filter ly:stencil? nums))))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gridStaffParams =
#(define-music-function (scale-notes color-func instrument-names instrument-idx) (list? procedure? list? number?)
  #{
    \override Staff.StaffSymbol.stencil = #(mk-create-grid scale-notes color-func instrument-names instrument-idx)
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
