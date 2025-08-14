\version "2.24.4"

#(use-modules (srfi srfi-1))

#(define MEASURES_PER_SYSTEM 2)
#(define system-counter 0)
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
    ((string=? voice-id "polos") (rgb-color 0.678 0.88 0.898))
    ((string=? voice-id "sangsih") (rgb-color 0.980 0.500 0.564))
    (else (rgb-color 0.5 0.5 0.5))))

#(define (create-auto-grid grob)
  "Create a grid using notes collected for this specific system"
  (let* ((staff-space 1.0)
         (thickness 0.1)
         (pitches (length reyong_notes_low))
         (beats-per-measure 16)
         (cell-size staff-space)
         (current-system system-counter)
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
               (y-pos (* (- note-idx (/ (length reyong_notes_low) 2)) cell-size))
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
    (map pitch->components reyong_notes_low))

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
	   (zz (display note-index))
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

gridStaff = {
  \override Staff.StaffSymbol.stencil = #create-auto-grid
  \override Staff.StaffSymbol.line-count = #(length reyong_notes_low)
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

polos = {
      \key a \major

      r16    a'16	i'8
      a'16   i'8	        a'16
      i'8    		a'16	i'16
      r16    i'16	a'16	i'16    |

      i'16   u'16	a'8
      u'16   a'8		u'16
      a'8    		u'16  	a'16
      r16    u'16	a'8
}

sangsih = {
      \key a \major

      o'8		e''16	o'16
      r16    e''16	o'8
      e''16  o'8		e''16
      o'16   e''8	     	e''16  |

      e''16  e''16	e''16	i'16
      r16    e''16	i'8
      i'16   e''8		i'16
      e''8   		i'16	e''16

}

%polos = \relative {
%      \key a \major
%
%      r16  a'16	i8
%      a16  i8	     a16
%      i8        a16  i16
%      r16  i16	a16  i16    |
%
%      r16  a16  i8
%      a16  i8        a16
%      i8        a16  i16
%      r16  i16  a16  i16    |
%
%      i16  u16  a8
%      u16  a8        u16
%      a8        u16  a16
%      r16  u16  a8          
%}

%sangsih = \relative {
%      \key a \major
%
%      o'8       e16  o16
%      r16  e16	o8
%      e16  o8	     e16
%      o16  e8	     e16    |
%
%      o8        e16  o16
%      r16  e16  o8
%      e16  o8        e16
%      o16  e8        e16    |
%
%      o8        e16  o16
%      r16  e16  o8
%      e16  o8        e16
%      o16  e8        e16    |
%
%}

reyong_notes = {
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


%   (let* ((components (map pitch->components reyong_notes_low))
%          (target (ly:music-property mu 'pitch))
%	  (target (pitch->components target)))
%	(list-index (lambda (x) (equal? x target)) components)))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


\markup {
  \column {
    \vspace #1 
    \line { "Approximate" \italic "Pelog" "scale for the" \italic "Reyong" }
    \vspace #1
  }
}

\score {
    \new Staff \reyong_notes

    \layout {}
}

\markup {
  \column {
    \line { "Example of" \italic "Kotekan" "(interlocked melodies) from McPhee, C. (1940)" }
    \vspace #1
  }
}

\score {
    \new StaffGroup <<
        \new Staff \with {
            instrumentName = "Sangsih (S)"
            shortInstrumentName = "S."
        } { \voiceOne \stemDown \sangsih }

        \new Staff \with {
            instrumentName = "Polos (P)"
            shortInstrumentName = "P."
        } { \voiceTwo \stemUp \polos }
    >>

    \layout {}
}

\score {
    \new Staff \with {
        instrumentName = " S. + P."
    } {
    <<
	\new Voice = "polos" \with {
	    \consists #(voice_color_engraver (rgb-color 0.0 0.0 0.0))
	} {
	  \stemDown \polos
	}

	\new Voice = "sangsih" \with {
	    \consists #(voice_color_engraver (rgb-color 0.0 0.0 0.0))
	} {
	  \stemUp \sangsih
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
          \new Voice = "polos" {
            \voiceOne
            \polos
          }
          \new Voice = "sangsih" {
            \voiceTwo
            \sangsih
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
      }      
      indent = 0
  }
}
