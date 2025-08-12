\version "2.24.4"

#(use-modules (srfi srfi-1))

#(define MEASURES_PER_SYSTEM 8)
#(define system-counter 0)
#(define notes-by-system (make-hash-table))

polos = \relative  {
      \key fis \major
      \time 4/4

      r16 fis'16 ais8
      fis16 ais8 fis16
      ais8 fis16 ais16
      r16 ais16 fis16 ais16

      ais16 eis16 fis8
      eis16 fis8 eis16
      fis8 eis16 fis16
      r16 eis16 fis8
}

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Gather notes into a hash table

#(define (pitch->components p)
   (list (ly:pitch-notename p)
         (ly:pitch-alteration p)
         (ly:pitch-octave p)))

#(define reyong_notes_low
  (list 
    #{ cis'  #}
    #{ eis'  #}
    #{ fis'  #}
    #{ ais'  #}
    #{ b'    #}
    #{ cis'' #}))

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
           (context (ly:translator-context engraver))
           (measure-num (ly:context-property context 'currentBarNumber 1))
           (moment (ly:context-current-moment context))
           (beat-pos (ly:moment-main moment))
           (beat-in-measure (modulo (inexact->exact (floor (* beat-pos 4))) 4))
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
	(display "\n")
	(display new-note)
	(display "\n")
        (hash-set! notes-by-system system-num (cons new-note current-notes)))
      
      (ly:grob-set-property! grob 'stencil empty-stencil)))))

gridStaff = {
  %\override Staff.StaffSymbol.stencil = #create-auto-grid
  \override Staff.StaffSymbol.line-count = (length reyong_notes_low)
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

%\score {
%  \new Staff \with {
%      % Setting parameters for the Staff context
%      % Importantly, it customises the Staff.StaffSymbol stencil to actually plot the grids
%      
%      \gridStaff
%  }  {
%    \polos
%  }
%
%  \layout {
%      \context {
%          % Add the autoBreakEngraver engraver to the Score context.
%	  % It breaks the music into systems
%          \Score
%	  \consists \autoBreakEngraver
%      }
%
%      \context {
%          % Add the note_collector_engraver to the Staff context
%	  % It disables plotting traditional notes, and saves all pitch information in a hash table
%          \Staff
%          \consists \note_collector_engraver
%      }
%      
%      indent = 0
%  }
%
%}

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
