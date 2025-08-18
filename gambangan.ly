\version "2.24.4"

#(use-modules (srfi srfi-1))

\include "gambangan-music.ly"
\include "grid-display.ly"
\include "utils.ly"

#(define MEASURES_PER_SYSTEM 6)

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
     \high_register
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
     \all_voices
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
