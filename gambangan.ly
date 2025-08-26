\version "2.24.4"

#(use-modules (srfi srfi-1))

\include "gambangan-music.ly"
\include "grid-display.ly"
\include "utils.ly"

#(define MEASURES_PER_SYSTEM 6)
#(define SCALE REYONG-NOTES)

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

\score {
    \new Staff \reyong_notes_display
    
    \header {
        piece = "Approximate Pelog scale for the Reyong" 
    }
    
    \layout {}
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\score {
   \new Staff \with {
        instrumentName = " S. + P."
   } {
     \key a \major 
%     \sliceMusic #7 #12 \polos_low
      \keepWithTag #'section_two \polos_low
   }

   \header {
       piece = "Example of Kotekan (interlocked melodies) from McPhee, C. (1940)"
   }
   
   \layout {
        \context {
    	    \Voice
    	    \override Rest.transparent = ##t
        }
   }

}

%%%%%%%%%%%%%%%%%%%%%%%%%%%% MIDI

\score {
   \new Staff {
     \sliceMusic #1 #4 \all_voices
   }

   \midi {
   	 \tempo 4 = 80
   }
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Engrave in grid format

\score {
  \new Staff \with {
      % Setting parameters for the Staff context
      % Importantly, it customises the Staff.StaffSymbol stencil to actually plot the grids
      \gridStaffParams #SCALE #voice-color-func
  }  {
%     \set Score.currentBarNumber = 5
%     \sliceMusic #1 #-1 \all_voices
      \all_voices
  }

  \header {
       piece = \markup {
           \column {
               "The Reyong Kotekan in grid format"
               \vspace #0.5 }
       }
  }

  \layout {
      \context {
          % Add the autoBreakEngraver engraver to the Score context.
	  % It breaks the music into systems
          \Score
	  \consists \autoBreakEngraver
	  %currentBarNumber = #6
      }

      \context {
          % Add the note_collector_engraver to the Staff context
	  % It disables plotting traditional notes, and saves all pitch information in a hash table
          \Staff
          \consists #(mk-note-collector SCALE)
	  \omit StaffSymbol
      }      
      indent = 0
  }

}
