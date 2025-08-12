\version "2.24.4"

#(use-modules (srfi srfi-1))

%polos = \relative  {
%      \key fis \major
%
%      r16 fis'16 ais8
%      fis16 ais8 fis16
%      ais8 fis16 ais16
%      r16 ais16 fis16 ais16
%
%      ais16 eis16 fis8
%      eis16 fis8 eis16
%      fis8 eis16 fis16
%      r16 eis16 fis8
%}

#(define (pitch->components p)
   (list (ly:pitch-notename p)
         (ly:pitch-alteration p)
         (ly:pitch-octave p)))

#(ly:parser-set-note-names
   `((i . ,#{ ais #})
     (o . ,#{ b #})
     (e . ,#{ cis #})
     (u . ,#{ eis #})
     (a . ,#{ fis #})))

#(define voice_color_engraver
  (lambda (voice-color)
    (make-engraver
     (acknowledgers
      ((note-head-interface engraver grob source-engraver)
       (ly:grob-set-property! grob 'color voice-color))))))


polos_display = \relative  {
      \key a \major

      r16  a'16	i8
      a16  i8	a16
      i8   a16	i16
      r16  i16	a16	a16
}


sangsih_display = \relative  {
      \key a \major

      o'8  e16	o16
      r16  e16	o8
      e16  o8	e16
      o16  e8	e16

}

polos = \relative {
      \key a \major

      r16  a'16	i8
      a16  i8	a16
      i8   a16	i16
      r16  i16	a16	a16
}


sangsih = \relative {
      \key a \major

      o'8  e16	o16
      r16  e16	o8
      e16  o8	e16
      o16  e8	e16

}

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

#(define reyong_notes_low
  (list 
    #{ e'  #}
    #{ u'  #}
    #{ a'  #}
    #{ i'  #}
    #{ o'  #}
    #{ e'' #}))

#(define (reyong_note_idx mu)
   (let* ((components (map pitch->components reyong_notes_low))
          (target (ly:music-property mu 'pitch))
	  (target (pitch->components target)))
	(list-index (lambda (x) (equal? x target)) components)))

\score {
    <<
        \new Staff = "sangsih" \sangsih_display
        \new Staff = "polos" \polos_display
    >>

    \layout {}
}

\score {
    \new Staff {
    <<
	\new Voice = "polos" \with {
	     \consists #(voice_color_engraver (rgb-color 0 0 1))
	} {
	  \polos
	}


	\new Voice = "sangsih" \with {
	     \consists #(voice_color_engraver (rgb-color 1 0 0))
	} {
	  \sangsih
	}
    >>
    }
    \layout {
        \context {
    	    \Voice
    	    \override Rest.transparent = ##t
        }
    }
}

\markup {
  \vspace #2 

  \bold "Notes for Reyong"
  
  \vspace #2
}

\score {
    \new Staff \reyong_notes

    \layout {}
}


