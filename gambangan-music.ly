\version "2.24.4"

\include "gamelan-solfege.ly"

% Use the i, o, e, u, a for notenames
#(ly:parser-set-note-names pelog-fis)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
