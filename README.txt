SchArpeggio - A Scheme Music Arpeggiator
Jeremy Poulin and Mike Stowell

______________________________________________________________________________
Code:

- driver.rkt
- arpeggiator.rkt
- drawing.rkt
- objects.rkt

______________________________________________________________________________
Overall structure:

The user interacts with driver.rkt providing input for both a path to
save the dynamic sheet and the chord configurations to arpeggiate over.
The chord configurations, defined in objects.rkt along with many additional
objects and hash references, are then sent to drawing.rkt and
arpeggiator.rkt.  Drawing.rkt draws out the chord configuration and saves
the file path defined by the user in the driver.  Arpeggiator.rkt performs
music synthesis and plays out the sound, constantly sending drawing.rkt
single notes to draw as the music is playing.

______________________________________________________________________________
Who worked on what:

Mike was responsible for the majority of the driver code, some of the objects,
and the entirety of the drawing file.  Jeremy was responsible for tweaking some
code in the driver, many additional objects, and the entirety of the arpeggiator.
Code comments are left in the objects file to point out who worked on what.

______________________________________________________________________________
How the code exhibits key ideas of the course:

>> Mike <<

- Data abstraction and message passing are used in many objects like the 
	chord-configuration.
- String and symbolic manipulation is used to transform user input into 
	something our program can understand.
- Notes are drawn using a recursive function.
- Heavy use of let statements prevent tedious code repetitions in the 
	drawing program.
- Hash tables are kept to perform lookups on notes and their properties.
- Lists are utilized to allow easy passing of multiple chord configurations.
- Optional arguments allow procedures like that which draws a musical staff 
	behave differently on different input.
- Begin is used heavily in the draw-note procedure to allow multiple 
	s-expressions to be evaluated in one if-block. 

>> Jeremy <<

- Data abstraction and message passing are used to interface with notes objects.
- Strings and symbolic manipulation are used to transform user input into 
	options/commands the arpeggiator understands.
- Progressions are played in a recursive fashion.
- A let statement provides internal variables for the play-chord function.
- Hash tables are used to organize both notes and their relative positions.
- Lists are manipulated to allow the user to play notes in different orders.
- If and cond statements allow configuration for different sounds.
- Begin is the foundation of the core functionality for playing multiple notes 
	in the same chord.
- A lambda function provides a handle for multi-threading. 

______________________________________________________________________________
How to run the code:

- install qjackctl, jack, and supercollider
- reroute pulseaudio to dump its output to jack
- run a startup script to make jack your default sound provider
- download the source for rsc3
- install the source for rsc3 into drracket using raco
- start qjackctl
- start the supercollider server
- reconnect the sound ports so that the supercollider gives its outpt 
	to jack and jack gives its input to supercollider
- run our code
- and finally, reinstall linux at your convenience to undo the horrible things
	 you just did to your sound set-up

In other words, it is unlikely you will be able to run it.

______________________________________________________________________________
Code blocks we are happy with:

>> Mike <<

I am quite content with this block of code in drawing.rkt:
; fill in the note if needed
(define (draw-fill-in speed)
  (if (or (equal? speed quarter)
          (equal? speed eighth)
          (equal? speed sixteenth))
      (begin
        (move-offset 0 -1)
        (fill-in-helper 5)
        (move-offset 0 1)
      )
      (nothing)
  )
)

(define (fill-in-helper level)
  (if (= level 0)
      (nothing)
      (begin
        (draw-offset level (- 0 level))
        (draw-offset (- 0 level) (- 0 level))
        (draw-offset (- 0 level) level)
        (draw-offset level level)
        (fill-in-helper (- level 1))
      )
  )
)

Programming in Scheme has made me think recursively a lot more often than
I would have.  Had this have been any other language, I likely would have a
for or while loop.  What draw-fill-in does is it fills in a note circle if
it is required (i.e. if it is a quarter, eighth, or sixteenth note).
The fill-in-helper was my clever way to recursively color in a note using
straight line.  The draw-offset call draw a straight line up-right, up-left,
down-left, and finally down-right for "level" amount of pixels.  This fills
in the note with a square rotated 45 degrees.  The recursive call then
decrements the level and continues to draw concentric squares to fill in the
note.

I was also quite happy with how our giant hash table in objects.rkt came out.
This shared hash table is essentially the backbone to our entire application.


>> Jeremy <<


______________________________________________________________________________
Annoyances:

>> Mike <<

Drawing when you only have 3 basic functions - move, draw, and turn - is
quite difficult when you want circular objects.  When browsing through the
drawing code, it may become obvious why some of the numbers were also
particularly annoying to deal with.  Just 1 pixel off and everything gets
messed up.

>> Jeremy <<



______________________________________________________________________________

