Create a README.txt file that lists all files that you are submitting
- driver.rkt
- arpeggiator.rkt
- drawing.rkt
- objects.rkt

   
Also include one or several introductory paragraphs that explain the overall structure of the code.
VERY PARAGRAPH MUCH DETAIL WOW
   
Indicate which files are ones that you performed substantial work.
Jeremy: arpeggiator.rkt
Mike: drawing.rkt
Both: driver.rkt, objects.rkt (see code comments to distinguish parts)  

Make sure to highlight how the code exhibits the key ideas of the class.

Mike:

- Data abstraction and message passing are used in many objects like the chord-configuration.
- String and symbolic manipulation is used to transform user input into something our program can understand.
- Notes are drawn using a recursive function.
- Heavy use of let statements prevent tedious code repetitions in the drawing program.
- Hash tables are kept to perform lookups on notes and their properties.
- Lists are utilized to allow easy passing of multiple chord configurations.
- Optional arguments allow procedures like that which draws a musical staff behave differently on different input.
- Begin is used heavily in the draw-note procedure to allow multiple s-expressions to be evaluated in one if-block. 

Jeremy:

- Data abstraction and message passing are used to interface with notes objects.
- Strings and symbolic manipulation are used to transform user input into options/commands the arpeggiator understands.
- Progressions are played in a recursive fashion.
- A let statement provides internal variables for the play-chord function.
- Hash tables are used to organize both notes and their relative positions.
- Lists are manipulated to allow the user to play notes in different orders.
- If and cond statements allow configuration for different sounds.
- Begin is the foundation of the core functionality for playing multiple notes in the same chord.
- A lambda function provides a handle for multi-threading. 
  
Explain how to run the code. 
YOU CANT LOLOLOLOLOLOLOL

Well, actually you can.

All you need to is this:

- install qjackctl, jack, and supercollider
- reroute pulseaudio to dump its output to jack
- run a startup script to make jack your default sound provider
- download the source for rsc3
- install the source for rsc3 into drracket using raco
- start qjackctl
- start the supercollider server
- reconnect the sound ports so that the supercollider gives its outpt to jack and jack gives its input to supercollider

- run our code

- and finally, reinstall linux at your convenience to undo the horrible things you just did to your sound set-up
