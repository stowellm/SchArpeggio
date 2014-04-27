Mo' README, mo' money.

Jeremy -
	1. When entering a file save path, enter \\ between
		directories.  The escape character is needed.
	2. Don't use a path with spaces.  Things get wonk if
		you try that.
	3. I added *.1 and .png to the gitignore.  *.1 because
		Windows generates random file extension for back-up
		racket files, and .png since sometimes my test code
		fails and saves the turtle image as a file called
		".png" in our repository code lololol.