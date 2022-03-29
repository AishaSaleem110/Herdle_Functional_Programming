# Functional_Programming Assessment 1 (Herdle)
The aim of this assignment is to implement Herdle, a clone of the game Wordle but in Haskell.

Wordle is a single-player game with the following rules: each day a five-letter English word is
chosen randomly (by the computer). A player has six attempts to guess the word by inputting
five letters. If the player guesses the word correctly, they win. Otherwise, they are given feedback
for each letter of their guess which indicates whether the letter is in the word and in the right
place, is present in the actual word but in a different position, or is not in the word at all.

For example, if the actual word is enter and the player guesses green, they are told that:
• g occurs nowhere in the word;
• r occurs elsewhere in the word;
• e occurs elsewhere in the word;
• e occurs here in the word;
• n occurs elsewhere in the word.

Then on their next turn the player can use this information to make a better guess.

You can play the real game at https://www.powerlanguage.co.uk/wordle/.
We will build up our own version of this which can be played from the command line.

### The Code

This code is written in Python 3.10 (using PyCharm IDE)

### Running The Code
*To run the code, open the terminal and go to the directory where the code resides :
For example to run the extended version which is herdle_functional_assessment.hs:

From command line open the directory containing all code files, then run the following commands:

ghci
:l iteration_extension.hs
main

*The files are self-contained and all necessary libraries are imported.

### A snippet from commandline to start the game

D:\KentMSc\Semester2\FunctionalProgramming\Haskel\Herdle_Assessment2\herdle_functional_assessment> ghci

GHCi, version 8.10.7: https://www.haskell.org/ghc/  :? for help

Prelude> :l iteration_extension.hs

[1 of 2] Compiling Base             ( Base.hs, interpreted )

[2 of 2] Compiling Main             ( iteration_extension.hs, interpreted )

Ok, two modules loaded.

*Main> main

Attempt 1
Guess [any] or quit [q]?

### What is in the code
-The files contain :

-The game is developed in iterations.

-herdle_extension is the final version of the game.

-iteration1.hs produces a working game.The subsequent iterations make the game closer to the described game play and with more features. 
