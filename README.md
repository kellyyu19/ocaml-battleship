# Battleship 
Project by: Kelly Yu, Moses Eboh, Michael Rigney

## Overview
This is a text-based implementation of a popular childhood game, Battleship, in the functional programming language OCaml. A user is able to play a Solo or a Versus game. In both versions, the user can either place each ship manually or have the program place all the ships randomly. The user can then start firing at coordinates and the terminal will show whether or not it was a "Hit", a "Miss", or a "Sunk".

As a twist, we added the functionality of a "bomb," which fires in a cross-like pattern, and each player is given three at the start of the game. The AI in the Solo game that was implemented has the ability to utilize these bombs and makes strategic, smart decisions for all of its moves. When the AI gets a “Hit,” it will fire/bomb around that point until it sinks the ship. If there is no strategic move to be made, the AI will then randomly fire until it gets another hit, essentially mimicking the actions of a human.

## Instructions
To play: 
1. Download the files or git clone
2. Navigate to the directory where the files are located
3. Start the game: 
```
$ make play
```
4. Enjoy!
