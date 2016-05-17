## Macbeth - A beautiful FICS client for OSX

![Screenshot] (homepage/Macbeth_observeGames.png "Screenshot")

### Supported FICS categories
* Chess
* Crazyhouse
* Suicide
* Losers
* Atomic
* Wild

see: http://www.freechess.org/Help/HelpFiles/category.html

(Bughouse will be supported in the future.)

### Features

* Auto-Save games
* Pre-Moves
* Multiple Piece Sets
* Resume Pending games
* Observing games

### Shortcuts

| All game types | &nbsp;
|------------------------------------|-----------------------
| <kbd>&#8984;</kbd> + <kbd>W</kbd>   | Close Window
| <kbd>&#8984;</kbd> + <kbd>O</kbd>   | Toggle promotion piece
| <kbd>Esc</kbd> | Drop dragged piece

| Bughouse / Crazyhouse | &nbsp;
|--------------|-------------
| <kbd>Q</kbd> | Queen 
| <kbd>R</kbd> | Rook 
| <kbd>K</kbd> | Knight 
| <kbd>B</kbd> | Bishop 
| <kbd>P</kbd> | Pawn 

### Building Macbeth
```
brew install haskell-stack  # if necesarry
brew install wxmac
brew install freealut

git clone https://github.com/tgass/macbeth.git
cd macbeth

stack setup # if necesarry
stack build # this will take a while!
stack exec Macbeth
```

### Download

You can find a compiled and package version ready to play at http://www.macbeth-ficsclient.com 

### Copyrights

The piece sets you find in /resources are taken form [here] (http://ixian.com/chess/jin-piece-sets/). They are the work of Eric De Mund, licensed under a Creative Commons Attribution-Share Alike 3.0 Unported License.
