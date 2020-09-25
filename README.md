## Macbeth - A beautiful FICS client

![Screenshot](/Macbeth_screenshot.jpg)

### Supported Platforms
* macos
* Linux
* Windows, potentially in the future


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
| <kbd>Y</kbd> | Accept offer
| <kbd>N</kbd> | Decline offer
| <kbd>X</kbd> | Cancel last premove


| Bughouse / Crazyhouse | &nbsp;
|--------------|-------------
| <kbd>Q</kbd> | Queen 
| <kbd>R</kbd> | Rook 
| <kbd>K</kbd> | Knight 
| <kbd>B</kbd> | Bishop 
| <kbd>P</kbd> | Pawn 

### Building and Running Macbeth

#### macos

You'll need to run several commands in a terminal window. Copy and paste each command separately and press `Enter` (The commands start after `>`). Now let's get started, and open a terminal window.

```
> brew --version
```

If it says "command not found", goto https://brew.sh/ and follow the instructions there. When homebrew is installed run these commands:

```
> brew install git haskell-stack wxmac freealut

> git clone https://github.com/tgass/macbeth.git
> cd macbeth
> cd macbeth-macos

> ./bundle.sh
> open Macbeth.dmg
```

#### Linux (Ubuntu)
```
sudo apt-get install git haskell-stack libalut-dev libglu1-mesa-dev freeglut3-dev mesa-common-dev libwxgtk3.0-dev lib32z1-dev libwxgtk-media3.0-dev

git clone https://github.com/tgass/macbeth.git
cd macbeth

stack setup
./env.sh stack build --extra-lib-dirs=/usr/lib/x86_64-linux-gnu/
stack exec Macbeth
```

### Download

From time to time I upload a binary for a recent macos here: http://www.macbeth-ficsclient.com 

### Copyrights

* The piece sets you find in /resources are taken form http://ixian.com/chess/jin-piece-sets/. They are the work of Eric De Mund, licensed under a Creative Commons Attribution-Share Alike 3.0 Unported License.
* The chess board background tiles that I provide are taken from Tim Mann's XBoard/ WinBoard: http://www.tim-mann.org/xboard.html
* The icons used are taken from https://icons8.com/. They are beautiful! 

