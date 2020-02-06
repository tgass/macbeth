## Macbeth - A beautiful FICS client for OSX

![Screenshot](/Macbeth_screenshot.jpg)

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

### Building and running on Linux (Ubuntu)

```
sudo apt-get install haskell-stack libalut-dev libopengl-dev libglu1-mesa-dev freeglut3-dev mesa-common-dev libwxgtk3.0-dev lib32z1-dev libwxgtk-media3.0-dev

git clone https://github.com/tgass/macbeth.git
cd macbeth

stack build
./env.sh stack build
```

There is a known problem linking to wxc.so, see https://github.com/commercialhaskell/stack/issues/2299
Calling env.sh fixes it.

Put this in your "global" config.yaml (See https://docs.haskellstack.org/en/stable/yaml_configuration/)
```
extra-lib-dirs: 
- /usr/lib/x86_64-linux-gnu/
```


### Download

You can find a compiled and package version ready to play at http://www.macbeth-ficsclient.com 

### Copyrights

* The piece sets you find in /resources are taken form http://ixian.com/chess/jin-piece-sets/. They are the work of Eric De Mund, licensed under a Creative Commons Attribution-Share Alike 3.0 Unported License.
* The chess board background tiles that I provide are taken from Tim Mann's XBoard/ WinBoard: http://www.tim-mann.org/xboard.html
* The icons used are taken from https://icons8.com/. They are beautiful! 

