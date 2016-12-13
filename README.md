# h-reversi
Haskell blank canvas reversi game  

[![Build Status](https://travis-ci.org/apoorvingle/h-reversi.svg?branch=master)](https://travis-ci.org/apoorvingle/h-reversi)  

### Installation

```
$ git clone https://github.com/apoorvingle/h-reversi.git
$ cd h-reversi
$ cabal sandbox init
$ cabal install --dependencies-only
```

### Test
```
$ cabal configure --enable-tests
$ cabal install --enable-tests
$ cabal build && cabal test
```

### Run
```
$ cabal clean && cabal configure && cabal install && ./.cabal-sandbox/bin/h-reversi
```

### browser

```
http://localhost:3000
```

### How to play
[Reversi wiki](https://en.wikipedia.org/wiki/Reversi)

### Screenshots
![Game screenshot](images/screenshot.jpeg)
![End Game screenshot](images/screenshot-endgame.jpeg)
