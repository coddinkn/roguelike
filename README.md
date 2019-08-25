# roguelike

## Overview

This is a side project of mine that is intended to help me accomplish a few goals:

1. Become a better Haskell programmer
   1. Develop a better understanding of managing side effects with monads
   2. Experience working on a moderately sized Haskell program
   3. Write terse but expressive code while maintaining good style
2. Learn interesting algorithms for procedural generation
3. Explore novel gameplay mechanics in one of my favorite genres

While not set in stone this list is more or less in priority order.
Right now my main focus is writing nice code and learning new things.
As a result code is likely to be refactored frequently and feature progress may be slow.

My target for base functionality before adding in my own mechanics/ideas is feature parity with [Just-For-Fun-RL](https://github.com/Brinsky/Just-For-Fun-RL).

## Dependencies

- [stack](https://docs.haskellstack.org/en/stable/README/)
- ncurses

## Building

With the proper dependencies installed building and running should be as simple as:

```
$ stack build
$ stack run
```

## How to play

Use arrow keys or `hjkl` to move & attack monsters and `q` to quit.
