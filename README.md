# digitgame: Shut The Box code
Copyright (c) 2006 Bart Massey

Here are two Haskell programs for playing a dice-and-digits
game. The game has had many names over time: it is currently
best known as
*[Shut The Box](https://secure.wikimedia.org/wikipedia/en/wiki/Shut_the_Box)*.

One program is a really simple GUI program written in
[Gtk2Hs](http://projects.haskell.org/gtk2hs/) for playing
the game solitaire. I wrote this one all by myself.

The other program, written cooperatively with Jamey Sharp,
is a solver for perfect play in the solitaire variant of the
game; it can compute best-expected move or move with best
chance of reaching a given score threshold. This latter
program has a textual "tutor" interface that allows the
player to pick a move and then shows what the expected
payoffs were for the various choices.

This game was introduced to me by Andreas Junghanns, who
also built a solver for it. His agrees with mine, so I hope
we're both correct.

This work is available under the "MIT License". Please see
the file `COPYING` in this distribution for license terms.
