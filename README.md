# [Notty][notty] ✕ [Miou][miou]

A simple library which implements the _scheduler-logic_ needed by `notty` to
play with your terminal. A "game-of-life" example exists and you can execute it
with (you can stop the application with Esc or Ctrl^C):
```sh
$ git clone https://github.com/robur-coop/notty-miou
$ cd notty-miou
$ opam pin add -y .
$ dune exec bin/life.exe
```


[notty]: https://github.com/pqwy/notty
[miou]: https://github.com/robur-coop/miou
