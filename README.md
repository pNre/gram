## gram

Command line based, bare-bones client for Telegram

#### Requirements

- OCaml 4.07+
- Dune 1.x
- [TDLib](https://core.telegram.org/tdlib)
- Readline

#### Compiling

Check missing dependencies using:

```bash
dune external-lib-deps --missing @all
```

and install them, then:

```bash
dune build @all
```

#### Running

`./_build/default/main.exe`

