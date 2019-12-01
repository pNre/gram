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

1. Get `api_id` and `api_hash` from https://my.telegram.org/auth
2. Export the following environment variables:
   - `TG_API_ID`
   - `TG_API_HASH`
   - `TG_DB_KEY`: encryption key for TDLib to encrypt local data 
3. Run `./_build/default/main.exe`

