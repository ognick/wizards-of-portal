# Wizards of Portal

This is like tic-tac-toe, but with deep strategy and tactics.

## How to run

You need to have JRE8+ and bash. 

1. Clone repository
2. Enter cloned directory
3. `./sbt` to open SBT console
4. `server/re-start` in DBT console
5. Open [http://localhost:7181](http://localhost:7181) in your brouser

To stop server run `server/re-stop` in SBT console.

## How to build

For Debian

```bash
$ ./sbt debian:packageBin
$ ls ./server/target/wizards-of-portal_0.1.0_all.deb
```

Universal package

```bash
$ ./sbt universal:packageBin
$ ls ./server/target/universal/wizards-of-portal-0.1.0.zip
```

