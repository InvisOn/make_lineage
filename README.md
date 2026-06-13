# make_lineage

A simple Makefile to dot conversion tool. It also supports pruning and highlighting lineage.

Build with `lake build -Krelease`.

## Highlighting the lineage of `common.h`

```sh
LANG=C make -n -p -f res/Makefile | .lake/build/bin/ml --highlight-lineage common.h | dot -Tpng -o res/prune-common.png
```

![highlight](./res/highlight-common.png)

## Keeping only the lineage of `common.h`

```sh
LANG=C make -n -p -f res/Makefile | .lake/build/bin/ml --keep-lineage common.h | dot -Tpng -o res/prune-common.png
```

![keep](./res/keep-common.png)

## Pruning the lineage of `bench`

```sh
LANG=C make -n -p -f res/Makefile | .lake/build/bin/ml --prune-lineage common.h | dot -Tpng -o res/prune-common.png
```

![prune](./res/prune-common.png)

