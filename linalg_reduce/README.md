This document uses [mpark/wg21](https://github.com/mpark/wg21)
as a git submodule.

To set up the build environment,
the following Docker command is known to work.

```bash
$ docker run ubuntu:22.04 bash -c '(apt update && DEBIAN_FRONTEND=noninteractive apt install -y git build-essential curl python3 python3-venv pandoc texlive-xetex)'
```

The following command builds the HTML version of the document
in the `generated/` subdirectory
(which the command will create if it doesn't already exist).

```bash
$ make linalg_reduce.html
```


