# GNU guix with conda

When we have GNU Guix we have no longer a need for bootstrapping
conda. GNU Guix replaces miniconda!

Get a rock-solid conda bootstrap from GNU Guix. At this point conda
gets build in a reproducible way using the latest packages. To make
the conda build system even more reproducible we'll need to propagate
gcc etc.

It already works great!

This repository is a temporary "channel" of sorts for conda and its
dependencies, to be used before these packages make their way to
mainline guix.

## Install conda on GNU Guix

Checkout the repository and run

```sh
env GUIX_PACKAGE_PATH=guix-conda/ guix package -i conda
```

Now conda is in Guix. Run it after pasting the results of

```sh
guix package --search-paths
```

in your shell. Next

```sh
conda

    usage: .conda-real [-h] [-V] command ...

    conda is a tool for managing and deploying applications, environments and packages.
```

Finally, you'll need to create a clone because GNU Guix is read-only, so after this error

```
conda install bwa
CondaIOError: IO error: Missing write permissions in: /gnu/store/alk9r3rir93pjmv8im20f8xrvv90219z-python-3.5.2
```

set the name space to (for example) my_conda

```
conda create -n my_conda --clone=/gnu/store/alk9r3rir93pjmv8im20f8xrvv90219z-python-3.5.2
```

and use it from then on

```
conda install -n my_conda bwa
```

run the tool

```
~/.conda/envs/my_conda/bin/bwa
```

## Install conda library

conda also installs as a library:

```sh
env GUIX_PACKAGE_PATH=../guix-conda/ guix package -i python python-conda
```

```python
env PYTHONPATH=/home/wrk/.guix-profile/lib/python3.5/site-packages python3.5
Python 3.5.2
[GCC 4.9.4] on linux
Type "help", "copyright", "credits" or "license" for more information.
>>> import conda
>>>
```
