# guix-conda

A temporary "channel" of sorts for conda and its dependencies, to be
used before these packages make their way to mainline guix. Currently
conda installs as a library:

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
