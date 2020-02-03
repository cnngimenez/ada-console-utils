Miscelaneous Ada libraries and tools.

# Requirements
Only Ada and GNAT libraries. gprbuild to build the sources easily.

In Fedora:

    dnf install gcc-gnat gprbuild

In Mangaro:

    pacman -S gcc-ada
    yaourt -S gprbuild

gprbuild is on Arch repositories (AUR). Thus, use yaourt or your preferred AUR package manager.

## Data
A text file is used by the emoji test executable (`emoji_test_read` binary) and library (`emoji-list`). These were downloaded from unicode.org, specifiacally, the following URL:

https://unicode.org/Public/emoji/12.1/

The emoji-test.txt file is the required one. Once the executables are compiled, you can search for all emojis with "face" in their description by typing `bin/emoji_test_read emoji-test.txt face` on the terminal.

# Compiling
Simply call `make` to compile the library and all binaries.

- `make install` will install the library and binaries into the prefix.
- `make params` will show the parameters used by the Makefile.

To change the prefix use `make install PREFIX=your_path_here` were `your_path_here` is the path you want to install. For example: `make install PREFIX=/home/me/all_Ada_codes` will install:
libs on `/home/me/all_Ada_codes/lib`, binaries on `/home/me/all_Ada_codes/bin`, etc.

Variables: 

- PREFIX : The path where to install.
- LIBRARY_KIND : "dynamic" or "static". The type of library to create.

## gprbuild alternative
Use gprbuild as follows.

```
gprbuild utils.gpr
gprbuild util_tools.gpr
```

gprbuild is usefull to compile libraries only by using the first line.

gprinstall can install the project. 

`gprinstall -p utils.gpr` will install all the libraries at the default PREFIX. use `--prefix=PREFIX_PATH_HERE` parameter to change where the library should be installed.

# License
This project is under the GPL version 3 license. 

Christian Gimenez, 2019.
