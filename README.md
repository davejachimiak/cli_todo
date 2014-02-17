This is a simple todo app for the command line written in Haskell.
It is an expanded version of the todo app from Chapter 9 of Learn You
a Haskell for Great Good! by Miran Lipovača. It adds items to, removes items
from, and bumps items to the top of lists of declared files.

### Install

1. `$ git clone git@github.com:davejachimiak/cli_todo.git`
2. ``$ cd cli_todo && export PATH=$PATH:`pwd` ``

### Usage

general usage: **todo [-h[command] | command arguments]**

specific usage:

```
todo add filepath "task item"
```
```
todo bump filepath task_number
```
```
todo remove filepath task_number
```
```
todo view filepath
```

### Example

```sh
$ touch morning.txt
$ todo add ./morning.txt "Brush teeth"
$ todo add ./morning.txt "Drink coffee"
$ todo add ./morning.txt "Go to work"
$ todo view ./morning.txt                                                                                 ~/github/cli_todo system  (master) ✗
0 -- Brush teeth
1 -- Drink coffee
2 -- Go to work
$ todo bump ./morning.txt 1
$ todo view ./morning.txt                                                                                 ~/github/cli_todo system  (master) ✗
0 -- Drink coffee
1 -- Brush teeth
2 -- Go to work
$ todo remove ./morning.txt 0
$ todo view ./morning.txt                                                                                 ~/github/cli_todo system  (master) ✗
0 -- Brush teeth
1 -- Go to work
```
