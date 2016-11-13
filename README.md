# Martian Robots

## Getting started

### Viewing the project (no need to compile or install anything)

Just clone this repo and open `index.html` in a browser.

If you're using Mac OS X just copy paste this in your terminal:

```
git clone https://github.com/simonewebdesign/elm-on-mars.git
cd elm-on-mars
git checkout production
python -m SimpleHTTPServer 8000 &
open http://localhost:8000/
```

### Compiling from source

If you want to compile this project you need [Elm](http://elm-lang.org/) 0.17 installed on your machine.

Compile it with:

    elm make src/App.elm

Then view it in the browser (will compile too if you didn't):

    elm reactor

### Running the test suite

Make sure you have Node.js installed, then run:

    elm test

You should see something like:

    Success! Compiled 1 module.
    Successfully generated /var/folders/pd/4y9l5m_517zdg1bj0l9szg4r0000gn/T/elm_test_116919-19066-mppg8p.o95uq5mi.js

    elm-test
    --------

    Running 14 tests. To reproduce these results, run: elm-test --seed 1383778577


    TEST RUN PASSED

    Duration: 29 ms
    Passed:   14
    Failed:   0


## The Problem

The surface of Mars can be modelled by a rectangular grid around which robots are able to
move according to instructions provided from Earth. You are to write a program that
determines each sequence of robot positions and reports the final position of the robot.
A robot position consists of a grid coordinate (a pair of integers: x-coordinate followed by
y-coordinate) and an orientation (N, S, E, W for north, south, east, and west).
A robot instruction is a string of the letters “L”, “R”, and “F” which represent, respectively, the
instructions:

- Left : the robot turns left 90 degrees and remains on the current grid point.
- Right : the robot turns right 90 degrees and remains on the current grid point.
- Forward : the robot moves forward one grid point in the direction of the current
orientation and maintains the same orientation.

The direction North corresponds to the direction from grid point (x, y) to grid point (x, y+1).
There is also a possibility that additional command types may be required in the future and
provision should be made for this.

Since the grid is rectangular and bounded (…yes Mars is a strange planet), a robot that
moves “off” an edge of the grid is lost forever. However, lost robots leave a robot “scent” that
prohibits future robots from dropping off the world at the same grid point. The scent is left at
the last grid position the robot occupied before disappearing over the edge. An instruction to
move “off” the world from a grid point from which a robot has been previously lost is simply
ignored by the current robot.

#### The Input

The first line of input is the upper-right coordinates of the rectangular world, the lower-left
coordinates are assumed to be 0, 0.

The remaining input consists of a sequence of robot positions and instructions (two lines per
robot). A position consists of two integers specifying the initial coordinates of the robot and
an orientation (N, S, E, W), all separated by whitespace on one line. A robot instruction is a
string of the letters “L”, “R”, and “F” on one line.

Each robot is processed sequentially, i.e., finishes executing the robot instructions before the
next robot begins execution.

The maximum value for any coordinate is 50.

All instruction strings will be less than 100 characters in length.

### The Output

For each robot position/instruction in the input, the output should indicate the final grid
position and orientation of the robot. If a robot falls off the edge of the grid the word “LOST”
should be printed after the position and orientation.

#### Sample Input

```
5 3
1 1 E
RFRFRFRF

3 2 N
FRRFLLFFRRFLL

0 3 W
LLFFFLFLFL
```

#### Sample Output

```
1 1 E
3 3 N LOST
2 3 S
```

## Takeaways

- `Html msg` are introspectable via `Debug.log`, but you can't query them
- `Cmd msg` are not yet testable. You can do end-to-end testing only if you don't have side effects at all.
- [elm-combine](https://github.com/Bogdanp/elm-combine) is amazing!
