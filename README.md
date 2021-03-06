# The π€ Programming Language

__A simple Emoji-based programming language to control a physical or simulated [mBot](https://www.makeblock.com/mbot).__

While π€ was not meant to be serious, it does work. This repository provides an interpreter for the language, written in Haskell. If you have a physical mBot, you can write programs and execute them on there. If not, we provide a simulator to mimick the mBot and its surroundings.


## Example Programs

### Police Car

This program makes the robot blink its LEDs like a police car.

```
ππ
   π¨ π 100 0 0
   π¨ π 0 0 100
   π΄ π
   π¨ π 0 0 100
   π¨ π 100 0 0
   π΄ π
```

In the infinite loop, we set the left LED to red and the right LED to blue. After that, we wait 400 milliseconds, swap the colors, wait once more and start over.

### Obstacle Avoidance

This program makes the robot drive straight ahead and avoid any obstacles.

```
π­ And I would drive 500 miles..

dist βͺ 20

ππ
   β π > dist
      π¨  β¬οΈ
   βοΈ
      π¨  β¬οΈ
      π΄  π
      π¨  β‘οΈ
      π΄  π
```

We continuously read the distance sensor (π) and compare it to the threshold `dist`.
If the distance is greater than `dist`, we keep driving straight ahead.
Otherwise, we back up for 800 ms and then turn right for 400 ms before we continue our way.


### Follow The Line

This program makes the robot follow a black line on the floor.

```
prev βͺ π

ππ
   new βͺ π­

   π­ Reuse the last line reading if there's no line in sight,
   π­ or save this reading if the we know the direction of the line
   βnew == π
      new βͺ prev
   βοΈ new  == π || new  == π
      prev βͺ new

   β new  == π
      π¨ β¬οΈ
   βοΈ new  == π
      π¨ β¬οΈ
   βοΈ new  == π
      π¨ β‘οΈ

```

We continuously read the line sensor (π­) and steer to follow the line.
If no line is detected (π), we reuse the last detected value.
If the line is detected on the left (π), we steer left and if the line is detected on the right (π), we steer right.
If the line is directly underneath us (π), we drive straight on.

##  Usage

To run a π€-program `examples/line.txt` on a physical mBot connected to your computer:

```
$ runhaskell src/Interpreter.hs examples/line.txt
```

To run a π€-program `examples/line.txt` on the built-in simulator:
```
$ runhaskell src/Simulator.hs examples/line.txt
```
<p align="center"><img alt="Emulator" src="docs/emulator.png" width="60%" /></p>

## Syntax
The full syntax is described below in [Extended Backus-Naur form](https://en.wikipedia.org/wiki/Extended_BackusβNaur_form). To make this easier to read, syntax elements of the EBNF itself have a light color, while non-terminal symbols are framed and symbols denoting whitespace have a blue background.

<p align="center"><img alt="Syntax" src="docs/syntax.png" width="60%" /></p>


## Semantics

A π€-program is a `StmtSeq`, i.e. a sequence of one or more statements (`Stmt`) separated by newlines.
Just like in Python, blocks are expressed by their indentation (the [off-side rule](https://en.wikipedia.org/wiki/Off-side_rule)).
There are five types of statements:

### Assignment (βͺ)
The value of the arithmetic expression (`AExp`) on the right of the assignment operator (βͺ) is assigned to the  _Identifier_ specified on the left hand side.

### While (π)
This is a traditional while-loop. The body keeps being executed as long as the specified boolean expression (`BExp`) evaluates to π.

### If (β)
This is a traditional if/else-if/else-statement. We first evaluate the boolean expression next toβ.

If this evaluates to π, we execute the corresponding body. If this evaluates to π, we evaluate the βοΈ-conditions one-by-one and execute the body corresponding the the first condition evaluating to π. If none of the boolean expressions evaluate to π, the body of theβοΈ-branch is executed, if one is specified.

### Skip (π­)
This is a comment.
Any text to the right of the π­ is ignored.

### Command
This is a command for the mBot (or the simulator) to execute. There are three types of command

#### Drive (π¨)
Start driving in the specified direction: forward (β¬οΈ), backward, (β¬οΈ), left (β¬οΈ) or right (β‘οΈ).

#### Sleep (π΄)
Sleep for the specified duration. You can pass in an arithmetic expression denoting the number of milliseconds to sleep, or use one of the built-in constants: 400 ms (π), 800 ms (π), 1200 ms (π§), 1600 ms (π) of 2 s (π).

#### Light (π¨)
Sets the color of the specified LED to a specific color. The first argument denotes whether to set the left (π) or right (π) LED. The next three arguments are arithmetic expressions denoting the RGB value on a scale of 0 to 100.

<br><br>
In the description of statements above, we touched a few times on arithmetic and boolean expressions.
Their semantics are explained below.

### Arithmetic expressions
An arithmetic expression is an expression that evaluates to a whole number.
Any decimal literal is a valid arithmetic expression.
You can use all the traditional operators (`+`, `-`, `*` and `/`) to construct new arithmetic expressions.
Furthermore, you can use one of the built-in constants: π, π, π and π  to denote 0, 1, 2 and 3 respectively. (Think about the binary representation of those numbers to see why this makes sense).

Another arithmetic expression is querying one of the built-in sensors. The distance sensor (π) returns the distance to the object in front of the mBot. The line sensor ( π­) returns the position of the line under the mBot. The value of this sensor is always between 0 and 3:  π (line on both sides), π (line on the left), π (line on the right) or π (no line detected).

### Boolean expressions
A boolean expression is an expression that either evaluates to either true (π) or false (π).
We support OR (`||`) and AND (`&&`) to combine boolean expressions.
The comparison of two arithmetic expressions (`>`, `==` and `<`) is also a valid boolean expression.
