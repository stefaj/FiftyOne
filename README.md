# FIFTY ONE
A simple language for a simple microcontroller

## Variables

```
varName <- value;
```

A variable is created with the given name if it does not already exist,
otherwise its value is updated.
A variable name starts with a letter and afterwards may contain number of alphabetic
characters

The value of a variable is always a 8 bits.

## Arithmetic
Currently only addition is supported

```
b <- 10 + a
```

## Control

A while loop takes the form of

```
while condition do
  stmt1;
  stmt2
end
```
where condition is a boolean expression.

An if takes the form of

```
if condition then
  stmt1;
  stmt2
else
  stmt1;
  stmt2
end
```

Note that when there are multiple statements, each statement is separated by a
`;`, except the last one.

Statements may also be grouped using parentheses.

## Logic

`condition` may be one of the following

```
a < b
a > b
a and b
a or b
```

with the operators reflecting their usual meaning

## Example program

```
var a <- 10;
var b <- 20;
var c <- a+b;

if 2 < 3 then
  c <- 0
else
  c <- c + 1
end;

while c<100 do
  c <- c + 2
end
```
