# Matlang Specifications

## Variables

### Variable declaration and types

Matlang is a staticaly typed interpreted language. Variables must start a letter and be alpha-numeric afterwise.
Variable declaration is specified by the keyword `let`. The type of the variable is induced at compile time.
Matlang has 6 types:

- Integer
- Float
- String
- Matrix
- Boolean
- Function

Note that in matlang, funtions are first-class citiziens.

example:

```
let a;
let hello;
```

### Variable initialisation

In Matlang, variable declaration and initialization need't be done at the same time.

```
let a;
a = 10;
# this is equivalent to: 
let a = 10;
```
If a variable is read before it is initialized, an error is thrown:

```
let a;

a + 10;

# error => a is not initialized, undefined behaviour.
```

### Types

Matlang support 6 types. The type of a variable is deduced syntactically at initialisation.

```
# A matrix :
# 12 4
#  1 4
let m = [12 4; 1 4];

# A string
let s = "hello world!"

# An integer

let n = 13;

# A float:

let x = 3.14;

# A function
let f = fn(x) { 
	ret x + 1; 
	};

# A boolean
let b = true || false;
```

### Scoping

In Matlang, blocks define scope. A variable defined in a scope is defined in any subscopes, unless shadowed.

```
let main = fn() {
	let a = 0;
	let b = 3.14;
	{
		let a = "Hello world";	
		b = 2.718;
	}
	a = a + 1;
	a == 1; # true
	b == 2.718; # true
};
```


### Shadowing

Matlang is a statically typed language, therefore, variables types can't be changed during assignation. However variables can be redeclared with the same name. The new variable will *shadow* the previous one. This allow for the redelaratoin of the variable with a different type.

```
let main = fn() {
	let a = 1; # an integer;
	let a = "hello"; # a string
}
```

## Operations and Flow Control

### Operations

#### Matrices

In matlang, matrices are first class citiziens, and they natively allow for
linear algebra operations. Arrays are a specific case of matrix where the
number of rows equals 1.

##### concat

The `<-` allows to concat values at the end of a matrix. Fof the concatenation to work, the number of rows of the two matrices need to match.

```
let a = [1 2; 3 4];

b = a <- [3; 5];

# b:
# 1 2 3
# 3 4 5
```

If the appended value is an integer, or a float, the value is cast to an array first, and concatenation in tried after.

##### Addition - Subtraction - Products


Matrices support for addition and subtraction with `+` and `-`. It works like linear algebra addition and subtraction. 

the `*` symbolises either the scalar-matrix multiplication or the Hadamard product (element-wise) between two matrices.

the `.` symbolyses the dot product between two matrices

the `'` operator symbolises the Transpose operation.

#### Numbers

The numbers support the `+`, `-`, `*` and `/` operations.

#### Booleans

Booleans support for the following operations: `||` and `&&`

### Control Flow

#### if-else statements

```
if true {
	# do something
} else {
	# do something else
}
```
The else clause is optional

#### while

```
while true {
	# do things
}
```


## Functions

Functions in matlang are first class citiziens. They are defined as follow:

```
let main = fn(args) {
	# do things
};
```

functions are called as follow:

```
main();
```

functions capture the scope they are defined in.


## Programm Structure

Unless specicied otherwise, upon parsing the programm the interpretter will search for the `main` function an execute it. Every variable declared in a scope in accesible scopewise.
