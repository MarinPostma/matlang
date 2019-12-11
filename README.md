# matlang

a toy programming language for linear algebra

## guide

### Types

there are 4 types:

* integers
* floats (not yet supported)
* matrices
* booleans

The typing of matlang is static and infered at compile time.

### Variable declaration

The following syntax allows you to declare a variable:

```
;;decalare an int
my_var = 12;

;; declare a matrix :
;; 1 2
;; 3 4

let my_mat = [1 2; 3 4];
```


### Control flow

```
if 12 == 17 {
	;; do something
} else {
;; the else close is optional.
	;; do something else
}
```

### loops

matlang only supports for while loops:

```
while true {
	;; do something
}
```

