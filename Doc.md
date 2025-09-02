# Xbase++ Hands On

[alaska-software xbase reference](https://doc.alaska-software.com/content/grp_toc_all.cxp)

## Setup

## Basics

Xbase is case insensitive

Really important to know:
- list and strings index start at 1

## Xbase is weird

String inequality: for comparing a string with a single letter string it does not work properly
```
? "abcde" != "a"        * N
? "abcde" == "a"        * N
* if both are False where is the truth ?
```

## Compile

Compile a single file:
```
xpp file.prg
alink file.obj
```

## Xbase Projects



## Run

Run:
```
file.exe
```

Programs can be terminated with `ALT + C`

## Modularization

## Good Practices

Variables
- prefix the var name with the expected content of the var (hungarian notation: one letter prefix + camelCase)
- - `c` for chars
- - `n` for numerics
- - `l` for logical / booleans
- - `d` for dates
- - `o` for objects
- - `b` for code blocks
- - `a` for arrays

Function name are in PascalCase

## Synthax

### Comments

```
* this is a comment

/* this is also a comment */

/*
    multiple line
    comment
*/
```

### In / Out

Output a result to the console
```
? valueVariableOrExpression
```

Console input
```
ACCEPT "displayed input text" TO varName
```

### NIL

`NIL`, the value denoting the absence of value

### Variables

Assignment to variables
```
variable = valueOrExpression
variable := valueOrExpression
```

`:=` can also be used within expressions:
```
b := (a := 12)
```

In expressions `=` behaves as an equality comparison


### Booleans

Booleans literals
- `.T.` for true
- `.F.` for false

Logic operators
- `.NOT.`, logical not
- `!`, logical not
- `.AND.`, and between two values
- `.OR.`, or between two values

  
### Strings

Strings:
```
cVariable = "string"
cVariable = 'string'
```

String length:
```
nStringLenght = Len("string")
```

String concatenation:
```
"string1" + "string2"
```

Is string1 a substring of string2:
```
sString1 $ sTring2
```

Access a single char:
```
string[index]
```

Get a substring from a string:
```
SubStr(string, index, lenght)
```

String comparison: char by char, not ref :)

### Lists

Array:
```
array = { 1, "test", 3}

multidimensionalArray = { 1, "test", { 3, 5 } }
```

Array lenght:
```
nLenght = Len(aArray)
```

add an element to an array:
```
AAdd(aArray, element)

* if the element is an array, the content of the array 
* is copied
```

Access element
```
variable = aArray[i] 
aArray[i] = newValue
```

arrays are dynamic and heterogene grouping of data.

### Dates

Date + numeric => numeric is treated as the number of days to add to the date

Date - Date => number of days between the two dates

Date:
```
SET DATE FRENCH

dToday = Date()
dDateValue = CtoD("DD/MM/YYYY")
```

### Object

Objet = variables + program code

### Code blocks

A value containing executable code:
```
bVariable = {|| expression , expression, expression }

bVariable = {|param1, param2| expression , expression, expression }
```

The code block is executed when the variable is passed to the function `Eval()`.

Evaluate a code block:
```
Eval(bVariable)

Eval(bVariable, argument1, argument2)
```

### Comparison operators

All returns a boolean
- `==`, exact equality
- `=`, simple equality, weak not to be used
- `!=`

Array, code blocks and objects are compared with relation to refs.

Both operands must have the same type.

### if / else

```
if conditionOrBooleen
    * if code block
elseif conditionOuBooleen
    * else if code block
else
    * else code block
endif
```

### do case

Equivalent to else / if as it is based on conditions and not behaving like a switch acting on values
```
do case
case condition
    * code block
case condition
    * code block
otherwise
    * code block
endcase
```

### do while

```
do while conditionOrBooleen
    * code block
enddo
```

### for loop

Loop from startValue to nEnd both included.

Default increment of 1:
```
for counterVar := StartValue to nEnd
    * loop code block
next
```

Change the increment:
```
FOR counterVar := StartValue TO nEnd STEP nStep
    * loop code block
NEXT
```

Break out of a loop
```
EXIT
```

Skip the rest of the current loop iteration, and move to next iteration
```
LOOP
```

### functions

Declare a function (must be outside of a procedure):
```
function FunctionName()
    * function code block
    return 

function FunctionName(param1, param2, param3)
    * function code block
    return
```

Function call:
```
FunctionName()

FunctionName(arg1, arg2, arg3)
```

Functions can be called with a varying number of arguments.
When an argument is not specified => NIL.

Get the number of received arguments
```
PCount()
```

### Types

- variables are not strongly typed
- dynamic values

Get the type of a var
```
ValType()
```

Transform, String -> Int
```
nVar = Val(string)
```

Transform, Int -> String
```
cVar = Str(numeric)
cVar = Str(numeric, lenght)
cVar = Str(numeric, lenght, decimalRounding)
```

## Extensions

DLL, dynamic link library, compiled binary library
- from windows DLL

## Databases

