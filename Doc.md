# Xbase++ Hands On

[alaska-software xbase reference](https://doc.alaska-software.com/content/grp_toc_all.cxp)

## Setup

## Compile

Compile a single file:
```
xpp file.prg
alink file.obj
```

Run:
```
file.exe
```

Programs can be terminated with `ALT + C`

## Xbase Projects

### Introduction

ProjectBuilder is a tool for managing entire software projects.

The central element: one file with extension `.xpj` (for XbaseProJect) as a description of the project.
- information for the builder
- information for compiler
- information for linker
- which executable to create
- name of source files

### Basic Manifest Template

Generate the basic template from a list of files:
```
pbuild app.prg file1.prg file2.prg file3.prg
```

Generate the basic template from files in current workdir:
```bat
dir /b > project-filess.txt
pbuild @project-files.txt

:: The @ indicates the compiler to take additional arguments from a file.
```

The manifest structure, example:
```
//
// Project - Definition - File created by PBUILD Version 2.00.2383
// Date: 03.09.2025 Time: 08:20:46
//

[PROJECT]
    VERSION       = 2.2
    TARGET_DIR    = .\run
    project.xpj                 // the root of the project

[project.xpj]
    project-app.exe

[project-app.exe]
    COMPILE              = xpp
    COMPILE_FLAGS        = /q
    DEBUG                = yes
    GUI                  = no
    LINKER               = alink
    LINK_FLAGS           = 
    RC_COMPILE           = arc
    RC_FLAGS             = 
    INTERMEDIATE_DEBUG   = .debug
    INTERMEDIATE_RELEASE = .release
    app.prg
```

The PROJECT section 
- is the entry point for the Project Builder
- lists definitions valid for the entire project

The project.xpj section
- only one per project
- the root of the project
- lists all build artefacts executables / dll resulting of the project

The next sections, one per DLL / executable 
- the section name is the name of the resulting file
- the requirements for the executable / dll
- the configuration of the executable / dll
- to build an executable one of the .prg files must contain a startup procedure

Summary, the project manifests
- lists build artefacts
- how to build them
- keep track of program dependencies updates

### Dependencies update 

Pbuild is able to find file dependencies and autocomplete the XBJ manifest to include the dependencies. <br/>
Complete the manifest with new dependencies:
```bat
pbuild project.xpj /g

:: resulting lines are inserted in the manifest between 
:: // $START-AUTODEPEND 
:: and 
:: // $STOP-AUTODEPEND
:: // don't put something there, it might be erased
```

### Project build

Build a project:
```bat
pbuild      
    :: builds all target in the project, when project manifest 
    :: has the default name, project.xbj

pbuild manifest-file
    :: builds all target in the project, when project manifest
    :: has a custom filename

pbuild manifest-file.xpj
    :: builds all target in the project, when project manifest
    :: has a custom filename
```

Add flag `/t:targetName` to command above to build a specific target within a project.

### Manifest options

Choose where to place the build result:
```
[PROJECT]
    TARGET_DIR  = .\artefact 
```

## Modularization

Functions / class can be called in any code that is compiled together with the prg containing the declaration.

## Basics

Xbase is case insensitive

Really important to know:
- list and strings index start at 1

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

Class methods on Objects should return `self` if not returning a meaningfull value (enables chaining)

Release GUI bitmap elements with `destroy` when no longer needed.

When functions require more than 3 parameters, create a `configuration object` passed to the function. So that you can have named arguments within the config and remove the risk of positionnal error + truly optionnal arguments. Not just starting from a position.
```
function FunctionName(oDataObject)
    arg1 = oDataObject:arg1
    arg2 = oDataObject:arg2
    arg3 = oDataObject:arg3
    return

oArguments = DataObject():New()
oArguments:arg1 = value
oArguments:arg2 = value
oArguments:arg3 = value
FunctionName(oArguments)
```

## Xbase is weird

String inequality: for comparing a string with a single letter string it does not work properly
```
? "abcde" != "a"        * N
? "abcde" == "a"        * N
* if both are False where is the truth ?
```

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

Remove leading and trailing blank spaces:
```
cNewString = AllTrim(cString)
```

Upper / Lower the chars in the string:
```
cNewString = Lower(cString)
cNewString = Upper(cString)
```

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

### Class Quickstart

Objet = variables + program code

Declare a class:
```
CLASS ClassName
    
ENDCLASS
```

Within the class definition are the declarations of the variables and methods.

They exist two types of vars:
- class vars, accessible directly on the object, shared with the instance
- - must be accessed on the object not on self
- vars, instance related var

Declare variables
```
    VAR varName1, varName2
    VAR varName
    CLASS VAR varname

    EXPORTED:
        VAR varName
        CLASS VAR varName
```

They exist two types of methods:
- class methods, accessible directly on the object
- methods, instance related method


Declare methods within the class declaration:
```
    CLASS METHOD MethodName1, MethodName2, MethodName3
    CLASS METHOD MethodName
    METHOD MethodName

    EXPORTED:
        CLASS METHOD MethodName
        METHOD MethodName
```

Sections of SHARED, EXPORTED, HIDDEN can mix both variables and methods.

Methods must be defined after `CLASSEND`, and on the class:
```
CLASS METHOD ClassName:MethodName(paramList)
    * method code 
RETURN

METHOD ClassName:MethodName(paramList)
    * method code 
RETURN
```

Access object elements (method or var):
```
oObjectRef:element
```

Methods reference their class instance with `self`
- `::`, a shorthand notation for `self:`

Default initialization (optional):
- class method `InitClass()`
- invoked once
- called after the call to the class function
- can intialize class variables
- `self` contains a reference to the class object

Constructor (optionnal):
- method `Init`
- can take parameter forwarded from the call to new `ClassName():new(arg1, arg2)`
- `self` contains a reference to the instance

Built-in class methods
- `className()`, returns the class name as a string
- `classObject()`, returns the class object 
- `isDerivedFrom(cClassName | oClassObject)`, is a class inheriting the class

Initialize an object (class instance):
```
oClassInstance = ClassName():new()
oClassInstance = ClassName():new(arg1, arg2, arg3)
```

Classes annotations
- `STATIC`, makes the class to be called only within source code file of declaration
- `FINAL`, cannot be herited from

Inheritance
- both single and multiple
- `FROM listOfSuperClass`, inherits all members and functions of the parent
- - the child class can call all parent methods that are not marked as `HIDDEN`

### Class elements visibility

- `HIDDEN` only visible within methods of this class
- `PROTECTED` only visible within methods of this class and its subclass
- `EXPORTED` visible to the entire application

additionnal annotation for variables:
- `READONLY`, restrict outside var accessibility to read only, still editable by the class methods

By default, all of the instance variables and methods declared in the class are HIDDEN.

### Class Theory

STATIC CLASS
- A call to a STATIC CLASS outside of its definition file returns NIL

FEEZE
- a class cannot be replaced by another class with same name
- class replacement can occur due to dynamically created class at runtime

SHARED
- class variables that are not declared as `SHARED` by the parent class are "injected" into the child as distinct from the parent

### DataClass

Object cannot have new members at runtime, they must be declared together with class declaration.

DataObject:
```
oDataObject = DataObject():New()

oDataObject:Key1 = value
oDataObject:Key2 = value
oDataObject:Key3 = value
```

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

### Functions

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

By default argument are provided by value (a copy of the arg content). <br/>
Pass an argument by reference with the `@` annotation:
```
functionCall(@variable)
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

## GUI

Xbase Parts
- provide graphic dialog elements
- based on os resources
- event driven design of the OS

To use Xbase Parts the program must be linked for GUI mode. <br/>
In the project manifest, set GUI to yes for the tqrgets requiring GUI:
```
    GUI                  = yes
```

All GUI output occurs in a window on the screen.

App windows
- is implicit
- can be retrieved with `SetAppWindow()` 
- an instance of XpbCrt
- hybrid, enables to do both text mode IO and GUI
- data can be output using pure text oriented functions 
- - `DispOut()`
- - `QOut()`

Child parent relationship between elements.

### XBP lifecycle

Create the XBP object
- 1. instantiate: `XBPName():new()`
- 2. configure the element (title, apparence)
- 3. request system resource: `oXBPElement:create()`, create the underlying GUI resource
- - the component is fully functionnal
- 4. release a resource: `oXBPElement:destroy()`
- - generally not needed

Get the current lifecyle of an element with `:status()`
- returs a numeric value, defined as constants
- - `XBP_STAT_INIT`
- - `XBP_STAT_CREATE`
- - `XBP_STAT_FAILURE`

A resource can be reconfigured after an XBP is created:
```
oXBPElement:key = value
oXBPElement:configure()
```

### XBP elements

All XBP elements embed `cargo` variable to serve a user defined data. 

`XbpPushButton`
- `:caption`, text configuration
- `:activate`, code block configuration for when the button is pressed
- - automatically called when using the handler

You can inherit XBP elements and extend them, ex:
```
CLASS MyPushbutton FROM XbpPushbutton
    EXPORTED:
    METHOD init, activate
ENDCLASS

// initialize superclass and self
METHOD MyPushbutton:init( cCaption, aPos, aSize )
    ::XbpPushButton:init(,, aPos, aSize )
    ::caption := cCaption
RETURN self

// callback method for the event xbeP_Activate
METHOD MyPushbutton:activate
    QOut( "Pushbutton ", ::caption )
RETURN self
```

### Events

GUI message stream
- messages identify the events that have taken place
- - mouse clicks
- - keyboard event
- read events with `AppEvent()`
- - returns a numeric event code
- - events are read from the event queue

The event loop:
```
nEvent = 0
DO WHILE nEvent != xbeP_Close()
    nEvent := AppEvent( @mp1, @mp2, @oXbp )
        ** mp1, message parameter 1 
        ** mp2, message parameter 2
        ** oXbp, the object the event belongs to
    
    oXbp:HandleEvent( nEvent, mp1, mp2 )
        ** calling the handler of the element
ENDDO
```

For each XBP there are many instance variables that can contain code blocks that are executed in response to specific events.

Mouse events callback
- `:LbClick`, Click left button
- `:LbDblClick` Double click left button
- `:LbDown` Left button pressed
- `:LbUp` Left button released
- `:MbClick` Click middle button
- `:MbDblClick` Double click middle button
- `:MbDown` Middle button pressed
- `:MbUp` Middle button released
- `:Motion` Mouse moved
- `:RbClick` Click right button
- `:RbDblClick` Double click right button
- `:RbDown` Right button pressed
- `:RbUp` Right button released

Keyboard & others events callbacks
- `:Keyboard`, Keyboard entry occurred
- `:HelpRequest`, Help requested
- `:SetInputFocus`, Input focus granted
- `:KillInputFocus`, Input focus lost
- `:Paint`, XBP moved
- `:Paint`, XBP redrawn
- `:Quit`, Application terminated
- `:Resize`, Size of XBP changed

Write a custom handler:
```
function MyCustomHandler()
    ** code for the event handler
    return

oXBP:event = {|| MyCustomHandlerCall()}
```

The event is first attributed to the child, but if it can not handle it, the event is forwarded to its parents until it is handled or ignored (in case no one can handle it).

### GUI actions

Display text on the screen
- `QOut(cText)`

### GUI, HMTL and CSS Based



## Timers



## Databases



## HTTP


## File system



## Interact with OS



## Preprocessor

