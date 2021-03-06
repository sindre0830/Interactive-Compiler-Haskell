# Changelog for assignment-two-iteration-two

[0.0.0] **setup**: Initial commit

[0.0.1] **setup**: Copied code from iteration 1

[0.1.0] **development**: Added types

[0.2.0] **development**: Added type parser

[0.3.0] **development**: Added tokenize tester

[0.4.0] **development**: Added more types

[0.5.0] **development**: Added simple parser

[0.6.0] **development**: Added string parser

[0.7.0] **development**: Added simple list parser

[0.8.0] **development**: Added string parsing to list parser

[0.9.0] **development**: Added codeBlock parser

[0.10.0] **development**: Added codeBlock parsing in list parser and vice versa

[0.11.0] **development**: Added list and codeBlock parser to the main parser function

[0.11.1] **development**: Added function headers

[0.12.0] **development**: Added failsafe to parser when codeBlock or list returns error

[0.13.0] **development**: Added printer functionality to stack

[0.14.0] **development**: Added input parser that updates global variable

[1.0.0] **milestone**: Implimented parser

[1.1.0] **development**: Implemented head functor

[1.2.0] **development**: Fixed memory garbage by deallocating list before removing pointer

[1.3.0] **development**: Implemented tail functor

[1.3.1] **formatting**: Removed useless brackets

[1.3.2] **formatting**: Changed output in StackState to output objects aswell

[1.4.0] **development**: Fixed stack formatter to print code blocks properly

[1.5.0] **development**: Fixed tail functor where I checked amount of parameters

[1.6.0] **development**: Changed output of one head test to printable stack

[1.7.0] **development**: Implemented empty functor

[1.8.0] **development**: Implemented cons functor and properly deallocates objects

[1.9.0] **development**: Implemented object functionality: generateAddress and deallocate

[1.10.0] **development**: Implemented allocate object

[1.11.0] **development**: Implemented update object

[1.12.0] **development**: Implemented append functor

[1.13.0] **development**: Implemented addition functor

[1.14.0] **development**: Fixed parse error when checking for errors in empty stacks

Fucked up last commit by commiting files that weren't ready

[1.14.1] **fix**: Fixed crashes in last commit where head and tail was used on empty stack

[1.14.2] **fix**: Fixed issue with postfix before prefix

[1.14.3] **fix**: Fixed arguments being the wrong way around

[1.15.0] **development**: Implemented subtraction functor

[1.16.0] **development**: Implemented AND functor

[1.17.0] **development**: Implemented equal functor

[1.17.1] **formatting**: Split functors by subject

[1.18.0] **development**: Implemented multiplication functor

[1.18.1] **formatting**: Reformatted arithmetic functors

[1.19.0] **development**: Implemented division functors

[1.20.0] **development**: Moved stack deallocation to it's own function

[1.21.0] **development**: Implemented OR and NOT functors

[1.22.0] **development**: Implemented less and greater functors

[1.23.0] **development**: Implemented pop and dup functors

[1.24.0] **development**: Implemented swap functor

[1.25.0] **development**: Implemented parseInteger and parseFloat functors

[1.26.0] **development**: Implemented words functor

[1.27.0] **development**: Implemented length functor

[1.28.0] **development**: Implemented simple version of exec functor

[1.29.0] **development**: Moved stack input to State monad so exec can manipulate the buffer directly

[1.30.0] **development**: Added possiblity to use equal functor on lists and code blocks

[1.31.0] **development**: Implemented if functor

[1.32.0] **development**: Implemented map functor

[1.33.0] **development**: Implemented each functor

[1.34.0] **development**: Implemented foldl functor

[1.35.0] **development**: Fixed foldl to use given value as starting point

[1.36.0] **development**: Implemented times functor

[1.37.0] **development**: Implemented loop functor

[1.38.0] **development**: Added functions map to state

[1.39.0] **development**: Implemented assignments of variables

[1.40.0] **development**: Implemented assignments of functions

[1.41.0] **development**: Fixed objects being duplicated in assignments

[1.42.0] **development**: Fixed functions to properly duplicate objects

[1.43.0] **development**: Added file reader to compiler mode

[1.44.0] **development**: Modified StackState so it can handle IO

[1.45.0] **development**: Implemented print functor

[1.46.0] **development**: Implemented read functor

[2.0.0] **milestone**: Implemented all functors

[2.1.0] **development**: Implemented debug cmd in interactive mode

[2.2.0] **development**: Added deallocation/duplication of nested objects

[2.3.0] **development**: Fixed print to only print when there is a string

[2.4.0] **development**: Modified printable stack to use intercalate

[2.5.0] **development**: Added handler when IO operation is used in map, each, foldl and loop

[2.6.0] **development**: Allow controlflow to read without blocks

[2.7.0] **development**: Modified UI

[2.8.0] **development**: Fixed some controlflow that would prematurely take elements

[2.9.0] **development**: Modified UI

[2.10.0] **development**: Added offical tests for assignment 2

[2.11.0] **development**: Changed INT type to Integer for larger values

[2.12.0] **development**: Fixed equal functor so it can check nested lists and codeblocks

[2.13.0] **development**: Fixed length functor to work on codeblocks

[2.14.0] **development**: Fixed words functor so it returns a list

[2.15.0] **development**: Fixed assignments being reversed

[2.16.0] **development**: Fixed exec functor deallocating too many objects

[2.17.0] **development**: Fixed if functor by allowing all types when called without blocks

[2.18.0] **development**: Fixed times functor by allowing all types when called without blocks

[2.19.0] **development**: Fixed variables so its set inside objects

[2.20.0] **development**: Fixed map and each functor so it works with nested objects

[2.21.0] **development**: Functions are now properly duplicated

[2.22.0] **development**: Simplified parser functions

[2.23.0] **development**: Fixed control flow functors so it deals with nested objects properly

[2.24.0] **development**: Allow for variables and functions to be reassigned

[3.0.0] **milestone**: Implemented offical tests and fixes for functors

[3.0.1] **structure**: Moved functors into their own modules

[3.0.2] **formatting**: Reduced indentation in functor calculations

[3.0.3] **formatting**: Added result variable in functors

[3.1.0] **development**: Modified each functor so it doesn't use nested executer

[3.2.0] **development**: Modified deallocateStack to only return object

[3.3.0] **development**: Modified functors to minimize deallocation calls

[3.4.0] **development**: Fixed head and tail functors to work with nested objects

[3.5.0] **development**: Added validateParameters to reduce clutter in functors

[3.6.0] **development**: Removed Print type and just use String type

[3.6.1] **formatting**: Renamed Objects to Containers

[3.7.0] **development**: Added getContainer to reduce clutter in functors

[3.8.0] **development**: Added isFunction and isVariable to check if Unknown value is assigned

[3.9.0] **development**: Added getBlock to reduce clutter in functors

[3.10.0] **development**: Added error handling when trying to divide by zero

[3.11.0] **development**: Added proper error handler (Ends executer on error, prints logs, reverts back to a stable version of the program)

[3.12.0] **development**: Added directory handler to display available files and allow the user to pick in the compiler mode

[3.12.1] **formatting**: Added space between elements in lists

[3.13.0] **development**: Modified updateContainer to take type instead of key and switched parameters of getContainer

[3.13.1] **formatting**: Added function headers

[3.13.2] **formatting**: Restructured spec tests to mimic folder structure

[3.14.0] **development**: Added the remaining tests for all Functor modules

[3.15.0] **development**: Added the remaining tests for the Compiler module

[3.16.0] **development**: Added the remaining tests for the Convert module

[3.17.0] **development**: Added the remaining tests for the Stack module

[3.17.1] **structure**: Renamed Parsing module to Parser

[3.17.2] **structure**: Renamed Stack module to MemoryHandler

[3.17.3] **structure**: Renamed Convert module to Converter

[3.18.0] **development**: Moved functions used for testing to Spec.hs

[3.18.1] **formatting**: Specified imports from modules

[3.19.0] **development**: Added information to the README

[3.19.1] **formatting**: Added comments

[3.19.2] **structure**: Added more example files

[3.19.3] **formatting**: Added disclaimer in README

[3.19.4] **formatting**: Fixed typo in README
