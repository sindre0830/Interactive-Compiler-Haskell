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
