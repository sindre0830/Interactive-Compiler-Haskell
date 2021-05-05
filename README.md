# [Assignment 2 - bprog](https://git.gvk.idi.ntnu.no/course/prog2006/prog2006-2021/-/wikis/Tasks/Assignment-2:-bprog)

### Information

- Author: Sindre Eiklid
    - While the submission is individual, I have discussed the tasks with Rickard Loland. We have also helped each other with problems that occurred during development ([rubber ducking](https://en.wikipedia.org/wiki/Rubber_duck_debugging) mostly).
- I am using the Haddock formatting style for documentation.
- I am using Stack to format the structure of my repository.
- [First iteration](https://git.gvk.idi.ntnu.no/course/prog2006/as/sindre0830/assignment-two/assignment-two-iteration-one)
    - I decided to redo the assignment when I realized I couldn't implement everything using that design. If you are interested in seeing that design, you should look at the Dictionary file (hint: no sum types or state).

### Setup

Open a terminal and navigate to the project directory ```cd (...)/assignment-two-iteration-two```
1. Type in ```stack run``` to build and run the program
2. Type in ```stack test``` to build and run the tests

### Usage

I didn't have time to implement a help command, so you have to follow the tutorial below.

Commands:
- ```interactive``` starts the program in interactive mode.
    - ```--debug``` prints all the collections under the hood.
- ```compiler``` asks the user for a filename then compiles that through the program in compiler mode.
- Press ```CTRL-C``` at any time to terminate the program.

When you execute the program, you will be greeted by the menu. Here you can start either interactive or compiler mode. The interactive mode starts the program in a sandbox state where you can interact with the last result.
Compiler mode will ask for a filename that it will then compile. Here it will end the program when it's finished.

The program will catch any errors on the stack and display them to the user. If you are in interactive mode, it will revert to the last stable version of the program. In compiler mode, it will terminate the program.

### Notes

- I have written my program in bprog2 style
- I use sum types to build my stack type.
- I use State monad to handle the executer and functors. 
- I use data.Map to handle variables, lists, and code blocks.
- I handle IO by jumping out of the executer into the IO loop.
- I have made tests for all the pure functions in my program.
- All of the official tests for the assignment works.

The way I store variables and functions is (as recommended by the teacher) through data.Map. It works like the maps in other languages, key => value, and is a useful storage method for this assignment.

I wanted to store code blocks and lists as stacks and not as strings (like I did in iteration 1), but Haskell doesn't allow for cyclic types. I figured the best way would be to use maps almost like pointers in C/C++. The key is the address and the value is the stack. While it works well, it brings with it a lot of complexity. 
To reduce garbage collection, I have to make sure that I deallocate the container (list/code block) properly when removing the value, but this brought even more problems. Now I have to duplicate the map element every time I need to duplicate the value. This is because if I have two lists pointing to the same address and remove one of them, the other won't find the value and crash. Since a container can have nested containers inside it, I had to iterate through every possible element when deallocating or duplicating the value.
While this ended up being very complicated, it's also a great starting point for implementing actual pointer functors to the stack. If I had more time, I would probably do that to show the potential my design has. Two variables pointing to the same list would reduce the memory usage by half.

I handle IO by jumping out of the executer into the IO loop. While this keeps my code pure, it brings with it its disadvantages. Three functors require a nested executer in my program: map, foldl, and loop. I have no way of jumping out of the actual executer to the IO loop and back to the nested executer without losing data. I have added a specific error type for this problem: InvalidOperationIO.

The way I implemented the foldl functor is by always taking the head of the output. That can result in a wrong output if there is more than one element on the stack. I could fix this by only allowing single operations and not code blocks, but I decided to keep it like it is so it can handle complex operations. If this is the wrong decision, it would be easy to fix it. Just make sure that the length of the block is one and that it's an operation that requires two arguments.

### Reflections

While I'm happy with my solution, there are some things I would do differently. The pointer system was fun to implement but brought in so much complexity that I would probably store them as tokens instead.

I've learned a lot throughout this assignment. I got a better understanding of the type system, State monads, Data.map, file/directory reading, the importance of a good design, and the complications of deallocating/allocating properly (collections).
