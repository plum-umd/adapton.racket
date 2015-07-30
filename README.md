# adapton.racket
Racket-based implementation of Adapton

The purpose of this repo is to provide a home for the development of Adapton for 
the Racket programming langauge. 

================================= General ============================

Racket is a powerful functional langauge that can be found here:
http://racket-lang.org/

Adapton provides a set of tools that the user can leverage to reason about his
or her programming. In addition, it defines a number of functions that accumulate
information about the flow of the program into a DCG (directed computation graph).
The goal of Adapton is to make use of laziness and the data collected into the DCG
to optimize re-computation of areas of the program that have been changed, without 
having to re-compute the entire program.

There are a number of papers on Adapton by Matthew Hammer, the first of which can 
be found here: http://www.cs.umd.edu/~hammer/adapton/


=============================== Using Adapton ==========================

Adapton has two key data structures: cells and nodes. Cells are structures
which contain mutable boxes. Inside these boxes the user should place input
values. In the mergesort example, the list provided to mergesort is a list
of cells that contain values. The cell structure enables us to keep track 
of when an input is mutated and which nodes make use of the data stored in a
given cell. This in turn enables us to visualize the implications of mutating
a cell. Your algorithm should operate on some combination of cells and nodes. 

A node contains a delayed function evaluation, called a thunk. Forcing a node
evaluates the thunk contained in that node, and keeps track of whether or not 
that node creates or forces other nodes. The successor / predecessor 
relationships between nodes, other nodes, and cells forms the algorithm's
DCG. In addition, nodes are memoized, which means that forcing a node that
has already been forced at some point in the past will make use of the result
of the last force. 

Learning to program with thunks (nodes) and mutable inputs (cells) can be 
difficult, just like learning to program lazily can be difficult. There are,
however, situations where the power gained by programming in this way can 
be worth the effort. 

===================== Understanding the code in this repo ====================

The code in this repo is seperated into several files to make it more managable.
Each file in the repo has a brief comment at the top of the file that describes 
the code stored within. In addition, each function within each file will have a 
brief purpose statement and some information about the use of that file. Further,
each function has a few tests ABOVE its definition which are intended to help 
an outside viewer understand what the function DOES before they see how it WORKS.

As of right now (August 2015), the easiest way to write code that works with Adapton
is to import the files containing the "meat" of Adapton to a new file that will 
contain your algorithm. Once your code is written, either import the "tools-for-testing" 
file, or consider creating a new file for your tests that leverages the code in 
the files related to testing.

============================== Graphing ===============================

There is code throughout the repo that builds a visual representation of the DCG
of your program. Obviously this code will effect performance, and so can be turned 
off by setting the parameter "graphing-on" to false (#f) in the file "graphing.rkt".
If you choose to leave graphing enabled, the results of the graphing are 
automatically stored in the file graph.gmv in the same directory as the "graphing.rkt"
file. You can modifiy the location and name of this file in "graphing.rkt".

To view the graph that is built you will need graphmovie, which can be downloaded 
here: https://github.com/kyleheadley/graphmovie

Once you have graphmovie download, simply open "graph.html" in your browser and open
the .gmv file containing your graph. 

It is worth noting that when the graph.gmv file is cleared and a new graph is created
each time your code is compiled. If you do not re-compile your code, changes will 
simply be added to the graph that currently exists in "graph.gmv". This means that you
perfrom some computations and load your DCG, and then go back and mutate your inputs. 
This will dirty your DCG, which will be reflected in graph.gmv until you re-compile.
