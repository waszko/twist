#*Twist*

##What is *Twist*?

*Twist* is a tool for solving arbitrary NP-Complete problems. 

These problems are written in a form of second-order logic. *Twist* first reduces these into instances of the Boolean Satisfiability problem, which can then be solved by SAT-solving software. *Twist* then converts this solution back to produce an answer to the original problem.

##Documentation

The documentation available is included in the docs/ directory. This will consist of the original dissertation from which this project originated, and a user guide.

Example problem and instance files can be found in src/problems/ and src/graphs/, respectively.

##Installation

Build shell scripts for *Twist* and the test code are available in the src/ directory. 

##Usage

Once installed, *Twist* can be run as:

    twist.byte|native [options] <problem file> <instance file>

Available options can be listed by running:

    twist.byte|native --help

##SAT-solvers

*Twist* has been tested with MiniSat for solving regular CNF instances, and MiniSat+ for solving pseudo-Boolean constraints. These are available, respectively, at:

* http://minisat.se/MiniSat.html
* http://minisat.se/MiniSat+.html

