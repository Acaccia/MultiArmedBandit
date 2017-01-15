Multi-Armed Bandit
=======

The easiest way to build and run this program is by using *Stack*.
First, to install the compiler and build the executables, run

    stack setup
    stack build

 Now you have two executables: MultiArmedBandit and JointActions.


Multi-Armed Bandit
-------

 For MultiArmedBandit, execute

    stack exec -- MultiArmedBandit e +RTS -Nc

  with *e* the exercise number and *c* the number of cores to use. For example

    stack exec -- MultiArmedBandit 1 +RTS -N6
   will generate the graphs for exercise 1 using 6 cores.

  Joint-Actions Learners
-------

 For JointActions, execute

    stack exec -- JointActions +RTS -Nc

  with *c* the number of cores to use. For example

    stack exec -- JointActions +RTS -N3
   will generate the graphs using 3 cores.
