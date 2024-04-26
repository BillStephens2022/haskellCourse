# Lazy Evaluation

Expressions in Haskell are evaluated using a simple technique called lazy evaluation, which:

- Avoids doing unnecessary evaluation;
- Ensures termination whenever possible;
- Supports programming with infinite lists;
- Allows programs to be more modular;

Any way of evaluating the same expression will give the same result, provided it terminates.

Evaluation Strategies
There are 2 main strategies for deciding which reducible expression (redex) to consider next:
- Choose a redex that is innermost, in the sense that does not contain another redex
- Choose a redex that is outermost, in the sense that is not contained in another redex


## Termination
infinity = 1 + infinity
Example: fst (0, infinity)
Innermost evaluation:
    evaluates what's in the parentheses first
    = fst (0, 1 + infinity)
    = fst (0, 1 + (1 + inifinity))
    = fst (0, 1 + (1 + (1 + infinity)))
    ... etc, etc,,,

Outermost evaluation:
    evaluates that fst just takes the first element in the pair, and gets zero
    = 0

In this example, outermost evaluation yields a result and terminates, so lazy evaluation
would take this approach.

Note: 
    - Outermost evaluation gives a result when innermost evaluation
      fails to terminate
    - If any evaluation sequence terminates, then so does outermost with same result


Example 2

square Num a => a -> a
square n = n * n

square (1 + 2)

Innermost Evaluation:
= square 3
= 3 * 3
= 9
Total steps to evaluate: 3

Outermost evaluation:
= (1 + 2) * (1 + 2)
= 3 * (1 + 2)
= 3 * 3
= 9
Total steps to evaluate: 4

Note:
- The outermost version is inefficient, because the argument 1+2 is duplicated when square
  is applied and is hence evaluated twice.
- Due to such duplication, outermost evaluation may require more steps than innermost.
- This problem can easily be avoided by using 'pointers' to indicate sharing of arguments.

using pointers
= * 1+2  - imagine there are two pointers - 1 from space before and 1 from the space after the *
  which are pointing to the (1+2)
= * 3 - shared argument, the (1+2) is only evaluated once
= 9 

lazy evaluation = outermost evaluation + sharing of arguments
Note: Lazy evaluation ensures termination whenever possible, but never requires more steps
      than innermost evaluation and sometimes fewer

## Infinite Lists
Example
recursive definition produces an infinite list of 1's.
ones = 1 : ones 

ones
= 1 : ones
= 1 : 1 : ones
= 1 : 1 : 1 : ones
... etc etc

Suppose we just want the head.

head ones

Innermost evaluation:
= head (1:ones)
= head (1:(1:ones))
= head (1:(1:(1:ones)))
... etc goes on infinitely, does not terminate

Lazy:
will just use outermost
= 1
and terminates

Notes:
- in the lazy case, only the first element of ones is produced, as the rest is not required
- In general, with lazy evaluation expressions are only evaluated as much as required
  by the context in which they are used
- Hence, ones is really a potentially infinite list

## Modular Programming

Lazy evaluation allows us to make programs more modular by separating control from data.

Following previous 'ones' infinite list example:

> take 5 ones
[1,1,1,1,1]

The data part ones (i.e. the actual infinite list of 1's) is only evaluated as much as required by the control part "take 5"



