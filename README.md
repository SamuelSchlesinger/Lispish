Lispish language

Programs consist of a series of top level definitions.

The main definition is the one which will be evaluated
when the program is run.

Def ::= Sym { Sym } = Term

Term ::= \ Sym { Sym } . Term
      | Nat
      | Sym
      | ( { Term } )
      | 'Term
