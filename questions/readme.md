1. <solve> can compile but cannot run the program 'main' in ghci to input numbers to sum.
   1.  ghc --make sum
   2.  ./sum
2. unit4/lesson22/l22_1exercises.hs cant compile
   1. getContents is for everything into 1 string regardless of how many time u hit enter
   2. better to use getLine if there are several IO steps
3. unit6/lesson34/l34_exercises/glitchArt can compile with `ghc Main.hs` but when `./Main  ` i get error `Main: Prelude.head: empty list`...previously taken from unit4/lesson25
4. I would stack new to start a project, adjust package.yaml then stack run to make sure the dependencies go into the .cabal file. But stack run always take a long time with a message of xxMB
5. I could power up unit7/lesson40/json-lesson but when keep getting error when i run stack ghci main