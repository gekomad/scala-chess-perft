scala-chess-perft
======

Perft (performance test) is a debugging function to walk the chess move tree of strictly legal moves to count all the leaf nodes of a certain depth.


```  
  import com.github.gekomad.chessgengenerator.perft.Perft
  import com.github.gekomad.chessgengenerator.util.PrintAndSum

  val start = System.currentTimeMillis()
  val l     = Perft.perft("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", depth = 6, printPartialResult = true)
  val end   = System.currentTimeMillis()
  val time  = (end - start) / 1000
  PrintAndSum.printAndSum(l, time)
```
```

     a   b   c   d   e   f   g   h
   ----+---+---+---+---+---+---+----
8 | r | n | b | q | k | b | n | r |
   ----+---+---+---+---+---+---+----
7 | p | p | p | p | p | p | p | p |
   ----+---+---+---+---+---+---+----
6 |   | . |   | . |   | . |   | . |
   ----+---+---+---+---+---+---+----
5 | . |   | . |   | . |   | . |   |
   ----+---+---+---+---+---+---+----
4 |   | . |   | . |   | . |   | . |
   ----+---+---+---+---+---+---+----
3 | . |   | . |   | . |   | . |   |
   ----+---+---+---+---+---+---+----
2 | P | P | P | P | P | P | P | P |
   ----+---+---+---+---+---+---+----
1 | R | N | B | Q | K | B | N | R |
   ----+---+---+---+---+---+---+----
     a   b   c   d   e   f   g   h


fen: rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1
zobrist key: 4375564139730772040
en passant position: None
right castle: 240
side to move: White

1)         h2-h4   5385554
2)         g2-g4   5239875
3)         f2-f4   4890429
4)         e2-e4   9771632
5)         d2-d4   8879566
6)         c2-c4   5866666
7)         b2-b4   5293555
8)         a2-a4   5363555
9)         h2-h3   4463070
10)        g2-g3   5346260
11)        f2-f3   4404141
12)        e2-e3   9726018
13)        d2-d3   8073082
14)        c2-c3   5417640
15)        b2-b3   5310358
16)        a2-a3   4463267
17)        g1-h3   4877234
18)        g1-f3   5723523
19)        b1-c3   5708064
20)        b1-a3   4856835
Tot 119060324 nodes in 13 seconds (9158k nodes per seconds)
```  

## License  
  
Licensed under licensed under GNU GPL 3
