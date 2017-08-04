package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) { case (block, move) => move match {
        case Left => block.left
        case Right => block.right
        case Up => block.up
        case Down => block.down
      }
    }
  }

  trait Level1 extends SolutionChecker {
      /* terrain for level 1*/

    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }


	test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0,0)), "0,0")
      assert(terrain(Pos(1,1)), "1,1") // start
      assert(terrain(Pos(4,7)), "4,7") // goal
      assert(terrain(Pos(5,8)), "5,8")
      assert(!terrain(Pos(5,9)), "5,9")
      assert(terrain(Pos(4,9)), "4,9")
      assert(!terrain(Pos(6,8)), "6,8")
      assert(!terrain(Pos(4,11)), "4,11")
      assert(!terrain(Pos(-1,0)), "-1,0")
      assert(!terrain(Pos(0,-1)), "0,-1")
    }
  }

	test("findChar level 1") {
    new Level1 {
      assert(startPos === Pos(1,1))
    }
  }

  test("findChar search for invalid char") {
    new Level1 {
      lazy val vector: Vector[Vector[Char]] =
        Vector(level.split("\n").map(str => Vector(str: _*)): _*)  // so I can test an invalid terrain character.
      assert(findChar('Q', vector) === Pos(-1,-1))
    }
  }

  test("isStanding") {
    new Level1 {
      assert(Block(Pos(1,1), Pos(1,1)).isStanding === true)
      assert(Block(Pos(1,1), Pos(1,2)).isStanding === false)
      assert(Block(Pos(2,1), Pos(3,1)).isStanding === false)
    }
  }

  test("isLegal") {
    new Level1 {
      assert(Block(Pos(0, 0), Pos(0, 0)).isLegal === true)      // standing
      assert(Block(Pos(1, 1), Pos(1, 2)).isLegal === true)      // laying horizontal
      assert(Block(Pos(0, 2), Pos(0, 3)).isLegal === false)     // hanging off right side of legal terrain
      assert(Block(Pos(2, 0), Pos(3, 0)).isLegal === false)     // hanging off bottom of legal terrain
      assert(Block(Pos(3, 8), Pos(3, 9)).isLegal === true)      // on right edge of legal terrain
      assert(Block(Pos(3, 9), Pos(3, 10)).isLegal === false)    // hanging off right edge of defined board
      assert(Block(Pos(-1, 0), Pos(0, 0)).isLegal === false)    // hanging off left edge of defined board
      assert(Block(Pos(0, -1), Pos(0, 0)).isLegal === false)    // hanging off top edge of defined board
      assert(Block(Pos(4, 0), Pos(5, 0)).isLegal === false)     // completely on defined board, but outside terrain.
      assert(Block(Pos(5, 0), Pos(6, 0)).isLegal === false)     // hanging off bottom edge of defined board
      assert(Block(Pos(20, 20), Pos(20, 20)).isLegal === false) // completely off game board.
    }
  }

  test("neighbors") {
    new Level1 {
      //println("Block(1,1) has 2 legal neighbors (right, down) and 2 illegal ones (left, up)")
      //println("Block(1,1) neighbors:  " + Block(Pos(1,1), Pos(1,1)).neighbors)
      assert(Block(Pos(1,1), Pos(1,1)).neighbors.toSet === Set(
        (Block(Pos(1,-1), Pos(1,0)), Left),
        (Block(Pos(1,2), Pos(1,3)), Right),
        (Block(Pos(-1,1), Pos(0,1)), Up),
        (Block(Pos(2,1), Pos(3,1)), Down)
      ))
      //println("Block(2,2) has 4 legal neighbors")
      //println("Block(2,2) neighbors:  " + Block(Pos(2,2), Pos(2,2)).neighbors)
      assert(Block(Pos(2,2), Pos(2,2)).neighbors.toSet === Set(
        (Block(Pos(2,0), Pos(2,1)), Left),
        (Block(Pos(2,3), Pos(2,4)), Right),
        (Block(Pos(0,2), Pos(1,2)), Up),
        (Block(Pos(3,2), Pos(4,2)), Down)
      ))
    }
  }

  test("legal neighbors") {
    new Level1 {
      //println("Block(1,1) has 2 legal neighbors (right, down) and 2 illegal ones (left, up)")
      //println("Block(1,1) legal neighbors:  " + Block(Pos(1,1), Pos(1,1)).legalNeighbors)
      assert(Block(Pos(1,1), Pos(1,1)).legalNeighbors.toSet === Set(
        (Block(Pos(1,2), Pos(1,3)), Right),
        (Block(Pos(2,1), Pos(3,1)), Down)
      ))
      //println("Block(2,2) has 3 legal neighbors and 1 illegal one (down)")
      //println("Block(2,2) legal neighbors:  " + Block(Pos(2,2), Pos(2,2)).legalNeighbors)
      assert(Block(Pos(2,2), Pos(2,2)).legalNeighbors.toSet === Set(
        (Block(Pos(2,0), Pos(2,1)), Left),
        (Block(Pos(2,3), Pos(2,4)), Right),
        (Block(Pos(0,2), Pos(1,2)), Up)
      ))
      //println("Block(5,2) has 0 legal neighbors")
      //println("Block(5,2) legal neighbors:  " + Block(Pos(5,2), Pos(5,2)).legalNeighbors)
      assert(Block(Pos(5,2), Pos(5,2)).legalNeighbors === List())
    }
  }

  test("neighborsWithHistory") {
    new Level1 {
      val b = Block(Pos(1,1), Pos(1,1))
      val moves = List(Left, Up)
      //println("neighborsWithHistory of (1,1)")
      //println("Stream:  " + neighborsWithHistory(b, moves))
      //println("List:  " + neighborsWithHistory(b, moves).toList)
      //println("end neighborsWithHistory")
      assert( neighborsWithHistory(b, moves).toSet === Set(
                                                            (Block(Pos(1,2), Pos(1,3)), List(Right, Left, Up)),
                                                            (Block(Pos(2,1), Pos(3,1)), List(Down, Left, Up))
                                                          ))
    }
  }

  test("newNeighborsOnly") {
    new Level1 {
      val b = Block(Pos(1,1), Pos(1,1))
      val moves = List(Left, Up)
      val nh = neighborsWithHistory(b, moves)
      val explored = Set(Block(Pos(1,2), Pos(1,3)), Block(Pos(1,1), Pos(1,1)))
      //println("newNeighborsOnly of (1,1), been to (1,2-3), (1,1)")
      //println("Stream:  " + newNeighborsOnly(nh, explored))
      //println("List:  " + newNeighborsOnly(nh, explored).toList)
      //println("end newNeighborsOnly")
      assert( newNeighborsOnly(nh, explored).toSet === Set( (Block(Pos(2,1), Pos(3,1)), List(Down, Left, Up)) ) )
    }
  }

	test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) === Block(goal, goal))
    }
  }

	test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length === optsolution.length)
    }
  }

  trait IsolatedStart extends SolutionChecker {
    /* terrain to test no solution because cannot leave starting position */

    val level =
      """ooo----S--
        |oooooo----
        |ooooooooo-
        |-ooooooooo
        |-----ooToo
        |------ooo-""".stripMargin

  }

  test("No solution, start position is an island.") {
    new IsolatedStart {
      assert(solution === Nil)
    }
  }

  trait IsolatedFinish extends SolutionChecker {
    /* terrain to test no solution because cannot reach goal position */

    val level =
      """ooo-------
        |oSoooo----
        |ooooooooo-
        |-ooooooooo
        |-----ooooo
        |--T---ooo-""".stripMargin

  }

  test("No solution, target position is an island.") {
    new IsolatedFinish {
      assert(solution === Nil)
    }
  }

}
