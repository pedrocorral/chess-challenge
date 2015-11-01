
package chess.challenge.test

import chess.challenge._

import org.scalatest._
import Matchers._

class Rules extends FlatSpec {

  // .: Increase the number of squares if you do not trust the tests :. //
  val squares = 20

  /**
   * Tests if the main piece threatens any other into a squares x squares board
   *
   * @param main_piece
   * @param rule
   */
  def testAnyDistributionOf( main_piece: Piece, rule: (Int,Int,Int,Int) => Boolean ): Unit =
    ( King :: Queen :: Rook :: Bishop :: Knight :: Nil ).foreach {
      piece => for {
        main_piece_rank <- (0 until squares)
        piece_rank <- (0 until squares) if main_piece_rank != piece_rank
        main_piece_file <- (0 until squares)
        piece_file <- (0 until squares) if main_piece_file != piece_rank
      } Rules.threats(
          (main_piece_rank,main_piece_file,main_piece),
          (piece_rank,piece_file,piece)
        ) shouldBe rule( main_piece_rank, main_piece_file, piece_rank, piece_file )
    }


  "rooks" should "threaten other pieces" in {
    def rule( r0: Int, f0: Int, r1: Int, f1: Int ) = ( r1 - r0 , f1 - f0 ) match {
      case (0,_) | (_,0) => true
      case _ => false
    }
    testAnyDistributionOf( Rook, rule )
  }

  "bishop" should "threaten other pieces" in {
    def rule( r0: Int, f0: Int, r1: Int, f1: Int ) = Math.abs( r0 - r1 ) - Math.abs( f0 - f1 ) match {
      case 0 => true
      case _ => false
    }
    testAnyDistributionOf( Bishop, rule )
  }

  "king" should "threaten other pieces" in {
    def rule( r0: Int, f0: Int, r1: Int, f1: Int ) = ( r1 - r0, f1 - f0 ) match {
      case (-1,-1) | (-1,0) | (0,1) | (-1,1) | (0,-1) | (0,1) | (1,-1) | (1,0) | (1,1) => true
      case _ => false
    }
    testAnyDistributionOf( King, rule )
  }

  "knight" should "threaten other pieces" in {
    def rule( r0: Int, f0: Int, r1: Int, f1: Int ) = ( r1 - r0, f1 - f0 ) match {
      case (-2,-1) | (-1,-2) | (-2,1) | (-1,2) | (2,-1) | (1,-2) | (2,1) | (1,2) => true
      case _ => false
    }
  }

  "queen" should "threaten other pieces" in {
    def rule( r0: Int, f0: Int, r1: Int, f1: Int ) = ( Math.abs(r1 - r0) , Math.abs(f1 - f0) ) match {
      case (0,_) | (_,0) => true
      case (n,m) if n == m => true
      case _ => false
    }
  }

}
