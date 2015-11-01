
package chess.challenge.test

import chess.challenge._

import org.scalatest._

class Distributions extends FlatSpec with Inspectors with Matchers {

  lazy val foldResults = (distributions: List[Distribution], distribution: Distribution) => distribution :: distributions

  "Easy 2x2 distribution" should "be created for two rooks" in {
    val xs = Grid(2, 2)( "RR", Nil, foldResults )
    xs.length shouldEqual 2
    xs.foreach { Rules.isValid( _) shouldBe true }
  }

  "Easy 2x2 distribution" should "be created for two knights" in {
    val xs = Grid(2, 2)( "NN", Nil, foldResults )
    xs.length shouldEqual 6
    xs.foreach { Rules.isValid( _) shouldBe true }
  }

  "Easy 2x2 distribution" should "be created for two queens" in {
    val xs = Grid(2, 2)( "QQ", Nil, foldResults )
    xs shouldBe empty
  }

  "Easy 2x2 distribution" should "be created for two kings" in {
    val xs = Grid(2, 2)( "KK", Nil, foldResults )
    xs shouldBe empty
  }

  "Easy 2x2 distribution" should "be created for two bishops" in {
    val xs = Grid(2, 2)( "BB", Nil, foldResults )
    xs.length shouldEqual 4
    xs.foreach { Rules.isValid( _) shouldBe true }
  }

  "2x2 distributions" should "be created for any combination of two pieces" in {
    def validDistributions( piece0: Piece, piece1: Piece ): Int = ( piece0, piece1 ) match {
      case (Queen,_) | (_,Queen) => 0
      case (King,_) | (_,King) => 0
      case (Rook,Bishop) | (Bishop,Rook) => 0
      case (Rook,_) | (_,Rook) => 4
      case (Bishop,_) | (_,Bishop) => 8
      case _ => throw new Error( "ERRoR::Unknown distribution for $piece0 vs $piece1" )
    }

    val pieces = Queen :: King :: Bishop :: Rook :: Knight :: Nil
    val grid2x2 = Grid( 2, 2 )

    for {
      piece0 <- pieces
      piece1 <- pieces ; if piece0 != piece1
    } grid2x2( piece0 :: piece1 :: Nil, Nil, foldResults ).length shouldEqual validDistributions( piece0, piece1 )
  }

  "3x3 distribution" should "be created for two kings and one rook" in {
    val xs = Grid( 3, 3 )( "KRK", Nil, foldResults )
    xs.length shouldEqual 4
    exactly( 1, xs ) should contain only ( (0,2,King), (0,0,King), (2,1,Rook) )
    exactly( 1, xs ) should contain only ( (2,0,King), (0,0,King), (1,2,Rook) )
    exactly( 1, xs ) should contain only ( (2,2,King), (0,2,King), (1,0,Rook) )
    exactly( 1, xs ) should contain only ( (2,2,King), (2,0,King), (0,1,Rook) )
  }

  "4x4 distribution" should "be created for two rooks and 4 knights" in {
    val xs = Grid( 4, 4 )( "NRNNRN", Nil, foldResults )
    xs.length shouldEqual 8
    exactly( 1, xs ) should contain only ( (0,1,Knight), (0,3,Knight), (1,2,Rook), (2,1,Knight), (2,3,Knight), (3,0,Rook) )
    exactly( 1, xs ) should contain only ( (0,1,Knight), (0,3,Knight), (1,0,Rook), (2,1,Knight), (2,3,Knight), (3,2,Rook) )
    exactly( 1, xs ) should contain only ( (0,0,Rook), (1,1,Knight), (1,3,Knight), (2,2,Rook), (3,1,Knight), (3,3,Knight) )
    exactly( 1, xs ) should contain only ( (0,2,Rook), (1,1,Knight), (1,3,Knight), (2,0,Rook), (3,1,Knight), (3,3,Knight) )
    exactly( 1, xs ) should contain only ( (0,1,Rook), (1,0,Knight), (1,2,Knight), (2,3,Rook), (3,0,Knight), (3,2,Knight) )
    exactly( 1, xs ) should contain only ( (0,3,Rook), (1,0,Knight), (1,2,Knight), (2,1,Rook), (3,0,Knight), (3,2,Knight) )
    exactly( 1, xs ) should contain only ( (0,0,Knight), (0,2,Knight), (1,3,Rook), (2,0,Knight), (2,2,Knight), (3,1,Rook) )
    exactly( 1, xs ) should contain only ( (0,0,Knight), (0,2,Knight), (1,1,Rook), (2,0,Knight), (2,2,Knight), (3,3,Rook) )
  }

}

