
package chess.challenge.test

import chess.challenge._

import org.scalatest._
import Matchers._

class Distributions extends FlatSpec {

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
    val range_map = Map[Piece,Int](
      Queen -> 0,
      King -> 0,
      Bishop -> 8,
      Rook -> 4,
      Knight -> 12
    )
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

}

