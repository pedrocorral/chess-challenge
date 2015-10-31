
package chess.challenge.test

import chess.challenge._

import org.scalatest._
import Matchers._

class Distribution extends FlatSpec {

  "Easy distribution" should "be created for two knights" in {
    val distribution = Grid( 3, 3 ).initialDistribution( "NN" )
    distribution.length shouldEqual 2
    ( distribution match {
      case (0,0,Knight)::(0,1,Knight)::Nil => true
      case _ => false
    } ) shouldBe true
  }

  "Easy distribution" should "be created for two knights and one rook" in {
    val distribution = Grid( 3, 3 ).initialDistribution( "NNR" )
    distribution.length shouldEqual 3
    ( distribution match {
      case (0,0,Knight) :: (0,1,Knight) :: (0,2,Rook) :: Nil => true
      case (0,0,Rook) :: (0,1,Knight) :: (0,2,Knight) :: Nil => true
      case _ => false
    } ) shouldBe true
  }

}

