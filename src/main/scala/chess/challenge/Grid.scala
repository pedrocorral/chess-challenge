
package chess.challenge

/**
 * The rows are called ranks, while the cols are called files
 * The files are usually denoted with letter, but we will use integers
 * because our grid could have different dimensions
 *
 * @param ranks
 * @param files
 */
case class Grid( ranks: Int, files : Int ) {

  def initialDistribution( pieces: String ): Option[Distribution] = ???
  def initialDistribution( pieces: List[Char] ): Option[Distribution] = ???
  def initialDistribution( pieces: Map[Char,Int] ): Option[Distribution] = ???

}

