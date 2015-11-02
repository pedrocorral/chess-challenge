
package chess.challenge

/**
  * Simply outputs any result that could obtain the class Grid
  */
object Output {

  /**
    * Draws a "nice" grid with the pieces' distribution
    * 
    * @param grid
    * @param distribution
    * @return
    */
  def printDistribution( grid: Grid, distribution: Distribution ): Unit = println( distributionToString( grid, distribution ) )

  /**
    * Creates a "nice" string of the grid and the pieces
    * 
    * @param grid
    * @param distribution
    * @return
    */
  def distributionToString( grid: Grid, distribution: Distribution ): String = {
    val distribution_map = distribution.map {
      case ( rank, file, piece ) => ((rank,file),piece)
    }.toMap

    val str_line = List.fill( grid.files )( "-" ).mkString( "+", "+", "+" ) + "\n"

    ( 0 until grid.ranks ).map {
      rank => ( 0 until grid.files ).map {
        file => distribution_map.get( (rank,file) ) match {
          case None => " "
          case Some( King ) => "K"
          case Some( Queen ) => "Q"
          case Some( Rook ) => "R"
          case Some( Bishop ) => "B"
          case Some( Knight ) => "N"
          case Some( unknown_piece ) => throw new Error( s"ERRoR::Output::printDistribution::grid(${grid.ranks},${grid.files})::Unknown piece => $unknown_piece" )
        }
      }.mkString( "|", "|", "|\n" )
    }.mkString( str_line, str_line, str_line )
  }

  lazy val foldResults = ( n: Int, d: Distribution, g: Grid ) => {
    printDistribution( g, d )
    n+1
  }

}
