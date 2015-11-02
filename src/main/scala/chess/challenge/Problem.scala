
package chess.challenge

object Main extends App {

  /**
    * Comment or uncomment the following lines for see some samples
    * or the problem solution
    */
  problem
  //example1
  //example2

  def problem: Unit = {
    println( "Instructions:" )
    println( "" )
    println( "If you want to know the time of this running, please 'compile' the source first and then use 'run' from the sbt. This way you avoid mixing the compilation time with the running time" )
    println( "" )
    println( "If you want to run some examples, please execute the 'example1' or 'example2' functions of the Main object instead of this function: 'problem'" )
    println( "" )
    println( "As the number of results produces a very verbose output, we use the lambda function \"blind_fold_results\" that does not output anything. This is because the output process itself consumes a lot of time that does not fall purely into the logic. But if you want to see all the output uncomment/comment the 'val n = Grid( 7, 7 )...' lines" )
    println( "" )

    val pieces = "QKBNQKB"
    val blind_fold_results = ( n: Int, d: Distribution, g: Grid ) => n+1
    val n = Grid( 7, 7 )( pieces, 0, blind_fold_results )
    // val n = Grid( 7, 7 )( pieces, 0, Output.foldResults )
    println( s"The total number of valid distributions is => $n" )
    println( "To see the time, please see the seconds below. Thank you" )
  }

  def example1: Unit = Grid( 3, 3 )( "KRK", 0, Output.foldResults )
  def example2: Unit = Grid( 4, 4 )( "NRNNRN", 0, Output.foldResults )


}
