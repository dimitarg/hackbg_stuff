/**
 * Created by fmap on 12.06.15.
 */
object NameMatching extends App {

  //utility function to convert a list of string names to a Int,Int tuple yielding the number of males and number of females in the list
  def fromStrings(strings: List[String]) : (Int,Int) = {
    def isMale (str: String) : Boolean = {
      if(!str.endsWith("ss") || str.endsWith("tta")) {
        throw new IllegalArgumentException()
      }
      str.endsWith("ss")
    }
    val maleFemale = strings.partition(isMale)
    (maleFemale._1.length,maleFemale._2.length)
  }

  def solve(knownMales: Int, knownFemales: Int, names: List[String]) : Double = solve(knownMales, knownFemales, fromStrings(names))

  def solve(knownMales: Int, knownFemales: Int, count : (Int,Int)) : Double = solve(knownMales,count._1) * solve(knownFemales,count._2)

  def solve(known: Int, total: Int) : Double = {
    if ( (total-known) <=1) 1
    else if(known==0) (1.toDouble/total) * solve(0,total-1)
    else solve(known-1, total-1)
  }


  println(solve(1,2,(3,5)))

  val foo = Some("ok")

  val f = foo.fold(throw new IllegalStateException()) (s=>43)
  println(f)

}
