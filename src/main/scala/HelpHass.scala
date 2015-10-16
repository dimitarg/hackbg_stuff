/**
 * Created by fmap on 12.06.15.
 */
object HelpHass extends App {

  //we represent a graph node simply by the character which is its label
  type Node = Char

  //we represent a graph as a mapping from a node to the set of its neighbours. I.e. a partial function
  type Graph =  Map[Node,Set[Node]]

  //our output is a Path, which is an ordered collection of Nodes
  type Path = List[Node]

  //utility to build a Graph out of flat input (for example one which may come from console,file,etc)
  def buildGraph(flatInput :List[(Node,Node)]) : Graph = {

    def build(input: List[(Node,Node)], soFar: Graph) : Graph = input match {
      case List() => soFar
      case x::xs => {
        val aNeighbours = if (soFar.contains(x._1)) soFar(x._1)+x._2 else Set(x._2)
        val bNeighbours = if (soFar.contains(x._2)) soFar(x._2)+x._1 else Set(x._1)
        val updated = soFar + (x._1->aNeighbours) + (x._2->bNeighbours)
        build(xs, updated)
      }
    }

    build(flatInput, Map())
  }

  //the algorithm is from slides 6,7 of https://courses.engr.illinois.edu/cs473/sp2011/lectures/10_handout.pdf
  def shortest(graph: Graph, from: Node, to: Node) : Option[Path] = optimal(graph,from,to,graph.keySet.size-1)

  def optimal(graph: Graph, source: Node, dest: Node, limit: Int) : Option[Path] = {
    if(limit<1) {
      None
    } else if(graph(source).contains(dest)) {
      Some(List(source,dest))
    } else {
      if(limit==1) {
        None
      } else {
        val fromNeighbors = graph(source).map(neighbour => optimal(graph,neighbour,dest,limit-1).map(path => source::path))
        val sorted = sort(fromNeighbors.toList)

        if(sorted.isEmpty) None else  sorted.head
      }
    }

  }

  //a sorting function for options of path. If a path is None, it is considered longer than any Some(x) path, i.e. it is infinity
  def sort(xs: List[Option[Path]]): List[Option[Path]] = {
    xs.sortWith((a,b) => (a,b) match {
      case (None,None) => false
      case (Some(x), None) => true
      case (None, Some(x)) => false
      case (Some(path1)  , Some(path2)) => path1.size<path2.size
    })
  }

  //some test scenarios

  val g1 = buildGraph(('H','F')::('F','L')::('H','L')::Nil)
  shortest(g1,'H','L') foreach println

  val g2 = buildGraph(('H','A')::('K','L')::('S','K')::('A','S')::Nil)
  shortest(g2, 'H','L') foreach println

  val g3 = buildGraph(('A','B')::('B','C')::('B','D')::('C','K')::('D','E')::('E','K')::Nil)
  shortest(g3, 'A', 'K') foreach println



}
