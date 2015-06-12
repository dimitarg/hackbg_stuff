/**
 * Created by fmap on 12.06.15.
 */
object Hass extends App {

  //we represent a graph node as the character which is its label
  type Node = Char

  //we represent a graph as a mapping from a node to the set of its neighbours. I.e. a partial function
  type Graph =  Map[Node,Set[Node]]


  def buildGraph(rawInput :List[(Node,Node)]) : Graph = {

    def build(input: List[(Node,Node)], soFar: Graph) :Graph = {
      input match {
        case List() => soFar
        case x::xs => {
          val aNeighbours = if (soFar.contains(x._1)) soFar(x._1)+x._2 else Set(x._2)
          val bNeighbours = if (soFar.contains(x._2)) soFar(x._2)+x._1 else Set(x._1)
          val updated = soFar + (x._1->aNeighbours) + (x._2->bNeighbours)
          build(xs, updated)
        }
      }
    }

    build(rawInput, Map())
  }

  def shortest(graph: Graph, from: Node, to: Node) : Option[List[Node]] = {
    optimal(graph,from,to,graph.keySet.size-1)
  }

  def optimal(graph: Graph, source: Node, dest: Node, limit: Int) : Option[List[Node]] = {
    if(limit<1) {
      None
    }

    if(graph(source).contains(dest)) {
      Some(List(source,dest))
    } else {
      if(limit==1) {
        None
      } else {
        val fromNeighbors = graph(source).map(neighbour => optimal(graph,neighbour,dest,limit-1).map(path => source::path))
        val sorted = fromNeighbors.toList.sortWith((o1,o2)=> (o1,o2) match {
          case (None,None) => false
          case (Some(path), None) => true
          case (None, Some(path)) => false
          case (Some(path1)  , Some(path2)) => path1.size<path2.size
        })

        if(sorted.isEmpty) None else  sorted.head
      }
    }

  }

  val g1 = buildGraph(('H','F')::('F','L')::('H','L')::Nil)
  shortest(g1,'H','L') foreach println

  val g2 = buildGraph(('H','A')::('K','L')::('S','K')::('A','S')::Nil)
  shortest(g2, 'H','L') foreach println

  val g3 = buildGraph(('A','B')::('B','C')::('B','D')::('C','K')::('D','E')::('E','K')::Nil)
  shortest(g3, 'A', 'K') foreach println



}
