
object HuffmanTree {
     def buildHuffmanTree(pNodes: List[Node]): Node = {

       var nodes = List() ++ pNodes
       var root: Node = null
       var builtIndex = 0
       while(nodes.size > 1) {
         val mins: List[Node] = nodes.sortBy(_.weight).takeRight(2)
         mins.foreach(_.increase())

         val builtNode = new Node(builtIndex)
         builtIndex += 1

         builtNode.left = mins.head
         builtNode.right = mins.last
         builtNode.left.parent = builtNode
         builtNode.right.parent = builtNode

         builtNode.weight = builtNode.left.weight + builtNode.right.weight

         nodes = nodes.diff(mins)
         nodes = nodes :+ builtNode

         root = builtNode
       }
       root
     }

}

class Node {
  var name: String = null
  var weight: Int = 0
  var parent: Node = null
  var left: Node = null
  var right: Node = null
  var layers: Int = 0
  var isBuilt: Boolean = false

  def this(index: Int) {
    this()
    name = "builtIn" + index
    isBuilt = true
  }

  def sum: Int = {
    val myWeight = if (isBuilt) 0 else weight * layers
    val leftW  = if(left == null) 0 else left.sum
    val rightW = if(right == null) 0 else right.sum

    myWeight + leftW + rightW
  }

  def increase() {
    layers += 1

    if (left != null)
      left.increase()

    if (right != null)
      right.increase()
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[Node]

  override def equals(other: Any): Boolean = other match {
    case that: Node =>
      (that canEqual this) &&
        name == that.name
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(name)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}