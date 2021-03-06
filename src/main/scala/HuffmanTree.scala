
object HuffmanTree {
     def buildHuffmanTree(pNodes: List[Node]): Node = {

       var nodes = List() ++ pNodes
       var root: Node = null
       var builtIndex = 0
       while(nodes.size > 1) {
         val mins: List[Node] = nodes.sortBy(_.weight).take(2) //Select min 2 nodes as new node
         mins.foreach(_.increase())

         val builtNode = new Node(builtIndex)
         builtIndex += 1

         //Create a parent node to the 2 min nodes
         builtNode :+ mins

         //Replace 2 min nodes with new parent nodes
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

  def :+(lr: List[Node]) {
    left = lr.head
    right = lr.last
    left.parent = this
    right.parent = this

    weight = left.weight + right.weight
  }

  def this(index: Int) {
    this()
    name = "builtIn" + index
    isBuilt = true
  }


  def this(name: String, weight: Int) {
    this()
    this.name = name
    this.weight = weight
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
object Node {
  def apply(name: String, weight: Int) = new Node(name, weight)
}