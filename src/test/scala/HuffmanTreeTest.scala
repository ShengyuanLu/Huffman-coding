import com.google.common.collect.Sets
import org.junit.Assert._
import org.junit.Test

class HuffmanTreeTest {
  @Test
  def testAB {
    val nodes = List(Node("A", 2), Node("B", 3))
    val tree: Node = HuffmanTree.buildHuffmanTree(nodes)
    assertEquals(5, tree.sum)
  }

  @Test
  def testABC {
    val nodes = List(Node("A", 2), Node("B", 3), Node("C", 4))
    val tree: Node = HuffmanTree.buildHuffmanTree(nodes)
    assertEquals(14, tree.sum)
  }

  @Test
  def testComplex {
    val nodes = List(Node("A", 5), Node("B", 2), Node("C", 4), Node("D", 7), new Node("E", 9))
    val tree: Node = HuffmanTree.buildHuffmanTree(nodes)
    assertEquals(60, tree.sum)
  }

}
