package example

import org.json4s.DefaultFormats
import org.json4s.JsonAST.{JArray, JInt, JObject}
import org.json4s.native.JsonMethods

import scala.collection.immutable.Queue
import scala.math.Ordering.Implicits.infixOrderingOps
import scala.math.Numeric.Implicits.infixNumericOps

object StringNumericConversion {
  implicit object IntConversion extends StringNumericConversion[Int] {
    override def conv(s: String): Int = augmentString(s).toInt
  }

  implicit object FloatConversion extends StringNumericConversion[Float] {
    override def conv(s: String): Float = augmentString(s).toFloat
  }

  implicit object DoubleConversion extends StringNumericConversion[Double] {
    override def conv(s: String): Double = augmentString(s).toDouble
  }
}

trait KdTreeT[+A <: IndexedSeq[C], +C] {
  def numDimensions: Int
  def count: Int
  def isLeaf: Boolean
  def findClosest[B >: A <: IndexedSeq[D], D >: C : Numeric](p: B) : B
  def stringSpec: (String, String)
}

object KdTreeT {
  def parseStringSpec[C: Ordering](spec: (String, String))(implicit conv: StringNumericConversion[C]): (Array[(Boolean, C, Int, Int)], Seq[IndexedSeq[C]]) = {
    val (tree_rom_str, p_rom_str) = spec
    val points = Seq[IndexedSeq[C]](p_rom_str.split("\n").map{ line => IndexedSeq[C](line.split(", ").map{ conv.conv }:_*) }:_*)
    val treeSpec = tree_rom_str
      .split("\n")
      .map(s => List(s.split(", "):_*))
      .map { t: List[String] =>
        require(t.length == 4, "Invalid tree_rom spec - each line needs to have exactly 4 values")
        val isLeafStr :: medianStr :: pAddrStr :: pLenStr :: Nil  = t
        (if (isLeafStr.toInt == 0) false else true, conv.conv(medianStr), pAddrStr.toInt, pLenStr.toInt)
      }
    (treeSpec, points)
  }

  def parseJson[C: Ordering](json: String)(implicit conv: StringNumericConversion[C]): (Int, Int, Int, Int, IndexedSeq[(Boolean, C, Int, Int)], IndexedSeq[IndexedSeq[C]]) = {
    JsonMethods.parse(json) match { case JObject(l) =>
      implicit lazy val formats: DefaultFormats.type = DefaultFormats
      val m = l.toMap
      (
        m("D").extract[Int],
        m("C").extract[Int],
        m("N").extract[Int],
        m("P_A").extract[Int],
        m("tree_rom") match { case JArray(l) => IndexedSeq(l.map { case JArray(List(a, b, c ,d)) => (if (a.extract[Int] == 0) false else true, conv.conv(b.extract[String]), c.extract[Int], d.extract[Int]) }:_*) },
        m("p_rom") match { case JArray(l) => IndexedSeq(l.map { case JArray(el) => IndexedSeq(el.map{ e => conv.conv(e.extract[String]) }:_*) }:_*) }
      )
    }
  }

  def distSquared[A <: IndexedSeq[C], C : Numeric](a: A, b: A): C = {
    assert(a.length == b.length)
    (a zip b).map { case (ea, eb) => (eb - ea) * (eb - ea) }.sum
  }
}

trait KdTree[+A <: IndexedSeq[C], +C] extends KdTreeT[A, C] {
  override def findClosest[B >: A <: IndexedSeq[D], D >: C : Numeric](p: B): B = findClosestWithDist[B, D](p)._1
  override def stringSpec: (String, String) = (treeRomSpecInternal(0, Queue.empty), pRomSpecInternal)
  def json[D >: C : Numeric](bitWidth: Int, pRomAddressWidth: Int): String =
    s"""{
      |  "D": $numDimensions,
      |  "C": $bitWidth,
      |  "N": ${math.ceil(math.log(count+1)/math.log(2)).toInt},
      |  "P_A": $pRomAddressWidth,
      |  "tree_rom": ${treeRomEntries[A, D](0, Queue.empty).map { case (leaf, median, p_addr, p_len) =>
      s"""    [
      |      $leaf,
      |      $median,
      |      $p_addr,
      |      $p_len
      |    ]""".stripMargin
      }.mkString("[\n", ",\n", "\n  ]")},
      |  "p_rom": ${
      pRomEntries.map( _.mkString("    [\n      ", ",\n      ", "\n    ]") )
        .mkString("[\n", ",\n", "\n  ]")
    }
      |}""".stripMargin
  protected def treeRomEntries[B >: A <: IndexedSeq[D], D >: C](startPos: Int, queue: Queue[(KdTree[B, D], Int)])(implicit numeric: Numeric[D]): Seq[(Int, D, Int, Int)]
  protected def pRomEntries: Seq[Seq[C]]

  protected def findClosestWithDist[B >: A <: IndexedSeq[D], D >: C : Numeric](p: B) : (B, D)
  protected def treeRomSpecInternal[B >: A <: IndexedSeq[D], D >: C](startPos: Int, queue: Queue[(KdTree[B, D], Int)]): String
  protected def pRomSpecInternal: String

}

object KdTree {
  def apply[A <: IndexedSeq[C], C : Ordering](numNodes: Int, points: Seq[A], bruteforceNodes: Boolean = false): KdTree[A, C] = {
    require(numNodes > 0)
    require(numNodes % 2 == 1)
    require(points.nonEmpty)
    val nDim = points.head.length
    require(nDim > 0)
    require(points.forall { _.length == nDim })

    def internal(numNodes: Int, points: Seq[A], depth: Int): KdTree[A, C] = {
      assert(numNodes % 2 == 1)

      if (numNodes == 1) new Leaf[A, C](points)
      else {
        val medianIdx = points.length / 2 - 1
        val pointElemIdx = depth % nDim
//        def medianSplit: (C, Seq[A], Seq[A]) = {
//          val sorted = points.sortWith { (p1, p2) => p1(pointElemIdx) < p2(pointElemIdx)}
//          val median = sorted(medianIdx)(pointElemIdx)
//          val (leftPoints, rightPoints) = sorted.partition { _(pointElemIdx) < median }
//          require(leftPoints.nonEmpty && rightPoints.nonEmpty, "Invalid points - tree will not be balanced")
//          // (sorted.take(medianIdx + 1), sorted.drop(medianIdx + 1))
//          (median, leftPoints, rightPoints)
//        }
        def medianSplit: (C, Seq[A], Seq[A]) = {
          val sorted = points.sortWith { (p1, p2) => p1(pointElemIdx) < p2(pointElemIdx)}
          val median = sorted(medianIdx)(pointElemIdx)
          val (leftPoints, rightPoints) = (sorted.take(medianIdx + 1), sorted.drop(medianIdx + 1))
          require(leftPoints.nonEmpty && rightPoints.nonEmpty, "Invalid points - tree will not be balanced")
          (median, leftPoints, rightPoints)
        }

        val balancedCount = (1 << math.floor(math.log(numNodes + 1) / math.log(2)).toInt) - 1
        assert(balancedCount <= numNodes)
        val addLeft = math.min(numNodes - balancedCount, (balancedCount + 1) / 2)
        val addRight = math.max(numNodes - balancedCount - (balancedCount + 1) / 2, 0)
        assert(addLeft <= (balancedCount + 1) / 2)
        assert(addRight <= (balancedCount + 1) / 2)
        assert(addLeft + addRight == numNodes - balancedCount)

        val (median, leftPoints, rightPoints) = medianSplit
        if (bruteforceNodes) new BruteforceNonLeaf(
          median,
          depth,
          internal((balancedCount - 1) / 2 + addLeft, leftPoints, depth + 1),
          internal((balancedCount - 1) / 2 + addRight, rightPoints, depth + 1)
        ) else new NonLeaf(
          median,
          depth,
          internal((balancedCount - 1) / 2 + addLeft, leftPoints, depth + 1),
          internal((balancedCount - 1) / 2 + addRight, rightPoints, depth + 1)
        )
      }
    }

    internal(numNodes, points, 0)
  }

  def fromSpec[C : Ordering](treeRom: IndexedSeq[(Boolean, C, Int, Int)], pRom: IndexedSeq[IndexedSeq[C]]): KdTree[IndexedSeq[C], C] = {
    def internal(treeRomIndex: Int, depth: Int): KdTree[IndexedSeq[C], C] = {
      val (isLeaf, median, pAddr, pLen) = treeRom(treeRomIndex)
      if (isLeaf)
        new Leaf[IndexedSeq[C], C](pRom.slice(pAddr, pAddr + pLen))
      else
        new NonLeaf[IndexedSeq[C], C](median, depth, internal(2 * treeRomIndex + 1, depth + 1), internal(2 * treeRomIndex + 2, depth + 1))
    }
    internal(0, 0)
  }

  def fromJson[C : Ordering](json: String)(implicit conv: StringNumericConversion[C]): KdTree[IndexedSeq[C], C] = {
    val (_, _, _, _, treeRom, pRom) = KdTreeT.parseJson[C](json)
    fromSpec(treeRom, pRom)
  }

  protected class NonLeaf[A <: IndexedSeq[C], C: Ordering](median: C, depth: Int, val left: KdTree[A, C], val right: KdTree[A, C]) extends KdTree[A, C] {
    override def numDimensions: Int = {
      val nDim = left.numDimensions
      assert(nDim == right.numDimensions)
      nDim
    }
    override def count: Int = left.count + right.count
    override def isLeaf : Boolean = false
    override def findClosestWithDist[B >: A <: IndexedSeq[D], D >: C : Numeric](p: B): (B, D) = {
      val medianP = p(depth % p.length)
//      print(s"isLeaf = $isLeaf; median = $median; depth = $depth; medianP = $medianP... ")
      val (firstChild, secondChild) = if (medianP < median) (left, right) else (right, left)
//      print("Enqueue first child\n")
      val (closestFirst, closestFirstDist) = firstChild.findClosestWithDist[B, D](p)
//      print(s"isLeaf = $isLeaf; median = $median; depth = $depth; medianP = $medianP... Try enqueue second child... ")
      if ((medianP - median) * (medianP - median) < closestFirstDist) {
//        print("Push second child\n")
        val (closestSecond, closestSecondDist) = secondChild.findClosestWithDist[B, D](p)
//        print(s"isLeaf = $isLeaf; median = $median; depth = $depth; medianP = $medianP... ")
        if (closestSecondDist < closestFirstDist) {
//          print(s"Update closest point: $closestSecond\n")
          (closestSecond, closestSecondDist)
        }
        else {
//          print(s"Update closest point: $closestFirst\n")
          (closestFirst, closestFirstDist)
        }
      } else {
//        print("Don't push second child... ")
//        print(s"Update closest point: $closestFirst\n")
        (closestFirst, closestFirstDist)
      }

    }

    override def treeRomSpecInternal[B >: A <: IndexedSeq[D], D >: C](startPos: Int, queue: Queue[(KdTree[B, D], Int)]): String = {
      val newQueue: Queue[(KdTree[B, D], Int)] = queue.enqueue((left, startPos)).enqueue((right, startPos + left.count))
      val (next, nextStartPos) = newQueue.head
      s"0, $median, 0, 0\n" + next.treeRomSpecInternal[B, D](nextStartPos, newQueue.tail)
    }

    override def pRomSpecInternal: String = left.pRomSpecInternal + right.pRomSpecInternal

    override def toString: String = s"${getClass.getSimpleName}($median, $left, $right)"

    override protected def treeRomEntries[B >: A <: IndexedSeq[D], D >: C](startPos: Int, queue: Queue[(KdTree[B, D], Int)])(implicit numeric: Numeric[D]): Seq[(Int, D, Int, Int)] = {
      val newQueue: Queue[(KdTree[B, D], Int)] = queue.enqueue((left, startPos)).enqueue((right, startPos + left.count))
      val (next, nextStartPos) = newQueue.head
      (0, median, 0, 0) +: next.treeRomEntries[B, D](nextStartPos, newQueue.tail)
    }

    override protected def pRomEntries: Seq[Seq[C]] = left.pRomEntries ++ right.pRomEntries
  }

  private class BruteforceNonLeaf[A <: IndexedSeq[C], C: Ordering](median: C, depth: Int, override val left: KdTree[A, C], override val right: KdTree[A, C]) extends NonLeaf(median, depth, left, right) {
    override def findClosestWithDist[B >: A <: IndexedSeq[D], D >: C : Numeric](p: B): (B, D) = {
      val (closestLeft, closestLeftDist) = left.findClosestWithDist[B, D](p)
      val (closestRight, closestRightDist) = right.findClosestWithDist[B, D](p)
      if (closestLeftDist <= closestRightDist) (closestLeft, closestLeftDist) else (closestRight, closestRightDist)
    }
  }

  private class Leaf[A <: IndexedSeq[C], C: Ordering](points: Seq[A]) extends KdTree[A, C] {
    assert(points.nonEmpty)
    assert(points.forall(_.length == points.head.length), "All points must have the same dimensionality")
    override def numDimensions: Int = points.head.length
    override def count: Int = points.length
    override def isLeaf: Boolean = true
    override def findClosestWithDist[B >: A <: IndexedSeq[D], D >: C : Numeric](p: B): (B, D) = {
//      print(s"isLeaf = $isLeaf... Checking points...\n")
      val points : Seq[B] = this.points
      points.tail.foldLeft { (points.head, KdTreeT.distSquared[B, D](points.head, p)) } { case ((cPoint, cPointDist), point) =>
        val pointDist = KdTreeT.distSquared[B, D](point, p)
        if (pointDist < cPointDist) (point, pointDist) else (cPoint, cPointDist)
      }
    }

    override def treeRomSpecInternal[B >: A <: IndexedSeq[D], D >: C](startPos: Int, queue: Queue[(KdTree[B, D], Int)]): String = {
      val spec = s"1, 0, $startPos, $count\n"
      if (queue.isEmpty) spec
      else spec + queue.head._1.treeRomSpecInternal(queue.head._2, queue.tail)
    }

    override def pRomSpecInternal: String = points.map { p => p.mkString("", ", ", "\n") }.mkString

    override def toString: String = points.mkString(s"${getClass.getSimpleName}(", ", ", ")")

    override protected def treeRomEntries[B >: A <: IndexedSeq[D], D >: C](startPos: Int, queue: Queue[(KdTree[B, D], Int)])(implicit numeric: Numeric[D]): Seq[(Int, D, Int, Int)] = {
      (1, numeric.zero, startPos, count) +: (if (queue.isEmpty) Seq.empty else queue.head._1.treeRomEntries(queue.head._2, queue.tail))
    }

    override protected def pRomEntries: Seq[Seq[C]] = points
  }
}

trait KdTreeComplete[+A <: IndexedSeq[C], +C] extends KdTreeT[A, C] {
  def numDimensions: Int = rootPoint.length
  def count : Int
  def incl[B >: A <: IndexedSeq[D], D >: C : Ordering](p: B): KdTreeComplete[B, D]
  def rootPoint: A
  def left: KdTreeComplete[A, C]
  def right: KdTreeComplete[A, C]
  def isEmpty: Boolean
  def distSquared[B >: A <: IndexedSeq[D], D >: C : Numeric](p: B): D = KdTreeT.distSquared[B, D](rootPoint, p)
}

trait StringNumericConversion[T] {
  def conv(s: String): T
}

object KdTreeComplete {
  def apply[A <: IndexedSeq[C], C: Ordering](points: Seq[A], bruteforceNodes: Boolean = false): KdTreeComplete[A, C] = {
    val nDim = points.head.length

    def construct(points: Seq[A], depth: Int): KdTreeComplete[A,C] = {
      if (points.isEmpty) EmptyComplete
      else if (points.length == 1) new NodeComplete[A, C](points.head)
      else {
        val ind = depth % nDim
        val sorted = points.sortWith((l: A, r: A) => l(ind) < r(ind))
        val pivotInd = sorted.length / 2
        if(bruteforceNodes)
          BruteforceNodeComplete(
            sorted(pivotInd),
            construct(sorted.take(pivotInd), depth + 1),
            construct(sorted.drop(pivotInd).tail, depth + 1)
          )
        else
          new NodeComplete(
            sorted(pivotInd),
            construct(sorted.take(pivotInd), depth + 1),
            construct(sorted.drop(pivotInd).tail, depth + 1)
          )
      }
    }

    construct(points, 0)
  }

  def fromStringSpec[C: Ordering](spec: (String, String), bruteforceNodes: Boolean = false)(implicit conv: StringNumericConversion[C]): KdTreeComplete[IndexedSeq[C], C] = {
    val (treeSpec, points) = KdTreeT.parseStringSpec(spec)
    def fromTreeSpec(treeSpecIndex: Int, depth: Int) : KdTreeComplete[IndexedSeq[C], C] = {
      if (treeSpecIndex >= treeSpec.length) EmptyComplete
      else {
        val (_, _, pointIndex, _) = treeSpec(treeSpecIndex)
        require(pointIndex < points.length, "Invalid tree_rom spec - address of point is out of range")
        if(bruteforceNodes) BruteforceNodeComplete(points(pointIndex), fromTreeSpec(2 * treeSpecIndex + 1, depth + 1), fromTreeSpec(2 * treeSpecIndex + 2, depth + 1))
        else new NodeComplete(points(pointIndex), fromTreeSpec(2 * treeSpecIndex + 1, depth + 1), fromTreeSpec(2 * treeSpecIndex + 2, depth + 1))
      }
    }
    fromTreeSpec(0, 0)
  }
}

case object EmptyComplete extends KdTreeComplete[Nothing, Nothing] {
  override def count: Int = 0
  override def findClosest[B <: IndexedSeq[D], D : Numeric](p: B): B = throw new java.util.NoSuchElementException("Can't find closest point in an empty tree")
  override def incl[B <: IndexedSeq[D], D : Ordering](p: B): KdTreeComplete[B, D] = new NodeComplete[B, D](p)
  override def rootPoint = throw new java.util.NoSuchElementException("Empty tree has no root point")
  override def left = throw new java.util.NoSuchElementException("Empty tree has no children")
  override def right = throw new java.util.NoSuchElementException("Empty tree has no children")
  override def isEmpty: Boolean = true
  override def isLeaf: Boolean = false
  override def stringSpec: (String, String) = ("", "")
}

class NodeComplete[+A <: IndexedSeq[C], +C : Ordering](point: A, l: KdTreeComplete[A, C] = EmptyComplete, r: KdTreeComplete[A, C] = EmptyComplete)  extends KdTreeComplete[A, C] {
  override def count: Int = left.count + right.count + 1
  override def findClosest[B >: A <: IndexedSeq[D], D >: C](p: B)(implicit numeric: Numeric[D]): B = {
    def kdSearch(n: KdTreeComplete[B, D], depth: Int, closestPoint: B, closestDist: D, closestIsCurrent: Boolean): (B, D) = {
      if (n.isEmpty) (closestPoint, closestDist)
      else if (n.rootPoint == p) (rootPoint, numeric.zero)
      else {
        val (medianRootPoint, medianP) = (n.rootPoint(depth % n.rootPoint.length), p(depth % n.rootPoint.length))
        val (firstChild, secondChild) = if (medianP < medianRootPoint) (n.left, n.right) else (n.right, n.left)
        val (currentPoint, currentDist) = (n.rootPoint, n.distSquared(p))
        val (currentClosestPoint, currentClosestDist) = if(!closestIsCurrent && currentDist < closestDist) (currentPoint, currentDist) else (closestPoint, closestDist)
        val (newClosestPoint, newClosestDist) = kdSearch(firstChild, depth + 1, currentClosestPoint, currentClosestDist, closestIsCurrent = false)
        if ((medianP - medianRootPoint) * (medianP - medianRootPoint) < newClosestDist)
          kdSearch(secondChild, depth + 1, newClosestPoint, newClosestDist, closestIsCurrent = false)
        else
          (newClosestPoint, newClosestDist)
      }
    }
    val (closestPoint, _) = kdSearch(this, 0, rootPoint, distSquared[B, D](p), closestIsCurrent = true)
    closestPoint
  }

  override def incl[B >: A <: IndexedSeq[D], D >: C : Ordering](p: B): KdTreeComplete[B, D] = {
    require(p.length == point.length, "New point needs to have the same dimensionality as existing points")

    def inclDepth(n: KdTreeComplete[B, D], p: B, depth: Int): KdTreeComplete[B, D] = {
      if (n.isEmpty) n.incl[B, D](p)
      else {
        val ind = depth % point.length
        if (p(ind) < n.rootPoint(ind)) {
          new NodeComplete[B, D](point, inclDepth(n.left, p, depth + 1), right)
        } else {
          new NodeComplete[B, D](point, left, inclDepth(n.right, p, depth + 1))
        }
      }
    }

    inclDepth(this, p, 0)
  }

  override def rootPoint: A = point
  override def left: KdTreeComplete[A, C] = l
  override def right: KdTreeComplete[A, C] = r

  override def isEmpty: Boolean = false
  override def isLeaf: Boolean = left.isEmpty && right.isEmpty

  override def stringSpec: (String, String) = {
    def bfsInternal(q: List[(KdTreeComplete[A, C], Int)]): List[(KdTreeComplete[A, C], Int)] = {
      if (q.isEmpty) List()
      else {
        val (tree, depth) = q.head
        val q1 = q.tail ::: List(tree.left, tree.right).filterNot(_.isEmpty).map(el => (el, depth + 1))
        (tree, depth) :: bfsInternal(q1)
      }
    }
    val (first, second, _) = bfsInternal((this, 0) :: Nil).foldLeft(z = ("", "", 0)){ (z, a: (KdTreeComplete[A, C], Int)) =>
      val (sNode, sPoint, ind) = z
      val (tree, depth) = a
      val p = tree.rootPoint
      val nodeInfo = Seq(if(tree.isLeaf) 1 else 0, p(depth % p.length), ind, tree.count).mkString("", ", ", "\n")
      val pointInfo = p.mkString("", ", ", "\n")
      (sNode + nodeInfo, sPoint + pointInfo, ind + 1)
    }
    (first, second)
  }

  override def toString: String = s"${getClass.getSimpleName}($rootPoint, $left, $right)"
}

case class BruteforceNodeComplete[+A <: IndexedSeq[C], +C : Ordering](point: A, l: KdTreeComplete[A,C], r: KdTreeComplete[A,C]) extends NodeComplete[A, C](point, l, r) {
  override def findClosest[B >: A <: IndexedSeq[D], D >: C : Numeric](p: B): B = {
    def closestNode(n: KdTreeComplete[B, D]): (KdTreeComplete[B, D], D) =
      Seq(n.left, n.right).filterNot(_.isEmpty)
      .foldLeft(z = (n, n.distSquared(p))) { (z: (KdTreeComplete[B, D], D), n: KdTreeComplete[B, D]) =>
        val (_, zd) = z
        val (nc, ncd) = closestNode(n)
        if (ncd < zd) (nc, ncd) else z
      }

    val (node, _) = closestNode(this)
    node.rootPoint
  }
}
