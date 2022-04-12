package example

import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class KdTreeSpec extends AnyFlatSpec {

  "KdTree" should "not allow construction without points" in {
    assertThrows [IllegalArgumentException] { KdTree[Vector[Int], Int](1, Seq.empty) }
  }

  it should "not allow construction without nodes" in {
    assertThrows [IllegalArgumentException] { KdTree[Vector[Int], Int](0, Seq(Vector(0,0))) }
  }

  it should "not allow construction with an even number of nodes" in {
    assertThrows [IllegalArgumentException] { KdTree[Vector[Int], Int](2, Seq.fill(100)(Vector(0,0))) }
    assertThrows [IllegalArgumentException] { KdTree[Vector[Int], Int](4, Seq.fill(100)(Vector(0,0))) }
    assertThrows [IllegalArgumentException] { KdTree[Vector[Int], Int](6, Seq.fill(100)(Vector(0,0))) }
    assertThrows [IllegalArgumentException] { KdTree[Vector[Int], Int](8, Seq.fill(100)(Vector(0,0))) }
    assertThrows [IllegalArgumentException] { KdTree[Vector[Int], Int](10, Seq.fill(100)(Vector(0,0))) }
    assertThrows [IllegalArgumentException] { KdTree[Vector[Int], Int](12, Seq.fill(100)(Vector(0,0))) }
  }

  it should "not allow construction with zero-dimensional points" in {
    assertThrows [IllegalArgumentException] { KdTree[Vector[Int], Int](3, Seq(Vector(), Vector())) }
  }

  it should "not allow construction if points don't have the same number of dimensions" in {
    assertThrows [IllegalArgumentException] { KdTree[Vector[Int], Int](3, Seq(Vector(0), Vector(0, 0), Vector(0, 0, 0))) }
    assertThrows [IllegalArgumentException] { KdTree[Vector[Int], Int](3, Seq(Vector(0), Vector(0), Vector(0, 0))) }
    assertThrows [IllegalArgumentException] { KdTree[Vector[Int], Int](3, Seq(Vector(0, 0), Vector(0), Vector(0, 0))) }
    assertThrows [IllegalArgumentException] { KdTree[Vector[Int], Int](3, Seq(Vector(0, 0), Vector(0), Vector(0))) }
  }

  it should "answer the closest point query only with one of the points stored inside it" in {
    val points = Seq(Vector(-1, -1), Vector(0, 0), Vector(1, 1))
    val tree = KdTree[Vector[Int], Int](1, points)

    assert(points contains tree.findClosest(Vector(0, 0)))
    assert(points contains tree.findClosest(Vector(1, -1)))
    assert(points contains tree.findClosest(Vector(-1, 1)))
    assert(points contains tree.findClosest(Vector(1, 2)))
    assert(points contains tree.findClosest(Vector(-2, -1)))
    assert(points contains tree.findClosest(Vector(0, 5)))
    assert(points contains tree.findClosest(Vector(0, -5)))
  }

  it should "answer the closest point query with the same input point when it's the same as a point stored in the tree" in {
    val tree = KdTree[Vector[Int], Int](3, Seq(Vector(-1, -1), Vector(-1, 1), Vector(1, -1), Vector(1, 1)))

    assertResult(Vector(-1, -1)) { tree.findClosest(Vector(-1, -1)) }
    assertResult(Vector(1, -1)) { tree.findClosest(Vector(1, -1)) }
    assertResult(Vector(-1, 1)) { tree.findClosest(Vector(-1, 1)) }
    assertResult(Vector(1, 1)) { tree.findClosest(Vector(1, 1)) }
  }

  it should "answer the closest point query with a point at a minimal distance from the input point" in {
    def dist(vec1: Vector[Int], vec2: Vector[Int]): Int = {
      assert(vec1.length == vec2.length)
      vec1.zip(vec2).map { case (c1, c2) => (c2 - c1) * (c2 - c1) }.sum
    }

    val treePoints = Seq.fill(200) { Vector(Random.nextInt(2000) - 1000, Random.nextInt(2000) - 1000, Random.nextInt(2000) - 1000) }
    val queryPoints = Seq.fill(2000) { Vector(Random.nextInt(2000) - 1000, Random.nextInt(2000) - 1000, Random.nextInt(2000) - 1000) }

    for (
      numNodes <- 1 until 100 by 2;
      tree = KdTree[Vector[Int], Int](numNodes, treePoints);
      inPoint <- queryPoints
    ) {
      val minDist = treePoints.map(dist(_, inPoint)).min
      assertResult(minDist) { dist(tree.findClosest(inPoint), inPoint) }
    }
  }
}
