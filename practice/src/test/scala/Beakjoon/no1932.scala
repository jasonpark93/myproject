package Beakjoon

import org.scalatest.FunSuite

class no1932 extends FunSuite {
  test("no1932 recusive"){
    val tri ="""7
3 8
8 1 0
2 7 4 4
4 5 2 6 5""".trim.split("\n").toList.flatMap{
      x => x.split(' ').toList
    }
    println(tri)

    def solve = {

      def layerList(num:Int) = {
        val start = (num) * (num-1)/2
        tri.drop(start).take(num).map{
          x => x.toInt
        }
      }

      val maxList = List.newBuilder[Int]
      def recusive(total:Int = 0 ,layer:Int = 1, position: Int = 0): Unit = {
        val nowLayer = layerList(layer)
        if(layer == 6)
          maxList += total
        else if(position == layer -1){
          recusive(total + nowLayer(position),layer +1 , position)
        }
        else{
          recusive(total + nowLayer(position),layer +1 , position)
          recusive(total + nowLayer(position+ 1),layer +1 , position + 1)
        }
      }
      recusive()
      maxList.result().max
    }

    println(solve)


  }

  test("no1932 reverse"){
    def solve18() = {
      val str =
        """
7
3 8
8 1 0
2 7 4 4
4 5 2 6 5
"""

      val arr = str.trim().split("\n").map(_.split(" ").map(_.toInt)).reverse
      def reduceFunc(a: Array[Int],b: Array[Int]) : Array[Int] = {
        assert(a.size == b.size + 1)
        (0 until b.size).map(i => b(i) + Math.max(a(i),a(i+1))).toArray
      }
      arr.reduce(reduceFunc(_, _)).toSeq(0)
    }

    println(solve18())
  }
}
