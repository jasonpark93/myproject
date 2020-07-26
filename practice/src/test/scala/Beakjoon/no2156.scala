package Beakjoon

import org.scalatest.FunSuite

import scala.util.Try

class no2156 extends FunSuite{
  test("no2156"){
    val tri =
      """0 6 10 13 9 8 1""".trim.split(" ").toList.map{
        x => x.toInt
      }
    println(tri)
    var max = 0
    import scala.collection.mutable.ListBuffer

    var dpList = new ListBuffer[Int]()
    def getDpList (int :Int)={
      Try(dpList(int)).getOrElse{
        dpList += 0
        0
      }
    }
    for(i <- 3 until tri.size){
      dpList += Math.max(Math.max(getDpList(i-3) + tri(i-1)+tri(i-2), getDpList(i-2) + tri(i)) , getDpList(i-1))
      max = Math.max(max,dpList(i))
    }
    println(dpList)
    println(max)
  }

}
