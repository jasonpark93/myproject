package Beakjoon

import org.scalatest.FunSuite

import scala.collection.mutable
import scala.util.Try

class no1697 extends FunSuite {
  test("no1697"){
    val map = scala.collection.mutable.Map[Int,Int]()
    val countList = scala.collection.mutable.MutableList[Int]()

    def solve(x:Int, result:Int): Unit = {
      countList += 987654321
      def recusive(num:Int, count:Int) :Unit = {

        if(num == result){
          countList += count
        } else if(countList.min > count){
          if(num + 1 < 100000){
            recusive(num + 1 , count +1)
          }
          if(num -1 > 0){
            recusive(num - 1 , count +1)
          }
          if(2*num < result*2){
            recusive(2*num , count +1)
          }

        }

      }
      recusive(x,0)
      println(countList)
      println(countList.result().min)
    }

    solve(5 , 17)

  }
}
