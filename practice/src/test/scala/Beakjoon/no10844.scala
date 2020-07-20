package Beakjoon

import org.scalatest.FunSuite

class no10844 extends FunSuite{
  test("no10844"){
    val mod = 100000000
    def solve(n : Int) ={
      val map = scala.collection.mutable.HashMap[(Int,Int),Int]()
      map += (1,0)->0
      for(i <- 1 to 10){
        map += (1,i)-> 1
        map += (i,10)-> 0
      }

      for(i <- 2 to n){
        map.getOrElseUpdate((i,0),map(i-1,1))
        for(j <- 1 to 9){
          map.getOrElseUpdate((i,j),{
            (map(i-1,j-1) + map(i-1,j+1))%mod
          })
        }
      }

      val sum = for(i <- 1 to 9) yield {
        map(n,i)
      }
      println(sum.sum%mod)
    }

    (2 to 9).foreach(solve(_))
  }
}
