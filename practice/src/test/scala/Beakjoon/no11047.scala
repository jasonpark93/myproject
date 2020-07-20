package Beakjoon

import org.scalatest.FunSuite
import util.control.Breaks._

class no11047 extends FunSuite{
  test("no11047"){
    val list =
      """1 5 10 50 100 500 1000 5000 10000 50000""".trim.split(" ").toList.map{
        x => x.toInt
      }.reverse

    def solve(money:Int)={
      var result = 0
      var sum = money
      breakable {
        for(i <- list) {
          if(i < sum){
            val a = sum/i
            sum = sum - a*i
            result += a
          }else if(sum == 0){
            break
          }
      }}
      result
    }

    println(solve(4200))
  }
}
