package Beakjoon

import org.scalatest.FunSuite

class July2 extends FunSuite{
  test("no11052") {
    val str = """1 1 2 3 5 8 13 21 34 55""".trim.split(" ").toList.map{
      x => x.toDouble}
    val str2 = """5 10 11 12 13 30 35 40 45 47""".trim.split(" ").toList.map{
      x => x.toDouble}

    def solve(num:Int, str:List[Double]) ={
      val list = (for(i <- 0 until str.size)yield{str(i)/(i+1) -> (i+1)}).sorted.reverse

      var sum = num
      var result = 0
      for(i<- list){
        if(sum != 0) {
          val value = i._2
          if (sum >= value) {
            sum = sum - value
            result += str(value-1).toInt
          }
        }
      }
      result
    }

    println(solve(10,str2))
  }

  test("no11403"){
    val base =
      """0 0 0 1 0 0 0
        |0 0 0 0 0 0 1
        |0 0 0 0 0 0 0
        |0 0 0 0 1 1 0
        |1 0 0 0 0 0 0
        |0 0 0 0 0 0 1
        |0 0 1 0 0 0 0
      """.stripMargin.replace(" ","").trim.replace("\n","").toArray.map(_.toString.toInt).grouped(7).toArray
    def solve(num:Int, base:Array[Array[Int]]) ={
      var arr = Array.fill[Int](num,num)(0)
      var visited = Array.ofDim[Int](num)
      println(arr)

      def dfs(x: Int, y: Int): Unit = {
        visited(y) = 1
        arr(x)(y) = 1
        for( i <- 0 until num)
          if(base(y)(i) == 1 && visited(i) == 0) dfs(x,i)
      }

      for(i <- 0 until num){
        for(j <- 0 until num){
          visited(j) = 0
        }
        for(j <- 0 until num){
          if(base(i)(j) ==1 && visited(j) == 0){
            dfs(i,j)
          }
        }
      }
      arr
    }

    val arr = solve(7,base)
    println(arr.deep.mkString("\n"))
  }

  test("no11057"){
    // 3중포문

    def solve(num : Int) ={
      var arr = Array.fill[Int](1001,10)(0)
      for(i <- 0 to 9) {
        arr(1)(i) = 1
        println(arr(1)(i))
      }

      for(i <-2 to num){
        for(j <- 0 to 9){
          for(k <- j to 9){
            arr(i)(j) = arr(i)(j) +arr(i-1)(k)
          }
        }
      }

      var result = 0
      for (i <- 0 to 9) {
        result += arr(num)(i)
        println(result)
      }
      result
    }

    println(solve(3))

  }

  test("no2293"){
    val str = """1 2 5""".trim.split(" ").toList.map{
      x => x.toInt}
    println(str)


    def solve(str:List[Int], num: Int): Unit ={

      val resultList = scala.collection.mutable.Map[Int,Int]()
      println(resultList)

      val size = str.size
      for( i <- 0 until size){
        for(j <- 1 to num if j >= str(i)){
          val tmp = resultList.getOrElse(j - str(i),0)
          resultList.getOrElseUpdate(j,resultList.getOrElseUpdate(j,0)+tmp)
        }
      }
      println(resultList)
      println(resultList.get(num))
    }
    solve(str,10)

  }

  test("no14888"){
    val str = """1 2 3 4 5 6""".trim.split(" ").toList.map{
      x => x.toInt}
    val operator = """2 1 1 1""".trim.split(" ").toList.map{x => x.toInt}

    val result = List.newBuilder[Int]
    val size = str.size

    def solve(): Unit ={

      def dfs(count:Int,sum:Int,operator:List[Int]):Unit ={
        if(count == size)  {
          result += sum
        }
        else {
          if(operator(0) > 0){
            val list = for(i <- 0 until 4) yield {
              if(i == 0) operator(0) - 1
              else operator(i)
            }
            dfs(count+1,sum + str(count),list.toList)
          }
          if(operator(1) > 0){
            val list = for(i <- 0 until 4) yield {
              if(i == 1) operator(1) - 1
              else operator(i)
            }
            dfs(count+1,sum - str(count),list.toList)
          }
          if(operator(2) > 0){
            val list = for(i <- 0 until 4) yield {
              if(i == 2) operator(2) - 1
              else operator(i)
            }
            dfs(count+1,sum * str(count),list.toList)
          }
          if(operator(3) > 0){
            val list = for(i <- 0 until 4) yield {
              if(i == 3) operator(3) - 1
              else operator(i)
            }
            dfs(count+1,sum / str(count),list.toList)
          }
        }
      }

      dfs(1,str(0),operator)
      val min = result.result().min
      val max = result.result().max
      println(result.result())
      println(s"$max $min")
    }

    solve()

  }
}
