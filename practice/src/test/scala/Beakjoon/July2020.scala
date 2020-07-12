package test.Beakjoon

import org.scalatest.FunSuite

class July2020 extends FunSuite {

  def time(f : => Unit) = {
    val a = System.currentTimeMillis()
    f
    println ( s"${System.currentTimeMillis() - a} ms")
  }

  test("no2178") {
    val arr =
      """101111
        |101010
        |101011
        |111011
      """.stripMargin.trim.replace("\n","").toArray.map(_.toString.toInt).grouped(6).toArray

    val arr2 =
      """110110
        |110110
        |111111
        |111101
      """.stripMargin.trim.replace("\n","").toArray.map(_.toString.toInt).grouped(6).toArray

    val dx = List(0,1,0,-1)
    val dy = List(-1,0,1,0)

    def no2178(n:Int , m : Int, arr : Array[Array[Int]]) = {

      val tmpMap = scala.collection.mutable.Map[(Int,Int),Int]()
      val queue = new scala.collection.mutable.Queue[(Int,Int)]
      tmpMap.getOrElseUpdate((0,0),1)
      queue.enqueue((0,0))

      while(!queue.isEmpty) {
        val deq = queue.dequeue()
        val x = deq._1
        val y = deq._2
        val mapValue = tmpMap.get((x,y)).get

        for(i <- 0 until 4){
          val nx = x + dx(i)
          val ny = y + dy(i)
          if(nx >= 0 && ny >= 0 && nx < n && ny < m){
            if(tmpMap.get((nx,ny)).isEmpty && arr(nx)(ny) == 1){
              queue.enqueue((nx,ny))
              tmpMap.getOrElseUpdate((nx,ny),mapValue+1)
            }
          }

        }
      }

      tmpMap.getOrElse((n-1,m-1),0)
    }

    println(no2178(4,6, arr2))
    println(time(no2178(4,6, arr2)))
  }


  test("no1149"){
    val arr =
      """26 40 83
49 60 57
13 89 99""".trim.split("\n").toList.map{_.split(" ").toList.map(_.toInt)}

    def solve(num:Int, arr:List[List[Int]]) = {

      val resultList = List.newBuilder[Int]
      def down(position:Int, sum: Int, layer:Int) : Unit = {
        if(layer == num)
          resultList += sum
        else{
          val tmpLayer = arr(layer)
          for(i<- 0 until 3 if (i != position)){
            down(i,sum + tmpLayer(i),layer +1)
          }

        }
      }

      def down2(position:Int, sum: Int, layer:Int) : Unit = {
        if(layer == num)
          resultList += sum
        else{
          val tmpLayer = arr(layer)
          var key = 10000
          var value = 10000
          for(i<- 0 until 3 if (i != position)){
            if(tmpLayer(i) < value){
              key = i
              value =tmpLayer(i)
            }
          }
          down2(key, sum + value, layer + 1)

        }
      }

      for(i<- 0 until num) {
        down(i,arr(0)(i),1)
      }

      resultList.result().min
    }

    println(solve(3,arr))
    println(arr)
    println(time(solve(3,arr)))

  }


  test("no2667"){
    var arr =
      """0110100
        |0110101
        |1110101
        |0000111
        |0100000
        |0111110
        |0111000
      """.stripMargin.trim.replace("\n","").toList.map(_ - '0').grouped(7).toList

    val dx = List(0,1,0,-1)
    val dy = List(-1,0,1,0)

    def solve(num:Int, list :List[List[Int]]) ={
      var visited = Array.ofDim[Int](num,num)
      var sumList = Array.ofDim[Int](num*num)
      var number = 0

      def dfs(x:Int, y:Int): Unit ={
        visited(x)(y) = 1
        sumList(number) +=1

        for(i <- 0 until 4){
          val nx = x + dx(i)
          val ny = y + dy(i)
          if(nx >= 0 && ny >= 0 && nx < num && ny < num){
            if(arr(nx)(ny) == 1 && visited(nx)(ny) == 0){
              dfs(nx,ny)
            }
          }

        }

      }

      for{
          i <- 0 until num
          j <- 0 until num
      }{
        if(arr(i)(j) == 1 && visited(i)(j) == 0){
          number += 1
          dfs(i,j)
        }
      }

      println(number)
      sumList.filterNot(_ == 0).sorted.foreach(println(_))
    }

    solve(7,arr)


  }

}
