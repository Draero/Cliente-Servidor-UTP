//import concurrency._
import org.scalameter._
import scala.util.Random.nextInt

object CodeFi {

  class HelloWorldThread extends Thread {
    override def run() {
      println("Hello world from thread")
    }
  }

  def randIntValues(n:Int, max:Int=100): Array[Int] = {
    Array.fill(n)(nextInt(max))
  }

  def run[B](block: => B) : Double = {
    val time = config (
      Key.exec.benchRuns -> 20//,
      //        Key.verbose -> true
    ) withWarmer {
      new Warmer.Default
    } withMeasurer {
      new Measurer.IgnoringGC
    } measure {
      block
    }
    return time.value
  }

  def pfib(n:Int):Int = {
    if (n < 2) {
      n
    }
    else {
      val (a,b) = parallel(pfib(n - 1), pfib(n - 2))
      a + b
    }
  }

  def swap(a: Array[Int], pos1: Int, pos2: Int): Unit = {
    val stash = a(pos1)
    a(pos1) = a(pos2)
    a(pos2) = stash
  }

  def merge(a: Array[Int], aux: Array[Int], start: Int, mid: Int, end: Int): Unit {
  var (i, j, dest) = (start, mid, start)
  while(i < mid && j <= end) {
    if (a(i) <= a(j)) { aux(dest) = a(i); i +=1 }
    else { aux(dest) = a(j); j += 1 }
    dest += 1
  }
  while (i < mid) { aux(dest) = a(i); i += 1; dest += 1 }
  while (j <= end) { aux(dest) = a(j); j += 1; dest += 1 }
  dest = start
  while (dest <= end) { a(dest) = aux(dest); dest += 1 }
  }

  def   _mergesortNaive(a: Array[Int], start: Int, end: Int, aux: Array[Int]): Unit = {
    if (start >= end) return
    val size= end - start
    if (size == 1) {
      return
    } else if (size == 2){
      if (a(start) > a(end-1)){
        swap(a,start,end-1)
      }
      return
    } else{
      val mid = start + (end - start)/2
      _mergesortNaive(a, start, mid, aux)
      _mergesortNaive(a, mid+1, end, aux)
      merge(a,aux,start,mid+1,end)
    }
  }

  def _mergesortThreshold(a: Array[Int], start: Int, end: Int, aux: Array[Int], threshold: Int): Unit = {
    if (start >= end) return
    val size = end - start
    if(size < threshold){
      quickSort.quicksort(a, low = 0, end)
    } else {
      val mid = start + (end - start)/2
      _mergesortThreshold(a, start, mid, aux, threshold)
      _mergesortThreshold(a, mid+1, end, aux, threshold)
      merge(a, aux, start, mid+1, end)
    }
  }

  def main(args: Array[String]) : Unit = {
    println("Hola mundo")
    val time = run{fib(20)}
    println(time)

    val ptime = run{pfib(20)}
    println(ptime)

    /*for(i <- 0 to 10){
      val time = config (
        Key.exec.benchRuns -> 20//,
//        Key.verbose -> true
      ) withWarmer {
        new Warmer.Default
      } withMeasurer {
        new Measurer.IgnoringGC
      } measure {
        fib(20)
      }
      println(time)
    }*/

    /*val mem = config(
      Key.exec.benchRuns -> 20
    ) withMeasurer(new Measurer.MemoryFootprint) measure {
      (0 to 1000000).toArray
    }
    println(s"Total memory: $mem")*/
    /*for(i <- 0 to 10){
      val time = withWarmer(new Warmer.Default) measure {
        (0 until 1000000).toArray
      }
      println(time)
    }*/
    /*val time = measure {
      val n = fib(20)
    }
    println(time)*/
    /*for (i <- 0 to 15){
      println(fib(i))
    }*/

    /*al a = randIntValues(1000)
    println(a.mkString(","))*/

    /*val t = new HelloWorldThread
    val u = new HelloWorldThread
    t.start()
    u.start()
    t.join()
    u.join()*/

    /*val t = new Thread {
      override def run(): Unit = {
        println("Hello world from thread")
      }
    }
    t.start()
    t.join()*/
  }
}