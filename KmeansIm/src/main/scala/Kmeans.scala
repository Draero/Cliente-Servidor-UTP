import concurrency._

import scala.:+
import scala.collection.immutable.Nil.++
import scala.util.Random.between
import scala.util.control.Breaks.{break, breakable}

// import org.scalameter._
import scala.math.sqrt
import scala.io.Source
import scala.util.Random
import scala.math.BigDecimal
import Array._

object Kmeans {

  def distance(p: Array[Double], q: Array[Double]): Double = {
    var dist = 0.0
    var i = 0
    while (i < p.length) {
      val d = p(i) - q(i)
      dist += d * d
      i += 1
    }
    sqrt(dist)
  }

  def mediaDistance(p: Array[Double], cluster: Array[Array[Double]], s:Int, f:Int, minlength:Int): Double ={
    val l = f - s
    var dist = 0.0
    if(l < minlength) {
      var i = s
      while(i < f){
        dist = dist + distance(p,cluster(i))
        i += 1
      }
    }
    else {
      val m = s + (f - s) / 2
      val (x, y) = parallel(mediaDistance(p, cluster, s, m, minlength), mediaDistance(p, cluster, m, f, minlength))
      dist = x + y
    }
    dist
  }

  def mediaDist(p: Array[Double],q: Array[Array[Double]], flagpoint:Int): Double = {
    val d = mediaDistance(p,q,0,q.length,5)
    d/(q.length-flagpoint)
  }
  //  def relocatedCentroid()

  def nearestCentroid(p: Array[Double], c: Array[Array[Double]]): Array[Int] = {
    var dist = Double.PositiveInfinity //falta paralelizar (similar a la paralelizacion de la subdist)
    var nearest = new Array[Int](2)
    nearest(0) = 0
    nearest(1) = 0
    var i = 0
    while (i < c.length) {
      val di = distance(p, c(i))
      if (di < dist) {
        dist = di
        nearest(1) = nearest(0)
        nearest(0) = i
      }
      i = i + 1
    }
    nearest
  }

  def kmeansSum(dataset: Array[Array[Double]], clusters:Array[Array[Double]], s:Int, f:Int, minlength:Int): (Array[Array[Double]], Array[Int]) = {
    val l = f - s
    var mediaCluster = new Array[Array[Double]](clusters.length)
    var numPointsCluster = new Array[Int](clusters.length)
    for(i <- 0 until clusters.length){
      mediaCluster(i) = Array(0,0,0,0,0,0,0)
      numPointsCluster(i) = 0
    }
    if(l < minlength){
      for(i <- s until f){
        val centroidPosNum = nearestCentroid(dataset(i), clusters)
        numPointsCluster(centroidPosNum(0)) += 1
        for (iter <- 0 until 7) {
          mediaCluster(centroidPosNum(0))(iter) += dataset(i)(iter)
        }
      }
    }
    else{
      val m = s + (f - s) / 2
      val (x, y) = parallel(kmeansSum(dataset, clusters, s, m, minlength), kmeansSum(dataset, clusters, m, f, minlength))
      var (t,w) = x
      var (r,u) = y
      for(i <- 0 until clusters.length){
        numPointsCluster(i) = w(i) + u(i)
        for(iter <- 0 until 7){
          mediaCluster(i)(iter) = t(i)(iter) + r(i)(iter)
        }
      }
    }
    (mediaCluster, numPointsCluster)
  }

  def kmeansIteraction(dataset:Array[Array[Double]], clusters:Array[Array[Double]]): Array[Array[Double]] = {
    var (mediaCluster, numPointsCluster) = kmeansSum(dataset, clusters, 0, dataset.length, 8)
    for(iter <- 0 until clusters.length){
      if(numPointsCluster(iter) == 0){
        mediaCluster(iter) = Array(0.0,0.0,0.0,0.0,0.0,0.0,0.0)
      }
      else{
        for(i <- 0 until 7){
          mediaCluster(iter)(i) /= numPointsCluster(iter)
          mediaCluster(iter)(i) = BigDecimal(mediaCluster(iter)(i)).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble
        }
      }
    }
    mediaCluster
  }

  def centroidsGenerate(min: Array[Double], max: Array[Double], numclust: Int): Array[Array[Double]] = {
    var clusters = new Array[Array[Double]](0)
    for (i <- 0 until numclust) {
      var centroid = new Array[Double](0)
      for (e <- 0 until min.length) {
        val percent = 2
        var newmax = max(e) + ((max(e) * percent) / 100)
        var randomnum = between(min(e), newmax)
        randomnum = BigDecimal(randomnum).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble
        val concatarray = Array(randomnum)
        centroid = centroid ++ concatarray
      }
      clusters = clusters ++ Array(centroid)
    }
    clusters
  }

  def silhouetteCoefficient(mediaDataSMap: Map[Int, Array[Array[Double]]], clusters: Array[Array[Double]]): Double = {
    var silhoCoeff = 0.0
    for(i <- 0 until clusters.length){
      val dataSetPoints = mediaDataSMap(i)
      var coefSil = 0.0
      for(iter <- 0 until dataSetPoints.length){
        var mainMedia = mediaDist(dataSetPoints(iter), dataSetPoints, 1)
        val auxNeighb = nearestCentroid(dataSetPoints(iter), clusters)
        var neighboor = auxNeighb(1)
        val neighMedia = mediaDist(dataSetPoints(iter), mediaDataSMap(neighboor), 0)
        var coef = (neighMedia - mainMedia)/((mainMedia).max(neighMedia))
        coefSil += coef
      }
      coefSil = coefSil/(dataSetPoints.length)
      silhoCoeff += coefSil
    }
    silhoCoeff/(clusters.length)
  }

  def printMap(point:Array[Double]): Unit = {
    println("\t" + point.mkString(", "))
  }

  def main(args: Array[String]): Unit = {
    println("Hola mundo")

    // -------------------------- Variables --------------------------------
    val filename = "Happiness.csv"
//   floors y ceils son para sacar los valores minimos y maximos del data set asi generar los centroides
    var floors = Array(Double.PositiveInfinity, Double.PositiveInfinity, Double.PositiveInfinity, Double.PositiveInfinity, Double.PositiveInfinity, Double.PositiveInfinity, Double.PositiveInfinity)
    var ceils = Array(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
    var dataset = new Array[Array[Double]](0)

    // ---------------------------------------------------------------------------------

    // foooooooooooorrrrr
    // se crean los floors, ceils y se llena una variable con el dataset
    for (line <- Source.fromFile(filename).getLines) {
      //      println(line.getClass)
      val poin = line.split(",")
      var count = 0
      var dataPoint = new Array[Double](0)
        // foooooooooorrrrr
      for (po <- poin) {
        if (count == 0) {
          count += 1
        }
        else if (count == 1) {
          count += 1
        }
        else {
          val dou = po.toDouble
          dataPoint = dataPoint ++ Array(dou)
          if (!(floors(count - 2) < dou)) {
            floors(count - 2) = dou
          }
          if (!(ceils(count - 2) > dou)) {
            ceils(count - 2) = dou
          }
          count += 1
        }
      }
      dataset = dataset ++ Array(dataPoint)
    }

    var silhouetteCoeff = -1.0
    var centroids = new Array[Array[Double]](0)

    for (e <- 2 until 7){

      val clusters = centroidsGenerate(floors, ceils, e)
      var mediaCluster = kmeansIteraction(dataset,clusters)
      for(i <- 0 until 8){
        mediaCluster = kmeansIteraction(dataset, mediaCluster)
      }

      for(i <- 0 until mediaCluster.length){
        val point = Array(0.0,0.0,0.0,0.0,0.0,0.0,0.0)
        if(mediaCluster(i)(0) == point(0)){
          println("Entro")
          var newPoint = new Array[Double](7)
          for(iter <- 0 until 7){
            newPoint(iter) = 0
          }
          for(iter <- 0 until mediaCluster.length){
            if(!(iter == i)){
              for(it <- 0 until 7){
                newPoint(it) += mediaCluster(iter)(it)
              }
            }
          }
          for(it <- 0 until 7){
            newPoint(it) /= (mediaCluster.length - 1)
            newPoint(it) = BigDecimal(newPoint(it)).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble
          }
          mediaCluster(i) = newPoint
        }
      }

      for(i <- 0 until 8){
        mediaCluster = kmeansIteraction(dataset, mediaCluster)
      }

      var mediaClusterGroup = dataset.groupBy(x => nearestCentroid(x, mediaCluster)(0))
      for(i <- 0 until e){
        for(iter <- 0 until 7){
          print(mediaCluster(i)(iter))
          print(", ")
        }
        println()
      }

      for((k,v) <- mediaClusterGroup){
        println(k)
        for(p <- v){
          try{
            printMap(p)
          }
          catch {
            case e: NoSuchElementException => println("No hay puntos designados al cluster " + k)
          }
        }
      }
      

      var silhoCo = silhouetteCoefficient(mediaClusterGroup, mediaCluster)
      if (silhoCo > silhouetteCoeff){
        silhouetteCoeff = silhoCo
        centroids = mediaCluster
      }
//      silhoCo = BigDecimal(silhoCo).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble
      println(silhoCo)
      println("-------------------------------------------------------")
    }


    centroids.foreach(point => println(point.mkString(", ")))
    println(silhouetteCoeff)
    var flag = true

    while(flag){
      var cent = centroids
      var cont = 0
      cent = kmeansIteraction(dataset,cent)
      var mediaClusterGroup = dataset.groupBy(x => nearestCentroid(x, cent)(0))
      val silhoCo = silhouetteCoefficient(mediaClusterGroup, cent)
      if(silhoCo <= silhouetteCoeff){
        println(silhoCo)
        println(cont)
        flag = false
      }
      else{
        centroids = cent
        cont += 1
      }
    }


  }

}