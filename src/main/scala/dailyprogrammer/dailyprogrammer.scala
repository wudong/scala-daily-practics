package dailyprogrammer

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

/**
  * Created by wudong on 27/03/2017.
  */
object dailyprogrammer extends App{

  //https://www.reddit.com/r/dailyprogrammer/comments/611tqx/20170322_challenge_307_intermediate_scrabble/
  //[2017-03-22] Challenge #307 [Intermediate] Scrabble problem
  def scrabble_problem(): Unit ={
    val allWords = Source.fromURL("https://raw.githubusercontent.com/dolph/dictionary/master/enable1.txt").getLines()

    val wordMap = allWords.map(
      x => (x.length, x)
    ).foldLeft(Map[Int, Set[String]]())(
      (map, entry)=>{
        val key: Int = entry._1
        val set: Set[String] = map.getOrElse(key, Set[String]())
        map + (key -> (set + entry._2))
      }
    )

    var currentBest = ArrayBuffer[String]()
    var currentBestLength = 0

    for (s <- wordMap.values.flatten) {
      val result = ArrayBuffer[String]()
      if (buildWord(s, wordMap, result)) {
        if (currentBestLength < result.length){
          currentBestLength = result.length
          currentBest.clear()
        }

        if (currentBestLength <= result.length ){
          currentBest += s
        }
      }
    }

    println(  currentBest.mkString(", "))

  }

  @tailrec
  def buildWord(word: String, map: Map[Int, Set[String]], result: ArrayBuffer[String]) : Boolean ={
     if (word.length ==2 ) {
       if (checkWordExists(word, map)){
         result+=word
       }
       return checkWordExists(word, map)
     }

     val tailStr =word.tail
     val headStr = word.substring(0, word.length -1)

     if (checkWordExists(tailStr, map)){
       buildWord(tailStr, map, result+=word)
     }else if (checkWordExists(headStr, map)){
       buildWord(headStr, map, result+=word)
     }else{
       false
     }
  }

  def checkWordExists(word: String, map: Map[Int, Set[String]] )= (map.get(word.length).getOrElse(Set()).contains(word))


  scrabble_problem()

}
