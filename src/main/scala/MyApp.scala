/**
 * Created by jim on 06/11/2016.
 */

import scala.io.Source
import scala.io.StdIn.readInt
import scala.io.StdIn.readLine
import scala.collection.immutable.ListMap

class Node(private val value: Int) {
  private var next: Node = null;

  def getValue(): Int = value;
  def getNext(): Node = next;
  def setNext(next : Node): Unit = {
    this.next = next;
  }
}

class Product(private val INDEX: String) {
  // Class variables
  private var size : Int = 0;
  private var head: Node = null;
  private var tail: Node = null;

  // Getters
  def getHead(): Node = head;
  def getTail(): Node = tail;
  def getIndex(): String = INDEX;
  def getSize(): Int = this.size;

  def getTotal(): Int = {
    var n = this.head;
    var total = 0;
    var count = 0;
    while(n != null)
    {
      total += n.getValue();
      n = n.getNext();
      count += 1;
    }
    return total;
  }

  def getMax(): Int = {
    var n = this.head;
    var max = Int.MinValue;
    while(n != null)
    {
      if(n.getValue() > max)
        {
          max = n.getValue()
        }
      n = n.getNext();
    }
    return max;
  }

  def getMin(): Int = {
    var n = this.head;
    var min = Int.MaxValue;
    while(n != null)
    {
      if(n.getValue() < min)
      {
        min = n.getValue()
      }
      n = n.getNext();
    }
    return min;
  }

  def getAverage(): Double = this.getTotal().toDouble  /this.getSize();

  def getMedian(): Double = {
    val productArray = this.toArray();
    System.out.print("Before: ");
    for(p <- productArray)
      {
        System.out.print(p.getValue());
        System.out.print(", ");
      }
      System.out.println();

    this.sort(productArray);
    System.out.print("After: ");
    for(p <- productArray)
    {
      System.out.print(p.getValue());
      System.out.print(", ");
    }
    System.out.println();
    if(this.getSize() % 2 == 0)
      {
        val mid1 = productArray(this.getSize()/2 - 1).getValue()
        val mid2 = productArray(this.getSize()/2).getValue()
        return (mid1 + mid2)/2.00;
      }
      return productArray(this.getSize()/2).getValue() ;
  };

  // Setters
    def setHead(hVal : Int): Unit = {
      val nHead = new Node(hVal);
      nHead.setNext(this.head);
      if(this.tail == null)
      {
        this.tail = this.head;
      }
      this.head = nHead;
      this.size += 1;
  }

  // Public Functions
  def Print(): Unit = {
    System.out.print(INDEX)
    System.out.print(": ")
    var n = this.head;
    while(n != null)
    {
      System.out.print(n.getValue())
      System.out.print(", ")
      n = n.getNext();
    }
    System.out.println();
  }

  def toArray(): Array[Node] = {
    val productArray = new Array[Node](this.getSize());
    var n = this.head;
    var c : Int = 0;
    while(n != null)
    {
      productArray(c) = n;
      n = n.getNext();
      c+= 1;
    }
    return productArray;
  }

  def sort(array : Array[Node]): Unit ={
    for (i <- 1 until array.length) {
      // Linear Sort Algorithm
      var index = i - 1
      val temp = array(i)
      while (index >= 0 && array(index).getValue() > temp.getValue()) {
        array(index + 1) = array(index);
        index -= 1
      }
      array(index + 1) = temp
    }
  }
}

object MyApp extends App {

  // *******************************************************************************************************************
  // application logic

  // read data from file
  val mapdata = readFile("./src/main/scala/data.txt")
  val customList = readFileCustom("./src/main/scala/data.txt")
  // print data to check it's been read in correctly
  for(p <- customList)
    {
      p.Print();
    }

  println(mapdata)

  // define menu options as a Map of actions
  // for each menu item:
  // key is an Int, the value that will be read from the input
  // value is a function () => Boolean, i.e. no params and returns Boolean
  val actionMap = Map[Int, () => Boolean](
    1 -> handleOne,
    2 -> handleTwo,
    3 -> handleThree,
    4 -> handleFour,
    5 -> handleFive,
    6 -> handleSix,
    0 -> handleZero
  )

  // loop to read input and invoke menu option
  // uses function readOption to show menu and read input
  // uses function menu to invoke menu action
  // will terminate if menu returns false
  var opt = 0
  do {
    opt = readOption
  } while (menu(opt))


  // *******************************************************************************************************************
  // FUNCTIONS FOR MENU

  // shows menu and reads input
  def readOption: Int = {
    println(
      """|Please select one of the following:
         |  1 - show points for all teams
         |  2 - show points for selected team
         |  3 - show highest points for select team
         |  4 - show lowest points for selected team
         |  5 - show median points for selected team
         |  6 - show average points for selected team
         |  0 - quit""".stripMargin)
    readInt()
  }

  // invokes selected menu option
  // finds corresponding function to invoke in action map using get
  // pattern matching used as get returns an Option
  def menu(option: Int): Boolean = {
    actionMap.get(option) match {
      case Some(f) => f()
      case None =>
        println("Sorry, that command is not recognized")
        true
    }
  }

  // handlers for menu options
  def handleOne(): Boolean = {
    mnuShowPoints(currentPoints) // calls function mnuShowPoints, which invokes function currentPoints
    true
  }

  def handleTwo(): Boolean = {
    mnuShowPointsForTeam(currentPointsForTeam)
    true
  }

  def handleThree(): Boolean = {
    mnuShowHighestPointsForTeam()
    true
  }

  def handleFour(): Boolean = {
    mnuShowLowestPointsForTeam()
    true
  }

  def handleFive(): Boolean = {
    mnuShowMedianPointsForTeam()
    true
  }

  def handleSix(): Boolean = {
    mnuShowAveragePointsForTeam()
    true
  }

  def handleZero(): Boolean = {
    println("selected quit") // returns false so loop terminates
    false
  }


  // *******************************************************************************************************************
  // UTILITY FUNCTIONS

  // reads data file - comma separated file
  def readFile(filename: String): Map[String, Int] = {
    // create buffer to build up map as we read each line
    var mapBuffer: Map[String, Int] = Map()
    try {
      for (line <- Source.fromFile(filename).getLines()) {
        // for each line
        val splitline = line.split(",").map(_.trim).toList // split line at , and convert to List

        // add element to map buffer
        // splitline is line from file as List, e.g. List(Bayern Munich, 24)
        // use head as key
        // tail is a list, but need just the first (only in this case) element, so use head of tail and convert to int
        mapBuffer = mapBuffer ++ Map(splitline.head -> splitline.tail.head.toInt)

      }
    } catch {
      case ex: Exception => println("Sorry, an exception happened.")
    }
    mapBuffer
  }

  def readFileCustom(filename: String): Array[Product] = {
    val ProductList = new Array[Product](Source.fromFile(filename).getLines().length);
    var count = 0;

    try {
      for (line <- Source.fromFile(filename).getLines()) {

        val splitline = line.split(",").map(_.trim).toList
        val p = new Product(splitline.head);
        for(v <- splitline.tail)
          {
            p.setHead(v.toInt);
          }

        ProductList(count) = p;
        count+=1

      }
    } catch {
      case ex: Exception => println("Sorry, an exception happened.")
    }
    ProductList
  }


  // *******************************************************************************************************************
  // FUNCTIONS THAT INVOKE ACTION AND INTERACT WITH USER
  // each of these functions accepts user input if required for an operation,
  // invokes the relevant operation function and displays the results

  def mnuShowPoints(f: () => Map[String, Int]) = {
    f() foreach { case (x, y) => println(s"$x: $y") }
    for(p <- customList)
    {
      p.Print();
    }
  }

  def mnuShowPointsForTeam(f: (String) => (String, Int)) = {
    print("Team>")
    val data = f(readLine)
    println(s"${data._1}: ${data._2}")
    for(p <- customList)
    {
      if(p.getIndex() == data._1){
        p.Print();
      }
    }
  }

  def mnuShowHighestPointsForTeam(): Unit = {
    print("Team>")
    val team = readLine
    for(p <- customList)
    {
      if(p.getIndex() == team){
        println(s"${team}: ${p.getMax()}")
      }
    }
  }

  def mnuShowLowestPointsForTeam(): Unit = {
    print("Team>")
    val team = readLine
    for(p <- customList)
    {
      if(p.getIndex() == team){
        println(s"${team}: ${p.getMin()}")
      }
    }
  }

  def mnuShowMedianPointsForTeam(): Unit = {
    print("Team>")
    val team = readLine
    for(p <- customList)
    {
      if(p.getIndex() == team){
        println(s"${team}: ${p.getMedian()}")
      }
    }
  }

  def mnuShowAveragePointsForTeam(): Unit = {
    print("Team>")
    val team = readLine
    for(p <- customList)
    {
      if(p.getIndex() == team){
        println(s"${team}: ${p.getAverage()}")
      }
    }
  }

  // *******************************************************************************************************************
  // OPERATION FUNCTIONS
  // each of these performs the required operation on the data and returns
  // the results to be displayed - does not interact with user

  def currentPoints(): Map[String, Int] = {
    // sort map by value in descending order -
    // see http://alvinalexander.com/scala/how-to-sort-map-in-scala-key-value-sortby-sortwith
    ListMap(mapdata.toSeq.sortWith(_._2 > _._2): _*)
  }

  def currentPointsForTeam(team: String): (String, Int) = {
    val points = mapdata.get(team) match{
      case Some(p) => p
      case None => 0
    }
    (team, points)
  }


  // *******************************************************************************************************************

}