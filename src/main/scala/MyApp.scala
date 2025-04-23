/**
 * Created by jim on 06/11/2016.
 * Modified by Andrew on 23/04/2025
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
  def getHead(): Int = head.getValue();
  def getTail(): Int = tail.getValue();
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

    this.sort(productArray);
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

class Basket() {
  private var basket : Map[Product, Integer] = Map();

  def add(p : Product): Unit = {
    if(basket.contains(p)) { basket = basket.updated(p, basket(p) + 1); }else{
      basket = basket ++ Map(p -> 1);
    }
  }

  def getTotal() : Float = {
    var total = 0.00f;
    basket.foreach{case (p, q) => total += p.getHead() * q}
    total;
  }

  def Print() : Unit = {
    println("Product\tQuantity\tCurrent Amount\t Total")
    basket.foreach{ case (p, q) => println(s"${p.getIndex()} : \t${q} : \t${p.getHead()} : \t\t\t${p.getHead() * q}")}
  }
}

object MyApp extends App {

  // *******************************************************************************************************************
  // application logic

  // To test?
  val toTest: Boolean = true;
  if(toTest)
    {
      test();
    }


  // read data from file
  val customMap = readFileCustom("./src/main/scala/data.txt")
  // print data to check it's been read in correctly
  println(customMap);

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
    7 -> handleSeven,
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
         |  3 - show highest points for all teams
         |  4 - show lowest points for all teams
         |  5 - show median points for all teams
         |  6 - compare average between selected teams
         |  7 - Add items to basket
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
    mnuShowHighestPoints(highestPoints)
    true
  }

  def handleFour(): Boolean = {
    mnuShowLowestPoints(lowestPoints)
    true
  }

  def handleFive(): Boolean = {
    mnuShowMedianPoints(medianPoints)
    true
  }

  def handleSix(): Boolean = {
    mnuShowAveragePoints(currentPointsForTeam)
    true
  }

  def handleSeven(): Boolean = {
    mnuBasket(currentPointsForTeam)
    true
  }

  def handleZero(): Boolean = {
    println("selected quit") // returns false so loop terminates
    false
  }


  // *******************************************************************************************************************
  // UTILITY FUNCTIONS

  def readFileCustom(filename: String): Map[String, Product] = {
    var map : Map[String, Product] = Map();
    try {
      for (line <- Source.fromFile(filename).getLines()) {
        val splitline = line.split(",").map(_.trim).toList
        val p = new Product(splitline.head);
        for(v <- splitline.tail)
          {
            p.setHead(v.toInt);
          }
        map = map ++ Map(p.getIndex() -> p);;
      }
    } catch {
      case ex: Exception => println("Sorry, an exception happened.")
    }
    map
  }


  // *******************************************************************************************************************
  // FUNCTIONS THAT INVOKE ACTION AND INTERACT WITH USER
  // each of these functions accepts user input if required for an operation,
  // invokes the relevant operation function and displays the results

  def mnuShowPoints(f: () => Map[String, Product]) = {
    f() foreach { case (x, y) => println(s"${x} current: ${y.getHead()}")}
  }

  def mnuShowPointsForTeam(f: (String) => (Product)) = {
    print("Team>")
    val data = f(readLine)
    if(data != null) data.Print();

  }

  def mnuShowHighestPoints(f: () => Map[String, Product]): Unit = {
    f().foreach{ case(i, p) => println(s"${i} Highest: ${p.getMax()}") }
  }

  def mnuShowLowestPoints(f : () => Map[String, Product]): Unit = {
    f().foreach{ case(i, p) => println(s"${i} Lowest: ${p.getMin()}") }
  }

  def mnuShowMedianPoints(f: () => Map[String, Product]): Unit = {
    f().foreach{ case(i, p) => println(s"${i} Median: ${p.getMedian()}") }
  }

  def mnuShowAveragePoints(f: (String) => (Product)): Unit = {
    print("Team 1>")
    val team1 = f(readLine)
    print("Team 2>")
    val team2 = f(readLine)
    if(team1 != null && team2 != null)
      {
        println(s"${team1.getIndex()} Average: ${team1.getAverage()}")
        println(s"${team2.getIndex()} Average: ${team2.getAverage()}")
      }
  }

  def mnuBasket(f: (String) => (Product)): Unit = {
    print("Type a product name or type Quit to leave\nProduct>")
    val basket : Basket = new Basket();
    var tmp : Product = null;
    var team :String = readLine()
  while(team != "Quit")
    {
      tmp = f(team);
      if(tmp != null){basket.add(tmp)}
      print("Type a product name or type Quit to leave\nProduct>")
      team = readLine();
    }
    println("You have selected quit");
    println(s"Your total is: ${basket.getTotal()}")
    basket.Print();

  }

  // *******************************************************************************************************************
  // OPERATION FUNCTIONS
  // each of these performs the required operation on the data and returns
  // the results to be displayed - does not interact with user

  def currentPoints(): Map[String, Product] = {
    // see http://alvinalexander.com/scala/how-to-sort-map-in-scala-key-value-sortby-sortwith

    // sort list from highest to loqest based on the most recent value
    ListMap(customMap.toSeq.sortWith(_._2.getHead > _._2.getHead): _*)
  }

  def highestPoints(): Map[String, Product] = {
    // Sorts list from highest to lowest
    ListMap(customMap.toSeq.sortWith(_._2.getMax() > _._2.getMax()): _*);
  }

  def lowestPoints(): Map[String, Product] ={
    // sorts list from lowest to highest
    ListMap(customMap.toSeq.sortWith(_._2.getMin() < _._2.getMin()): _*);
  }

  def medianPoints(): Map[String, Product] = {
    // sort list based on the result of the median from highest to lowest
    ListMap(customMap.toSeq.sortWith(_._2.getMedian() > _._2.getMedian()): _*);
  }

  def averagePoints(): Map[String, Product] = {
    // sort list based on the average from highest to lowest
    ListMap(customMap.toSeq.sortWith(_._2.getAverage() > _._2.getAverage()): _*);
  }

  def currentPointsForTeam(team: String): Product = {
    customMap.get(team) match{
      case Some(p) => p;
      case None => println("Team not found"); null
    }
  }
  // *******************************************************************************************************************
  // TESTING FUNCTIONS
  // Each of these are test scenarios in which I create a predefined list
  // Ensuring that each item that is given will give back the expected data result

  def test() : Unit = {

    // Pre-defined Product Class to test items
      val p = new Product("Test");
      p.setHead(54);
    p.setHead(41);
    p.setHead(49);

    val p2 = new Product("Test2");
    p2.setHead(90);
    p2.setHead(50);
    p2.setHead(30);
    p2.setHead(60);

    val basket = new Basket();

    //Testing individual product
    testIndexName(p);
    testRecent(p);
    testLast(p);
    testSize(p);
    testTotal(p);
    testMedian(p);
    testAverage(p);
    testMax(p);
    testMin(p);
    testPrint(p);


    //Testing Basket
    // 1 of each item
    basket.add(p);
    basket.add(p2);
    testBasket(basket);

    // Adding an additional item
    basket.add(p)
    testBasketAdditional(basket);
  }

  def testIndexName(p : Product): Unit = {
    assert(p.getIndex() == "Test", s"Expected 'Test' but got '${p.getIndex()}'");
  }

  def testRecent(p : Product): Unit = {
    assert(p.getHead() == 49, s"Expected 49 but got ${p.getHead()}");
  }
  def testLast(p : Product): Unit = {
    assert(p.getTail() == 54, s"Expected 54 but got ${p.getTail()}")
  }

  def testSize(p : Product): Unit = {
    assert(p.getSize() == 3, s"Expected 3 but got ${p.getSize()}")
  }

  def testTotal(p: Product): Unit ={
    assert(p.getTotal() == 144, s"Expected 144 but got ${p.getTotal()}")
  }

  def testMedian(p : Product) : Unit = {
    assert(p.getMedian() == 49, s"Expected 49 but got ${p.getMedian()}")
  }

  def testAverage(p : Product) : Unit = {
    assert(p.getAverage() == 48, s"Expected 48 but got ${p.getAverage()}")
  }

  def testMax(p : Product) : Unit ={
    assert(p.getMax() == 54, s"Expected 54 but got ${p.getMax()}")
  }

  def testMin(p : Product) : Unit ={
    assert(p.getMin() == 41, s"Expected 41 but got ${p.getMin()}")
  }

  def testPrint(p : Product) : Unit = {
    // Test the print function, prints the last item added and so on,
    // head will always be the most recent
    println("Test: 49, 41, 54");
    p.Print();
  }

  def testBasket(b : Basket) : Unit = {
    assert(b.getTotal() == 109.00, s"Expected 109.00 but got ${b.getTotal()}")
  }
  def testBasketAdditional(b : Basket) : Unit = {
    assert(b.getTotal() == 158.00, s"Expected 158.00 but got ${b.getTotal()}")
  }

}