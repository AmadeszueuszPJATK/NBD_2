object Cw_1 extends App {
  /*
  1.	Stwórz 7 elementową listę zawierającą nazwy dni tygodnia. Napisz funkcję tworzącą w oparciu o nią stringa z elementami oddzielonymi przecinkami korzystając z:
    a.	Pętli for
   */
  def task_1_A(list: List[String]): String ={
    var output = ""
    for(i <- 0 to list.size - 2){
      output += list(i) + ", "
    }
    output += list.last
    return output
  }

  /*
  1b.	Pętli for wypisując tylko dni z nazwami zaczynającymi się na „P”
   */
  def task_1_B(list: List[String]): String ={
    return task_1_A(list.filter(_.charAt(0) == 'P'))
  }

  /*
  1c.	Pętli while
   */
  def task_1_C(list: List[String]): String ={
    var output = ""
    var i = 0
    while(i < list.size - 1){
      output = output + list(i) + ", "
      i += 1
    }
    output = output + list.last
    return output
  }

  /*
    2.	Dla listy z ćwiczenia 1 napisz funkcję tworzącą w oparciu o nią stringa z elementami oddzielonymi przecinkami korzystając z:
  a.	Funkcji rekurencyjnej
   */
  @scala.annotation.tailrec
  def task_2_A(list: List[String], i : Int, output : String) : String = {
    if(i < list.size - 1){
      return task_2_A(list, i + 1,  output + (list(i) + ", ") )
    }else if(i == list.size - 1){
      return output + list(i)
    }else{
      return output
    }
  }

  /*
  2b.	Funkcji rekurencyjnej wypisując elementy listy od końca
 */
  @scala.annotation.tailrec
  def task_2_B(list: List[String], i : Int, output : String) : String = {
    if(i > 0){
      return task_2_B(list, i - 1,  output + (list(i) + ", ") )
    }else if(i == 0){
      return output + list(i)
    }else{
      return output
    }
  }

  /*
  3.	Stwórz funkcję korzystającą z rekurencji ogonowej do zwrócenia oddzielonego przecinkami stringa zawierającego elementy listy z ćwiczenia 1
   */
  @scala.annotation.tailrec
  def task_3(list: List[String], i : Int, output : String) : String = {
    if(i == list.size - 1){
      return output + list(i)
    }else if(i == list.size){
      return output
    }
    return task_3(list, i + 1,  output + (list(i) + ", ") )
  }

  /*
  4.	Dla listy z ćwiczenia 1 napisz funkcję tworzącą w oparciu o nią stringa z elementami oddzielonymi przecinkami korzystając z:
a.	Metody foldl
   */
  def task_4_A(list : List[String]) : String = {
    list.foldLeft(""){ (acc, str) =>
        if(!acc.isEmpty)
          acc + ", " + str
        else
          acc + str
    }
  }

  /*
  b.	Metody foldr
   */
  def task_4_B(list : List[String]) : String = {
    list.foldRight(""){ (acc, str) =>
      if(!str.isEmpty)
        acc + ", " + str
      else
        acc
    }
  }

  /*
  c.	Metody foldl wypisując tylko dni z nazwami zaczynającymi się na „P”
   */
  def task_4_C(list : List[String]) : String = {
    list.foldLeft(""){ (acc, str) =>
      if(!str.isEmpty && str.charAt(0) == 'P')
        if(!acc.isEmpty)
          acc + ", " + str
        else
          acc + str
      else
        acc
    }
  }

  /*
  5.	Stwórz mapę z nazwami produktów i cenami. Na jej podstawie wygeneruj drugą, z 10% obniżką cen. Wykorzystaj mechanizm mapowania kolekcji.
   */
  def task_5(in_map: Map[String, Float], discount : Float) : Map[String, Float] = {
    in_map.map(v => Tuple2(v._1, v._2 * (1.0f - discount)))
  }

  /*
  6.	Zdefiniuj funkcję przyjmującą krotkę z 3 wartościami różnych typów i wypisującą je
   */
  def task_6(in_tuple : (String, Int, Float)): Unit ={
    print("\nTask 6:\n Input tuple: " + in_tuple)
  }

  /*
  7.	Zaprezentuj działanie Option na dowolnym przykładzie (np. mapy, w której wyszukujemy wartości po kluczu)
   */
  def task_7(): Unit ={
    val in_map = Map("Key" -> "Value")
    print("\nTask 7:\n Map(NonExistingKey) is " + in_map.get("NonExistingKey"))
  }

  /*
  8.	Napisz funkcję usuwającą zera z listy wartości całkowitych (tzn. zwracającą listę elementów mających wartości różne od 0).
  Wykorzystaj rekurencję.
   */
  def task_8(in_list : List[Int], idx : Int): List[Int] = {
    if(idx == in_list.size)
      return in_list

    if(0 == in_list(idx)){
      return task_8(in_list.take(idx) ++ in_list.drop(idx + 1), idx)
    }
    return task_8(in_list, idx + 1)
  }

  /*
  9.	Zdefiniuj funkcję, przyjmującą listę liczb całkowitych i zwracającą wygenerowaną na jej podstawie listę,
  w której wszystkie liczby zostały zwiększone o 1. Wykorzystaj mechanizm mapowania kolekcji.
   */
  def task_9(in_list : List[Int]): List[Int] ={
    in_list.map( v => v + 1)
  }

  /*
  10.	Stwórz funkcję przyjmującą listę liczb rzeczywistych
  i zwracającą stworzoną na jej podstawie listę zawierającą wartości bezwzględne elementów
  z oryginalnej listy należących do przedziału <-5,12>. Wykorzystaj filtrowanie.
   */
  def task_10(in_list : List[Double]): List[Double] ={
    in_list.filter(v => v >= -5.0 && v <= 12.0).map(v => v.abs)
  }

  val weekdays = List("Poniedzialek", "Wtorek", "Sroda", "Czwartek", "Piatek", "Sobota", "Niedziela")

  print("\nTask 1 A:\n " + task_1_A(weekdays))
  print("\nTask 1 B:\n " + task_1_B(weekdays))
  print("\nTask 1 C:\n " + task_1_C(weekdays))
  print("\nTask 2 A:\n " + task_2_A(weekdays, 0, ""))
  print("\nTask 2 B:\n " + task_2_B(weekdays, weekdays.size - 1, ""))
  print("\nTask 3:\n " + task_3(weekdays, 0, ""))
  print("\nTask 4 A:\n " + task_4_A(weekdays))
  print("\nTask 4 B:\n " + task_4_B(weekdays))
  print("\nTask 4 C:\n " + task_4_C(weekdays))
  print("\nTask 5:\n " + task_5(Map("Chleb" -> 2.0f, "Maka" -> 10.0f), 0.1f))
  task_6(Tuple3("1", 2, 3.0f))
  task_7()
  print("\nTask 8:\n " + task_8(List(0,1,0,2,0,3,0), 0))
  print("\nTask 9:\n " + task_9(List(1,2,3)))
  print("\nTask 10:\n " + task_10(List(-10.0, -2.0, 0.0, 4.0, 12.0, 15.0)))
}
