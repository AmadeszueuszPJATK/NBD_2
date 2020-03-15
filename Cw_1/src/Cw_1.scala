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

  def task_4_A(list : List[String]) : String = {
    list.foldLeft(""){ (acc, str) =>
        if(!acc.isEmpty)
          acc + ", " + str
        else
          acc + str
    }
  }

  def task_4_B(list : List[String]) : String = {
    list.foldRight(""){ (acc, str) =>
      if(!str.isEmpty)
        acc + ", " + str
      else
        acc
    }
  }

  def task_4_C(list : List[String]) : String = {
    list.foldLeft(""){ (acc, str) =>
      if( str.charAt(0) != 'P')
        if(!acc.isEmpty)
          acc + ", " + str
        else
          acc + str
      else
        acc
    }
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
}
