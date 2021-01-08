class Car (name: String, year : Int)
{
  val Name : String = name
  val Year : Int = year

  def == (that: Car): Boolean =
  {
    if ((Name == that.Name)  && (Year == that.Year)) true
    else false
  }
}

class Book (title: String, author : String)
{
  val Name : String = title
  val Author : String = author

  def == (that: Book): Boolean =
  {
    if ((Name == that.Name) && (Author == that.Author)) true
    else false
  }
}

class Employee (name: String, surname : String, id : Int)
{
  val Name : String = name
  val Surname : String = surname
  val ID : Int = id

  def == (that: Employee): Boolean =
  {
    if ((Name == that.Name) && (Surname == that.Surname) && (ID == that.ID)) true
    else false
  }
}
