object Task1
{

  def MisraGries[T](args: LazyList[T], k: Int, c: Class[_]): Map[T, Int] =
  {
    val n = args.length
    var out: Map[T, Int] = Map()

    val obj = args.filter(x => x.getClass == c)

    obj.foreach
    {
      x =>
      {
        if (out.contains(x))  out = out.updated(x, out(x) + 1)
        else if (out.size < k-1) out += (x -> 1)
        else out.foreach
        {
          case (key, value) =>
          {
            out = out.updated(key, out(key) - 1)
            if (out(key) == 0) out -= key
          }
        }
      }
    }
    out
  }

  def main(args:Array[String]): Unit =
  {
    val car1 = new Car("Mazda", 1999)
    val car2 = new Car("Mazda", 2020)
    val car3 = new Car("BMW", 2011)
    val book1 = new Book( "Hobbit", "Tolkien")
    val book2 = new Book( "Harry Potter", "Rowling")
    val book3 = new Book( "Godfather", "Puzo")
    val emp1 = new Employee("Filip", "Jaszczyk", 123)
    val emp2 = new Employee("Jan", "Kowalski", 111)
    val emp3 = new Employee("Anna", "Nowak", 101)

    val arg = car2 #:: car1 #:: car2 #:: car3 #:: car1 #:: car2 #:: car1 #:: car1 #:: car1 #:: car2 #::
      book2 #:: book1 #:: book2 #:: book3 #:: book1 #:: book2 #:: book1 #:: book1 #:: book1 #:: book2 #::
      emp2 #:: emp1 #:: emp2 #:: emp3 #:: emp1 #:: emp2 #:: emp1 #:: emp1 #:: emp1 #:: emp2 #:: LazyList.empty

    val cars = MisraGries(arg, 2, classOf[Car])
    val books = MisraGries(arg, 3, classOf[Book])
    val emps = MisraGries(arg, 4, classOf[Employee])

    cars.foreach
    {
      case (key, value) =>
      {
        val Item = key.asInstanceOf[Car]
        println(Item.Name + " " + Item.Year + " : " + value)
      }
    }

    books.foreach
    {
      case (key, value) =>
      {
        val Item = key.asInstanceOf[Book]
        println(Item.Name + " by " + Item.Author + " : " + value)
      }
    }

    emps.foreach
    {
      case (key, value) =>
      {
        val Item = key.asInstanceOf[Employee]
        println(Item.Name + " " + Item.Surname + " " + Item.ID +  " : " + value)
      }
    }
  }
}
