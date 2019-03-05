package Quantum
import scala.collection.mutable.ArrayBuffer

object TP3 extends App {
  class Person(var firstName: String, var lastName: String) {
    override def toString = s"my name is $firstName $lastName"
  }
  class Employee (firstName: String, lastName: String, age:Int)
    extends Person (firstName, lastName) {
    override def toString = super.toString + s"my age is $age"
  }

  trait Pet {
    val name: String
  }

  class Cat(val name: String) extends Pet
  class Dog(val name: String) extends Pet

  val dog = new Dog("Harry")
  val cat = new Cat("Sally")

  val animals = ArrayBuffer.empty[Pet]
  animals.append(dog)
  animals.append(cat)
  animals.foreach(pet => println(pet.name))
}
