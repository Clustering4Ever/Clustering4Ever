package Quantum

object tp33 extends App{
abstract class Personne(nom : String){

}
  class Etudiant (nom:String ,prenom:String) extends Personne(nom){

  }

  val p = new Etudiant("abc","cdb")
  println(p)
}
