package hello
import scalaz._
object Hi {

  def main(args: Array[String]) =  {
    val unused = NonEmptyList("head", "bread")
    val xs = IList("head", "bread")
    //val bads = 'boo :: xs // throws the correct type error, reported better than Inteillij!

    xs.headMaybe.map(x => x) // shows type of anonymous parameters

    xs.headMaybe.map(x => x.toInt) // autocomplete works on anonymous params

    // val s:Int = "foo"; //will get error-hilighted 
    println("Hello there")
    }
        
              
      }
