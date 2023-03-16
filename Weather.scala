import scala.io.Source
import scala.language.postfixOps
import java.io.File
  
case class Weather(sensor: String, humidity: String)

  def readWeather(file:java.io.File): Seq[Weather] = {
		for {
			line <- Source.fromFile(file).getLines().drop(1).toVector
			values = line.split(",").map(_.trim)
		}yield Weather(values(0), values(1))
    }
  
	  val dir = new File(args(0))
	  var s1 = Seq.empty[Weather]
	  var noFiles = 0
	  val files = dir.listFiles.filter(_.isFile).filter(_.getName.endsWith(".csv"))
      for (file <- files) {
			noFiles = noFiles + 1
			println(s"Reading file ${file.getName}")
			 s1 = s1 ++ readWeather(file)
	  }

val WeatherSeq = s1  
val p = WeatherSeq.filter(x=>x.humidity!=("NaN")).length
val f = WeatherSeq.filter(x=>x.humidity==("NaN")).length


def getTotalNumberOfWeather(): Int = WeatherSeq size

def avg(WeatherType: Seq[Weather]): Double = WeatherType.map(_.humidity.toInt).sum / WeatherType.size

def mn(WeatherType: Seq[Weather]): Double = WeatherType.map(_.humidity.toInt).min

def mx(WeatherType: Seq[Weather]): Double = WeatherType.map(_.humidity.toInt).max

val ag = WeatherSeq.filter(x=>x.humidity!=("NaN")).groupBy(_.sensor).mapValues(avg(_)).toIterator.toMap
val m1 = WeatherSeq.filter(x=>x.humidity!=("NaN")).groupBy(_.sensor).mapValues(mx(_)).toIterator.toMap
val m2 = WeatherSeq.filter(x=>x.humidity!=("NaN")).groupBy(_.sensor).mapValues(mn(_)).toIterator.toMap


println(s"Num of processed files: ${noFiles}")
println(s"Num of processed measurements: ${p}")
println(s"Num of failed measurements: ${f}")

println("Sensors with highest avg humidity:")
println("sensor-id,avg")
ag.foreach(println)

println("Sensors with min humidity:")
println("sensor-id,min")
m1.foreach(println)

println("Sensors with max humidity:")
println("sensor-id,max")
m2.foreach(println)

