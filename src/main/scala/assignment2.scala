import scala.io.Source

//case class BedRecord to store data
case class BedRecord (date: String,
                      state: String,
                      beds: Int,
                      beds_covid: Int,
                      beds_noncrit: Int,
                      admitted_pui: Int,
                      admitted_covid: Int,
                      admitted_total: Int,
                      discharged_pui: Int,
                      discharged_covid: Int,
                      discharged_total: Int,
                      hosp_covid: Int,
                      hosp_pui: Int,
                      hosp_noncovid: Int
                     )

//case class to round to two decimal places
case class RoundTo2DP(num: Double):
  def round: Double = math.round(num*100)/100.0

//class to encapsulate methods
class HospitalBedAnalysis(private val data: Array[BedRecord]):

  //Method to find the state with the highest total of beds
  def calcStateWithMostBeds(): (String, Int) ={
    //to get total beds of each state (maximum number of beds provided by each state)
    val bedsOfState = data.groupBy(_.state).map {
      case (state, data) =>
        state -> data.map(_.beds).max
    }
    //to get the state with the highest max beds
    bedsOfState.maxBy(_._2)
  }

  //method to get average ratio of covid beds to hospital beds
  def calcCovidBedsRatio(): Double = {
    val totalBeds: Int = data.map(_.beds).sum
    val totalCovidBeds: Int = data.map(_.beds_covid).sum

    //validate to avoid zero division error
    if (totalBeds == 0 || totalCovidBeds == 0) {
      0.0
    } else {
      totalCovidBeds.toDouble / totalBeds.toDouble
    }
  }

  def getAvgAdmissionByState(): Map[String, (Double, Double, Double)] = {
    val groupedData = data.groupBy(_.state)

    groupedData.map { case (state, data) =>
      try {
        val totalSuspectedAdmitted = data.map(_.admitted_pui).sum.toDouble
        val totalCovidAdmitted = data.map(_.admitted_covid).sum.toDouble
        val totalNonCovidAdmitted = data.map(record => record.admitted_total - record.admitted_covid).sum.toDouble
        val count = data.length

        if (count == 0) throw new ArithmeticException("There are no records for state" + state)

        val avgAdmitted = totalSuspectedAdmitted / count
        val avgCovidAdmitted = totalCovidAdmitted / count
        val avgNonCovidAdmitted = totalNonCovidAdmitted / count

        state -> (avgAdmitted, avgCovidAdmitted, avgNonCovidAdmitted)
      }

      catch {
        case e: Exception =>
          println(s"Error processing state $state: ${e.getMessage}")
          state -> (0.0, 0.0, 0.0)
      }
    }
  }

object Assignment2 extends App:

  //reads .csv files into an array of each record as a String
  val sourceFile = Source.fromFile("src/main/resources/hospital.csv")
  val records = sourceFile.getLines.drop(1).toArray

  //parse data into case class BedRecord
  val data = records.map{
    line =>
      val cols = line.split(",")
      BedRecord(
        date = cols(0),
        state = cols(1),
        beds = cols(2).toInt,
        beds_covid = cols(3).toInt,
        beds_noncrit = cols(4).toInt,
        admitted_pui = cols(5).toInt,
        admitted_covid = cols(6).toInt,
        admitted_total= cols(7).toInt,
        discharged_pui = cols(8).toInt,
        discharged_covid = cols(9).toInt,
        discharged_total = cols(10).toInt,
        hosp_covid = cols(11).toInt,
        hosp_pui = cols(12).toInt,
        hosp_noncovid = cols(13).toInt
      )
  }

  val analysis = new HospitalBedAnalysis(data)

  //Question 1: Which state has the highest total hospital bed ?
  val (stateName, numberOfBeds) = analysis.calcStateWithMostBeds()
  println(s"Question 1: $stateName has the highest number of beds at $numberOfBeds beds.")

  //Question 2: What are the ratio of bed dedicated for COVID-19 to total of available hospital bed in the dataset ?
  val covidBedRatio = RoundTo2DP(analysis.calcCovidBedsRatio()).round
  println("----")
  println(s"Question 2: The average ratio of beds used for covid-19 is $covidBedRatio.")

  //Question 3: What are the average of individuals in category x where x can be suspected COVID-19 positive, or non-COVID is being admitted to hospital for each state ?
  println("----")
  println(s"Question 3: The average of individuals in category x where x can be suspected COVID-19 positive, or non-COVID is being admitted to hospital for each state are:")
  println("")
  val getAvgAdmissionByState = analysis.getAvgAdmissionByState()
  getAvgAdmissionByState.foreach { case (state, (avgSuspectedAdmitted, avgCovidAdmitted, avgNonCovidAdmitted)) =>
    println(s"State: $state")
    println(s"Average number of suspected admission = ${RoundTo2DP(avgSuspectedAdmitted).round}")
    println(s"Average number of COVID admission = ${RoundTo2DP(avgCovidAdmitted).round}")
    println(s"Average number of non-COVID admission = ${RoundTo2DP(avgNonCovidAdmitted).round}")
    println()
  }

  sourceFile.close()

end Assignment2

