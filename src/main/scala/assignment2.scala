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

//class to encapsulate methods
class HospitalBedAnalysis(private val data: List[BedRecord]):

  //Method to find the state with the highest total of beds
  def getStateWithMostBeds(): (String, Int) ={
    //to get total beds of each state (maximum number of beds provided by each state)
    val bedsOfState = data.groupBy(_.state).map {
      case (state, data) =>
        state -> data.map(_.beds).max
    }
    //to get the state with the highest max beds
    bedsOfState.maxBy(_._2)
  }

  //method to get average ratio of covid beds to hospital beds rounded to two d.p.
  def getCovidBedsRatio(): (Double) = {
    val totalBeds : Int = data.map(_.beds).sum
    val totalCovidBeds: Int = data.map(_.beds_covid).sum

    //round to 2 decimal places and return
    math.round((totalCovidBeds.toDouble / totalBeds.toDouble) * 100) / 100.0
  }

  //method to get average admitted of category x
  def getAverageAdmittedOfX(): Array[(String, Double, Double, Double)] = {
    //group data by state
    data.groupBy(_.state).map { case (state, data) =>
      (
        state,
        //to get average admissions in suspected category
        math.round(data.map(_.admitted_pui).sum.toDouble / data.length.toDouble).toDouble,

        //to get average admissions in Covid category
        math.round(data.map(_.admitted_covid).sum.toDouble / data.length.toDouble).toDouble,

        //to get average admissions in Non-covid category
        ((data.map(_.admitted_total).sum.toDouble - data.map(_.admitted_covid).sum.toDouble) / data.length.toDouble).toDouble
      )
    }.toArray
  }

object Assignment2 extends App:

  //reads .csv files into an array of each record as a String
  val filepath = "src/main/resources/hospital.csv"
  val records = Source.fromFile(filepath).getLines.drop(1).toList

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

  val record1 = data(0)
  println(record1)

  //Question 1: Which state has the highest total hospital bed ?
  val (stateName, numberOfBeds) = HospitalBedAnalysis(data).getStateWithMostBeds()
  println(s"Question 1: $stateName has the highest number of beds at $numberOfBeds beds.")

  //Question 2: What are the ratio of bed dedicated for COVID-19 to total of available hospital bed in the dataset ?
  val covidBedRatio = HospitalBedAnalysis(data).getCovidBedsRatio()
  println(s"Question 2: The average ratio of beds used for covid-19 is $covidBedRatio.")

  //Question 3: What are the averages of individuals in category x where x can be suspected/probable, COVID-19 positive, or non-COVID is being admitted to hospitals for each state?
  /**println(s"Question 3: The average number of individuals of category X in each state: ")
  val averageAdmittedOfX = HospitalBedAnalysis(data).getAverageAdmittedOfX()
  averageAdmittedOfX.foreach { case (state, xSuspected, xCovid, xNonCovid) =>
    println("----")
    println (s"State = $state")
    println (s"The average number of suspected admissions is $xSuspected")
    println (s"The average number of covid admissions is $xCovid")
    println (s"The average number of non-covid admissions is $xNonCovid")
    println("----")
  }**/


end Assignment2

