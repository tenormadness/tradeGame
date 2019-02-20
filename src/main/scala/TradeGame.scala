
import plotly._
import element._
import layout._
import Plotly._
import plotly.Plotly

import scala.util.Random

object TradeGame extends App {

  type Day = Int
  type Dollars = Double

  case class TeamStrategy(
                           teamName: String,
                           strategy: (Day, Dollars) => Double,
                           held: Seq[Int] = Seq(100),
                           cash: Seq[Double] = Seq(0)
                         )

  val initialPrice = 100.0

  def currentPrice(day: Day, lastPrice: Dollars, totalTrade: Dollars): Double = {

    val naturalPrice = initialPrice - day.toDouble / 2

    val relaxation = (lastPrice - naturalPrice) / 10.0

    naturalPrice + relaxation + totalTrade / 5.0

  }

  val days = (1 to 100 by 1).toList

  val noTradesPrice  = days.foldLeft(List(100.0)) { (prices, day) => prices :+ currentPrice(day, prices.last, 0.0) }

  val noTradesPricePlot = Scatter(days, noTradesPrice)//.plot(title = "unperturbed price move")


  ///////////////

  val initialTeamState = Seq(
    TeamStrategy("example", (day, price) => -1.0),
    TeamStrategy("all last day", (day, price) => if (day > 99.5) -100.0 else 0.0),
    TeamStrategy("all first", (day, price) => if (day == 1) -100.0 else 0.0)
  )

  ///////

  val (price, teamStats) = days.foldLeft((List[Dollars](), initialTeamState)) { case ((prices, teamState), day) =>

    val todayPrice = prices.lastOption.getOrElse(initialPrice)
    val teamTrades = teamState.map { team =>

      val proposedTrade = team.strategy(day, todayPrice)
      val trade = if ((team.held.last + proposedTrade) < 0.0) {
        -team.held.last
      } else if ((-proposedTrade * todayPrice + team.cash.last) < 0.0) {
        team.cash.last / todayPrice
      } else {
        proposedTrade
      }

      team -> trade

    }

    val todayTrade = teamTrades.map(_._2).sum

    val newPrice = currentPrice(day, todayPrice, todayTrade)

    val newTeamStatus = teamTrades.map { case (team, trade) =>

      team.copy(held = team.held :+ (team.held.last + trade).toInt, cash = team.cash :+ (team.cash.last - newPrice * trade))

    }

    (prices :+ newPrice, newTeamStatus)
  }

  val pricePlot = Scatter(days, price, "realized price", line = Line(width = 2.0, color = Color.StringColor("red")))

  Seq(noTradesPricePlot, pricePlot).plot(title = "price history")

  val teamPlots = teamStats.foreach { team =>

    val initAssets = team.cash.head + team.held.head * initialPrice
    val returns = (team.cash zip team.held zip price).map {case ((cash, held), price) => cash + held * price - initAssets}

    Seq(
      Scatter(days, price, name = "Price", line = Line(width = 1.0, color = Color.StringColor("black"))),
      Scatter(days, team.held, name = "Held", line = Line(width = 2.0, color = Color.StringColor("blue"))),
      Scatter(days, team.cash, name = "Cash", line = Line(width = 2.0, color = Color.StringColor("green"))),
      Scatter(days, returns, name = "returns", line = Line(width = 4.0, color = Color.StringColor("red")))
    ).plot(title = team.teamName + " return = " + returns.last.toString)

  }

}
