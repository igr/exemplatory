import scala.collection.immutable.HashMap

enum Color { case RED; case BLUE; case GREEN; case BLACK }

case class Toy(
  name: String,
  color: Color,
  broken: Boolean,
  price: Double
) {
  def expensive = price > 10.0
}

case class Stats(
  count: Int = 0,
  blueCount: Int = 0,
  colorCounts: HashMap[Color, Int] = HashMap.empty,
  brokenExpensiveCount: Int = 0
) {
  def aggregate(toy: Toy): Stats = Stats(
    this.count + 1,
    this.blueCount + (if (toy.color == Color.BLUE) 1 else 0),
    this.colorCounts.updated(toy.color, this.colorCounts.getOrElse(toy.color, 0) + 1),
    this.brokenExpensiveCount + (if (toy.broken && toy.expensive) 1 else 0),
  )
}

def processStats(toys: List[Toy], stats: Stats = Stats()): Stats = toys.match {
  case Nil => stats
  case toy::tail => processStats(tail, stats.aggregate(toy))
}

val toys = List(
  Toy("bear", Color.BLACK, false, 11.99),
  Toy("doll", Color.RED, false, 4.0),
  Toy("car", Color.BLUE, true, 9.99),
  Toy("truck", Color.BLUE, true, 13.00),
  Toy("spaceship", Color.GREEN, false, 25.0),
)

val stats = processStats(toys)

println(stats)