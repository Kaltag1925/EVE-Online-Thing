package mud

case class Item(name: String, desc: String, damage: Int, speed: Int) {
  def lowerName = name.toLowerCase()
}

object NoItem extends Item("ship", "", 2, 10)