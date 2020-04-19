package mud

import scala.collection.mutable.ListBuffer

import org.junit.Assert.assertEquals
import org.junit.Before
import org.junit.Test

class TestPlayer {
  private var player: Player = null
  private var sword: Item = null
  
  @Before def getRoom(): Unit = {
    sword = Item("sword", "A long pointy thing to slice and dice.", 10, 1)
//    player = Player(ListBuffer(sword), Room.start)
  }
  
  @Test def testGetFromInventory: Unit = {
    assertEquals(None, player.getFromInventory("Homework"))
    val gottenSword = player.getFromInventory("sword").get
    assertEquals(sword, gottenSword)
    assertEquals(None, player.getFromInventory("sword"))
  }
  
  @Test def testAddToInventory: Unit = {
    assertEquals(None, player.getFromInventory("Homework"))
    val item = Item("Homework", "Fun stuff you do for Dr. Lewis.", 0, 0)
    player.addToInventory(item)
    assertEquals(item, player.getFromInventory("Homework"))
  }
}