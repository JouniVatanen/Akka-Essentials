package part3testing

import scala.concurrent.duration._
import akka.actor.{Actor, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.{BeforeAndAfterAll, WordSpecLike}

import scala.util.Random

class BasicSpec extends TestKit(ActorSystem("BasicSpec"))
  with ImplicitSender
  with WordSpecLike
  with BeforeAndAfterAll {

  // setup
  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
  }

  import BasicSpec._

  "A simple actor" should {
    "send back the same message" in {
      // testing scenario
      val echoActor = system.actorOf(Props[SimpleActor])
      val message = "hello, test"
      echoActor ! message

      expectMsg(message) // akka.test.single-expect-default
      testActor
    }
  }

  "A blackhole actor" should {
    "send back some message" in {
      val blackhole = system.actorOf(Props[Blackhole])
      val message = "hello, test"
      blackhole ! message

      expectNoMessage(1 second)
    }
  }

  // message assetions
  "A lab test actor" should {
    val labTestActor = system.actorOf(Props[LabTestActor])

    "turn a string into uppercase" in {
      labTestActor ! "I love Akka"
      val reply = expectMsgType[String]

      assert(reply == "I LOVE AKKA")
    }
  }


}

object BasicSpec {

  class SimpleActor extends Actor {
    override def receive: Receive = {
      case message => sender() ! message
    }
  }
  class Blackhole extends Actor {
    override def receive: Receive = Actor.emptyBehavior
  }

  class LabTestActor extends Actor {
    val random = new Random()

    override def receive: Receive = {
      case "greeting" =>
        if (random.nextBoolean()) sender() ! "hi" else sender() ! "hello"
      case message: String => sender() ! message.toUpperCase()
    }
  }
}
