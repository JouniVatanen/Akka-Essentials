package part2actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import part2actors.ChangingActorBehavior.Mom.MomStart

object ChangingActorBehavior extends App {

  object FuzzyKid {
    case object KidAccept
    case object KidReject
    val HAPPY = "happy"
    val SAD = "sad"
  }
  class FuzzyKid extends Actor {
    import FuzzyKid._
    import Mom._

    // internal state of the kid
    var state = HAPPY
    override def receive: Receive = {
      case Food(VEGETABLE) => state = SAD
      case Food(CHOCOLATE) => state = HAPPY
      case Ask(_) =>
        if (state == HAPPY) sender() ! KidAccept
        else sender() ! KidReject
    }
  }
  class StatelessFuzzyKid extends Actor {
    import FuzzyKid._
    import Mom._

    override def receive: Receive = happyReceive

    def happyReceive: Receive = {
      case Food(VEGETABLE) => context.become(sadReceive) // change my receive handler to sadReceive
      case Food(CHOCOLATE) =>
      case Ask(_) => sender() ! KidAccept
    }
    def sadReceive: Receive = {
      case Food(VEGETABLE) => context.become(sadReceive, false)// stay sad
      case Food(CHOCOLATE) => context.unbecome()// change my receive handler to happyReceive
      case Ask(_) => sender ! KidReject
    }
  }

  object Mom {
    case class MomStart(kidRef: ActorRef)
    case class Food(food: String)
    case class Ask(message: String) // do you want to play
    val VEGETABLE = "veggies"
    val CHOCOLATE = "chocolate"
  }
  class Mom extends Actor {
    import Mom._
    import FuzzyKid._

    override def receive: Receive = {
      case MomStart(kidRef) =>
        // test for interaction
        kidRef ! Food(VEGETABLE)
        kidRef ! Food(VEGETABLE)
        kidRef ! Food(CHOCOLATE)
        kidRef ! Food(CHOCOLATE)
        kidRef ! Ask("do you want to play?")
      case KidAccept => println("Yay, my kid is happy!")
      case KidReject => println("My kid is sad, but at least he's healthy")
    }
  }
  val system = ActorSystem("changingActorBehaviorDemo")
  val fuzzyKid = system.actorOf(Props[FuzzyKid], "fuzzyKid")
  val statelessFuzzyKid = system.actorOf(Props[StatelessFuzzyKid], "statelessFuzzyKid")
  val mom = system.actorOf(Props[Mom], "mom")

  mom ! MomStart(statelessFuzzyKid)

  /*
    mom receives MomStart
      kid receives Food(veg) -> kid will change handler to sadReceive
      kid receives Ask(play?) -> kid replies with sadReceive handler
    mom receives kidReject
   */

}