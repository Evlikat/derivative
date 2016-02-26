package akkaex

import akka.actor.{PoisonPill, Actor, ActorSystem, Props}
/**
  * Created by rprokhorov on 2/26/16.
  */
object Application {

  def main(args: Array[String]) : Unit = {
    val system = ActorSystem.create("hello")
    val actorRef = system.actorOf(Props[HelloActor], "hello-actor")
    actorRef ! "hi!"
    actorRef ! "how's going?"
    actorRef ! PoisonPill
    actorRef ! "hi!"
  }
}

class HelloActor extends Actor {

  override def receive = {
    case "hi!" => println("hello")
    case _ => println("o_O")
  }
}