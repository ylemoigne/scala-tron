package fr.javatic.tron

import scala.actors.{TIMEOUT, Actor}

class TimerActor(val timeout: Long, val who: Actor, val message: Any) extends Actor {

  private case object Stop

  def act {
    loop {
      reactWithin(timeout) {
        case TIMEOUT => who ! message
        case Stop => exit()
      }
    }
  }

  override def start() = {
    if (getState == Actor.State.Terminated)
      restart()
    else
      super.start()

    this
  }

  def stop() = {
    this ! Stop
  }
}
