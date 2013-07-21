package fr.javatic.tron.displayEngine

import fr.javatic.tron.gameModel.{Player, Moto}

abstract trait Renderer {
  def render(motos: Iterable[Moto]): Unit

  def displayLoser(player: Player): Unit
}
