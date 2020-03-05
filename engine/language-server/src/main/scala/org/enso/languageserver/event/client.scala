package org.enso.languageserver.event

import org.enso.languageserver.data.Client

object client {

  sealed trait ClientEvent extends Event

  case class ClientConnected(client: Client) extends ClientEvent

  case class ClientDisconnected(clientId: Client.Id) extends ClientEvent

}
