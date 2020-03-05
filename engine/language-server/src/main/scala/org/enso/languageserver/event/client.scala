package org.enso.languageserver.event

import org.enso.languageserver.data.Client

object client {

  case class ClientConnected(client: Client)

  case class ClientDisconnected(clientId: Client.Id)

}
