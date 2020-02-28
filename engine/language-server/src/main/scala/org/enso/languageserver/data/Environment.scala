package org.enso.languageserver.data

import java.io.File
import java.util.UUID

import org.enso.languageserver.filemanager.{
  ContentRootNotFound,
  FileSystemFailure,
  Path
}

/**
  * The config of the running Language Server instance.
  *
  * @param contentRoots a mapping between content root id and absolute path to
  *                     the content root
  */
case class Config(contentRoots: Map[UUID, File] = Map.empty) {

  def findContentRoot(rootId: UUID): Either[FileSystemFailure, File] =
    contentRoots
      .get(rootId)
      .toRight(ContentRootNotFound)

}

/**
  * The state of the running Language Server instance.
  *
  * @param clients the list of currently connected clients.
  * @param openFiles the collection of all currently opened files.
  */
case class Environment(
  clients: List[Client],
  openFiles: Map[Path, OpenBuffer]
) {

  /**
    * Adds a new client to this `Env`
    * @param client the client to add.
    * @return a new version of the environment with the client added.
    */
  def addClient(client: Client): Environment = {
    copy(clients = client :: clients)
  }

  /**
    * Removes a client by id.
    *
    * @param clientId the id of the client to remove.
    * @return a new version of the environment with the client removed.
    */
  def removeClient(clientId: Client.Id): Environment =
    copy(clients = clients.filter(_.id != clientId))

  /**
    * Finds all the capability registrations accepted by a given predicate.
    *
    * @param predicate the predicate to test registrations against.
    * @return a list of matching registrations, together with the clients they
    *         are registered for.
    */
  def findCapabilitiesBy(
    predicate: CapabilityRegistration => Boolean
  ): List[(Client, CapabilityRegistration)] =
    clients.flatMap(
      client => client.capabilities.filter(predicate).map((client, _))
    )

  /**
    * Removes all registered capabilities matching a given predicate.
    *
    * @param predicate the predicate to match capabilities against.
    * @return a new version of `Env` without the capabilities matching the
    *         predicate and a list of all clients, together with capabilities
    *         that got removed for them.
    */
  def removeCapabilitiesBy(
    predicate: CapabilityRegistration => Boolean
  ): (Environment, List[(Client, List[CapabilityRegistration])]) = {
    val newClients = clients.map { client =>
      val (removedCapabilities, retainedCapabilities) =
        client.capabilities.partition(predicate)
      val newClient = client.copy(capabilities = retainedCapabilities)
      (newClient, removedCapabilities)
    }
    (copy(clients = newClients.map(_._1)), newClients)
  }

  /**
    * Modified a client at a given id.
    *
    * @param clientId the id of the client to modify.
    * @param modification the function used to modify the client.
    * @return a new version of this env, with the selected client modified by
    *         `modification`
    */
  def modifyClient(
    clientId: Client.Id,
    modification: Client => Client
  ): Environment = {
    val newClients = clients.map { client =>
      if (client.id == clientId) {
        modification(client)
      } else {
        client
      }
    }
    copy(clients = newClients)
  }

  /**
    * Grants a given client a provided capability.
    *
    * @param clientId the id of the client to grant the capability.
    * @param registration the capability to grant.
    * @return a new version of this env, with the capability granted.
    */
  def grantCapability(
    clientId: Client.Id,
    registration: CapabilityRegistration
  ): Environment =
    modifyClient(clientId, { client =>
      client.copy(capabilities = registration :: client.capabilities)
    })

  /**
    * Releases a capability from a given client.
    *
    * @param clientId the id of the client that releases the capability.
    * @param capabilityId the id of the capability registration to release.
    * @return a new version of this env, with the selected capability released.
    */
  def releaseCapability(
    clientId: Client.Id,
    capabilityId: CapabilityRegistration.Id
  ): Environment =
    modifyClient(clientId, { client =>
      client.copy(
        capabilities = client.capabilities.filter(_.id != capabilityId)
      )
    })

  /** Gets an open file buffer for a given path.
    *
    * @param path the file path.
    * @return the open file buffer, if it exists.
    */
  def getFile(path: Path): Option[OpenBuffer] = openFiles.get(path)

  /** Set a file buffer at a given path.
    *
    * @param path the file path.
    * @param buffer the buffer to use.
    * @return new environment, including the new buffer.
    */
  def setFile(path: Path, buffer: OpenBuffer): Environment =
    copy(openFiles = openFiles + (path -> buffer))

  /** Grants a [[CanEdit]] capability to a client at a given path, if there's no
    * other client already holding the capability.
    *
    * @param client the client being granted the capability.
    * @param path the path to grant the capability for.
    * @param idGenerator a random IDs generator.
    * @return the capability registration, if granted, and a new environment
    *         with the capability registered.
    */
  def grantCanEditIfVacant(
    client: Client.Id,
    path: Path
  )(
    implicit idGenerator: IdGenerator
  ): (Option[CapabilityRegistration], Environment) = {
    val existingWriteCapabilities = findCapabilitiesBy(
      _.capability match {
        case CanEdit(capabilityPath) => path == capabilityPath
        case _                       => false
      }
    )
    if (existingWriteCapabilities.isEmpty) {
      val capability = CapabilityRegistration(CanEdit(path))
      val newEnv     = grantCapability(client, capability)
      (Some(capability), newEnv)
    } else (None, this)
  }
}

object Environment {

  /**
    * Constructs an empty env.
    *
    * @return an empty env.
    */
  def empty: Environment = Environment(clients = List(), openFiles = Map())
}
