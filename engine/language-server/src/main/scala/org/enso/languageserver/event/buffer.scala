package org.enso.languageserver.event

import org.enso.languageserver.filemanager.Path

object buffer {

  case class BufferCreated(path: Path) extends Event

  case class BufferClosed(path: Path) extends Event

}
