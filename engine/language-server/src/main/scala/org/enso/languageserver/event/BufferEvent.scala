package org.enso.languageserver.event

import org.enso.languageserver.filemanager.Path

sealed trait BufferEvent extends Event

case class BufferCreated(path: Path) extends BufferEvent

case class BufferClosed(path: Path) extends BufferEvent
