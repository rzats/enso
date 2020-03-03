package org.enso.languageserver.buffer

trait ContentDigest {

  def digest(content: String): String

}
