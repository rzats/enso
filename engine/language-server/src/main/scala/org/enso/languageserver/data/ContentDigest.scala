package org.enso.languageserver.data

trait ContentDigest {

  def digest(content: String): String

}
