package org.enso.languageserver.buffer

import org.bouncycastle.jcajce.provider.digest.SHA3
import org.bouncycastle.util.encoders.Hex

object Sha3Digest extends ContentDigest {

  override def digest(content: String): String = {
    val digestSHA3 = new SHA3.Digest224()
    val hash       = digestSHA3.digest(content.getBytes("UTF-8"))
    Hex.toHexString(hash)
  }

}
