package org.enso.languageserver.buffer

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers

class Sha3DigestSpec extends AnyFlatSpec with Matchers {

  "A Sha3Digest" should "produce SHA3-224 digest" in {
    Sha3Digest.digest(" ") mustBe "4cb5f87b01b38adc0e6f13f915668c2394cb1fb7a2795635b894dda1"
    Sha3Digest.digest("The quick brown fox jumps over the lazy dog") mustBe "d15dadceaa4d5d7bb3b48f446421d542e08ad8887305e28d58335795"
  }

}
