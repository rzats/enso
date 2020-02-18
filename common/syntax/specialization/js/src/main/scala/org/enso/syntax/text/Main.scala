package org.enso.syntax.text

import org.enso.flexer.Reader
import org.enso.syntax.text

import scala.scalajs.js.annotation._
import scala.scalajs.js
import io.circe.parser._
import org.enso.data.{Index, Size, Span}

object Parse {
  @JSExportTopLevel("parse")
  def parse(input: String, idsJSON: String): String = {
    try {
      val ids = decode[Seq[((Int, Int), AST.ID)]](idsJSON)
        .getOrElse {
          throw new Exception("Could not decode IDMap from json.")
        }
        .map {
          case ((ix, len), id) => (Span(Index(ix), Size(len)), id)
        }
      new text.Parser().run(new Reader(input), ids).toJson().noSpacesSortKeys
    } catch {
      // FIXME We wrap the error message in JavaScriptException, so that javascript
      //  can display it. This is no longer needed in scalajs 1.0
      case e: Throwable => throw js.JavaScriptException(e.getMessage)
    }
  }
}
