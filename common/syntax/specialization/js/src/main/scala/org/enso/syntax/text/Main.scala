package org.enso.syntax.text

import org.enso.flexer.Reader
import org.enso.syntax.text

import scala.scalajs.js.annotation._
import scala.scalajs.js

import io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

object Parse {
    @JSExportTopLevel("parse")
    def parse(input: String): String = {
      try {
        new text.Parser().run(new Reader(input)).toJson().noSpacesSortKeys
      } catch {
        // FIXME We wrap the error message in JavaScriptException, so that javascript
        //  can display it. This is no longer needed in scalajs 1.0
        case e : Throwable => throw js.JavaScriptException(e.getMessage)
      }
    }

    @JSExportTopLevel("parseWithIDs")
    def parse(input: String, idsJSON: String): String = {
      try {
        val ids = decode[Parser.IDMap](idsJSON).getOrElse {
          throw new Exception("Could not decode IDMap from json.")
        }
        new text.Parser().run(new Reader(input), ids).toJson().noSpacesSortKeys
      } catch {
        // FIXME We wrap the error message in JavaScriptException, so that javascript
        //  can display it. This is no longer needed in scalajs 1.0
        case e : Throwable => throw js.JavaScriptException(e.getMessage)
      }
    }
}
