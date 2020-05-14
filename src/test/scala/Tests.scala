package xyz.hyperreal.rdb_sjs

import org.scalatest._
import freespec.AnyFreeSpec
import matchers.should.Matchers

import scala.scalajs.js
import js.Dynamic.{global => g}

class Tests extends AnyFreeSpec with Matchers {

  private val fs = g.require("fs")

  private def readFile(name: String) = {
    fs.readFileSync(name).toString
  }

  "basic tests" in {
    val conn = new Connection {
      load(readFile("samples/star-trek.tab"), doubleSpaces = true)
    }
    val oql = new OQL(readFile("samples/star-trek.erd"))

    oql
      .query(
        "character { name species.origin.name } [species.name = 'Betazoid']",
        conn) shouldBe
      List(
        Map("name" -> "Deanna Troi",
            "species" -> Map("origin" -> Map("name" -> "Betazed"))),
        Map("name" -> "Lwaxana Troi",
            "species" -> Map("origin" -> Map("name" -> "Betazed")))
      )
  }

}
