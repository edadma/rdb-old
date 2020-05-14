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

    oql.pretty(oql.query("character", conn)) shouldBe
      """
        |[
        |  {
        |    "char_id": 4,
        |    "name": "Worf, Son of Mogh",
        |    "home": {
        |      "plan_id": 1,
        |      "name": "Earth",
        |      "climate": "not too bad"
        |    },
        |    "species": {
        |      "spec_id": 4,
        |      "name": "Klingon",
        |      "lifespan": 150,
        |      "origin": {
        |        "plan_id": 4,
        |        "name": "Qo'noS",
        |        "climate": "turbulent"
        |      }
        |    }
        |  },
        |  {
        |    "char_id": 1,
        |    "name": "James Tiberius Kirk",
        |    "home": {
        |      "plan_id": 1,
        |      "name": "Earth",
        |      "climate": "not too bad"
        |    },
        |    "species": {
        |      "spec_id": 1,
        |      "name": "Human",
        |      "lifespan": 71,
        |      "origin": {
        |        "plan_id": 1,
        |        "name": "Earth",
        |        "climate": "not too bad"
        |      }
        |    }
        |  },
        |  {
        |    "char_id": 6,
        |    "name": "Lwaxana Troi",
        |    "home": {
        |      "plan_id": 3,
        |      "name": "Betazed",
        |      "climate": "awesome weather"
        |    },
        |    "species": {
        |      "spec_id": 3,
        |      "name": "Betazoid",
        |      "lifespan": 120,
        |      "origin": {
        |        "plan_id": 3,
        |        "name": "Betazed",
        |        "climate": "awesome weather"
        |      }
        |    }
        |  },
        |  {
        |    "char_id": 2,
        |    "name": "Spock",
        |    "home": {
        |      "plan_id": 1,
        |      "name": "Earth",
        |      "climate": "not too bad"
        |    },
        |    "species": {
        |      "spec_id": 2,
        |      "name": "Vulcan",
        |      "lifespan": 220,
        |      "origin": {
        |        "plan_id": 2,
        |        "name": "Vulcan",
        |        "climate": "pretty hot"
        |      }
        |    }
        |  },
        |  {
        |    "char_id": 7,
        |    "name": "Natasha Yar",
        |    "home": {
        |      "plan_id": 5,
        |      "name": "Turkana IV",
        |      "climate": null
        |    },
        |    "species": {
        |      "spec_id": 1,
        |      "name": "Human",
        |      "lifespan": 71,
        |      "origin": {
        |        "plan_id": 1,
        |        "name": "Earth",
        |        "climate": "not too bad"
        |      }
        |    }
        |  },
        |  {
        |    "char_id": 5,
        |    "name": "Kurn, Son of Mogh",
        |    "home": {
        |      "plan_id": 4,
        |      "name": "Qo'noS",
        |      "climate": "turbulent"
        |    },
        |    "species": {
        |      "spec_id": 4,
        |      "name": "Klingon",
        |      "lifespan": 150,
        |      "origin": {
        |        "plan_id": 4,
        |        "name": "Qo'noS",
        |        "climate": "turbulent"
        |      }
        |    }
        |  },
        |  {
        |    "char_id": 3,
        |    "name": "Deanna Troi",
        |    "home": {
        |      "plan_id": 1,
        |      "name": "Earth",
        |      "climate": "not too bad"
        |    },
        |    "species": {
        |      "spec_id": 3,
        |      "name": "Betazoid",
        |      "lifespan": 120,
        |      "origin": {
        |        "plan_id": 3,
        |        "name": "Betazed",
        |        "climate": "awesome weather"
        |      }
        |    }
        |  }
        |]
      """.trim.stripMargin
  }

}
