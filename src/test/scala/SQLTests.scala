package xyz.hyperreal.rdb_sjs

import org.scalatest._
import freespec.AnyFreeSpec
import matchers.should.Matchers

import Testing._

class SQLTests extends AnyFreeSpec with Matchers {

  "basic tests" in {
    sqlQuery("SELECT * FROM species", starTrekDB) shouldBe
      """
        | spec_id    name    lifespan  origin
        |       1  Human           71       1
        |       2  Vulcan         220       2
        |       3  Betazoid       120       3
        |       4  Klingon        150       4
        |       5  Horta         null       6
        |""".trim.stripMargin

    sqlQuery("SELECT * FROM species WHERE name = 'Betazoid'", starTrekDB) shouldBe
      """
        | spec_id    name    lifespan  origin
        |       3  Betazoid       120       3
        |""".trim.stripMargin

    sqlQuery("SELECT * FROM species WHERE NOT (lifespan < 100 OR lifespan > 200)", starTrekDB) shouldBe
      """
        | spec_id    name    lifespan  origin
        |       3  Betazoid       120       3
        |       4  Klingon        150       4
        |       5  Horta         null       6
        |""".trim.stripMargin
  }

  "order tests" in {
    sqlQuery("SELECT * FROM species ORDER BY lifespan", starTrekDB) shouldBe
      """
        | spec_id    name    lifespan  origin
        |       1  Human           71       1
        |       3  Betazoid       120       3
        |       4  Klingon        150       4
        |       2  Vulcan         220       2
        |       5  Horta         null       6
        |""".trim.stripMargin

    sqlQuery("SELECT * FROM species ORDER BY lifespan NULLS FIRST", starTrekDB) shouldBe
      """
        | spec_id    name    lifespan  origin
        |       5  Horta         null       6
        |       1  Human           71       1
        |       3  Betazoid       120       3
        |       4  Klingon        150       4
        |       2  Vulcan         220       2
        |""".trim.stripMargin

    sqlQuery("SELECT * FROM species ORDER BY lifespan NULLS LAST", starTrekDB) shouldBe
      """
        | spec_id    name    lifespan  origin
        |       1  Human           71       1
        |       3  Betazoid       120       3
        |       4  Klingon        150       4
        |       2  Vulcan         220       2
        |       5  Horta         null       6
        |""".trim.stripMargin

    sqlQuery("SELECT * FROM species ORDER BY lifespan DESC NULLS FIRST", starTrekDB) shouldBe
      """
        | spec_id    name    lifespan  origin
        |       5  Horta         null       6
        |       2  Vulcan         220       2
        |       4  Klingon        150       4
        |       3  Betazoid       120       3
        |       1  Human           71       1
        |""".trim.stripMargin

    sqlQuery("SELECT * FROM species ORDER BY lifespan DESC NULLS LAST", starTrekDB) shouldBe
      """
        | spec_id    name    lifespan  origin
        |       2  Vulcan         220       2
        |       4  Klingon        150       4
        |       3  Betazoid       120       3
        |       1  Human           71       1
        |       5  Horta         null       6
        |""".trim.stripMargin
  }

}
