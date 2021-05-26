package xyz.hyperreal.rdb_sjs

import xyz.hyperreal.table.TextTable

object Testing {

  def sqlQuery(sql: String, conn: Connection): String = {
    val l = conn.executeSQLQuery(sql)
    val res =
      new TextTable(headerUnderlined = false, headerBold = false) {
        headerSeq(l.metadata.header map (_.column))

        for (i <- 1 to l.metadata.header.length)
          if (l.metadata.header(i - 1).typ.isInstanceOf[NumericalType])
            rightAlignment(i)

        for (r <- l)
          rowSeq(r)
      }.toString

    res.replace(" \n", "\n")
  }

  val starTrekDB: Connection =
    new Connection {
      load(
        """
        |planet
        | plan_id: integer, pk  name: text   climate: text  
        | 1                     Earth       not too bad     
        | 2                     Vulcan      pretty hot      
        | 3                     Betazed     awesome weather 
        | 4                     Qo'noS      turbulent       
        | 5                     Turkana IV  null
        | 6                     Janus VI    boiling atmosphere
        |
        |species
        | spec_id: integer, pk  name: text  lifespan: integer  origin: integer, fk, planet, plan_id 
        | 1                     Human       71                 1                                    
        | 2                     Vulcan      220                2                                    
        | 3                     Betazoid    120                3                                    
        | 4                     Klingon     150                4
        | 5                     Horta       null               6
        |
        |character
        | char_id: integer, pk      name: text       home: integer, fk, planet, plan_id  species: integer, fk, species, spec_id 
        | 1                     James Tiberius Kirk  1                                   1                                      
        | 2                     Spock                1                                   2                                      
        | 3                     Deanna Troi          1                                   3                                      
        | 4                     Worf, Son of Mogh    1                                   4                                      
        | 5                     Kurn, Son of Mogh    4                                   4                                      
        | 6                     Lwaxana Troi         3                                   3                                      
        | 7                     Natasha Yar          5                                   1
        |""".stripMargin,
        doubleSpaces = true
      )
    }

}
