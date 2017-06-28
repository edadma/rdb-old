package xyz.hyperreal.rdb

import java.io.PrintWriter

import collection.mutable.HashMap
import jline.console.ConsoleReader

import xyz.hyperreal.table.TextTable


object REPLMain extends App {

	val baseRelations = new HashMap[String, BaseRelation]

	val reader =
		new ConsoleReader {
			setExpandEvents( false )
			setBellEnabled( false )
			setPrompt( "rdb> " )
		}
	val out = new PrintWriter( reader.getTerminal.wrapOutIfNeeded( System.out ), true )
	var line: String = _
	var stacktrace = false

	s"""
		 |Welcome to rdb/$VERSION
		 |Type in expressions to have them evaluated.
		 |Type :help for more information.
	""".trim.stripMargin.lines foreach println
	println

	while ({line = reader.readLine; line != null}) {
		val line1 = line.trim
		val com = line1 split "\\s+" toList

		try {
			com match {
				case List( ":help"|":h" ) =>
					"""
						|:help (h)                             print this summary
						|:quit (q)                             exit the REPL
						|:trace (t) on/off                     turn exception stack trace on or off
						|<RQL>                                 execute <RQL> query
						|?<expression>                         evaluate <expression>
					""".trim.stripMargin.lines foreach out.println
				case List( ":quit"|":q" ) =>
					sys.exit
				case List( ":trace"|":t", "on" ) => stacktrace = true
				case List( ":trace"|":t", "off" ) => stacktrace = false
				case Nil|List( "" ) =>
				case _ if line1 startsWith "?" =>
				case _ =>
					val p = new RQLParser
					val ast = p.parseFromString( line1, p.statement )

					ast match {
						case InsertStatement( target, relation ) =>
							if (!baseRelations.contains( target.name ))
								problem( target.pos, "base relation not found" )

							
						case r: RelationExpression =>
							val rel = RQLEvaluator.evalRelation( r )
							val t =
								new TextTable {
									headerSeq( rel.header map (_.name) )
									line

									for (i <- 1 to rel.header.length)
										if (rel.header(i - 1).typ.isInstanceOf[NumericalType])
											rightAlignment( i )

									for (r <- rel)
										rowSeq( r )
								}

							print( t )
					}
			}
		}
		catch
			{
				case e: Exception =>
					if (stacktrace)
						e.printStackTrace( out )
					else
						out.println( e )
			}

		out.println
	}

}