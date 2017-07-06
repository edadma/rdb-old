package xyz.hyperreal.rdb

import java.io.PrintWriter

import jline.console.ConsoleReader

import xyz.hyperreal.table.TextTable


object REPLMain extends App {

	val reader =
		new ConsoleReader {
			setExpandEvents( false )
			setBellEnabled( false )
			setPrompt( "rdb> " )
		}
	val out = new PrintWriter( reader.getTerminal.wrapOutIfNeeded( System.out ), true )
	var line: String = _
	var stacktrace = false
	var conn = new Connection

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
					conn.executeStatement( line1 ) match {
						case RelationResult( rel ) =>
							val l = rel.collect
							val t =
								new TextTable {
									headerSeq( l.metadata.header map (_.column) )
									line

									for (i <- 1 to l.metadata.header.length)
										if (l.metadata.header( i - 1 ).typ.isInstanceOf[NumericalType])
											rightAlignment( i )

									for (r <- l)
										rowSeq( r )
								}

							print( t )

							l.size match {
								case 0 => println( "empty relation" )
								case 1 => println( "1 row" )
								case s => println( s"$s rows" )
							}
						case AssignResult( name, update, count ) =>
							println(
								(count, update) match {
									case (0, false) => s"empty variable relation '$name' was created"
									case (0, true) => s"variable relation '$name' was updated with empty relation"
									case (1, false) => s"variable relation '$name' was created with 1 row"
									case (1, true) => s"variable relation '$name' was updated with 1 row"
									case (_, false) => s"variable relation '$name' was created with $count rows"
									case (_, true) => s"variable relation '$name' was updated with $count rows"
								} )
						case InsertResult( _, count, created ) =>
							println(
								(count, created) match {
									case (0, Some(name)) => s"base relation '$name' was created"
									case (0, None) => "no rows were inserted"
									case (1, Some(name)) => s"base relation '$name' was created with 1 row"
									case (1, None) => "1 row was inserted"
									case (_, Some(name)) => s"base relation '$name' was created with $count rows"
									case (_, None) => s"$count rows were inserted"
								} )
						case DeleteResult( 0 ) => println( "no rows were deleted" )
						case DeleteResult( 1 ) => println( "1 row was deleted" )
						case DeleteResult( count ) => println( s"$count rows were deleted" )
					}
			}
		} catch {
			case e: Exception =>
				if (stacktrace)
					e.printStackTrace( out )
				else
					out.println( e )
		}

		out.println
	}

}