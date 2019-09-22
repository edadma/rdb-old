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

	println(
		s"""
			 |Welcome to rdb/$VERSION
			 |Type in expressions to have them evaluated.
			 |Type :help for more information.
		""".trim.stripMargin )

	while ({line = reader.readLine; line != null}) {
		val line1 = line.trim
		val com = line1 split "\\s+" toList

		try {
			com match {
				case List( ":help"|":h" ) =>
					println(
						"""
							|:help (h)                             print this summary
							|:quit (q)                             exit the REPL
							|:relations (r)                        print the relations currently accessible
							|:trace (t) on/off                     turn exception stack trace on or off
							|<RQL>                                 execute <RQL> statement (query or command)
							|/<SQL>                                execute <SQL> statement (query or command)
							|?<expression>                         evaluate <expression>
						""".trim.stripMargin )
				case List( ":load"|":l", file ) =>
					conn.loadFromFile( file )
				case List( ":quit"|":q" ) =>
					sys.exit
				case List( ":relations"|":r" ) =>
					println( "base relations: " + conn.baseRelations.keys.mkString(", ") )
					println( "variable relations: " + conn.variables.keys.mkString(", ") )
				case List( ":trace"|":t", "on" ) => stacktrace = true
				case List( ":trace"|":t", "off" ) => stacktrace = false
				case Nil|List( "" ) =>
				case _ if line1 startsWith "?" =>
				case _ =>
					printResult( if (line1 startsWith "/") conn.executeSQLStatement(line1 substring 1) else conn.executeRQLStatement(line1) )
			}
		} catch {
			case e: Exception =>
				if (stacktrace)
					e.printStackTrace( out )
				else
					out.println( e.getMessage )
		}

		out.println
	}

	def printResult( r: StatementResult ) =
		r match {
			case TupleseqResult( tupleseq ) =>
				if (tupleseq.nonEmpty) {
					val t =
						new TextTable {
							tupleseq.header match {
								case None =>
								case Some( h ) =>
									headerSeq( h )

									for (i <- 1 to h.length)
										if (tupleseq.types( i - 1 ).isInstanceOf[NumericalType])
											rightAlignment( i )
							}

							for (r <- tupleseq)
								rowSeq( r )
						}

					print( t )
				}

				tupleseq.size match {
					case 0 => println( "empty tuple sequence" )
					case 1 => println( "1 tuple" )
					case s => println( s"$s tuples" )
				}
			case RelationResult( rel ) =>
				val l = rel.collect

				if (l.nonEmpty) {
					val t =
						new TextTable {
							headerSeq( l.metadata.header map (_.column) )

							for (i <- 1 to l.metadata.header.length)
								if (l.metadata.header( i - 1 ).typ.isInstanceOf[NumericalType])
									rightAlignment( i )

							for (r <- l)
								rowSeq( r )
						}

					print( t )
				}

				l.size match {
					case 0 => println( "empty relation" )
					case 1 => println( "1 row" )
					case s => println( s"$s rows" )
				}
			case CreateResult( name ) => println( s"base relation '$name' was created" )
			case DropResult( name ) => println( s"base relation '$name' was droped" )
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
			case InsertResult( _, count ) =>
				println(
					count match {
						case 0 => "no rows were inserted"
						case 1 => "1 row was inserted"
						case _ => s"$count rows were inserted"
					} )
			case DeleteResult( 0 ) => println( "no rows were deleted" )
			case DeleteResult( 1 ) => println( "1 row was deleted" )
			case DeleteResult( count ) => println( s"$count rows were deleted" )
			case UpdateResult( 0 ) => println( "no updates were made" )
			case UpdateResult( 1 ) => println( "1 row was updated" )
			case UpdateResult( count ) => println( s"$count rows were updated" )
		}

}