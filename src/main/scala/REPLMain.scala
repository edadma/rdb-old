package xyz.hyperreal.rdb

import java.io.PrintWriter

import collection.mutable.HashMap
import jline.console.ConsoleReader

import xyz.hyperreal.table.TextTable


object REPLMain extends App {

	val reader =
		new ConsoleReader {
			setExpandEvents( false )
			setBellEnabled( false )
			setPrompt( "> " )
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
						|:help                             print this summary
						|:quit                             exit the REPL
						|<RQL>                             execute <RQL> query
						|?<expression>                     evaluate <expression>
					""".trim.stripMargin.lines foreach out.println
				case List( ":quit"|":q" ) =>
					sys.exit
				case _ if line1 startsWith "?" =>
				case _ =>
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