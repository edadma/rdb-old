package xyz.hyperreal.rdb


object Main extends App {

	val p = new RQLParser

	println( p(""" {[a, b] (1, 2) (3, 4)} """) )
}