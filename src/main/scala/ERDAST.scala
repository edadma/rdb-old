package xyz.hyperreal.rdb_sjs

abstract class ERDAST
case class DefinitionERD( blocks: List[BlockERD] ) extends ERDAST

abstract class BlockERD extends ERDAST
case class TypeBlockERD( name: String, )