package xyz.hyperreal.rdb_sjs

abstract class ERDAST
case class DefinitionERD(blocks: List[BlockERD]) extends ERDAST

abstract class BlockERD extends ERDAST
case class TypeBlockERD(name: Ident,
                        underlying: Ident,
                        condition: ExpressionRED)
    extends BlockERD

abstract class ExpressionRED extends ERDAST
