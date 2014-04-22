package dk.itu.wsq.cases.raw

import dk.itu.wsq._

case class RawNode(val children: Seq[RawNode]) extends Node
