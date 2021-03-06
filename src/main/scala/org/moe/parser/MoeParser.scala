package org.moe.parser

import scala.util.parsing.combinator._

import org.moe.ast._
import org.moe.runtime._

object MoeParser extends MoeProductions {
  def getEntryPoint: Parser[AST] = statements

  // Parser wrapper -- indicates the start node
  def parseFromEntry(input: String): StatementsNode = {
    def error_msg(msg: String, next: Input) = "[" + next.pos + "] error: " + msg + "\n\n" + next.pos.longString

    parseAll(getEntryPoint, input) match {
      case Success(result, _)   => result.asInstanceOf[StatementsNode]
      case NoSuccess(msg, next) => if (next.atEnd)
                                    throw new MoeErrors.ParserInputIncomplete(error_msg(msg, next))
                                  else
                                    throw new MoeErrors.ParserInputError(error_msg(msg, next))
    }
  }

  def parseStuff(input: String): StatementsNode = parseFromEntry(input)

  // helper method to test with any parser combinator
  def testParser(input: String, parser: Parser[AST] = getEntryPoint): AST = {
    def error_msg(msg: String, next: Input) = "[" + next.pos + "] error: " + msg + "\n\n" + next.pos.longString

    parseAll(parser, input) match {
      case Success(result, _)   => result
      case NoSuccess(msg, next) => if (next.atEnd)
                                    throw new MoeErrors.ParserInputIncomplete(error_msg(msg, next))
                                  else
                                    throw new MoeErrors.ParserInputError(error_msg(msg, next))
    }
  }

}

