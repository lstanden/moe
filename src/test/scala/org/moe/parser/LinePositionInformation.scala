package org.moe.parser

import org.scalatest.{BeforeAndAfter, FunSuite}
import org.scalatest.matchers.ShouldMatchers
import org.moe.ast.AST
import org.moe.interpreter.guts.Utils
import scala.collection.mutable.ArrayBuffer
import scala.util.parsing.input.NoPosition

class LinePositionInformation extends FunSuite with BeforeAndAfter with ParserTestUtils with ShouldMatchers with Utils {
  def testCode =
    """#!moe
      my $foo = true;
      if ($foo) { print "hello" }
    """.stripMargin

  test("... verify line position information") {
    val result = MoeParser.parseStuff(testCode)
    var cb_count = 0
    var line_list: ArrayBuffer[Int] = new ArrayBuffer[Int]()
    val cb: (AST) => Unit = {ast: AST =>
      cb_count += 1
      ast.pos match {
        case NoPosition => None
        case _ => line_list += ast.pos.line
      }
    }
    walkAST(result, cb)

    // if statement
    assert(line_list(0) == 3)
  }
}
