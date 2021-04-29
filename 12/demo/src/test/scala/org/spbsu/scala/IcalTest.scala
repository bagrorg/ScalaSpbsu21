package org.spbsu.scala

import net.fortuna.ical4j.data.CalendarBuilder
import org.scalatest.funsuite.AnyFunSuite

import java.io.FileInputStream
import net.fortuna.ical4j.util.MapTimeZoneCache
import org.scalatest.BeforeAndAfter

class IcalTest extends AnyFunSuite with BeforeAndAfter {

  before {
    System.setProperty("net.fortuna.ical4j.timezone.cache.impl", classOf[MapTimeZoneCache].getName)
    System.setProperty("ical4j.parsing.relaxed", "true")
  }

  implicit class FISExt(is: FileInputStream) {
    def filteringDSTAMP: PrefixFilterInputStream = new PrefixFilterInputStream(is) {
      override def excludeLine(line: String): Boolean = line.startsWith("DTSTAMP")
    }
  }

  test("read ical from file") {

  }

  test("read ical from stream over http") {

  }
}
