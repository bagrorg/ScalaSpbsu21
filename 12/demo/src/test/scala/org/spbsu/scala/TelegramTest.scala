package org.spbsu.scala

import org.scalatest.funsuite.AnyFunSuite
import cats.instances.future._
import cats.syntax.functor._
import com.bot4s.telegram.api.RequestHandler
import com.bot4s.telegram.api.declarative.Commands
import com.bot4s.telegram.clients.{FutureSttpClient, ScalajHttpClient}
import com.bot4s.telegram.future.{Polling, TelegramBot}
import slogging.{LogLevel, LoggerConfig, PrintLoggerFactory}
import com.bot4s.telegram.api.RequestHandler
import com.bot4s.telegram.clients.FutureSttpClient
import com.bot4s.telegram.future.TelegramBot
import com.softwaremill.sttp.SttpBackend
import com.softwaremill.sttp.okhttp.OkHttpFutureBackend

import java.io.FileInputStream
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

class TelegramTest extends AnyFunSuite {

  val token: String = new String(new FileInputStream("TG_TOKEN").readAllBytes())


  test("send TG message") {

  }

}
