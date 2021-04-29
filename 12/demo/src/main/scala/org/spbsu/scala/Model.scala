package org.spbsu.scala

import java.time.LocalDateTime

case class User(id: Int, name: String)
case class Event(id: Int, user: User, data: String, start: LocalDateTime, end: LocalDateTime)
