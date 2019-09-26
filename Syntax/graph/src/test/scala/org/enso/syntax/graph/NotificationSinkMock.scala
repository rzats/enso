package org.enso.syntax.graph

import scala.collection.mutable

final case class NotificationSinkMock() extends NotificationSink {
  val notificationsReceived: mutable.ArrayBuffer[API.Notification] =
    mutable.ArrayBuffer()

  override def notify(notification: API.Notification): Unit = {
    println(s"Got notification: $notification")
    notificationsReceived += notification
  }
}
