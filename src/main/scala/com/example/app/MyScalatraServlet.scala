package com.example.app

import org.json4s.{DefaultFormats, Formats}
import org.scalatra._
import org.scalatra.json._

case class Message(id: Int, name: String)

case class CreateMessage(text: String)
case class MessageCreated(id: Int)

case class UpdateMessage(text: String)
case class MessageUpdated(id: Int)

class MyScalatraServlet extends ScalatraServlet with JacksonJsonSupport{

  protected implicit lazy val jsonFormats: Formats = DefaultFormats

  //This collection represents a simple in-memory data source (i.e. it is mutable and not thread-safe)
  var messages: List[Message]  = List(Message(1, "Dis is message you read"), Message(2, "Next one"), Message(3, "Again!"))

  before() {
    contentType = formats("json")
  }

  get("/messages") {
    messages
  }

  get("/messages/:id") {
    messages.find(m => m.id == params("id").toInt) match {
      case Some(message)  => Ok(message)
      case None           => NotFound("User not found")
    }
  }

  post("/messages") {
    val message = parsedBody.extract[CreateMessage]
    val id = messages.foldLeft(0)( (maxId, m) => if (m.id > maxId) m.id else maxId ) + 1

    messages = Message(id, message.text) :: messages

    MessageCreated(id)
  }

  put("/messages/:id") {

    val id = params("id").toInt
    val indexes = messages.zipWithIndex.collect { case (Message(`id`, _), index) => index}
    if (indexes.isEmpty){
      NotFound("Message not found")
    }else{
      val i = indexes.head
      val newMessage = parsedBody.extract[UpdateMessage]

      messages = messages.updated(i, Message(id, newMessage.text))

      Ok(MessageUpdated(id))
    }


  }

  delete("/messages/:id") {
    messages = messages filter {m => m.id != params("id").toInt}
    Ok("Success")
  }

}
