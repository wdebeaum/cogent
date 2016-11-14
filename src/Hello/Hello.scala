package TRIPS.Hello

/*
 * Note: do not write "import TRIPS.Foo._" where Foo is written in Java. This
 * will cause scalac to erroneously traverse the TRIPS symlink in the Foo
 * directory and try to look up subsequent imports via the wrong path.
 * For example, if below we had this instead:
 *   import TRIPS.KQML._
 *   import TRIPS.TripsModule.StandardTripsModule
 * scalac would load everything from TRIPS.KQML, including TRIPS.KQML.TRIPS.
 * Then it would try to load TRIPS.KQML.TRIPS.TripsModule.StandardTripsModule,
 * and fail because that's the wrong package for that class.
 */
import TRIPS.KQML.KQMLPerformative
import TRIPS.KQML.KQMLList
import TRIPS.TripsModule.StandardTripsModule

/**
  * Hello TRIPS module - replies to hello requests with hello tells.
  * Sending this: (request :content (hello) :sender fred)
  * Gets this reply: (tell :content (hello fred) :receiver fred)
  */
class Hello(argv: Array[String]) 
    extends StandardTripsModule(argv) {

  override def init() = {
    name = "hello"
    super.init()
    send(KQMLPerformative.fromString(
      "(subscribe :content (request &key :content (hello . *)))"))
    ready()
  }

  override def receiveRequest(msg: KQMLPerformative, content: Any): Unit = {
    val contentList = content match {
      case c: KQMLList => c
      case _ => {
	errorReply(msg, "expected :content to be a list")
	return
      }
    }
    val verb = contentList.get(0).toString().toLowerCase()
    verb match {
      case "hello" => {
	val replyMsg = new KQMLPerformative("tell")
	val replyContent = new KQMLList()
	replyContent.add("hello")
	val sender = msg.getParameter(":sender")
	if (sender != null) {
	  replyContent.add(sender)
	}
	replyMsg.setParameter(":content", replyContent)
	reply(msg, replyMsg)
      }
      case _ => errorReply(msg, "unknown request verb " + verb)
    }
  }
}

object Hello {
  def main(argv: Array[String]) = {
    new Hello(argv).run()
  }
}

