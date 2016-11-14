package TRIPS.Hello;

import java.io.IOException;
import TRIPS.KQML.*;
import TRIPS.TripsModule.StandardTripsModule;

/**
 * Hello TRIPS module - replies to hello requests with hello tells.
 * Sending this: (request :content (hello) :sender fred)
 * Gets this reply: (tell :content (hello fred) :receiver fred)
 */
public class Hello extends StandardTripsModule {
  public Hello(String[] argv) {
    super(argv);
  }

  public void init() {
    name = "hello";
    super.init();
    try {
      send(KQMLPerformative.fromString(
	"(subscribe :content (request &key :content (hello . *)))"));
    } catch (IOException e) {
      System.err.println("error attempting to subscribe: " + e.toString());
      return;
    }
    ready();
  }

  public void receiveRequest(KQMLPerformative msg, Object content) {
    if (!(content instanceof KQMLList)) {
      errorReply(msg, "expected :content to be a list");
      return;
    }
    KQMLList contentList = (KQMLList)content;
    String verb = contentList.get(0).toString().toLowerCase();
    if (verb.equals("hello")) {
      KQMLPerformative replyMsg = new KQMLPerformative("tell");
      KQMLList replyContent = new KQMLList();
      replyContent.add("hello");
      KQMLObject sender = msg.getParameter(":sender");
      if (sender != null) {
	replyContent.add(sender);
      }
      replyMsg.setParameter(":content", replyContent);
      reply(msg, replyMsg);
    } else {
      errorReply(msg, "unknown request verb " + verb);
    }
  }
  
  public static void main(String[] argv) {
    new Hello(argv).run();
  }
}

