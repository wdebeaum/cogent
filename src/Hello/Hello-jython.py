#!/usr/bin/jython2.7

# This is pretty much a direct translation of Hello.java into Jython.

from TRIPS.TripsModule import StandardTripsModule
from TRIPS.KQML import KQMLPerformative, KQMLList

class Hello(StandardTripsModule):
    """ Hello TRIPS module - replies to hello requests with hello tells.
    Sending this: (request :content (hello) :sender fred)
    Gets this reply: (tell :content (hello fred) :receiver fred)
    """
    def __init__(self, argv):
        StandardTripsModule.__init__(self, ["-name", "hello"] + argv)

    def init(self):
        # this doesn't work because name is protected; instead use -name in
        # __init__ as above:
        # self.name = "hello"
        StandardTripsModule.init(self)
        self.send(KQMLPerformative.fromString(
            "(subscribe :content (request &key :content (hello . *)))"))
        self.ready()

    def receiveRequest(self, msg, content):
        if not isinstance(content, KQMLList):
            self.errorReply(msg, "expected :content to be a list")
            return
        verb = content.get(0).toString().lower()
        if verb == "hello":
            replyMsg = KQMLPerformative("tell")
            replyContent = KQMLList()
            replyContent.add("hello")
            sender = msg.getParameter(":sender")
            if sender is not None:
                replyContent.add(sender)
            replyMsg.setParameter(":content", replyContent)
            self.reply(msg, replyMsg)
        else:
            errorReply(msg, "unknown request verb " + verb)

if __name__ == "__main__":
    import sys
    Hello(sys.argv[1:]).run()

