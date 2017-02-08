#!/usr/bin/python2.7

# This is pretty much a direct translation of Hello.java into Python.

from TripsModule import TripsModule
from KQML import KQMLPerformative
from KQML import KQMLList

class Hello(TripsModule):
    """ Hello TRIPS module - replies to hello requests with hello tells.
    Sending this: (request :content (hello) :sender fred)
    Gets this reply: (tell :content (hello fred) :receiver fred)
    """
    def __init__(self, argv):
        TripsModule.__init__(self, argv)

    def init(self):
        self.name = "hello"
        TripsModule.init(self)
        self.send(KQMLPerformative.from_string(
            "(subscribe :content (request &key :content (hello . *)))"))
        self.ready()

    def receive_request(self, msg, content):
        if not isinstance(content, KQMLList):
            self.error_reply(msg, "expected :content to be a list")
            return
        verb = content[0].to_string().lower()
        if verb == "hello":
            reply_msg = KQMLPerformative("tell")
            reply_content = KQMLList()
            reply_content.add("hello")
            sender = msg.get_parameter(":sender")
            if sender is not None:
                reply_content.add(sender)
            reply_msg.set_parameter(":content", reply_content)
            self.reply(msg, reply_msg)
        else:
            self.error_reply(msg, "unknown request verb " + verb)

if __name__ == "__main__":
    import sys
    Hello(sys.argv[1:]).run()

