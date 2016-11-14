#!/usr/bin/env ruby

require 'TripsModule/trips_module'

class Hello
  include TripsModule

  def initialize(argv)
    super
    @name = :Hello
    init()
    add_handler(KQML.from_s('(request &key :content (hello . *))'),
		method(:handle_hello))
  end

  def handle_hello(msg)
    send_msg(KQML[:tell, content:
      [:hello, *(msg.key?(:sender) ? [msg[:sender]] : [])]
    ] + KQML.in_reply_to(msg))
  end
end

Hello.new(ARGV).run() if ($0 == __FILE__)

