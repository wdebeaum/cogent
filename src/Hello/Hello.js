var util = require('util');
var KQML = require('KQML/kqml.js');
var TripsModule = require('TripsModule/trips-module.js');

function Hello(argv, oninit) {
  TripsModule.call(this, argv);
  this.name = 'Hello';
  var that = this;
  this.init(function() {
    that.addHandler(KQML.parse('(request &key :content (hello . *))'),
		    that.handleHello);
    oninit.call(that);
  });
}
util.inherits(Hello, TripsModule);

Hello.prototype.handleHello = function(msg) {
  var replyContent = ['hello'];
  if ('sender' in msg) {
    replyContent.push(msg.sender);
  }
  this.replyToMsg(msg, { 0: 'tell', content: replyContent });
}

new Hello(process.argv, function() { this.run() });

