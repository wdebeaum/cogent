/*
 * SpeechOutNot.java
 *
 * George Ferguson, ferguson@cs.rochester.edu, 28 Aug 2001
 * $Id: SpeechOutNot.java,v 1.3 2015/11/17 04:43:44 lgalescu Exp $
 */

package TRIPS.SpeechOutNot;

import TRIPS.TripsModule.StandardTripsModule;
import TRIPS.KQML.KQMLPerformative;
import TRIPS.KQML.KQMLObject;
import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLString;
import TRIPS.KQML.KQMLToken;

public class SpeechOutNot extends StandardTripsModule {
    //
    // Constructor
    //
    public SpeechOutNot(String argv[], boolean isApplication) {
	super(argv, isApplication);
    }
    public SpeechOutNot(String argv[]) {
	this(argv, false);
    }
    //
    // StandardTripsModule method
    //
    public void init() {
	// Unless overriden by parameters...
	name = "speech-out";
	// Perform standard initializations
	super.init();
	// Then parse our parameters
	handleParameters();
	// subscribe
	subscribe();
	// Tell the Facilitator we are ready
	ready();
    }
    
    // Parse parameters
    private void handleParameters() {
	// None!
    }

    private void subscribe() {
        KQMLPerformative perf = null;
	try {
            perf = KQMLPerformative.fromString("(subscribe :content (request &key :content (say . *)))");
            send(perf);
            perf = KQMLPerformative.fromString("(subscribe :content (request &key :content (exit . *)))");
            send(perf);
        } catch (Exception ex) { ex.printStackTrace(); }
    }

    
    //
    // KQMLReceiver methods overriding StandardTripsModule defaults
    //
    public void receiveTell(KQMLPerformative msg, Object contentobj) {
	if (!(contentobj instanceof KQMLList)) {
	    errorReply(msg, "content was not a list");
	    return;
	}
	KQMLList content = (KQMLList)contentobj;
	String content0 = content.nth(0).toString();
	if (content0.equalsIgnoreCase("start-conversation") ||
	    content0.equalsIgnoreCase("end-conversation") ||
	    content0.equalsIgnoreCase("new-scenario") ||
	    content0.equalsIgnoreCase("modify-scenario")) {
	    // Ignore
	} else {
	    errorReply(msg, "bad tell: " + content0);
	}
    }
    public void receiveRequest(KQMLPerformative msg, Object contentobj) {
	if (!(contentobj instanceof KQMLList)) {
	    errorReply(msg, "content was not a list");
	    return;
	}
	KQMLList content = (KQMLList)contentobj;
	String content0 = content.nth(0).toString();
	if (content0.equalsIgnoreCase("exit")) {
	    int status = 0;
	    if (content.length() > 1) {
		status = Integer.parseInt(content.nth(1).toString());
	    }
 	    exit(status);
	} else if (content0.equalsIgnoreCase("hide-window")) {
	    // Ignore
	} else if (content0.equalsIgnoreCase("show-window")) {
	    // Ignore
	} else if (content0.equalsIgnoreCase("chdir")) {
	    // Ignore
	} else if (content0.equalsIgnoreCase("say")) {
	    receiveRequestSay(msg, content);
	} else {
	    errorReply(msg, "bad request: " + content0);
	}
    }
    void receiveRequestSay(KQMLPerformative msg, KQMLList content) {
	String str = kqml2string(content.nth(1));
	KQMLPerformative rmsg = new KQMLPerformative("reply");
	KQMLList rcontent = new KQMLList();
	rcontent.add(new KQMLToken("done"));
	rcontent.add(content);
	rmsg.setParameter(":content", rcontent);
	reply(msg, rmsg);
	// Also need a general broadcast that something was said
	KQMLPerformative bmsg = new KQMLPerformative("tell");
	KQMLList bcontent = new KQMLList();
	bcontent.add(new KQMLToken("spoken"));
	bcontent.add(new KQMLToken(":who"));
	bcontent.add(new KQMLToken("sys"));
	bcontent.add(new KQMLToken(":what"));
	// LG 11/16/2015
	// historically, the say message had the format (SAY "something")
	// recently, this format has also been used: (SAY :content "something")
	KQMLObject what = content.getKeywordArg(":content");
	if (what == null) {
	    what = content.nth(1);
	}
	bcontent.add(what);
	bmsg.setParameter(":content", bcontent);
	send(bmsg);
    }
    //
    // Other methods
    //
    /**
     * If OBJ is a KQMLString, then its stringValue() is returned, otherwise
     * its toString() value is returned.
     */
    static String kqml2string(KQMLObject obj) {
	if (KQMLString.class.isInstance(obj)) {
	    return ((KQMLString)obj).stringValue();
	} else {
	    return obj.toString();
	}
    }
    //
    // When run as an application
    //
    public static void main(String argv[]) {
	new SpeechOutNot(argv, true).run();
    }
}
