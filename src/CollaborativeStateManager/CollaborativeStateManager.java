/*
 * File: Causalizer.java
 * Creator: Ian Perera
 */
//


//
// 1. Your module should live in its own package. By convention, the
//    name of the package is the name of the module and the name of
//    this main class for the module.
//
package TRIPS.CollaborativeStateManager;

import handlers.*;
import extractors.*;
import plans.*;
import utilities.OntologyRequester;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.File;
import java.util.*;
import java.math.*;

import TRIPS.KQML.*;
import TRIPS.TripsModule.StandardTripsModule;


/**
 * Grounding Module for learning properties
 */
public class CollaborativeStateManager extends StandardTripsModule  {

    private boolean speechEnabled;
    private int callingProcess;
    private String hostName;
    private OntologyReader ontologyReader;
    private OntologyRequester ontologyRequester;
    private GoalPlanner goalPlanner;
    private ReferenceHandler referenceHandler;
    private final String BASE_DIRECTORY = System.getenv("TRIPS_BASE");
    private String DATA_DIRECTORY = BASE_DIRECTORY + File.separator + "etc";
    private String JSON_EVENT_GOAL_FILE = BASE_DIRECTORY + File.separator + "cabot" + File.separator + "event-goals.json";
    
    //
    // 3. You should provide the following two constructors,
    //    both of which accept an array of String parameters. Usually
    //    you just invoke the superclass constructor here (but Java
    //    doesn't inherit constructors so you have to define them yourself).
    //

    /**
     * Construct and return a new GroundingModule with the given
     * StandardTripsModule parameters.
     */
    public CollaborativeStateManager(String argv[], boolean isApplication) {
    	super(argv, isApplication);
    }

    /**
     * Construct and return a new GroundingModule with the given
     * StandardTripsModule parameters (default is not be a standalone
     * application).
     */
    public CollaborativeStateManager(String argv[]) {
        this(argv, false);
    }

    //
    // 4. Override the superclass init() method to setup your
    //    module. At least you ought to change the name that it uses to
    //    register with the facilitator, as in the example below.
    //    You also typically define a method to parse whatever
    //    module-specific parameters you might have, and call it from here.
    //    If your module has a UI, you would create it here.
    //    And you do any facilitator subscriptions here also.
    //    End by calling ready() to tell the facilitator that the module
    //    is ready to receive messages.
    //
    
    

    /**
     * StandardTripsModule initialization method.
     */
    @Override
    public void init() {
	// Unless overriden by parameters...
	name = "CSM";
	
	// Perform standard initializations
	super.init();
	handleParameters();
	referenceHandler = new ReferenceHandler();
	goalPlanner = new GoalPlanner(referenceHandler);
	ontologyRequester = new OntologyRequester(this);
	
	System.out.println("JSON filename: " + DATA_DIRECTORY + File.separator + "event-goals.json");
	File jsonFile = new File(DATA_DIRECTORY + File.separator + "event-goals.json");
	if (jsonFile.exists())
	{
		ontologyReader = new JSONOntologyReader();
		((JSONOntologyReader)ontologyReader).readEventGoalJSONFile(DATA_DIRECTORY + File.separator + "event-goals.json");
		System.out.println("Loaded JSON File");
	}
	else
	{
		System.out.println("Events filename: " + DATA_DIRECTORY + File.separator + "events");
		ontologyReader = new OntologyReader();
		ontologyReader.readEventOntologyFromFile(DATA_DIRECTORY + File.separator + "events");
		ontologyReader.readGoalOntologyFromFile(DATA_DIRECTORY + File.separator + "goals-par");
		ontologyReader.readModelOntologyFromFile(DATA_DIRECTORY + File.separator + "models");
		System.out.println("Loaded non-JSON files");
	}
	
	// Subscriptions
	try {
	    KQMLPerformative perf =
		KQMLPerformative.fromString("(subscribe :content (request &key :content (quit . *)))");
	    send(perf);		
	} catch (IOException ex) {
	    error("Yow! Subscription failed: " + ex);
	}
	try {
	    KQMLPerformative perf =
		KQMLPerformative.fromString("(subscribe :content (request &key :content (set-calling-process . *)))");
	    send(perf);		
	} catch (IOException ex) {
	    error("Yow! Subscription failed: " + ex);
	}

	try {
	    KQMLPerformative perf =
		KQMLPerformative.fromString("(subscribe :content (request &key :content (interpret-speech-act . *)))");
	    send(perf);
	} catch (IOException ex) {
	    error("Yow! Subscription failed: " + ex);
	}
	try {
	    KQMLPerformative perf =
		KQMLPerformative.fromString("(subscribe :content (request &key :content (take-initiative? . *)))");
	    send(perf);
	} catch (IOException ex) {
	    error("Yow! Subscription failed: " + ex);
	}
	try {
	    KQMLPerformative perf =
		KQMLPerformative.fromString("(subscribe :content (request &key :content (update-csm . *)))");
	    send(perf);
	} catch (IOException ex) {
	    error("Yow! Subscription failed: " + ex);
	}
	try {
	    KQMLPerformative perf =
		KQMLPerformative.fromString("(subscribe :content (request &key :content (query-csm . *)))");
	    send(perf);
	} catch (IOException ex) {
	    error("Yow! Subscription failed: " + ex);
	}
	try {
	    KQMLPerformative perf =
		KQMLPerformative.fromString("(subscribe :content (request &key :content (set-parameters . *)))");
	    send(perf);
	} catch (IOException ex) {
	    error("Yow! Subscription failed: " + ex);
	}

	try {
	    KQMLPerformative perf =
		KQMLPerformative.fromString("(subscribe :content (request &key :content (load-properties-file . *)))");
	    send(perf);		
	} catch (IOException ex) {
	    error("Yow! Subscription failed: " + ex);
	}
	
	try {
	    KQMLPerformative perf =
		KQMLPerformative.fromString("(subscribe :content (tell &key :content (start-conversation . *)))");
	    send(perf);		
	} catch (IOException ex) {
	    error("Yow! Subscription failed: " + ex);
	}
	
	
	// Tell the Facilitator we are ready
	ready();
    }

    /*
     * Handles command-line parameters.
     */
    protected void handleParameters() {
        String value;
	if ((value = getParameter("-data")) != null) {
	    DATA_DIRECTORY = value;
	}
    }
    
    //
    // 5. You need handlers for any messages you expect to receive.
    //    Look at TRIPS.TripsModule.TripsModule for a list of these
    //    methods, which correspond to the KQML performatives.
    //    The StandardTripsModule defaults for these methods reply with
    //    an error message (since you didn't handle it yourself).
    //    The following is a simple example of picking apart a TELL
    //    message (actually matches the example subscription in init()).
    //    See the javadoc for the TRIPS.KQML classes for more info.
    //

    /**
     * Receive a TELL message from the Facilitator.
     */
    @Override
    public void receiveTell(KQMLPerformative msg, Object contentobj) {
    	
		if (!(contentobj instanceof KQMLList)) {
		    errorReply(msg, "content was not a list");
		    return;
		}
		KQMLList content = (KQMLList)contentobj;
		String content0 = content.get(0).toString();

		if (content0.equalsIgnoreCase("set-calling-process"))
		{
			String pid = content.getKeywordArg(":pid").stringValue();
			if (pid == null)
			{
				errorReply(msg, "no pid specified");
				return;
			}
			callingProcess = Integer.parseInt(pid);
		}
		else if (content0.equalsIgnoreCase("start-conversation"))
		{
			resetSystem();
		}
		else {
		    errorReply(msg, "bad tell: " + content0);
		}		
		
    }

    @Override
    public void receiveRequest(KQMLPerformative msg, Object contentobj) {
		if (!(contentobj instanceof KQMLList)) {
		    errorReply(msg, "content was not a list");
		    return;
		}
		
		KQMLList content = (KQMLList)contentobj;
		String content0 = content.get(0).toString();
		
		if (content0.equalsIgnoreCase("quit"))
		{
			System.out.println("Shutting down");
			shutdown();
		}
		else if (content0.equalsIgnoreCase("restart"))
			resetSystem();
		else if (content0.equalsIgnoreCase("set-parameters"))
		{

			// Hostname of processing host
			KQMLObject hostName = content.getKeywordArg(":hostname");
        
			
			if (hostName != null)
			{
				this.hostName = hostName.stringValue();

			}
		}
		else if (content0.equalsIgnoreCase("load-properties-file"))
		{
			String filename = content.getKeywordArg(":file").stringValue();
			readPropertiesFile(filename);
		}
		else if (content0.equalsIgnoreCase("interpret-speech-act"))
		{
			KQMLObject replyWith = msg.getParameter(":REPLY-WITH");
			InterpretSpeechActHandler isah = new InterpretSpeechActHandler(msg, content, 
													referenceHandler,
													goalPlanner, ontologyReader, this);

//			try {
//			KQMLList test = KQMLList.fromString("(ONT::RELN ONT::V33245 :INSTANCE-OF ONT::PUT "
//					+ ":AGENT ONT::V33467 :AFFECTED ONT::V33279 :RESULT ONT::V33296 "
//					+ ":TENSE W::PRES :VFORM W::BASE :FORCE ONT::TRUE :LEX W::PUT)");
//			
//			ontologyRequester.getOntologicalParents(test);
//			}
//			catch (IOException e)
//			{
//				e.printStackTrace();
//			}
			
			KQMLList responseContent = null;
			try {
				responseContent = isah.process();
			}
			catch (RuntimeException re)
			{
				re.printStackTrace();
				KQMLPerformative replyMessage = new KQMLPerformative("SORRY");
				KQMLString comment = new KQMLString("Exception in CSM");
				KQMLString text = new KQMLString(re.getMessage());
				replyMessage.setParameter(":COMMENT", comment);
				replyMessage.setParameter(":TEXT", text);
				//reply(msg, replyMessage);
				
			}
			if (responseContent != null)
			{
			    // LG debug info
			    // System.out.println("CSM response: " + responseContent);
			    sendContentViaPerformative("TELL", "DAGENT", responseContent, replyWith);
			}
			
		}
		else if (content0.equalsIgnoreCase("take-initiative?"))
		{
			KQMLObject replyWith = msg.getParameter(":REPLY-WITH");
			
			TakeInitiativeHandler tih = new TakeInitiativeHandler(msg, content, referenceHandler,
															goalPlanner, ontologyReader, this);
			KQMLList responseContent = null;
			try {
				responseContent = tih.process();
			}
			catch (RuntimeException re)
			{
				re.printStackTrace();
				KQMLPerformative replyMessage = new KQMLPerformative("SORRY");
				KQMLString comment = new KQMLString("Exception in CSM");
				KQMLString text = new KQMLString(re.getMessage());
				replyMessage.setParameter(":COMMENT", comment);
				replyMessage.setParameter(":TEXT", text);
				reply(msg, replyMessage);
			}
			if (responseContent != null)
			{
				sendContentViaPerformative("TELL", "DAGENT", responseContent, replyWith);
			}
			
		}
		else if (content0.equalsIgnoreCase("update-csm"))
		{
			KQMLObject replyWith = msg.getParameter(":REPLY-WITH");	
			UpdateCSMHandler uch = new UpdateCSMHandler(msg, content, referenceHandler, goalPlanner,
											 this, ontologyReader);
			KQMLList responseContent = null;
			try {
				responseContent = uch.process();
			}
			catch (RuntimeException re)
			{
				re.printStackTrace();
				KQMLPerformative replyMessage = new KQMLPerformative("SORRY");
				KQMLString comment = new KQMLString("Exception in CSM");
				KQMLString text = new KQMLString(re.getMessage());
				replyMessage.setParameter(":COMMENT", comment);
				replyMessage.setParameter(":TEXT", text);
				reply(msg, replyMessage);
			}
			if (responseContent != null)
			{
				sendContentViaPerformative("TELL", "DAGENT", responseContent, replyWith);
			}
			
		}
		else if (content0.equalsIgnoreCase("query-csm"))
		{
			KQMLObject replyWith = msg.getParameter(":REPLY-WITH");
			QueryCSMHandler qch = new QueryCSMHandler(msg, content, referenceHandler,
														goalPlanner, ontologyReader, this);
			KQMLList responseContent = null;
			try {
				responseContent = qch.process();
			}
			catch (RuntimeException re)
			{
				re.printStackTrace();
				KQMLPerformative replyMessage = new KQMLPerformative("SORRY");
				KQMLString comment = new KQMLString("Exception in CSM");
				KQMLString text = new KQMLString(re.getMessage());
				replyMessage.setParameter(":COMMENT", comment);
				replyMessage.setParameter(":TEXT", text);
				reply(msg, replyMessage);
			}
			if (responseContent != null)
			{
				sendContentViaPerformative("TELL", "DAGENT", responseContent, replyWith);
			}
			
			
		}
		else {
		    errorReply(msg, "bad request: " + content0);
		}

		
    }
    
    // I know this is a bad hack but ¯\_(ツ)_/¯
    public void sendReply(KQMLPerformative msg, KQMLPerformative replyMessage)
    {
    	reply(msg,replyMessage);
    }
    
    public void sendContentViaPerformative(String performativeType, String receiver,
    										KQMLObject content, KQMLObject replyWith)
    {
    	if (receiver == null)
    		receiver = "NIL";
    	if (content == null)
    		content = new KQMLList();
    	if (replyWith == null)
    		replyWith = new KQMLList();
		KQMLPerformative performative = new KQMLPerformative(performativeType);
		performative.setParameter(":RECEIVER", receiver);
		performative.setParameter(":CONTENT", content);
		if (replyWith != null)
			performative.setParameter(":IN-REPLY-TO", replyWith);
		send(performative);
    }

    
	/**
	 * Read the properties file from the given filename, and send results to the corresponding
	 * classes
	 * @param filename
	 */
	public void readPropertiesFile(String filename)
	{
		InputStream in = null;
		HashMap<String,String> properties = new HashMap<String,String>();
		try {
			in = new FileInputStream(filename);
		} catch (FileNotFoundException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		try {
				
				BufferedReader reader = new BufferedReader(new InputStreamReader(in));
				String line = null;
				System.out.println("Reading properties...");
				
			    while ((line = reader.readLine()) != null) {
			    	String commentRemoved = line.split("#")[0];
			    	String[] propertySplit = commentRemoved.split("=",2);

			    	// Any ='s in the property value should be okay with this method
			    	String propertyName = propertySplit[0];
			    	String propertyValue = propertySplit[1];
			    	
			    	properties.put(propertyName.toLowerCase(), propertyValue.toLowerCase());
			    	System.out.println(propertyName.toLowerCase() + ":" + propertyValue.toLowerCase());
			    }
			    in.close();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				System.err.println("Could not open properties file " + filename.toString() + " for reading.");
			} finally {
				try {
					if (in != null)
						in.close();
				} catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		
		this.readProperties(properties);

	}
	
	public void readProperties(HashMap<String, String> properties)
	{
		
	}
	
	public synchronized void sendKQMLPerformative(KQMLPerformative performative)
	{
		send(performative);
	}
	
	private void resetSystem()
	{
		
		goalPlanner = new GoalPlanner(referenceHandler);
	}
    
	
	@Override
	protected void exit(int n)
	{
		shutdown();
		if (isApplication) {
		    System.exit(n);
		} else {
		    // Try to at least bring down the KQMLDispatcher
		    // Should we maybe just close our socket?
		    if (dispatcher != null)
			dispatcher.shutdown();
		}
	}
	
	
	
	private void shutdown()
	{
		System.out.println("Shutting down CollaborativeStateManager");


		//System.exit(0);
	}
	
	
    //
    // 6. For debugging and testing, you probably want to define
    //    a main() method that launches an instance of your module.
    //    Most modules are launched internally by the facilitator as part
    //    of launching the entire TRIPS system.
    //

    /**
     * When run as an application, start an instance of this module.
     */
    public static void main(String argv[]) {
	new CollaborativeStateManager(argv, true).run();
    }
}
