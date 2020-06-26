package handlers;

import java.util.List;

import utilities.KQMLUtilities;
import TRIPS.CollaborativeStateManager.CollaborativeStateManager;
import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLObject;
import TRIPS.KQML.KQMLPerformative;
import extractors.OntologyReader;

public abstract class MessageHandler {

	KQMLPerformative msg;
	KQMLList content;
	ReferenceHandler referenceHandler;
	OntologyReader ontologyReader;
	protected CollaborativeStateManager csm;
	
	public MessageHandler(KQMLPerformative msg, KQMLList content, ReferenceHandler referenceHandler,
			CollaborativeStateManager csm, OntologyReader ontologyReader)
	{
		this.msg = msg;
		this.content = content;
		this.referenceHandler = referenceHandler;
		this.csm = csm;
		this.ontologyReader = ontologyReader;
		addContextToReferenceHandler();
		
	}
	
	public abstract KQMLList process();
	

	private void addContextToReferenceHandler()
	{
		KQMLObject contextObject = content.getKeywordArg(":CONTEXT");
		if (contextObject != null && contextObject instanceof KQMLList)
			referenceHandler.addReferences((KQMLList)contextObject);
		
	}
	


}
