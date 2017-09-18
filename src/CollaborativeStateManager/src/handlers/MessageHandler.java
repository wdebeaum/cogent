package handlers;

import java.util.List;

import utilities.KQMLUtilities;
import TRIPS.CollaborativeStateManager.CollaborativeStateManager;
import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLObject;
import TRIPS.KQML.KQMLPerformative;

public abstract class MessageHandler {

	KQMLPerformative msg;
	KQMLList content;
	ReferenceHandler referenceHandler;
	protected CollaborativeStateManager csm;
	
	public MessageHandler(KQMLPerformative msg, KQMLList content, ReferenceHandler referenceHandler,
			CollaborativeStateManager csm)
	{
		this.msg = msg;
		this.content = content;
		this.referenceHandler = referenceHandler;
		this.csm = csm;
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
