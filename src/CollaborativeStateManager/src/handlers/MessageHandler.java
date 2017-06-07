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
	
	public KQMLList failureMessage(String what, KQMLObject context)
	{
		return failureMessage(what,context,null);
	}
	
	private void addContextToReferenceHandler()
	{
		KQMLObject contextObject = content.getKeywordArg(":CONTEXT");
		if (contextObject != null && contextObject instanceof KQMLList)
			referenceHandler.addReferences((KQMLList)contextObject);
		
	}
	
	public KQMLList failureMessage(String what,KQMLObject context, KQMLObject reason, KQMLObject possibleSolutions)
	{
		
		KQMLList failureContent = new KQMLList();
		failureContent.add("FAILURE");
		failureContent.add(":type");
		failureContent.add("FAILED-TO-INTERPRET");
		failureContent.add(":WHAT");
		failureContent.add(what);
		failureContent.add(":REASON");
		if (reason == null)
			failureContent.add("NIL");
		else
			failureContent.add(reason);
		
		failureContent.add(":POSSIBLE-RESOLUTION");
		if (possibleSolutions == null)
			failureContent.add("NIL");
		else
			failureContent.add(possibleSolutions);
		
		
		return reportContent(failureContent, context);		
	}
	
	public KQMLList failureMessage(String what, KQMLObject context, KQMLObject reason)
	{
		return failureMessage(what,context,reason,null);	
	}
	
	public KQMLList failureMessage(KQMLObject context)
	{
		return failureMessage("NIL", context);
	}
	
	public KQMLList failureMessage()
	{
		return failureMessage(new KQMLList());
	}
	
    protected KQMLList reportContent(KQMLObject content, KQMLObject context)
    {
    	KQMLList reportContent = new KQMLList();
    	reportContent.add("REPORT");
    	reportContent.add(":content");
    	reportContent.add(content);
    	reportContent.add(":context");
    	if (context != null)
    		reportContent.add(KQMLUtilities.removedDuplicates((KQMLList)context));
    	else
    		reportContent.add(new KQMLList());
    	
    	return reportContent;
    }
    
	protected KQMLList missingGoalToModify(String what, KQMLObject context)
	{
		KQMLList failureReason = new KQMLList();
		failureReason.add("MISSING-GOAL-TO-MODIFY");

		KQMLList newContext = new KQMLList();
		newContext.addAll((KQMLList)context);
		KQMLList solutionList = new KQMLList();
		KQMLList selectList = new KQMLList();
		selectList.add("SELECT-GOAL-TO-MODIFY");
		
		solutionList.add(selectList);
		
		return failureMessage(what, newContext,failureReason, solutionList);
	}
}
