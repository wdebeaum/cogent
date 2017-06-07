package handlers;

import java.util.*;

import extractors.TermExtractor;
import plans.GoalPlanner;
import states.Action;
import states.Elaboration;
import states.Goal;
import states.Query;
import utilities.KQMLUtilities;
import TRIPS.CollaborativeStateManager.CollaborativeStateManager;
import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLObject;
import TRIPS.KQML.KQMLPerformative;
import TRIPS.KQML.KQMLString;


public class UpdateCSMHandler extends MessageHandler implements Runnable {

	KQMLList innerContent = null;
	KQMLObject context;
	String updateType;
	String activeGoal = null;
	GoalPlanner goalPlanner;
	KQMLObject replyWith;
	
	public UpdateCSMHandler(KQMLPerformative msg, KQMLList content, 
			ReferenceHandler referenceHandler,
			GoalPlanner goalPlanner, CollaborativeStateManager csm) {
		super(msg, content, referenceHandler, csm);
		this.goalPlanner = goalPlanner;
		// TODO Auto-generated constructor stub
	}
	
	public void run()
	{
		
		KQMLList responseContent = null;
		try {
			responseContent = process();
		}
		catch (RuntimeException re)
		{
			re.printStackTrace();
			KQMLPerformative replyMessage = new KQMLPerformative("SORRY");
			KQMLString comment = new KQMLString("Exception in CSM");
			KQMLString text = new KQMLString(re.getMessage());
			replyMessage.setParameter(":COMMENT", comment);
			replyMessage.setParameter(":TEXT", text);
			csm.sendReply(msg, replyMessage);
		}
		if (responseContent != null)
		{
			csm.sendContentViaPerformative("TELL", "DAGENT", responseContent, replyWith);
		}
		
	}

	@Override
	public KQMLList process() {
		KQMLObject innerContentObj = content.getKeywordArg(":content");
		innerContent = null;
		
		if (innerContentObj instanceof KQMLList)
			innerContent = (KQMLList)innerContentObj;
		
		updateType = innerContent.get(0).stringValue();
		context = content.getKeywordArg(":context");	
		
		KQMLObject activeGoalObject = innerContent.getKeywordArg(":active-goal");
		
		if ((activeGoalObject != null) &&
		    !activeGoalObject.stringValue().equalsIgnoreCase("nil") &&
		    !activeGoalObject.stringValue().equals("-"))
		    {
			activeGoal = activeGoalObject.stringValue();
		    }

		
		switch (updateType.toLowerCase())
		{
		case "proposed":
			return handleProposed();
		case "accepted-solution":
			return handleAcceptedSolution();
		case "accepted":
			return handleAccepted();
		case "action-completed":
			return handleActionCompleted();
		case "no-initiative-taken":
			return handleNoInitiativeTaken();
		case "initiative-taken-on-goal":
			return handleInitiativeTakenOnGoal();
		case "failed-on":
		case "unacceptable":
		case "failure":
			return handleFailedOn();
		case "solved":
			return handleSolved();
		case "private-system-goal":
			return handlePrivateSystemGoal();
		case "problem":
			return handleProblem();
		case "ba-waiting":
			return handleBaWaiting();
		case "set-default-initiative":
			return handleDefaultInitiative();
		case "set-override-initiative":
			return handleOverrideInitiative();
		case "status-report":
			return handleStatusReport();
		case "answer":
			return handleAnswer();
			
		}
		
		System.out.println("Unrecognized UPDATE-CSM action type.");
		
		return null;
		
	}
	
	private KQMLList handleStatusReport() {
		KQMLObject goalSymbolObject = innerContent.getKeywordArg(":goal");
		boolean completedStatus = false;
		String goalName = null;
		if (goalSymbolObject != null)
			goalName = goalSymbolObject.stringValue();

		KQMLObject statusObject = innerContent.getKeywordArg(":status");
		
		if (statusObject != null && statusObject.stringValue().equalsIgnoreCase("ONT::DONE"))
			completedStatus = true;
		
		// This was a specific goal that with initiative taken
		if (goalName != null && goalPlanner.hasGoal(goalName))
		{
			if (completedStatus)
			{
				goalPlanner.setCompleted(goalPlanner.getGoal(goalName));
				System.out.println("Set goal " + goalName + " as completed via status-report");
				return null;
			}
		}
		else if (goalName != null && goalPlanner.getQueryMapping().containsKey(goalName))
		{
			if (completedStatus)
			{
				goalPlanner.setCompleted(goalPlanner.getQueryMapping().get(goalName));
				System.out.println("Set goal " + goalName + " as completed via status-report");
				return null;
			}
		}
		
		System.out.println("No such goal to be completed");
		return null;
	
	}	
	
	private KQMLList handleActionCompleted() {
		KQMLObject goalNameObject = innerContent.getKeywordArg(":action");
		String goalName = null;
		if (goalNameObject != null)
			goalName = goalNameObject.stringValue();
		// This was a specific goal that with initiative taken
		if (goalName != null && goalPlanner.hasGoal(goalName))
		{
			goalPlanner.setCompleted(goalPlanner.getGoal(goalName));
			System.out.println("Set goal " + goalName + " as completed via action-completed");
			return null;
		}
		
		System.out.println("No such goal to be completed");
		return null;
	
	}

	private KQMLList handleOverrideInitiative()
	{
		System.out.println("Handling override initiative setting");
		KQMLObject initiativeOverrideObject = innerContent.getKeywordArg(":OVERRIDE");
		KQMLObject initiativeValueObject = innerContent.getKeywordArg(":VALUE");
		if (initiativeOverrideObject == null || initiativeValueObject == null)
			return null;
		
		if (initiativeOverrideObject != null)
		{
			if (initiativeOverrideObject.stringValue().equalsIgnoreCase("NIL"))
				goalPlanner.setOverrideSystemInitiative(false);
			else if (initiativeOverrideObject.stringValue().toUpperCase().contains("T"))
				goalPlanner.setOverrideSystemInitiative(true);
		}
		
		if (initiativeValueObject != null)
		{
			if (initiativeValueObject.stringValue().equalsIgnoreCase("NIL"))
				goalPlanner.setOverrideSystemInitiativeValue(false);
			else if (initiativeValueObject.stringValue().toUpperCase().contains("T"))
				goalPlanner.setOverrideSystemInitiativeValue(true);			
		}

		return null;		
	}
	
	private KQMLList handleDefaultInitiative()
	{
		System.out.println("Handling default initiative setting");
		KQMLObject initiativeObject = innerContent.getKeywordArg(":VALUE");
		if (initiativeObject == null)
			return null;
		
		if (initiativeObject.stringValue().equalsIgnoreCase("NIL"))
			goalPlanner.setGlobalSystemInitiative(false);
		else if (initiativeObject.stringValue().toUpperCase().contains("T"))
			goalPlanner.setGlobalSystemInitiative(true);
		
		return null;
	}
	
	private KQMLList handleSolved() {
		// TODO Auto-generated method stub
		return null;
	}

	private KQMLList handleProposed()
	{
		KQMLList proposeContent = (KQMLList)(innerContent.getKeywordArg(":CONTENT"));
		
		
		if (proposeContent == null)
		{
			System.out.println("No inner content parameter");
			return null;
		}
		
		KQMLObject proposeObject = proposeContent.getKeywordArg(":WHAT");
		
		if (proposeContent.getKeywordArg(":WHAT") == null)
			proposeObject = proposeContent.getKeywordArg(":QUERY");
		
		if (proposeObject == null)
		{
			System.out.println("No :WHAT or :QUERY parameter");
			return null;
		}
		
		String goalName = proposeObject.stringValue();
		System.out.println("Received proposed goal: " + goalName);
		
		if (goalPlanner.createGoalFromAct("PROPOSE",proposeContent, (KQMLList)context))
			System.out.println("Goal successfully created from act");
		else
			System.out.println("Failed to create goal from act");
		
		return null;
	}

	private KQMLList handlePrivateSystemGoal()
	{
		
		String goalName;
		System.out.println("Handling private system goal");
		KQMLObject goalWhatObject = innerContent.getKeywordArg(":WHAT");
		KQMLObject goalIdObject = innerContent.getKeywordArg(":ID");
		if (goalWhatObject instanceof KQMLList)
		{
			System.out.println(goalWhatObject.stringValue());
			System.out.println("Adding goal from content");
			goalPlanner.addPrivateGoal(new Goal((KQMLList)goalWhatObject,(KQMLList)context));
		}
		else
		{
			goalName = goalWhatObject.stringValue();
			KQMLList resultGoal = TermExtractor.extractTerm(goalName, (KQMLList)context);
			if (resultGoal == null)
			{
				System.out.println("Goal not found in context");
				return null;
			}
			
			Goal newPrivateGoal = new Goal(resultGoal,(KQMLList)context);
			if (goalIdObject != null)
				newPrivateGoal.setId(goalIdObject.stringValue());
			System.out.println("Adding private goal " + newPrivateGoal.getVariableName());
			goalPlanner.addPrivateGoal(newPrivateGoal);
			
		}
		
		return null;
	}
	
	private KQMLList handleFailedOn()
	{
		
		KQMLObject goalNameObject = innerContent.getKeywordArg(":WHAT");
		String goalName = null;
		
		if (goalNameObject instanceof KQMLList)
		{
			KQMLList goalNameObjectList = (KQMLList)goalNameObject;
			if (goalNameObjectList.getKeywordArg(":ID") != null)
				goalName = goalNameObjectList.getKeywordArg(":ID").stringValue();
		}
		else if (goalNameObject != null)
			goalName = goalNameObject.stringValue();
		

		// This was a specific goal that failed
		if (goalName != null && goalPlanner.hasGoal(goalName))
		{
			goalPlanner.getGoal(goalName).setFailed(true);
			System.out.println("Set goal + " + goalName + " as failed");
			return null;
		}
		
		// ... or, this was a new goal that was part of an existing goal
		
		KQMLObject failedAsObject = innerContent.getKeywordArg(":AS");
		
		if (failedAsObject != null && failedAsObject instanceof KQMLList)
		{
			KQMLList failedAsList = (KQMLList)failedAsObject;
			String parentGoalName = failedAsList.getKeywordArg(":OF").stringValue();
			if (goalPlanner.hasGoal(parentGoalName))
			{
				goalPlanner.getGoal(parentGoalName).setFailed(true);
				System.out.println("Set goal + " + parentGoalName + " as failed");
				return null;
			}
			
		}
					
		System.out.println("No such goal to set as failed");
		return null;
	}
	
	private KQMLList handleProblem()
	{
		
		KQMLObject actionObject = innerContent.getKeywordArg(":WHAT");
		KQMLObject problemType = innerContent.getKeywordArg(":TYPE");
		String problemTypeString = "";
		if (problemType != null)
			problemTypeString = problemType.stringValue().toUpperCase();
		
		KQMLList cpsAct = null;
		String goalName = null;
		
		if (actionObject instanceof KQMLList)
			cpsAct = (KQMLList)actionObject;
		else
			goalName = actionObject.stringValue();
		
		switch (problemTypeString)
		{
		case "CANNOT-PERFORM":
			if (cpsAct != null)
				cannotPerform(cpsAct);
			else if (goalName != null)
				cannotPerform(goalName);
			return null;
		}

					
		System.out.println("No such goal to set as problem");
		return null;
	}
	
	private void cannotPerform(KQMLList cpsAct)
	{
		if (cpsAct == null)
		{
			System.out.println("Invalid CPS act");
			return;
		}
		KQMLObject goalObject = cpsAct.getKeywordArg(":WHAT");
		String goalName = null;
		if (goalObject != null)
		{
			goalName = goalObject.stringValue();
			if (goalPlanner.hasGoal(goalName))
			{
				goalPlanner.getGoal(goalName).setFailed(true);
				System.out.println("Set goal " + goalName + " as failed");
				return;
			}
		}

	}
	
	private void cannotPerform(String goalName)
	{
		if (goalPlanner.hasGoal(goalName))
		{
			goalPlanner.getGoal(goalName).setFailed(true);
			System.out.println("Set goal " + goalName + " as failed");
			return;
		}
		else
		{
			System.out.println("No such goal " + goalName);
		}

	}
	
	private KQMLList handleAcceptedSolution()
	{
		return null;
	}
	
	private KQMLList handleAccepted()
	{
		KQMLList acceptContent = (KQMLList)(innerContent.getKeywordArg(":CONTENT"));
		KQMLObject effectContent = innerContent.getKeywordArg(":EFFECT");
		System.out.println("Received accept message");
		if (acceptContent == null)
		{
			System.out.println("No inner content parameter");
			return null;
		}
		KQMLObject whatObject = acceptContent.getKeywordArg(":WHAT");

		
		// New version with :what as a list?
		if (whatObject instanceof KQMLList)
		{
			
		}
		

		String goalName = "";
		if (acceptContent.getKeywordArg(":WHAT") != null)
			goalName = acceptContent.getKeywordArg(":WHAT").stringValue();
		KQMLObject asObject = acceptContent.getKeywordArg(":AS");
		KQMLObject idObject = acceptContent.getKeywordArg(":ID");
		String id = null;
		if (idObject != null)
			id = idObject.stringValue();
		String asType = "GOAL";
		String ofSymbol = "";
		if (asObject != null)
		{
			KQMLList asList = (KQMLList)asObject;
			if (asList.size() > 1)
				asType = asList.get(0).stringValue();
			if (asList.getKeywordArg(":OF") != null)
				ofSymbol = asList.getKeywordArg(":OF").stringValue();
			
		}
		
		if (asType.equalsIgnoreCase("MODIFICATION"))
			goalName = ofSymbol;
		System.out.println("Accepting goal: " + goalName);
		
		//TODO: Give better error message
		
		
		if (goalPlanner.hasGoal(goalName) || goalPlanner.hasGoalById(id))
		{
			Goal g = null;
			if (goalPlanner.hasGoalById(id))
			{
				g = goalPlanner.getGoalById(id);
				System.out.println("Found goal in planner by id " + id);
			}
			else if (goalPlanner.hasGoal(goalName))
			{
				g = goalPlanner.getGoal(goalName);
				System.out.println("Found goal in planner by name " + goalName);
			}
			
			if (g instanceof Action)
			{
				g.setAccepted();
				goalPlanner.setActiveGoal(g);
				System.out.println("G id: " + g.getId());
				System.out.println("Action " + goalName + " accepted.");
				System.out.println("Active goal now: " + goalName);
			}
			else if (g instanceof Elaboration)
			{
				g.setAccepted();
				System.out.println("Elaboration " + goalName + " accepted.");
			}
			else
			{
				goalPlanner.setActiveGoal(g);
				System.out.println("Active goal now: " + goalName);
			}			
		}
		else if (acceptContent.get(0).stringValue().toUpperCase().contains("ASK"))
		{
			String query = null;
			
			if (acceptContent.getKeywordArg(":QUERY") != null)
				query = acceptContent.getKeywordArg(":QUERY").stringValue();
			
			if (query == null)
				query = acceptContent.getKeywordArg(":OF").stringValue();

			
			String what = "";
			if (whatObject != null)
				what = whatObject.stringValue();
			String mapping = query + what;
			
			Goal foundQuery = goalPlanner.getGoalById(id);
			if (foundQuery != null)
			{
				foundQuery.setAccepted();
				goalPlanner.setActiveGoal(foundQuery);
			}
			
		}
		else // Do we want to add this if we don't know of the goal?
		{
			System.out.println("Goal with ID " + id + "accepted without being known");
			if (goalPlanner.createGoalFromAct("ACCEPT",acceptContent, (KQMLList)context))
				System.out.println("Goal successfully created from act");
			else
				System.out.println("Failed to create goal from act");
		}
		
		if (!KQMLUtilities.isKQMLNull(effectContent) && effectContent instanceof KQMLList)
		{
			goalPlanner.createGoalFromAct("ACCEPT", (KQMLList)effectContent, (KQMLList)context);
		}
		
		goalPlanner.hasAcceptedGoal = true;
		
		return null;
	}
	
	private KQMLList handleNoInitiativeTaken()
	{
		KQMLObject goalNameObject = innerContent.getKeywordArg(":WHAT");
		String goalName = null;
		if (goalNameObject != null)
			goalName = goalNameObject.stringValue();
		
		// This was a specific goal with initiative not taken
		if (goalName != null && goalPlanner.hasGoal(goalName))
		{
			goalPlanner.getGoal(goalName).setSystemTookInitiative(false);
			System.out.println("Set goal " + goalName + " as initiative not taken");
			return null;
		}
		
					
		//System.out.println("No such goal to set as initiative taken");
		return null;
	}

	private KQMLList handleInitiativeTakenOnGoal()
	{
		KQMLObject goalNameObject = innerContent.getKeywordArg(":WHAT");
		String goalName = null;
		if (goalNameObject != null)
			goalName = goalNameObject.stringValue();
		// This was a specific goal that with initiative taken
		if (goalName != null && goalPlanner.hasGoal(goalName))
		{
			goalPlanner.getGoal(goalName).setSystemTookInitiative(true);
			System.out.println("Set goal " + goalName + " as initiative taken");
			return null;
		}
		
					
		System.out.println("No such goal to set as initiative taken");
		return null;
	}
	
	private KQMLList handleBaWaiting()
	{
		return null;
	}
	
	// TODO: Store answer?
	private KQMLList handleAnswer()
	{
		KQMLObject toObject = innerContent.getKeywordArg(":TO");
		KQMLObject queryObject = innerContent.getKeywordArg(":QUERY");
		KQMLObject valueObject = innerContent.getKeywordArg(":VALUE");
		KQMLObject justificationObject = innerContent.getKeywordArg(":JUSTIFICATION");
		KQMLObject effectObject = innerContent.getKeywordArg(":EFFECT");
		
		String goalName = null;
		if (!KQMLUtilities.isKQMLNull(toObject))
			goalName = toObject.stringValue();
		
		
		// This was a specific goal that with initiative taken
		if (goalName != null && goalPlanner.hasGoal(goalName))
		{
			goalPlanner.setCompleted(goalPlanner.getGoal(goalName));
			System.out.println("Set goal " + goalName + " as completed via answer");
		}
		else
		{
			System.out.println("No such goal to be answered");
		}
		
		if (!KQMLUtilities.isKQMLNull(effectObject) && effectObject instanceof KQMLList)
		{
			KQMLList effectList = (KQMLList)effectObject;
			System.out.println("Applying effect " + effectList.stringValue());
			goalPlanner.createGoalFromAct("ACCEPT", effectList, (KQMLList)context);
			
		}
		
		return null;
	}
}
