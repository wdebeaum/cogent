package handlers;

import java.util.*;

import extractors.TermExtractor;
import plans.GoalPlanner;
import states.Action;
import states.Goal;
import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLObject;
import TRIPS.KQML.KQMLPerformative;
import TRIPS.KQML.KQMLString;

public class UpdateCSMHandler extends MessageHandler {

	KQMLList innerContent = null;
	KQMLObject context;
	String updateType;
	GoalPlanner goalPlanner;
	String activeGoal = null;
	
	public UpdateCSMHandler(KQMLPerformative msg, KQMLList content,
			GoalPlanner goalPlanner) {
		super(msg, content);
		this.goalPlanner = goalPlanner;
		// TODO Auto-generated constructor stub
	}

	@Override
	public KQMLList process() {
		KQMLObject innerContentObj = content.getKeywordArg(":content");
		innerContent = null;
		
		if (innerContentObj instanceof KQMLList)
			innerContent = (KQMLList)innerContentObj;
		
		updateType = innerContent.get(0).stringValue();
		context = innerContent.getKeywordArg(":context");	
		
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
		case "answer":
			return handleAnswer();
			
		}
		
		System.out.println("Unrecognized UPDATE-CSM action type.");
		
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
		return null;
	}

	private KQMLList handlePrivateSystemGoal()
	{
		
		String goalName;
		System.out.println("Handling private system goal");
		KQMLObject goalObject = innerContent.getKeywordArg(":CONTENT");
		if (goalObject instanceof KQMLList)
		{
			System.out.println(goalObject.stringValue());
			System.out.println("Adding goal from content");
			goalPlanner.addPrivateGoal(new Goal((KQMLList)goalObject));
		}
		else
		{
			goalName = goalObject.stringValue();
			KQMLList resultGoal = TermExtractor.extractTerm(goalName, (KQMLList)context);
			if (resultGoal == null)
			{
				System.out.println("Goal not found in context");
				return null;
			}
			
			Goal newPrivateGoal = new Goal(resultGoal);
			System.out.println("Adding private goal " + newPrivateGoal.getVariableName());
			goalPlanner.addPrivateGoal(newPrivateGoal);
			
		}
		
		return null;
	}
	
	private KQMLList handleFailedOn()
	{
		
		KQMLObject goalNameObject = innerContent.getKeywordArg(":WHAT");
		String goalName = null;
		if (goalNameObject != null)
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
		
		if (acceptContent == null)
		{
			System.out.println("No inner content parameter");
			return null;
		}
		
		if (acceptContent.getKeywordArg(":WHAT") == null)
		{
			System.out.println("No :WHAT parameter");
			return null;
		}
		String goalName = acceptContent.getKeywordArg(":WHAT").stringValue();
		System.out.println("Accepting goal: " + goalName);
		//TODO: Give better error message
		
		if (goalPlanner.hasGoal(goalName))
		{
			Goal g = goalPlanner.getGoal(goalName);
			if (g instanceof Action)
			{
				g.setAccepted();
				System.out.println("Action " + goalName + " accepted.");
			}
			else
				goalPlanner.setActiveGoal(goalName);
			System.out.println("Active goal now: " + goalName);
		}
		else // Do we want to add this if we don't know of the goal?
		{
			KQMLList goalLF = TermExtractor.extractTerm(goalName, (KQMLList)context);
			if (goalLF == null)
			{
				System.out.println("Not a valid goal to add to the system");
				return null;
			}
			Goal newGoal = new Goal(goalLF);
			goalPlanner.addGoal(newGoal);
			newGoal.setAccepted();
			goalPlanner.setActiveGoal(newGoal);
			System.out.println("Active goal now: " + goalName);
		}
		
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
		KQMLObject goalNameObject = innerContent.getKeywordArg(":GOAL");
		String goalName = null;
		if (goalNameObject != null)
			goalName = goalNameObject.stringValue();
		// This was a specific goal that with initiative taken
		if (goalName != null && goalPlanner.hasGoal(goalName))
		{
			goalPlanner.setCompleted(goalPlanner.getGoal(goalName));
			System.out.println("Set goal " + goalName + " as completed via answer");
			return null;
		}
		
		System.out.println("No such goal to be answered");
		return null;
	}
}
