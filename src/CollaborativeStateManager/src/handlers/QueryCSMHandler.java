package handlers;

import TRIPS.CollaborativeStateManager.CollaborativeStateManager;
import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLToken;
import TRIPS.KQML.KQMLObject;
import TRIPS.KQML.KQMLPerformative;

import java.util.*;

import extractors.OntologyReader;
import plans.GoalPlanner;
import plans.Messages;
import states.Goal;

public class QueryCSMHandler extends MessageHandler implements Runnable {

	KQMLList innerContent;
	GoalPlanner goalPlanner;
	String query;
	
	
	public QueryCSMHandler(KQMLPerformative msg, KQMLList content, ReferenceHandler referenceHandler,
			GoalPlanner goalPlanner, OntologyReader ontologyReader, CollaborativeStateManager csm) {
		super(msg, content,referenceHandler,csm, ontologyReader);
		this.goalPlanner = goalPlanner;
		
		// TODO Auto-generated constructor stub
	}
	
	public void run()
	{
		
	}
	
	@Override
	public KQMLList process() {
		KQMLObject innerContentObj = content.getKeywordArg(":content");
		innerContent = null;
		
		if (innerContentObj instanceof KQMLList)
			innerContent = (KQMLList)innerContentObj;
		
		query = innerContent.get(0).stringValue();
		

		
		KQMLObject activeGoalObject = innerContent.getKeywordArg(":active-goal");
		
		switch (query.toLowerCase())
		{
		case "active-goal":
			return handleActiveGoal();
		case "active-goal-list":
			return handleActiveGoalList();
		case "top-level-goals":
			return handleTopLevelGoals();
		}
		
		System.out.println("Unrecognized QUERY-CSM action type.");
		
		return null;
	}

	// Untested
	private KQMLList handleActiveGoalList() {
		KQMLList listToReturn = new KQMLList();
		KQMLList newContext = new KQMLList();
		if (content.getKeywordArg(":CONTEXT") != null)
			newContext.addAll((KQMLList)content.getKeywordArg(":CONTEXT"));
		List<Goal> pathToRoot = goalPlanner.getPathToRoot(goalPlanner.getActiveGoal());
		if (pathToRoot == null)
		{
			KQMLList failureReason = new KQMLList();
			failureReason.add("NO-ACTIVE-GOAL");
			return Messages.failureMessage("NIL",newContext,failureReason);
		}
		
		for (Goal g : pathToRoot)
		{
			listToReturn.add(g.getVariableName());
			//newContext.add(g.getKQMLTerm());
			newContext.addAll(g.getOriginalContext());
			newContext.addAll(referenceHandler.generateContextForTerm(g.getKQMLTerm()));
		}
		
		KQMLList response = new KQMLList();
		response.add("ACTIVE-GOAL-LIST");
		response.add(":WHAT");
		response.add(listToReturn);
		
		return Messages.reportContent(response, newContext);
	}
	
	private KQMLList handleTopLevelGoals()
	{
		KQMLList listToReturn = new KQMLList();
		
		KQMLList newContext = new KQMLList();
		if (content.getKeywordArg(":CONTEXT") != null)
			newContext.addAll((KQMLList)content.getKeywordArg(":CONTEXT"));
		
		for (String goal : ontologyReader.getRootGoals())
		{
			listToReturn.add(goal);
		}

		KQMLList response = new KQMLList();
		response.add("TOP-LEVEL-GOALS");
		response.add(":WHAT");
		response.add(listToReturn);
		
		return Messages.reportContent(response, newContext);
	}

	private KQMLList handleActiveGoal() {
		Goal activeGoal = goalPlanner.getActiveGoal();
		KQMLList newContext = new KQMLList();
		if (content.getKeywordArg(":CONTEXT") != null)
			newContext.addAll((KQMLList)content.getKeywordArg(":CONTEXT"));
		
		String goalVariable = "NIL";
		String goalId = "NIL";
		
		if (goalPlanner.hasAmbiguousActiveGoal())
		{
			List<Goal> topLevelGoals = new ArrayList<Goal>();
			topLevelGoals.addAll(goalPlanner.getTopLevelGoals());
			KQMLList whats = new KQMLList();
			KQMLList ids = new KQMLList();
			for (Goal g : topLevelGoals)
			{
				whats.add(g.getVariableName());
				ids.add(g.getId());
				newContext.addAll(g.getOriginalContext());
			}
			KQMLList response = new KQMLList();
			response.add("AMBIGUOUS-ACTIVE-GOAL");
			response.add(":IDS");
			response.add(ids);
			response.add(":WHATS");
			response.add(whats);
			
			return Messages.reportContent(response, newContext);
		}
		
		if (activeGoal != null)
		{
			goalVariable = activeGoal.getVariableName();
			goalId = activeGoal.getId();
			//newContext.add(activeGoal.getKQMLTerm());
			newContext.addAll(activeGoal.getOriginalContext());
			newContext.addAll(referenceHandler.generateContextForTerm(activeGoal.getKQMLTerm()));
			
		}
		KQMLList response = new KQMLList();
		response.add("ACTIVE-GOAL");
		response.add(":ID");
		response.add(new KQMLToken(goalId));
		response.add(":WHAT");
		response.add(new KQMLToken(goalVariable));
		
		
		return Messages.reportContent(response, newContext);
	}

}
