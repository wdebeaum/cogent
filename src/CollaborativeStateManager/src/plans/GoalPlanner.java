package plans;

import handlers.IDHandler;
import handlers.ReferenceHandler;

import java.util.*;
import java.util.Map.Entry;

import extractors.TermExtractor;
import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLObject;
import states.ActType;
import states.Action;
import states.Goal;
import states.Query;
import utilities.KQMLUtilities;

public class GoalPlanner {

	private HashMap<Goal,Goal> goalConnections; // Child -> Parent
	private HashMap<String,Goal> variableGoalMapping;
	private HashMap<String,Goal> idGoalMapping;
	private HashMap<Goal,Goal> proposedModifies;
	private HashMap<String,Query> queryMapping;
	private Goal activeGoal;
	private Goal privateGoal;
	private boolean globalSystemInitiative = false;
	private boolean overrideSystemInitiative = false;
	private boolean overrideSystemInitiativeValue = false;
	private boolean ambiguousActiveGoal = false;
	private Goal underDiscussion;
	private ReferenceHandler referenceHandler;
	public boolean hasAcceptedGoal = false;
	
	public GoalPlanner(ReferenceHandler referenceHandler)
	{
		goalConnections = new HashMap<Goal,Goal>();
		variableGoalMapping = new HashMap<String,Goal>();
		idGoalMapping = new HashMap<String,Goal>();
		proposedModifies = new HashMap<Goal,Goal>();
		queryMapping = new HashMap<String,Query>();
		privateGoal = null;
		activeGoal = null;
		underDiscussion = null;
		this.referenceHandler = referenceHandler;
	}
	
	public synchronized boolean addGoal(Goal goal, String parentVariableName)
	{

		if (goal == null)
		{
			System.out.println("Tried to add a null goal");
			return false;
		}
		
		System.out.println("Adding goal with id " + goal.getId() + 
				" and varname " + goal.getVariableName());
		
		if (hasGoal(goal.getVariableName()))
		{
			System.out.println("Goal " + goal.getVariableName() + " already in planner.");
			return false;
		}
		
		String upperCaseParentVariableName = null;
		if (parentVariableName != null)
			upperCaseParentVariableName = parentVariableName.toUpperCase();
		
		Goal parentGoal = getGoal(upperCaseParentVariableName);
		if (parentGoal != null && (parentGoal.isFailed() || parentGoal.isAbandoned()))
		{
			System.out.println("Replacing " + parentGoal.getVariableName() + 
					" with " + goal.getVariableName() + " and term: " + 
					goal.getKQMLTerm().stringValue());
			replaceGoal(goal,parentGoal);
		}
		else
		{
			goalConnections.put(goal,getGoal(upperCaseParentVariableName));
			variableGoalMapping.put(goal.getVariableName().toUpperCase(),goal);
			idGoalMapping.put(goal.getId(), goal);
			if (activeGoal != null && parentVariableName == null && getTopLevelGoals().size() > 1)
			{
				System.out.println("Active goal is now ambiguous after adding new "
						+ "top level goal");
				ambiguousActiveGoal = true;
				activeGoal = null;
			}
			goal.setParent(getGoal(upperCaseParentVariableName));
			System.out.println("Added goal " + goal.getVariableName() + " with parent "
					+ upperCaseParentVariableName + " and term: " + 
					goal.getKQMLTerm().stringValue());
		}
		
		referenceHandler.addReference(goal.getKQMLTerm());
		underDiscussion = goal;

		// LG: debug info
		System.out.println("Current goals:");
		for (Goal g : idGoalMapping.values()) {
		    String activeFlag="";
		    if ((activeGoal != null) &&
			(activeGoal.getVariableName().equalsIgnoreCase(g.getVariableName())))
			activeFlag="*";
		    System.out.println("\t" + activeFlag + g);
		}
		
		return true;
	}
	
	public boolean addGoal(Goal goal)
	{
		return addGoal(goal,null);
	
	}
	
	public Goal getGoalUnderDiscussion()
	{
		return underDiscussion;
	}
	
	public void addPrivateGoal(Goal goal)
	{
		this.privateGoal = goal;
		addGoal(goal);
	}
	
	public List<Goal> getFailedGoals()
	{
		List<Goal> result = new ArrayList<Goal>();
		for (Goal g : variableGoalMapping.values())
		{
			if (g.isFailed())
				result.add(g);
		}
		
		return result;
	}
	

	
	public List<Query> getQueries()
	{
		List<Query> result = new ArrayList<Query>();
		for (Goal g : variableGoalMapping.values())
		{
			if (g instanceof Query)
				result.add((Query)g);
		}
		
		return result;
	}
	

	
	public KQMLList modify(Goal newGoal, String oldGoalName, boolean strict)
	{
		System.out.println("Modifying goal " + newGoal.getVariableName());
		Goal oldGoal = getGoal(oldGoalName);
		if (oldGoal == null)
		{
			System.out.println("No goal " + oldGoalName + " found.");
			return null;
		}
		Goal parentGoal = oldGoal.getParent();
		System.out.println("Replacing goal " + oldGoal.getVariableName() + 
				" with " + newGoal.getVariableName());
		
		// This might be put back, but now we're just doing subgoals
		//replaceGoal(newGoal, oldGoal);
		
		addGoal(newGoal, oldGoalName);
		
		if (strict)
			return newGoal.adoptContent("MODIFICATION", oldGoal.getId());
		
		return newGoal.adoptContent("ELABORATION", oldGoal.getId());
	
		
	}
	

	
	
	public List<Goal> getPathToRoot(String goalName)
	{
		return getPathToRoot(getGoal(goalName));
	}
	
	public List<Goal> getPathToRoot(Goal goal)
	{

		if (goal == null || !goalConnections.containsKey(goal))
		{
			System.out.println("No such goal " + goal.getVariableName() + " in planner");
			return null;
		}
		List<Goal> listToReturn = new LinkedList<Goal>();
		listToReturn.add(goal);
		
		Goal parent = goal.getParent();
		while (parent != null)
		{
			listToReturn.add(parent);
			parent = parent.getParent();
		}
		
		return listToReturn;		
	}
	
	public KQMLList modify(Goal newGoal, boolean strict, boolean accepted)
	{
		System.out.println("Modifying goal " + newGoal.getVariableName());
		List<Goal> failedGoals = getFailedGoals();
		// No failed goals, just add this to the active goal
		if (failedGoals.isEmpty() && activeGoal != null && !strict)
		{
			System.out.println("Adding goal: " + newGoal.getVariableName());
			System.out.println("Active goal: " + activeGoal.getVariableName());
			addGoal(newGoal, activeGoal.getVariableName());

			return newGoal.adoptContent("ELABORATION", activeGoal.getId());

		}
		// The active goal has failed, replace it
		else if (activeGoal != null && (activeGoal.isFailed() || strict))
		{
			System.out.println("Replacing goal " + activeGoal.getVariableName() + 
					" with " + newGoal.getVariableName());
			Goal activeParentGoal = activeGoal.getParent();
			String activeGoalName = activeGoal.getVariableName();
			String activeGoalId = activeGoal.getId();
			if (accepted)
				replaceGoal(newGoal, activeGoal);
			
			return newGoal.adoptContent("MODIFICATION", activeGoalId);
			
		}
		else if (underDiscussion != null)
		{
			if (underDiscussion.isFailed() || strict == true)
			{
				String underDiscussionName = underDiscussion.getVariableName();
				String underDiscussionId = underDiscussion.getId();
				if (accepted)
					replaceGoal(newGoal,underDiscussion);
				underDiscussion = newGoal;
				
				return newGoal.adoptContent("MODIFICATION", underDiscussionId);
			}
			else
			{
				String underDiscussionName = underDiscussion.getVariableName();
				String underDiscussionId = underDiscussion.getId();
				if (accepted)
					replaceGoal(newGoal,underDiscussion);
				underDiscussion = newGoal;
				
				return newGoal.adoptContent("MODIFICATION", underDiscussionId);
			}
		}
		else
		{
			
			// TODO: Make this smarter
			for (Goal failedGoal : failedGoals)
			{
				Goal parentGoal = failedGoal.getParent();
				System.out.println("Replacing goal " + failedGoal.getVariableName() + 
						" with " + newGoal.getVariableName());
				if (accepted)
					replaceGoal(newGoal, failedGoal);
				
				return newGoal.adoptContent("MODIFICATION", failedGoal.getId());
				

			}
		}
		
		return null;
	}
	
	public boolean replaceGoal(Goal newGoal, Goal oldGoal)
	{
		if (newGoal == null || oldGoal == null)
		{
			System.out.println("A goal in replacement was null");
			return false;
		}
		Goal parent = oldGoal.getParent();
		newGoal.setParent(parent);
		if (oldGoal == activeGoal || activeGoal == null)
		{
			System.out.println("Set goal: " + newGoal.getVariableName() + " as active goal");
			activeGoal = newGoal;
		}
		removeGoal(oldGoal.getVariableName());
		if (parent == null)
			addGoal(newGoal,null);
		else
			addGoal(newGoal,parent.getVariableName());
		
		return true;
	}
	
	public boolean removeGoal(String variableName)
	{
		String upperCaseVariableName = variableName.toUpperCase();
		if (variableGoalMapping.containsKey(upperCaseVariableName))
		{
			Goal goalToRemove = variableGoalMapping.get(upperCaseVariableName);
			if (activeGoal == goalToRemove)
				activeGoal = null;
			variableGoalMapping.remove(upperCaseVariableName);
			if (idGoalMapping.containsKey(goalToRemove.getId()))
				idGoalMapping.remove(goalToRemove.getId());
			goalConnections.remove(goalToRemove);
			return true;
			// TODO: Remove child goals of removed parent
		}
		
		return false;
	}
	
	public Goal rollback()
	{
		if (activeGoal.getParent() != null)
		{
			activeGoal = activeGoal.getParent();
		}
		
		return activeGoal;
	}
	
	
	// Sets the goal to the first subgoal
	public Goal startOver()
	{
		if (activeGoal == null)
			return null;
		
		while (activeGoal.getParent() != null && activeGoal.getParent().getParent() != null)
		{
			activeGoal = activeGoal.getParent();
			return activeGoal;
		}
		
		if (activeGoal.getParent() != null)
		{
			activeGoal = activeGoal.getParent();
			
		}
		
		return activeGoal;
		
	}
	
	public Set<Goal> getTopLevelGoals()
	{
		HashSet<Goal> ambiguousGoals = new HashSet<Goal>();
		for (Entry<Goal,Goal> e : goalConnections.entrySet())
		{
			if (e.getValue() != null && e.getValue().isValidGoal())
				ambiguousGoals.add(e.getKey());
		}
		
		return ambiguousGoals;
		
	}
	
	public Goal getGoal(String variableName)
	{
		if (variableName == null)
			return null;
		
		if (variableName.equalsIgnoreCase("ACTIVE-GOAL"))
			return getActiveGoal();
		
		if (variableGoalMapping.containsKey(variableName.toUpperCase()))
			return variableGoalMapping.get(variableName.toUpperCase());
		
		if (idGoalMapping.containsKey(variableName.toUpperCase()))
			return idGoalMapping.get(variableName);
		
		return null;
	}
	
	public Goal getGoalById(String id)
	{
		if (id == null)
			return null;
		
		if (id.equalsIgnoreCase("ACTIVE-GOAL"))
			return getActiveGoal();
		
		if (idGoalMapping.containsKey(id))
			return idGoalMapping.get(id);
		
		return null;		
	}
	
	public boolean hasGoalById(String id)
	{
		return idGoalMapping.containsKey(id);
	}
	
	public boolean hasGoal(String variableName)
	{
		return (variableGoalMapping.containsKey(variableName.toUpperCase()) || 
				idGoalMapping.containsKey(variableName.toUpperCase()));
	}
	
	public boolean hasActiveGoal()
	{
		return (activeGoal != null);
	}
	
	public synchronized Goal getActiveGoal() {
		return activeGoal;
	}

	public synchronized boolean setActiveGoal(Goal goal) {
		boolean succeeded = false;
		
		if (hasGoalById(goal.getId()) || hasGoal(goal.getVariableName()))
		{
			this.activeGoal = goal;
			ambiguousActiveGoal = false;
			System.out.println("Active goal now ID: " + goal.getId() +
					" WHAT: " + goal.getVariableName());
			return true;
		}
		
		succeeded = addGoal(goal);
		
		if (succeeded)
		{
			this.activeGoal = goal;
			ambiguousActiveGoal = false;
			System.out.println("Active goal now ID: " + goal.getId() +
							" WHAT: " + goal.getVariableName());
		}
		
		
		
		return succeeded;
	}
	
	// Adds the goal from context if not already present
	public synchronized boolean setActiveGoal(String goal, KQMLList context)
	{
		if (hasGoal(goal))
		{
			System.out.println("Active goal now: " + goal);
			return setActiveGoal(getGoal(goal));
		}
		else
		{
			KQMLList goalTerm = TermExtractor.extractTerm(goal, context);
			if (goalTerm != null)
				return setActiveGoal(new Goal(goalTerm,context));
			else
				return false;
		}
		
	}
	
	public boolean setActiveGoal(String goal)
	{
		return setActiveGoal(getGoal(goal));
	}

	public List<Goal> generatePossibleGoals(Collection<String> goalTypes)
	{
		List<Goal> goalsToReturn = new ArrayList<Goal>();
		
		for (String goalType : goalTypes)
		{
			KQMLList goalTerm = new KQMLList();
			goalTerm.add("ONT::RELN");
			goalTerm.add(IDHandler.getNewID());
			goalTerm.add(":INSTANCE-OF");
			goalTerm.add(goalType);
			
			Goal newGoal = new Goal(goalTerm, new KQMLList());
			addGoal(newGoal);
			goalsToReturn.add(newGoal);
		}
		
		return goalsToReturn;
	}

	public Goal getPrivateGoal() {
		return privateGoal;
	}
	
	public Goal getNonActionAncestor(Goal goal)
	{
		Goal parent = goal.getParent();

		while (parent != null)
		{
			if (!(parent instanceof Action ))
				return parent;
			else
				parent = parent.getParent();
		}
		return parent;
	}
	
	public void setParentGoalToActive(Goal completedOrAbandonedGoal) {
		if (completedOrAbandonedGoal.getParent() != null)
		{

			boolean succeeded = setActiveGoal(completedOrAbandonedGoal.getParent());
			if (succeeded)
				System.out.println("Set active goal to " +
						completedOrAbandonedGoal.getParent().getVariableName());
			else
				System.out.println("Failed to set active goal to " +
						completedOrAbandonedGoal.getParent().getVariableName());
		}
        else
        {
	        	Set<Goal> topLevelGoals = getTopLevelGoals();
	        	int numberOfNewPossibleGoals = 0;
	        	Goal lastAcceptableGoal = null;
	        	for (Goal g : topLevelGoals)
	        	{
	        		if (g.isValidGoal())
	        		{
	        			numberOfNewPossibleGoals++;
	        			lastAcceptableGoal = g;
	        		}
	        	}
	        	
	        	if (numberOfNewPossibleGoals == 0)
	        		activeGoal = null;
	        	else if (numberOfNewPossibleGoals == 1)
	        		activeGoal = lastAcceptableGoal;
	        	else
	        	{
	        		activeGoal = null;
	        		ambiguousActiveGoal = true;
	        	}
        }
	}
	
	public synchronized void setCompleted(Goal goal)
	{
		goal.setCompleted(true);
		System.out.println("Completed goal " + goal.getVariableName());
		
		setParentGoalToActive(goal);
		
	}
	
	public boolean createAskFromAct(String cpsa, KQMLList act, KQMLList context)
	{
		if (act.getKeywordArg(":WHAT") == null && act.getKeywordArg(":QUERY") == null)
		{
			System.out.println("No :WHAT or :QUERY parameter");
			return false;
		}
		String goalName = act.getKeywordArg(":QUERY").stringValue();
		if (goalName == null)
			goalName = act.getKeywordArg(":WHAT").stringValue();
		
		
		System.out.println("Creating goal: " + goalName);
		
		KQMLList goalLF = TermExtractor.extractTerm(goalName, (KQMLList)context);
		if (goalLF == null)
		{
			System.out.println("Not a valid query to add to the system");
			return false;
		}
		
		KQMLObject asObject = act.getKeywordArg(":AS");
		String type = "QUERY-IN-CONTEXT";
		String parent = null;
		if (asObject != null)
		{
			KQMLList asList = (KQMLList)asObject;
			type = asList.get(0).stringValue();
            KQMLObject parentObject = asList.getKeywordArg(":OF");
            if (parentObject == null)
            	parentObject = asList.getKeywordArg(":GOAL");
			if (parentObject != null)
                parent = parentObject.stringValue();
		}
		
		Goal newGoal = null;

		if (type.equalsIgnoreCase("ANSWER"))
		{
			// TODO: Something here?
		}
		else if (type.equalsIgnoreCase("QUERY-IN-CONTEXT"))
		{
			newGoal = new Query(act,getGoal(parent),context);
			addGoal(newGoal,parent);
			
		}
		
		if (cpsa.equals("ACCEPT"))
		{
			newGoal.setAccepted();
			setActiveGoal(newGoal);
			System.out.println("Active goal now: " + goalName);
		}
		return true;
	}
	
	public boolean createGoalFromAct(String cpsa, KQMLList act, KQMLList context)
	{
		String goalName = "";
		KQMLObject goalIdObject = act.getKeywordArg(":ID");
		if (act.getKeywordArg(":WHAT") != null)
		{
			goalName = act.getKeywordArg(":WHAT").stringValue();
			System.out.println("Creating goal: " + goalName);
		}

		if (goalIdObject != null)
			System.out.println("Goal has id: " + goalIdObject.stringValue());
		
		
		KQMLList goalLF = TermExtractor.extractTerm(goalName, (KQMLList)context);
		if (goalLF == null)
		{
			
			goalLF = new KQMLList();
			goalLF.add("ONT::RELN");
			goalLF.add(goalName);
			goalLF.add(":INSTANCE-OF");
			goalLF.add("DUMMY-GOAL");
			System.out.println(goalIdObject + ": Not a valid goal to add to the system. Creating DUMMY-GOAL.");
			//return false;
		}
		
		KQMLObject asObject = act.getKeywordArg(":AS");
		// If this is a problem again, probably don't have a default
		String type = "GOAL";
		
		// Check if it's an assertion
		if (act.get(0).stringValue().equalsIgnoreCase("ASSERTION"))
		{
			type = "ASSERTION";
			System.out.println("This is an assertion");
		}
		else if (act.get(0).stringValue().equalsIgnoreCase("ASK-WH"))
		{
			type = "ASK-WH";
			System.out.println("This is a ASK-WH question");
		}
		else if (act.get(0).stringValue().equalsIgnoreCase("ASK-IF"))
		{
			type = "ASK-IF";
			System.out.println("This is a ASK-IF question");
		}
		else if (act.get(0).stringValue().equalsIgnoreCase("ANSWER"))
		{
			// This should probably be moved out of this function soon
			type = "ANSWER";
			
			if (act.getKeywordArg(":TO") != null)
			{
				String toId = act.getKeywordArg(":TO").stringValue();
				System.out.println("This is an answer to query " + toId);
				Goal toGoal = getGoalById(toId);
				if (toGoal == null)
					System.out.println("Query " + toId + " not found");
				else if (!(toGoal instanceof Query))
					System.out.println("Goal " + toId + " is not a query");
				else
				{
					System.out.println("Answer successful");
					((Query)toGoal).setAnswered(true);
				}
			}
			return false;
		}
		String parent = null;
		if (asObject != null && !type.equalsIgnoreCase("ASSERTION"))
		{
			KQMLList asList = (KQMLList)asObject;
			type = asList.get(0).stringValue();
            KQMLObject parentObject = asList.getKeywordArg(":OF");
            if (parentObject == null)
            	parentObject = asList.getKeywordArg(":GOAL");
			if (!KQMLUtilities.isKQMLNull(parentObject))
                parent = parentObject.stringValue();
		}
		
		if (parent == null && !type.equalsIgnoreCase("QUERY-IN-CONTEXT"))
		{
			// Queries can have no parent
			if (activeGoal != null)
				parent = activeGoal.getVariableName();
			else
				parent = underDiscussion.getVariableName();
		}
		
		Goal newGoal = null;
		if (type.equalsIgnoreCase("GOAL"))
		{
			newGoal = new Goal(goalLF,context);
			if (goalIdObject != null)
				newGoal.setId(goalIdObject.stringValue());
			addGoal(newGoal);
		}
		else if (type.equalsIgnoreCase("SUBGOAL") || type.equalsIgnoreCase("ASSERTION"))
		{
			newGoal = new Goal(goalLF,getGoal(parent),context);
			if (goalIdObject != null)
				newGoal.setId(goalIdObject.stringValue());
			addGoal(newGoal,parent);
		}
		else if (type.equalsIgnoreCase("MODIFY") || type.equalsIgnoreCase("MODIFICATION"))
		{
			// Not actually the "parent", just the :OF relation
			System.out.println("Parent: " + parent);
			System.out.println("Parent object: " + getGoal(parent));
			newGoal = new Goal(goalLF,getGoal(parent).getParent(),context);
			if (goalIdObject != null)
				newGoal.setId(goalIdObject.stringValue());
			underDiscussion = newGoal;
			if (cpsa.equalsIgnoreCase("ACCEPT"))
				replaceGoal(newGoal,getGoal(parent));
		}

		else if (type.equalsIgnoreCase("QUERY-IN-CONTEXT"))
		{
			newGoal = new Query(act,getGoal(parent),context);
			if (goalIdObject != null)
				newGoal.setId(goalIdObject.stringValue());
			//newGoal = new Query(goalLF,getGoal(parent),context);
			
			Goal oldGoal = getGoal(newGoal.getId());
			if (oldGoal!= null)
				replaceGoal(newGoal,oldGoal);
			else
				addGoal(newGoal,parent);
			
			
			// Hacky crap
			String query = null;
			if (act.getKeywordArg(":QUERY") != null)
				query = act.getKeywordArg(":QUERY").stringValue();
			if (query == null && act.getKeywordArg(":OF") != null)
				query = act.getKeywordArg(":OF").stringValue();
			
			KQMLObject whatObject = act.getKeywordArg(":WHAT");
			if (query == null)
				query = "";
			String what = "";
			if (whatObject != null)
				what = whatObject.stringValue();
			String mapping = query + what;
			queryMapping.put(mapping, (Query)newGoal);
			queryMapping.put(query, (Query)newGoal);
		}
		
		if (cpsa.equals("ACCEPT"))
		{
			if (newGoal == null)
			{
				System.out.println("No goal to accept.");
				return false;
			}
				
			newGoal.setAccepted();
			if (!hasAmbiguousActiveGoal())
			{
				setActiveGoal(newGoal);
				System.out.println("Active goal now: " + goalName);
			}
		}
		else if (cpsa.equals("PROPOSE"))
		{
			if (newGoal == null)
			{
				System.out.println("No goal proposed.");
				return false;
			}
			System.out.println("Proposed goal has: ");
			System.out.println("\tID: " + newGoal.getId());
			System.out.println("\tWhat: " + newGoal.getVariableName());
		}
		return true;
	}
	
	public Goal getNonActionAncestor(String goalName)
	{
		return  getNonActionAncestor(getGoal(goalName));
	}

	public boolean isGlobalSystemInitiative() {
		return globalSystemInitiative;
	}

	public void setGlobalSystemInitiative(boolean globalSystemInitiative) {
		this.globalSystemInitiative = globalSystemInitiative;
	}

	public boolean isOverrideSystemInitiative() {
		return overrideSystemInitiative;
	}

	public void setOverrideSystemInitiative(boolean overrideSystemInitiative) {
		this.overrideSystemInitiative = overrideSystemInitiative;
	}

	public boolean getOverrideSystemInitiativeValue() {
		return overrideSystemInitiativeValue;
	}

	public void setOverrideSystemInitiativeValue(
			boolean overrideSystemInitiativeValue) {
		this.overrideSystemInitiativeValue = overrideSystemInitiativeValue;
	}

	public HashMap<String, Query> getQueryMapping() {
		return queryMapping;
	}

	public boolean hasAmbiguousActiveGoal() {
		return ambiguousActiveGoal;
	}

	public HashMap<String, Goal> getIdGoalMapping() {
		return idGoalMapping;
	}
	
	
}
