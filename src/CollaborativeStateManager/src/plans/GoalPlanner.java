package plans;

import handlers.IDHandler;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map.Entry;

import extractors.TermExtractor;
import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLObject;
import states.Action;
import states.Goal;

public class GoalPlanner {

	private HashMap<Goal,Goal> goalConnections; // Child -> Parent
	private HashMap<String,Goal> variableGoalMapping;
	private Goal activeGoal;
	private Goal privateGoal;
	private boolean globalSystemInitiative = false;
	private boolean overrideSystemInitiative = false;
	private boolean overrideSystemInitiativeValue = false;
	
	public GoalPlanner()
	{
		goalConnections = new HashMap<Goal,Goal>();
		variableGoalMapping = new HashMap<String,Goal>();
		privateGoal = null;
		activeGoal = null;
	}
	
	public boolean addGoal(Goal goal, String parentVariableName)
	{
		if (goal == null)
		{
			System.out.println("Tried to add a null goal");
			return false;
		}
		
		if (hasGoal(goal.getVariableName()))
		{
			System.out.println("Goal " + goal.getVariableName() + " already in planner.");
			return false;
		}
		
		String upperCaseParentVariableName = null;
		if (parentVariableName != null)
			upperCaseParentVariableName = parentVariableName.toUpperCase();
		
		Goal parentGoal = getGoal(upperCaseParentVariableName);
		if (parentGoal != null && parentGoal.isFailed())
		{
			System.out.println("Replacing " + parentGoal.getVariableName() + 
					" with " + goal.getVariableName());
			replaceGoal(goal,parentGoal);
		}
		else
		{
			goalConnections.put(goal,getGoal(upperCaseParentVariableName));
			variableGoalMapping.put(goal.getVariableName().toUpperCase(),goal);
			goal.setParent(getGoal(upperCaseParentVariableName));
			System.out.println("Added goal " + goal.getVariableName() + " with parent "
					+ upperCaseParentVariableName);
		}
		
		return true;
	}
	
	public boolean addGoal(Goal goal)
	{
		return addGoal(goal,null);
	
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
	
	public KQMLList modify(Goal newGoal, String oldGoalName)
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
		replaceGoal(newGoal, oldGoal);
		
		return newGoal.adoptContent("MODIFICATION", oldGoalName);
		
//		if (parentGoal != null)
//			return newGoal.adoptContent("SUBGOAL", parentGoal.getVariableName());
//		else
//			return newGoal.adoptContent("GOAL", null);
		
		
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
	
	public KQMLList modify(Goal newGoal)
	{
		System.out.println("Modifying goal " + newGoal.getVariableName());
		List<Goal> failedGoals = getFailedGoals();
		// No failed goals, just add this to the active goal
		if (failedGoals.isEmpty() && activeGoal != null)
		{
			System.out.println("Adding goal: " + newGoal.getVariableName());
			System.out.println("Active goal: " + activeGoal.getVariableName());
			addGoal(newGoal, activeGoal.getVariableName());
			return newGoal.adoptContent("MODIFICATION", activeGoal.getVariableName());
			//return newGoal.adoptContent("SUBGOAL", activeGoal.getVariableName());
		}
		// The active goal has failed, replace it
		else if (activeGoal != null && activeGoal.isFailed())
		{
			System.out.println("Replacing goal " + activeGoal.getVariableName() + 
					" with " + newGoal.getVariableName());
			Goal activeParentGoal = activeGoal.getParent();
			String activeGoalName = activeGoal.getVariableName();
			replaceGoal(newGoal, activeGoal);
			
			return newGoal.adoptContent("MODIFICATION", activeGoalName);
			
//			if (activeParentGoal != null)
//				return newGoal.adoptContent("SUBGOAL", activeParentGoal.getVariableName());
//			else
//				return newGoal.adoptContent("GOAL", null);
		}
		else
		{
			
			// TODO: Make this smarter
			for (Goal failedGoal : failedGoals)
			{
				Goal parentGoal = failedGoal.getParent();
				System.out.println("Replacing goal " + failedGoal.getVariableName() + 
						" with " + newGoal.getVariableName());
				replaceGoal(newGoal, failedGoal);
				
				return newGoal.adoptContent("MODIFICATION", failedGoal.getVariableName());
				
//				if (parentGoal != null)
//					return newGoal.adoptContent("SUBGOAL", parentGoal.getVariableName());
//				else
//					return newGoal.adoptContent("GOAL", null);
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
	
	public Goal getGoal(String variableName)
	{
		if (variableName != null && 
				variableGoalMapping.containsKey(variableName.toUpperCase()))
			return variableGoalMapping.get(variableName.toUpperCase());
		
		return null;
	}
	
	public boolean hasGoal(String variableName)
	{
		return variableGoalMapping.containsKey(variableName.toUpperCase());
	}
	
	public boolean hasActiveGoal()
	{
		return (activeGoal != null);
	}
	
	public Goal getActiveGoal() {
		return activeGoal;
	}

	public boolean setActiveGoal(Goal goal) {
		boolean succeeded = false;
		
		if (hasGoal(goal.getVariableName()))
		{
			this.activeGoal = goal;
			return true;
		}
		
		succeeded = addGoal(goal);
		
		if (succeeded)
			this.activeGoal = goal;
		
		
		return succeeded;
	}
	
	// Adds the goal from context if not already present
	public boolean setActiveGoal(String goal, KQMLList context)
	{
		if (hasGoal(goal))
			return setActiveGoal(getGoal(goal));
		else
		{
			KQMLList goalTerm = TermExtractor.extractTerm(goal, context);
			if (goalTerm != null)
				return setActiveGoal(new Goal(goalTerm));
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
			
			Goal newGoal = new Goal(goalTerm);
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
	
	public void setCompleted(Goal goal)
	{
		goal.setCompleted(true);
		System.out.println("Completed goal " + goal.getVariableName());
		
		if (goal.getParent() != null)
		{

			boolean succeeded = setActiveGoal(goal.getParent());
			if (succeeded)
				System.out.println("Set active goal to " +
					goal.getParent().getVariableName());
			else
				System.out.println("Failed to set active goal to " +
						goal.getParent().getVariableName());
		}
	}
	
	public boolean createGoalFromAct(KQMLList act, KQMLList context)
	{
		if (act.getKeywordArg(":WHAT") == null)
		{
			System.out.println("No :WHAT parameter");
			return false;
		}
		String goalName = act.getKeywordArg(":WHAT").stringValue();
		System.out.println("Accepting goal: " + goalName);
		
		KQMLList goalLF = TermExtractor.extractTerm(goalName, (KQMLList)context);
		if (goalLF == null)
		{
			System.out.println("Not a valid goal to add to the system");
			return false;
		}
		
		KQMLObject asObject = act.getKeywordArg(":AS");
		String type = "GOAL";
		String parent = null;
		if (asObject != null)
		{
			KQMLList asList = (KQMLList)asObject;
			type = asList.get(0).stringValue();
			parent = asList.getKeywordArg(":OF").stringValue();
		}
		
		Goal newGoal = null;
		if (type.equalsIgnoreCase("GOAL"))
		{
			newGoal = new Goal(goalLF);
			addGoal(newGoal);
		}
		else if (type.equalsIgnoreCase("SUBGOAL"))
		{
			newGoal = new Goal(goalLF,getGoal(parent));
			addGoal(newGoal,parent);
		}
		else if (type.equalsIgnoreCase("MODIFY"))
		{
			newGoal = new Goal(goalLF,getGoal(parent).getParent());
			replaceGoal(newGoal,getGoal(parent));
		}
		else if (type.equalsIgnoreCase("ANSWER"))
		{
			// TODO: Something here?
		}
		newGoal.setAccepted();
		setActiveGoal(newGoal);
		System.out.println("Active goal now: " + goalName);
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
	
	
	
}
