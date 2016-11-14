package states;

import java.util.*;

import extractors.TermExtractor;
import TRIPS.KQML.KQMLList;

public class Goal {

	Goal parent;
	List<Goal> childGoals;
	KQMLList term;
	boolean accepted;
	boolean failed;
	boolean completed;
	boolean systemTookInitiative;
	List<KQMLList> failureMessages;
	
	public Goal(KQMLList term)
	{
		this(term, null);
		
	}
	
	public Goal(KQMLList term, Goal parent)
	{
		this.term = term;
		this.parent = parent;
		accepted = false;
		failed = false;
		completed = false;
		systemTookInitiative = false;
		childGoals = new LinkedList<Goal>();
		failureMessages = new ArrayList<KQMLList>();
	}
	
	public Goal(String variableName, KQMLList context, Goal parent)
	{
		this(TermExtractor.extractTerm(variableName, context),parent);
	}
	
	public Goal(String variableName, KQMLList context)
	{
		this(variableName,context,null);
	}
	
	
	public KQMLList getKQMLTerm()
	{
		return term;
	}
	
	public String getVariableName()
	{
		return term.get(1).stringValue();
	}
	
	public void setAccepted()
	{
		accepted = true;
	}

	public boolean isFailed() {
		return failed;
	}

	public void setFailed(boolean failed) {
		this.failed = failed;
	}

	public boolean systemTookInitiative() {
		return systemTookInitiative;
	}

	public void setSystemTookInitiative(boolean systemTookInitiative) {
		this.systemTookInitiative = systemTookInitiative;
	}
	
	public Goal getParent()
	{
		return parent;
	}
	
	public void setParent(Goal newParent)
	{
		this.parent = newParent;
	}
	
    public KQMLList adoptContent(String goalType, String subgoalOf)
    {
    	KQMLList adopt = new KQMLList();
    	
    	adopt.add("ADOPT");
    	adopt.add(":what");
    	adopt.add(getVariableName());
    	adopt.add(":as");
    	
    	KQMLList goal = new KQMLList();
    	goal.add(goalType);
    	if (!goalType.equalsIgnoreCase("GOAL"))
    	{
    		goal.add(":of");
    		goal.add(subgoalOf);
    	}
    	
    	adopt.add(goal);
    	
    	return adopt;
    }
	
	public String toString()
	{
		StringBuilder sb = new StringBuilder();
		sb.append("Goal:" + getVariableName());
		if (parent != null)
		{
			sb.append("\nParent: " + parent.getVariableName());
		}
		return sb.toString(); 
	}

	public boolean isAccepted() {
		return accepted;
	}

	public boolean isCompleted() {
		return completed;
	}

	public void setCompleted(boolean completed) {
		this.completed = completed;
	}
	
}
