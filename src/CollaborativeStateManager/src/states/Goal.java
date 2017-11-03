package states;

import handlers.IDHandler;
import handlers.ReferenceHandler;

import java.util.*;

import extractors.TermExtractor;
import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLObject;
import TRIPS.KQML.KQMLString;
import TRIPS.KQML.KQMLToken;

public class Goal {

	Goal parent;
	LinkedList<Goal> childGoals;
	Goal nextSibling;
	KQMLList term;
	String id;
	boolean accepted;
	boolean failed;
	boolean completed;
	boolean rejected;
	boolean abandoned;
	boolean systemTookInitiative;
	List<KQMLList> failureMessages;
	boolean initiativeSpecified;
	boolean specifiedSystemInitiative;
	boolean systemOwnedGoal;
	KQMLList additionalContext;
	KQMLList originalContext;
	
	
	public Goal(KQMLList term, KQMLList context)
	{
		this(term, null, context);
		
	}
	
	public Goal(KQMLList term, Goal parent, KQMLList context)
	{
		this.term = term;
		this.parent = parent;
		accepted = false;
		failed = false;
		abandoned = false;
		completed = false;
		systemTookInitiative = false;
		initiativeSpecified = false;
		specifiedSystemInitiative = false;
		childGoals = new LinkedList<Goal>();
		failureMessages = new ArrayList<KQMLList>();
		additionalContext = new KQMLList();
		originalContext = new KQMLList();
		originalContext.addAll(context);
		rejected = false;
		if (term.getKeywordArg(":ID") == null)
			id = IDHandler.getNewID();
		else
			id = term.getKeywordArg(":ID").stringValue();
		nextSibling = null;
	}
	
	public Goal(Goal toCopy)
	{
		String variableName = IDHandler.getNewID();
		// Take the old term but replace the variable name
		this.term = new KQMLList();
        this.term.addAll(toCopy.term);
		term.set(1, new KQMLToken(variableName));
		this.parent = toCopy.parent;
		accepted = toCopy.accepted;
		failed = toCopy.failed;
		completed = toCopy.completed;
		systemTookInitiative = toCopy.systemTookInitiative;
		initiativeSpecified = toCopy.initiativeSpecified;
		specifiedSystemInitiative = toCopy.specifiedSystemInitiative;
		childGoals = new LinkedList<Goal>(toCopy.childGoals);
		failureMessages = new ArrayList<KQMLList>(toCopy.failureMessages);
		additionalContext = new KQMLList(toCopy.additionalContext);
		originalContext = new KQMLList();
		originalContext.addAll(toCopy.originalContext);
		rejected = false;
		id = IDHandler.getNewID();
	}
	
	public Goal(String variableName, KQMLList context, Goal parent)
	{
		this(TermExtractor.extractTerm(variableName, context),parent, context);
		if (context != null)
			originalContext.addAll(context);
	}
	
	public Goal(String variableName, KQMLList context)
	{
		this(variableName,context,null);
	}

	public void addContext(KQMLList context)
	{
		originalContext.addAll(context);
	}
	
	public KQMLList getOriginalContext()
	{
		return originalContext;
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
	
	public String getId() {
		return id;
	}
	
	public void setId(String id)
	{
		this.id = id;
	}

	public void setInitiativeAgent(String agentSymbol, KQMLList context)
	{
		initiativeSpecified = true;
		
		KQMLList agentTerm = TermExtractor.extractTerm(agentSymbol, (KQMLList)context);
		if (agentTerm.getKeywordArg(":REFERS-TO") != null &&
				agentTerm.getKeywordArg(":REFERS-TO").stringValue().equalsIgnoreCase("ONT::SYS"))
			specifiedSystemInitiative = true;
		else
			specifiedSystemInitiative = false;
		
		setArgument(":AGENT", agentSymbol);
		additionalContext.add(TermExtractor.extractTerm(agentSymbol, context));
		
	}
	
	public void resetInitiative()
	{
		initiativeSpecified = false;
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
	
	private void addChild(Goal newChild)
	{
		Goal previousSibling = null;
		if (!childGoals.isEmpty())
			previousSibling = childGoals.getLast();
		childGoals.add(newChild);
		
	}
	
	private Goal getNextSibling()
	{
		// Yes, I know this isn't optimized. You can do it.
		if (parent != null)
		{
			int nextIndex = parent.childGoals.indexOf(this) + 1;
			if (nextIndex < parent.childGoals.size())
				return parent.childGoals.get(nextIndex);
		}
		return null;
	}
	
	private Goal getPreviousSibling()
	{
		// Yes, I know this isn't optimized. You can do it.
		if (parent != null)
		{
			int previousIndex = parent.childGoals.indexOf(this) - 1;
			if (previousIndex > 0)
				return parent.childGoals.get(previousIndex);
		}
		return null;
	}
	
	public void removeChild(Goal childToRemove)
	{
		childGoals.remove(childToRemove);
	}
	
	public void setArgument(String argument, String value)
	{
		term.removeKeywordArg(argument);
		term.add(argument);
		term.add(value);
	}
	
    public boolean isInitiativeSpecified() {
		return initiativeSpecified;
	}

	public KQMLList adoptContent(String goalType, String subgoalOf)
    {
    	KQMLList adopt = new KQMLList();
    	
    	adopt.add("ADOPT");
    	adopt.add(":what");
    	adopt.add(getVariableName());
    	adopt.add(":id");
    	adopt.add(getId());
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
	    // LG: added more information in a compact form
	    if (false) {
		StringBuilder sb = new StringBuilder();
		sb.append("Goal:" + getVariableName());
		if (parent != null)
		    {
			sb.append("\nParent: " + parent.getVariableName());
		    }
		return sb.toString();
	    }
	    return "Goal:" + getVariableName()
		+ "[" + ( "A:" + accepted +
			  ",R:" + rejected +
			  ",C:" + completed +
			  ",F:" + failed ) + "]"
		+ ( (parent != null) ? ("->" + parent.getVariableName()) : "" );
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
	
	public KQMLList getAdditionalContext()
	{
		return additionalContext;
	}
	
	public String getArgumentAsString(String argument)
	{
		KQMLObject resultObject = term.getKeywordArg(argument);
		if (resultObject == null)
			return null;
		
		return resultObject.stringValue();
	}

	public boolean isAbandoned() {
		return abandoned;
	}

	public void abandon() {
		this.abandoned = true;
		for (Goal child : childGoals)
		{
			child.abandon();
		}
	}
	
	public boolean isValidGoal()
	{
		return !isAbandoned() && !isFailed() && !isCompleted();
	}
	
}
