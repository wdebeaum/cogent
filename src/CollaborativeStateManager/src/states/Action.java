package states;

import TRIPS.KQML.KQMLList;

public class Action extends Goal {

	Goal contributesTo;
	String actionType;
	
	public Action(KQMLList term, KQMLList context) {
		super(term,context);
		actionType = "unknown";
		// TODO Auto-generated constructor stub
	}

	public Action(KQMLList term, Goal parent,KQMLList context) {
		super(term, parent, context);
		actionType = "unknown";
		// TODO Auto-generated constructor stub
	}

	public Action(String variableName, KQMLList context, Goal parent) {
		super(variableName, context, parent);
		actionType = "unknown";
		// TODO Auto-generated constructor stub
	}

	public Action(String variableName, KQMLList context) {
		super(variableName, context);
		actionType = "unknown";
		// TODO Auto-generated constructor stub
	}

	public Goal getContributesTo() {
		return contributesTo;
	}

	public void setContributesTo(Goal contributesTo) {
		this.contributesTo = contributesTo;
	}

	public String getActionType() {
		return actionType;
	}

	public void setActionType(String actionType) {
		this.actionType = actionType;
	}
	
	
}
