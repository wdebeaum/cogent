package states;

import TRIPS.KQML.KQMLList;

public class Elaboration extends Goal {

	public Elaboration(KQMLList term, KQMLList context) {
		super(term, context);
		// TODO Auto-generated constructor stub
	}

	public Elaboration(KQMLList term, Goal parent, KQMLList context) {
		super(term, parent, context);
		// TODO Auto-generated constructor stub
	}

	public Elaboration(Goal toCopy) {
		super(toCopy);
		// TODO Auto-generated constructor stub
	}

	public Elaboration(String variableName, KQMLList context, Goal parent) {
		super(variableName, context, parent);
		// TODO Auto-generated constructor stub
	}

	public Elaboration(String variableName, KQMLList context) {
		super(variableName, context);
		// TODO Auto-generated constructor stub
	}

}
