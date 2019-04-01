package plans;

import TRIPS.KQML.KQMLList;
import extractors.TermExtractor;
import states.Goal;
import states.Query;

public class GoalRemover {

	GoalSelector gs;
	GoalPlanner gp;
	
	public GoalRemover(GoalPlanner gp, GoalSelector gs) {
		this.gs = gs;
		this.gp = gp;
	}

	/**
	 * Takes in the content and context of an abandon speech act, returns the 
	 * goal that was abandoned if there was one, or null otherwise
	 * @param content
	 * @param context
	 * @return
	 */
	public boolean abandonGoal(Goal g)
	{

		if (g != null)
		{
			g.abandon();
	
			// The active goal is the one we're trying to cancel
			if (g.equals(gp.getActiveGoal()))
				gp.setParentGoalToActive(g);
			
			return true;
		}
		
		return false;
	}
	
	public static boolean removeGoal(GoalPlanner gp, String variableName)
	{
		String upperCaseVariableName = variableName.toUpperCase();
		
		if (gp.getVariableGoalMapping().containsKey(upperCaseVariableName))
		{
			Goal goalToRemove = gp.getVariableGoalMapping().get(upperCaseVariableName);
			if (gp.getActiveGoal() == goalToRemove)
				gp.clearActiveGoal();
			gp.getVariableGoalMapping().remove(upperCaseVariableName);
			if (gp.getIdGoalMapping().containsKey(goalToRemove.getId()))
				gp.getIdGoalMapping().remove(goalToRemove.getId());
			gp.getGoalConnections().remove(goalToRemove);
			return true;
			// TODO: Remove child goals of removed parent
		}
		
		return false;
	}
}
