package plans;

import java.util.HashSet;
import java.util.Iterator;

import TRIPS.KQML.KQMLList;
import extractors.OntologyReader;
import extractors.TermExtractor;
import states.Goal;
import states.Query;
import utilities.KQMLUtilities;

public class GoalSelector {

    GoalPlanner gp;


    public GoalSelector(GoalPlanner gp) {
	this.gp = gp;

    }

	public Goal getGoalByDescription(KQMLList term, KQMLList context)
	{
		String instanceOf = KQMLUtilities.getArgumentAsString(term,":INSTANCE-OF");
		String affectedResultInstanceOf = KQMLUtilities.getLinkedArgumentsAsString(term,context,
										":AFFECTED-RESULT",":INSTANCE-OF");
		
		HashSet<Goal> selection = new HashSet<Goal>();
		selection.addAll(gp.getIdGoalMapping().values());
		Iterator<Goal> it = selection.iterator();
		
		// First pass - remove failed goals
		while (it.hasNext())
		{
			Goal currentGoal = it.next();
			if (!currentGoal.isValidGoal())
				it.remove();
		}
		
		it = selection.iterator();		
		
		// Second pass with goal type
		while (it.hasNext())
		{
			Goal currentGoal = it.next();
			if (currentGoal.getArgumentAsString(":INSTANCE-OF") == null ||
					!currentGoal.getArgumentAsString(":INSTANCE-OF").equals(instanceOf))
				it.remove();
		}
		
		it = selection.iterator();

		// Third pass with affected-result goal type
		if (affectedResultInstanceOf != null)
		{
			while (it.hasNext())
			{
				Goal currentGoal = it.next();
				String currentAffRes = KQMLUtilities.getLinkedArgumentsAsString(currentGoal.getKQMLTerm(),
										currentGoal.getOriginalContext(), ":AFFECTED-RESULT",":INSTANCE-OF");
				if (currentAffRes == null || 
						!currentAffRes.equals(affectedResultInstanceOf))
					it.remove();
			}
		}
		
		
		if (selection.size() == 1)
			for (Goal g : selection)
				return g;
		
		return null;
	}
	
	/**
	 * Returns the most likely goal to abandon given the term in context 
	 * specified by the variable given.
	 * @param variable
	 * @param context
	 * @return Goal to abandon, or null if no appropriate goal found
	 */
	public Goal findGoalToAbandon(String variable, KQMLList context)
	{
		Goal referencedGoal = null;
		KQMLList variableTerm = TermExtractor.extractTerm(variable, context);
		// Check to see if this is a reference to a particular goal
		if (variableTerm != null)
		{
			referencedGoal = getGoalByDescription(variableTerm, context);
		}
		
		// If not, see if the goal under discussion is not a question
		if (referencedGoal == null)
		{
			Goal goalUnderDiscussion = gp.getGoalUnderDiscussion();
			
			if (!(goalUnderDiscussion instanceof Query) && goalUnderDiscussion.isAccepted())
				referencedGoal = goalUnderDiscussion;
			else // If it is, look for a non-question ancestor
			{
				System.out.println("Goal to abandon is a query or not accepted yet.");
				referencedGoal = getLeastNonQueryAcceptedAncestor(goalUnderDiscussion);
			}
		}
		
		return referencedGoal;
	}
	
	/**
	 * Returns the most likely query or proposal to reject given the term in context 
	 * specified by the variable given.
	 * @param variable
	 * @param context
	 * @return Goal to abandon, or null if no appropriate goal found
	 */	
	public Goal findProposalOrQuestionToReject(String variable, KQMLList context)
	{
		Goal referencedGoal = null;
		KQMLList variableTerm = TermExtractor.extractTerm(variable, context);
		// Check to see if this is a reference to a particular goal
		if (variableTerm != null)
		{
			referencedGoal = getGoalByDescription(variableTerm, context);
		}

		if (referencedGoal == null)
		{
			referencedGoal =  gp.getGoalUnderDiscussion();
		}
		
		if (referencedGoal.isAccepted())
			return null;
		
		return referencedGoal;
	}
	
	public Goal getLeastNonQueryAcceptedAncestor(Goal g)
	{
		Goal currentGoal = g;
		
		while ((currentGoal instanceof Query || !currentGoal.isAccepted()) 
				&& currentGoal.getParent() != null)
		{
			currentGoal = g.getParent();
		}
		
		if (currentGoal instanceof Query || !currentGoal.isAccepted())
			return null;
		
		return currentGoal;
	}
}
