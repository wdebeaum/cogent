package plans;

import java.util.List;

import extractors.OntologyReader;
import states.ActType;
import states.Goal;
import utilities.KQMLContentContext;
import utilities.KQMLUtilities;
import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLObject;

public class Messages {

	public Messages() {
		// TODO Auto-generated constructor stub
	}

	public static KQMLList adoptContent(String id, String what, String goalType, String subgoalOf) {
		KQMLList adopt = new KQMLList();

		adopt.add("ADOPT");
		if (id != null) {
			adopt.add(":ID");
			adopt.add(id);
		}
		adopt.add(":what");
		adopt.add(what);
		adopt.add(":as");

		KQMLList goal = new KQMLList();
		goal.add(goalType);
		if (!goalType.equalsIgnoreCase("GOAL")) {
			goal.add(":of");
			goal.add(subgoalOf);
		}
		adopt.add(goal);

		return adopt;
	}

	public static KQMLList selectContent(String id, String what) {
		KQMLList select = new KQMLList();

		select.add("SELECT");
		if (id != null) {
			select.add(":ID");
			select.add(id);
		}
		select.add(":what");
		select.add(what);

		return select;
	}

	public static KQMLList abandonContent(String id, String what) {
		KQMLList abandon = new KQMLList();
		
		abandon.add("ABANDON");
		
		if (id != null)
		{
			abandon.add(":ID");
			abandon.add(id);
		}
		if (what != null)
		{
			abandon.add(":WHAT");
			abandon.add(what);
		}
		
		return abandon;
	}
	
	public static KQMLList rejectContent(String id, String what) {
		KQMLList reject = new KQMLList();
		
		reject.add("REJECT");
		
		if (id != null)
		{
			reject.add(":ID");
			reject.add(id);
		}
		if (what != null)
		{
			reject.add(":WHAT");
			reject.add(what);
		}
		
		return reject;
	}

	public static KQMLList answerContent(String id, String what, String query) {
		KQMLList answer = new KQMLList();

		answer.add("ADOPT");
		answer.add(":ID");
		answer.add(id);
		answer.add(":what");
		answer.add(what);
		answer.add(":as");

		KQMLList goal = new KQMLList();
		goal.add("ANSWER");
		goal.add(":TO");
		goal.add(query);

		answer.add(goal);

		return answer;
	}

	public static KQMLList missingActiveGoal(String attemptedGoalType, String what, 
			ActType actType, KQMLList context,
			GoalPlanner goalPlanner, OntologyReader ontologyReader) {
		KQMLContentContext result = missingActiveGoalContentContext(attemptedGoalType, 
									what, actType, context, goalPlanner, ontologyReader);
		return reportContent(result.getContent(), result.getContext());
	}

	public static KQMLContentContext missingActiveGoalContentContext(String attemptedGoalType, String what,
			ActType actType, KQMLList context,
			GoalPlanner goalPlanner, OntologyReader ontologyReader) {
		KQMLList failureReason = new KQMLList();
		failureReason.add("MISSING-ACTIVE-GOAL");

		List<String> parentActGoalTypes = ontologyReader.getParentGoalsOfActType(actType);
		List<String> parentGoalTypes = ontologyReader.getParentGoals(attemptedGoalType);
		List<Goal> possibleSolutionGoals = null;

		if (parentGoalTypes == null)
			return missingActiveGoalContentContext(what, actType, context, goalPlanner, ontologyReader);

		// Try act-specific goals first
		if (parentActGoalTypes != null && !parentActGoalTypes.isEmpty())
			possibleSolutionGoals = goalPlanner.generatePossibleGoals(parentActGoalTypes);

		// LG: FIXME: if this is ok, then why is the previous one needed?!?
		// IP: I try act-specific goals first, for example, if there is an assertion
		// without a top-level goal, it will suggest TEACH-TRAIN as the top level goal
		if (possibleSolutionGoals == null)
			possibleSolutionGoals = goalPlanner.generatePossibleGoals(parentGoalTypes);

		KQMLList newContext = new KQMLList();
		newContext.addAll(context);
		KQMLList adoptContentList = new KQMLList();
		for (Goal possibleSolutionGoal : possibleSolutionGoals) {
			System.out.println("Possible solution: " + possibleSolutionGoal.getKQMLTerm());
			System.out.println("Possible solution goal: " + possibleSolutionGoal);
			newContext.add(possibleSolutionGoal.getKQMLTerm());

			KQMLList adoptContent = Messages.adoptContent(possibleSolutionGoal.getId(),
					possibleSolutionGoal.getVariableName(), "GOAL", null);
			System.out.println("ID: " + possibleSolutionGoal.getId());
			System.out.println("What: " + possibleSolutionGoal.getVariableName());
			System.out.println("Content: " + adoptContent);
			adoptContentList.add(adoptContent);
			// Temporary? Only add one
			break;
		}

		return new KQMLContentContext(failureMessageContent(
											what, failureReason, adoptContentList),
										newContext);
	}

	public static KQMLList missingActiveGoal(String what, ActType actType, KQMLList context, GoalPlanner goalPlanner,
			OntologyReader ontologyReader) {
		KQMLContentContext result = missingActiveGoalContentContext(what, actType, 
									context, goalPlanner,ontologyReader);
		return reportContent(result.getContent(),result.getContext());
	}
	

	public static KQMLContentContext missingActiveGoalContentContext(String what, ActType actType, KQMLList context, GoalPlanner goalPlanner,
			OntologyReader ontologyReader) {
		KQMLList failureReason = new KQMLList();
		failureReason.add("MISSING-ACTIVE-GOAL");
		List<Goal> possibleSolutionGoals;
		List<String> parentActGoalTypes = ontologyReader.getParentGoalsOfActType(actType);

		// See if there are act-specific goals first
		if (parentActGoalTypes != null && !parentActGoalTypes.isEmpty())
			possibleSolutionGoals = goalPlanner.generatePossibleGoals(parentActGoalTypes);
		else
			possibleSolutionGoals = goalPlanner.generatePossibleGoals(ontologyReader.getRootGoals());

		KQMLList newContext = new KQMLList();
		newContext.addAll((KQMLList) context);
		KQMLList adoptContentList = new KQMLList();
		for (Goal possibleSolutionGoal : possibleSolutionGoals) {
			System.out.println("Possible solution: " + possibleSolutionGoal.getKQMLTerm());
			newContext.add(possibleSolutionGoal.getKQMLTerm());
			KQMLList adoptContent = Messages.adoptContent(possibleSolutionGoal.getId(),
					possibleSolutionGoal.getVariableName(), "GOAL", null);
			System.out.println("Content: " + adoptContent);
			adoptContentList.add(adoptContent);
			// Temporary? Only add one
			break;
		}

		return new KQMLContentContext(failureMessageContent(
										what, failureReason, adoptContentList),
									newContext);
	}

	public static KQMLList missingGoal(String what, KQMLList context) {
		KQMLList failureReason = new KQMLList();
		failureReason.add("MISSING-GOAL");

		return failureMessage(what, context, failureReason);
	}
	
	public static KQMLList missingGoalContent(String what, KQMLList context) {
		KQMLList failureReason = new KQMLList();
		failureReason.add("MISSING-GOAL");

		return failureMessageContent(what, failureReason, new KQMLList());
	}

	public static KQMLList missingQueryToAnswer(String what, KQMLList context) {
		KQMLList failureReason = new KQMLList();
		failureReason.add("MISSING-QUERY-TO-ANSWER");

		return failureMessage(what, context, failureReason);
	}

	public static KQMLList noEventsInContext(String what, KQMLList context) {
		KQMLList failureReason = new KQMLList();
		failureReason.add("NO-EVENTS-IN-CONTEXT");

		return failureMessage(what, context, failureReason);
	}

	public static KQMLList unknownGoalRelationContent(KQMLList speechAct, String existingGoalId, String newGoalId,
			KQMLList context) {
		KQMLList failureReason = new KQMLList();
		failureReason.add("UNKNOWN-GOAL-RELATION");
		failureReason.add(":EXISTING-GOAL");
		failureReason.add(existingGoalId);
		failureReason.add(":NEW-GOAL");
		failureReason.add(newGoalId);
		return failureMessageContent(speechAct.stringValue(), failureReason, null);
	}
	
	public static KQMLList ambiguousAbandonContent(KQMLList speechAct,
			String goalToRejectId, String goalToRejectWhat, String goalToAbandonId,
			String goalToAbandonWhat)
	{
		KQMLList failureReason = new KQMLList();
		failureReason.add("AMBIGUOUS-ABANDON");
		
		KQMLList rejectContent = rejectContent(goalToRejectId, goalToRejectWhat);
		KQMLList abandonContent = abandonContent(goalToAbandonId, goalToAbandonWhat);
		KQMLList interpretations = new KQMLList();
		interpretations.add(rejectContent);
		interpretations.add(abandonContent);
		

		return failureMessageContent(speechAct.stringValue(), failureReason, interpretations);
	}

	public static KQMLList failureMessageContent(String what, KQMLObject reason,
			KQMLObject possibleSolutions) {
		KQMLList failureContent = new KQMLList();
		failureContent.add("FAILURE");
		failureContent.add(":type");
		failureContent.add("FAILED-TO-INTERPRET");
		failureContent.add(":WHAT");
		failureContent.add(what);
		failureContent.add(":REASON");
		if (reason == null)
			failureContent.add("NIL");
		else
			failureContent.add(reason);

		failureContent.add(":POSSIBLE-RESOLUTION");
		if (possibleSolutions == null)
			failureContent.add("NIL");
		else
			failureContent.add(possibleSolutions);

		return failureContent;
	}

	public static KQMLList failureMessage(String what, KQMLObject context, KQMLObject reason,
			KQMLObject possibleSolutions) {
		return reportContent(failureMessageContent(what, reason, possibleSolutions), context);
	}

	public static KQMLList failureMessage(String what, KQMLObject context, KQMLObject reason) {
		return failureMessage(what, context, reason, null);
	}

	public static KQMLList failureMessage(KQMLObject context) {
		return failureMessage("NIL", context);
	}

	public static KQMLList failureMessage() {
		return failureMessage(new KQMLList());
	}

	public static KQMLList failureMessage(String what, KQMLObject context) {
		return failureMessage(what, context, null);
	}

	public static KQMLList reportContent(KQMLObject content, KQMLObject context) {
		KQMLList reportContent = new KQMLList();
		reportContent.add("REPORT");
		reportContent.add(":content");
		reportContent.add(content);
		reportContent.add(":context");
		if (context != null)
			reportContent.add(KQMLUtilities.removedDuplicates((KQMLList) context));
		else
			reportContent.add(new KQMLList());

		// LG: debug
		// System.out.println("Sending: " + reportContent);

		return reportContent;
	}

	public static KQMLList missingGoalToModify(String what, KQMLObject context) {
		return reportContent(missingGoalToModifyContent(what, context), context);
	}
	
	public static KQMLList missingGoalToModifyContent(String what, KQMLObject context) {
		KQMLList failureReason = new KQMLList();
		failureReason.add("MISSING-GOAL-TO-MODIFY");

		KQMLList solutionList = new KQMLList();
		KQMLList selectList = new KQMLList();
		selectList.add("SELECT-GOAL-TO-MODIFY");

		solutionList.add(selectList);

		return failureMessageContent(what, failureReason, solutionList);
	}

}
