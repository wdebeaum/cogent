package plans;

import extractors.OntologyReader;
import extractors.TermExtractor;
import states.ActType;
import states.Goal;
import utilities.KQMLContentContext;
import utilities.KQMLUtilities;
import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLObject;

public class GoalAdder {

	GoalPlanner gp;
	OntologyReader or;

	public GoalAdder(GoalPlanner gp, OntologyReader or) {
		this.gp = gp;
		this.or = or;
	}

	/**
	 * Takes the description of a goal and determines how this goal should be added to the hierarchy 
	 * (i.e. elaboration, modification, goal, subgoal, answer)
	 * @param innerContent
	 * @param context
	 * @param id
	 * @param what
	 * @return
	 */
	public KQMLContentContext getGoalAdditionType(KQMLList innerContent, KQMLList context, String id, String what) {
		Goal currentAcceptedGoal = gp.getActiveGoal();

		KQMLObject asObject = innerContent.getKeywordArg(":AS");
		KQMLList asList = null;
		KQMLList term = TermExtractor.extractTerm(what, context);
		KQMLList responseContent = null;

		// It has an :AS parameter
		if (asObject != null && asObject instanceof KQMLList) {
			asList = (KQMLList) asObject;
			String asType = asList.get(0).stringValue().toUpperCase();
			boolean strictModify = false;
			switch (asType) {

			case "MODIFICATION":
			case "MODIFY":
				strictModify = true;
			case "ELABORATION":
				if (currentAcceptedGoal == null && gp.getGoalUnderDiscussion() == null) {
					return Messages.missingActiveGoalContentContext(what, ActType.PROPOSE,
												context, gp, or);
				}

				String ofSymbol = null;
				if (term.getKeywordArg(":MOD") != null) {
					String modSymbol = term.getKeywordArg(":MOD").stringValue();
					KQMLList modTerm = TermExtractor.extractTerm(modSymbol, (KQMLList) context);
					if (modTerm.getKeywordArg(":INSTANCE-OF") != null && modTerm.getKeywordArg(":INSTANCE-OF")
							.stringValue().equalsIgnoreCase("ONT::CHOICE-OPTION")) {
						strictModify = true;
					}
				}

				if (asList.size() > 1)
					asType = asList.get(0).stringValue();
				if (asList.getKeywordArg(":OF") != null)
					ofSymbol = asList.getKeywordArg(":OF").stringValue();

				if (ofSymbol == null || !gp.hasGoal(ofSymbol))
					responseContent = gp.modify(new Goal(what, (KQMLList) context), strictModify, false);
				else if (gp.hasGoal(ofSymbol))
					responseContent = gp.modify(new Goal(what, (KQMLList) context), ofSymbol, strictModify);

				if (responseContent == null) {
					return new KQMLContentContext(Messages.missingGoalToModifyContent(
													what, context),context);
				}
				break;
			case "ANSWER":
				KQMLObject toObject = asList.getKeywordArg(":TO");
				if (toObject != null) {
					if (!gp.hasGoal(toObject.stringValue()))
						return new KQMLContentContext(Messages.missingGoalContent(
											toObject.stringValue(), context), context);
					gp.setCompleted(gp.getGoal(toObject.stringValue()));
					responseContent = Messages.answerContent(id, what, toObject.stringValue());
				} else if (currentAcceptedGoal != null) {

					gp.setCompleted(gp.getActiveGoal());
					responseContent = Messages.answerContent(id, what, currentAcceptedGoal.getId());

				} else {
					return Messages.missingActiveGoalContentContext(what, ActType.ANSWER, context,
														gp, or);
				}
				break;

			}
		}
		// It has no :AS parameter, it's a top level goal
		else if (currentAcceptedGoal == null) {
			String goalType = term.getKeywordArg(":INSTANCE-OF").stringValue();
			if (!gp.hasAcceptedGoal && !or.isRootGoal(goalType,context))
				return Messages.missingActiveGoalContentContext(goalType, what, 
									ActType.PROPOSE, context, gp, or);
			Goal newGoal = new Goal(what, (KQMLList) context);
			responseContent = Messages.adoptContent(newGoal.getId(), what, "GOAL", null);
			gp.addGoal(newGoal);
		} else if (currentAcceptedGoal.isFailed()) {
			responseContent = gp.modify(new Goal(what, (KQMLList) context), true, false);
			if (responseContent == null) {
				return new KQMLContentContext(Messages.missingGoalToModifyContent(
												what, context),context);
			}
		} else // Currently adds as subgoal by default
		{

			Goal newGoal = new Goal(what, (KQMLList) context);
			if (isDefiniteNewGoal(term, context)) {
				responseContent = Messages.adoptContent(newGoal.getId(), what, "GOAL", null);
				gp.addGoal(newGoal, null);
			} else if (isDefiniteSubGoal(term, context)) {
				responseContent = Messages.adoptContent(newGoal.getId(), what, "SUBGOAL", currentAcceptedGoal.getId());
				gp.addGoal(newGoal, currentAcceptedGoal.getVariableName());
			} else {
				KQMLList speechAct = new KQMLList();
				speechAct.add("PROPOSE");
				speechAct.add(":CONTENT");
				speechAct.add(what);
				responseContent = Messages.unknownGoalRelationContent(speechAct, gp.getActiveGoal().getId(), newGoal.getId(),
						context);
			}

		}
		return new KQMLContentContext(responseContent,context);

	}

	private boolean isDefiniteNewGoal(KQMLList goalTerm, KQMLList context) {
		KQMLObject beneficiary = KQMLUtilities.getLinkedArguments(goalTerm, context, ":BENEFICIARY", ":GROUND",
				":REFERS-TO");
		KQMLObject parentBeneficiary = KQMLUtilities.getLinkedArguments(gp.getActiveGoal().getKQMLTerm(), context,
				":BENEFICIARY", ":GROUND", ":REFERS-TO");

		if (beneficiary == null && parentBeneficiary == null)
			return false;

		if (beneficiary != null && (parentBeneficiary == null || !parentBeneficiary.equals(beneficiary)))
			return false;

		return false;
	}

	private boolean isDefiniteSubGoal(KQMLList goalTerm, KQMLList context) {
		KQMLObject beneficiary = KQMLUtilities.getLinkedArguments(goalTerm, context, ":BENEFICIARY", ":GROUND",
				":REFERS-TO");
		KQMLObject parentBeneficiary = KQMLUtilities.getLinkedArguments(gp.getActiveGoal().getKQMLTerm(), context,
				":BENEFICIARY", ":GROUND", ":REFERS-TO");

		if (beneficiary == null && parentBeneficiary == null)
			return true;

		if (beneficiary != null && (parentBeneficiary == null || !parentBeneficiary.equals(beneficiary)))
			return false;

		return false;
	}

	private ActType getActTypeForAddition() {
		return null;
	}

}
