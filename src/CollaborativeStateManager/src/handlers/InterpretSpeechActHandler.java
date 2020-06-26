package handlers;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import plans.GoalAdder;
import plans.GoalPlanner;
import plans.GoalRemover;
import plans.GoalSelector;
import plans.Messages;
import states.ActType;
import states.Action;
import states.Elaboration;
import states.Goal;
import states.Query;
import utilities.KQMLContentContext;
import utilities.KQMLUtilities;
import extractors.EventExtractor;
import extractors.OntologyReader;
import extractors.TermExtractor;
import handlers.IDHandler;
import TRIPS.CollaborativeStateManager.CollaborativeStateManager;
import TRIPS.KQML.*;

public class InterpretSpeechActHandler extends MessageHandler implements Runnable {

	KQMLList innerContent = null;

	String speechAct;
	String what;
	String query;
	String id;
	KQMLList context;

	KQMLObject whatLF = null;
	String activeGoal = null;
	GoalPlanner goalPlanner;
	GoalAdder goalAdder;
	GoalSelector goalSelector;
	GoalRemover goalRemover;

	public InterpretSpeechActHandler(KQMLPerformative msg, KQMLList content, ReferenceHandler referenceHandler,
			GoalPlanner goalPlanner, OntologyReader ontologyReader, CollaborativeStateManager csm) {
		super(msg, content, referenceHandler, csm, ontologyReader);

		this.goalPlanner = goalPlanner;
		this.goalAdder = new GoalAdder(goalPlanner, ontologyReader);
		this.goalSelector = new GoalSelector(goalPlanner);
		this.goalRemover = new GoalRemover(goalPlanner, goalSelector);

	}

	@Override
	public void run() {
		// TODO Auto-generated method stub

	}

	public KQMLList process() {
		KQMLObject innerContentObj = content.getKeywordArg(":content");
		innerContent = null;

		if (innerContentObj instanceof KQMLList)
			innerContent = (KQMLList) innerContentObj;

		speechAct = innerContent.get(0).stringValue();
		what = innerContent.getKeywordArg(":content").stringValue();
		KQMLObject tempId = innerContent.getKeywordArg(":id");

		if (tempId != null)
			id = tempId.stringValue();
		else
			id = IDHandler.getNewID();

		KQMLObject contextObject = innerContent.getKeywordArg(":context");
		if (contextObject instanceof KQMLList)
			context = (KQMLList) contextObject;
		KQMLObject queryObject = innerContent.getKeywordArg(":QUERY");
		if (queryObject != null)
			query = queryObject.stringValue();

		if (KQMLUtilities.isKQMLNull(context))
			context = new KQMLList();

		for (KQMLObject lfTerm : (KQMLList) context) {
			if (((KQMLList) lfTerm).get(1).stringValue().equalsIgnoreCase(what))
				whatLF = lfTerm;
		}

		switch (speechAct.toLowerCase()) {
		case "propose":
			return handlePropose();
		case "answer":
			return handleAnswer();
		case "abandon":
			return handleAbandon();
		case "ont::ask-what-is":
			return handleWhatIs(false);
		case "ont::ask-conditional-what-is":
			return handleWhatIs(true);
		case "ont::evaluate-result":
			return handleEvaluateResult();
		case "assertion":
			return handleAssertion();
		case "ont::ask-if":
			return handleAskIf(false);
		case "ont::ask-conditional-if": // We check for differences in the handleAskIf function
			return handleAskIf(true);
		case "acceptable":
			return handleAcceptable();
		case "not-acceptable":
			return handleNotAcceptable();
		}

		System.out.println("Unrecognized speech act: " + speechAct);
		return null;
	}

	private KQMLList handleAnswer() {
		Goal currentAcceptedGoal = goalPlanner.getActiveGoal();
		if (currentAcceptedGoal != null) {
			activeGoal = currentAcceptedGoal.getVariableName();
		}

		if (activeGoal == null && currentAcceptedGoal == null) {
			return Messages.missingActiveGoal(what, ActType.ANSWER, context, goalPlanner, ontologyReader);
		}

		Query queryToAnswer = null;
		// If we start having a problem with answering other things check here
		for (Query q : goalPlanner.getQueries()) {
			if (q.getParent() != null && //q.getParent().equals(currentAcceptedGoal) &&
					!q.isAnswered() && !q.isFailed()) {
				queryToAnswer = q;
				break;
			}
		}

		if (queryToAnswer == null)
			return Messages.missingQueryToAnswer(activeGoal, innerContent);

		KQMLList eventTerm = null;
		if (!KQMLUtilities.isKQMLNull(context))
			eventTerm = TermExtractor.extractTerm(what, (KQMLList) context);

		if (eventTerm == null && goalPlanner.hasGoal(what)) {
			Goal g = goalPlanner.getGoal(what);
			if (g != null)
				eventTerm = g.getKQMLTerm();
		}

		Elaboration elaboration = null;
		if (eventTerm != null) {
			elaboration = new Elaboration(eventTerm, (KQMLList) context);
			referenceHandler.addReference(elaboration.getKQMLTerm());
		}

		KQMLList answerContent = queryToAnswer.answerContent(what, (KQMLList) context);

		KQMLList newContext = new KQMLList();
		newContext.addAll((KQMLList) context);
		if (currentAcceptedGoal != null)
			newContext.addAll(currentAcceptedGoal.getAdditionalContext());

		if (elaboration != null) {

			KQMLList replacementContext = referenceHandler.generateContextForTerm(elaboration.getKQMLTerm());
			newContext.addAll(replacementContext);
			newContext.addAll(elaboration.getOriginalContext());
		}

		newContext.addAll(queryToAnswer.getOriginalContext());
		return Messages.reportContent(answerContent, newContext);

	}

	private KQMLList handleNotAcceptable() {
		// TODO Auto-generated method stub
		return null;
	}

	private KQMLList handleAcceptable() {
		// TODO Auto-generated method stub
		return null;
	}

	private KQMLList handleAssertion() {

		Goal currentAcceptedGoal = goalPlanner.getActiveGoal();
		if (currentAcceptedGoal != null) {
			activeGoal = currentAcceptedGoal.getVariableName();
		}

		if (activeGoal == null && currentAcceptedGoal == null) {
			return Messages.missingActiveGoal(what, ActType.ASSERTION, context, goalPlanner, ontologyReader);
		}

		System.out.println("Active goal: " + activeGoal + " at time of assertion");

		EventExtractor ee = new EventExtractor(ontologyReader);
		ee.apply((KQMLList) context);
		if (ee.getEventIDsInContext().isEmpty()) {
			return Messages.noEventsInContext(what, (KQMLList) context);
		}

		KQMLList assertionRelnContent = new KQMLList();
		assertionRelnContent.add("ont::RELN");
		assertionRelnContent.add(ee.getWhat());
		assertionRelnContent.add(":instance-of");
		assertionRelnContent.add("ONT::EVENTS-IN-MODEL");
		assertionRelnContent.add(":events");
		assertionRelnContent.add(ee.getEventIDsInContext());
		KQMLList assertionEventContext = new KQMLList();
		assertionEventContext.add(assertionRelnContent);
		assertionEventContext.addAll((KQMLList) context);
		if (currentAcceptedGoal != null)
			assertionEventContext.addAll(currentAcceptedGoal.getAdditionalContext());

		KQMLList assertionContent = new KQMLList();
		assertionContent.add("ASSERTION");
		assertionContent.add(":ID");
		assertionContent.add(ee.getId());
		assertionContent.add(":what");
		assertionContent.add(ee.getWhat());

		KQMLList contributesList = new KQMLList();

		Goal contributesToGoal = goalPlanner.getGoal(activeGoal);
		System.out.println("ContributesTo: " + contributesToGoal);

		if (contributesToGoal instanceof Action) {
			System.out.println("Checking for better goal to contribute to");
			contributesToGoal = goalPlanner.getNonActionAncestor(activeGoal);
			System.out.println("ContributesTo: " + contributesToGoal);
		}
		if (contributesToGoal != null) {
			assertionContent.add(":as");
			contributesList.add("CONTRIBUTES-TO");
			contributesList.add(":goal");
			contributesList.add(contributesToGoal.getId());
			assertionContent.add(contributesList);
		}
		
//		List<String> implicitAcceptGoals = 
//				ontologyReader.getImplicitAcceptsOfActType(ActType.ASSERTION);
//		boolean implicitAccept = false;
//		Goal lastProposedGoal = goalPlanner.getGoalUnderDiscussion();
//		if (lastProposedGoal != null)
//		{
//			for (String goal : implicitAcceptGoals)
//			{
//				if (goal.equalsIgnoreCase(lastProposedGoal.getInstanceOf()))
//				{
//					implicitAccept = true;
//					break;
//				}
//			}
//		}
//		
//		if (implicitAccept)
//		{
//			Goal underDiscussion = goalPlanner.getGoalUnderDiscussion();
//			KQMLList prereqContent;
//			if (underDiscussion.getParent() != null)
//				prereqContent = Messages.adoptContent(underDiscussion.getId(),
//										underDiscussion.getVariableName(),
//										"GOAL", null);
//			else
//				prereqContent = Messages.adoptContent(underDiscussion.getId(),
//						underDiscussion.getVariableName(),
//						"SUBGOAL", underDiscussion.getParent().getId());
//			
//			assertionContent.add(":PREREQ");
//			assertionContent.add(prereqContent);
//		}
		

		Action action = new Action(assertionRelnContent, new KQMLList());
		action.setId(ee.getId());
		action.setActionType("ASSERTION");
		if (contributesToGoal != null)
			action.setContributesTo(contributesToGoal);
		goalPlanner.addGoal(action, contributesToGoal.getVariableName());

		return Messages.reportContent(assertionContent, assertionEventContext);

	}

	private KQMLList handleEvaluateResult() {
		Goal currentAcceptedGoal = goalPlanner.getActiveGoal();
		if (currentAcceptedGoal != null)
			activeGoal = currentAcceptedGoal.getVariableName();

		if (activeGoal == null && currentAcceptedGoal == null) {
			return Messages.missingActiveGoal(what, ActType.EVALUATE, context, goalPlanner, ontologyReader);
		}

		// TODO: Get test parameter

		String evaluateId = IDHandler.getNewID();
		String causeEffectId = IDHandler.getNewID();
		String goalId = IDHandler.getNewID();
		KQMLList evaAdoptContent = Messages.adoptContent(goalId, evaluateId, "SUBGOAL", currentAcceptedGoal.getId());
		KQMLList evaReln = new KQMLList();
		evaReln.add("ont::RELN");
		evaReln.add(evaluateId);
		evaReln.add(":INSTANCE-OF");
		evaReln.add("ONT::EVALUATE");
		evaReln.add(":content");
		evaReln.add(causeEffectId);

		KQMLList causeReln = new KQMLList();
		causeReln.add("ont::RELN");
		causeReln.add(causeEffectId);
		causeReln.add(":INSTANCE-OF");
		causeReln.add("ONT::CAUSE");
		causeReln.add(":action");

		causeReln.add(":result");
		causeReln.add(what);

		KQMLList evaReportContext = new KQMLList();
		evaReportContext.add(evaReln);
		evaReportContext.add(causeReln);
		evaReportContext.addAll((KQMLList) context);
		if (currentAcceptedGoal != null)
			evaReportContext.addAll(currentAcceptedGoal.getAdditionalContext());

		return Messages.reportContent(evaAdoptContent, evaReportContext);

	}

	// TODO: Deal with edge cases
	private KQMLList handlePropose() {
		Goal currentAcceptedGoal = goalPlanner.getActiveGoal();
		if (currentAcceptedGoal != null)
			activeGoal = currentAcceptedGoal.getVariableName();
		KQMLList proposeAdoptContent = null;
		Goal replacementGoal = null;
		KQMLList term = TermExtractor.extractTerm(what, (KQMLList) context);
		KQMLList newContext = new KQMLList();

		// The system wants to rollback a goal
		if (what != null) {

			System.out.println("Checking goal " + what);
			if (term != null) {
				if (term.getKeywordArg(":INSTANCE-OF").stringValue().equalsIgnoreCase("ONT::ROLLBACK")) {
					goalPlanner.rollback();
				} else if (term.getKeywordArg(":INSTANCE-OF").stringValue().equalsIgnoreCase("ONT::RESTART")) {
					goalPlanner.startOver();
				} else if (term.getKeywordArg(":INSTANCE-OF").stringValue().equalsIgnoreCase("ONT::EXECUTE")) {
					// Now we'll just return it as an elaboration of the goal under discussion
					if (goalPlanner.getGoalUnderDiscussion() == null)
						return Messages.missingActiveGoal(what, ActType.PROPOSE, context, goalPlanner, ontologyReader);
					proposeAdoptContent = Messages.adoptContent(IDHandler.getNewID(), what, "ELABORATION",
							goalPlanner.getGoalUnderDiscussion().getId());
				}
			}
		}

		if (proposeAdoptContent == null) {
			Goal testSelect = goalSelector.getGoalByDescription(term, context);
			if (testSelect != null) {
				System.out.println("This goal may be the same as " + testSelect);
				// LG: disabling it for now
				System.out.println("... but I'll ignore that!");
				testSelect = null;
			}
			if (testSelect != null) {
				proposeAdoptContent = Messages.selectContent(testSelect.getId(), testSelect.getVariableName());
				newContext.addAll(testSelect.getOriginalContext());
				goalPlanner.setActiveGoal(testSelect);
			}
		}
		
		// Get the right goal type
		KQMLContentContext proposeAdoptContentContext;
		if (proposeAdoptContent == null) {
			proposeAdoptContentContext = goalAdder.getGoalAdditionType(innerContent, 
															context, id, what);
		}
		else
			proposeAdoptContentContext = new KQMLContentContext(proposeAdoptContent,
																context);

		// Fill in additional context if needed
		newContext.addAll((KQMLList) proposeAdoptContentContext.getContext());
		if (currentAcceptedGoal != null)
			newContext.addAll(currentAcceptedGoal.getAdditionalContext());

		return Messages.reportContent(proposeAdoptContentContext.getContent(),
										newContext);
	}

	private KQMLList handleWhatIs(boolean conditional) {
		Goal currentAcceptedGoal = goalPlanner.getActiveGoal();
		if (currentAcceptedGoal != null)
			activeGoal = currentAcceptedGoal.getVariableName();

		String newId = IDHandler.getNewID();

		KQMLList whatTerm = TermExtractor.extractTerm(what, (KQMLList) context);
		String suchThat = null;
		
		boolean hasSuchThat = (innerContent.getKeywordArg(":SUCHTHAT") != null && 
				!KQMLUtilities.isKQMLNull(innerContent.getKeywordArg(":SUCHTHAT")));
		boolean hasWhatSuchThat = (whatTerm != null && whatTerm.getKeywordArg(":SUCHTHAT") != null &&
										KQMLUtilities.isKQMLNull(whatTerm.getKeywordArg(":SUCHTHAT")));

		if (hasSuchThat)
			suchThat = innerContent.getKeywordArg(":SUCHTHAT").stringValue();
		else if (hasWhatSuchThat)
			suchThat = whatTerm.getKeywordArg(":SUCHTHAT").stringValue();

		
		KQMLList suchThatTerm = null;
		if (suchThat != null)
			suchThatTerm = TermExtractor.extractTerm(suchThat, context);

		KQMLList askWhatIsContent = new KQMLList();
		askWhatIsContent.add("ASK-WH");
		askWhatIsContent.add(":ID");
		askWhatIsContent.add(newId);
		askWhatIsContent.add(":WHAT");
		askWhatIsContent.add(what);

		KQMLList conditionalContent = new KQMLList();

		if (conditional || (suchThatTerm != null && suchThatTerm.getKeywordArg(":CONDITION") != null)) {
			conditional = true;
			conditionalContent.add("ont::RELN");
			String newConditionalId = IDHandler.getNewID();
			conditionalContent.add(newConditionalId);
			conditionalContent.add(":instance-of");
			// TODO Determine if causal or conditional
			conditionalContent.add("ONT::COND");

			KQMLObject conditionObject = innerContent.getKeywordArg(":CONDITION");

			if (!KQMLUtilities.isKQMLNull(conditionObject)) {
				conditionalContent.add(":factor");
				String condition = conditionObject.stringValue();
				conditionalContent.add(condition);
			} else if (suchThatTerm != null && suchThatTerm.getKeywordArg(":CONDITION") != null) {
				conditionalContent.add(":factor");
				String condition = suchThatTerm.getKeywordArg(":CONDITION").stringValue();
				conditionalContent.add(condition);
			}

			conditionalContent.add(":OUTCOME");
			conditionalContent.add(suchThat);

			askWhatIsContent.add(":QUERY");
			askWhatIsContent.add(newConditionalId);
		} else if (suchThat != null) {
			askWhatIsContent.add(":QUERY");
			askWhatIsContent.add(suchThat);
		}

		askWhatIsContent.add(":as");

		KQMLList queryInContext = new KQMLList();
		queryInContext.add("QUERY-IN-CONTEXT");
		queryInContext.add(":goal");
		if (currentAcceptedGoal != null)
			queryInContext.add(currentAcceptedGoal.getId());
		else
			queryInContext.add(new KQMLToken("NIL"));

		askWhatIsContent.add(queryInContext);

		KQMLList contextToSend = new KQMLList();
		// contextToSend.add(askRelnContent);
		contextToSend.addAll((KQMLList) context);
		if (conditional)
			contextToSend.add(conditionalContent);
		if (currentAcceptedGoal != null)
			contextToSend.addAll(currentAcceptedGoal.getAdditionalContext());
		// result.add(reportContent(askWhatIsContent, contextToSend));
		return Messages.reportContent(askWhatIsContent, contextToSend);
	}

	private KQMLList handleAbandon() {
		// Check if this should be an adopt event of removing a constraint, part of a model, etc.
		KQMLList thingToAbandon = KQMLUtilities.findTermInKQMLList(what, context);
		
		if (thingToAbandon != null && thingToAbandon.getKeywordArg(":INSTANCE-OF") != null)
		{
			
			String instanceToAbandon = thingToAbandon.getKeywordArg(":INSTANCE-OF").stringValue();
			System.out.println("Instance to abandon: " + instanceToAbandon);
			if (ontologyReader.isGoalWithArgument("ABANDON", instanceToAbandon))
			{
				Goal currentAcceptedGoal = goalPlanner.getActiveGoal();
				if (currentAcceptedGoal != null)
					activeGoal = currentAcceptedGoal.getVariableName();
				KQMLContentContext proposeAdoptContentContext;
				KQMLList newContext = new KQMLList();
				KQMLList removeGoalKQML = null;
				try {
					removeGoalKQML = KQMLList.fromString("(ONT::RELN " + IDHandler.getNewTermID() + 
										" :INSTANCE-OF ONT::OMIT " + ":AFFECTED " + what + ")");
				} catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				Goal removeGoal = new Goal(removeGoalKQML,context);
				newContext.addAll(context);
				newContext.add(removeGoalKQML);
				proposeAdoptContentContext = goalAdder.getGoalAdditionType(innerContent, 
																	newContext, id, removeGoal.getVariableName());

				newContext.addAll((KQMLList) proposeAdoptContentContext.getContext());
				
				if (currentAcceptedGoal != null)
					newContext.addAll(currentAcceptedGoal.getAdditionalContext());

				newContext = KQMLUtilities.removedDuplicates(newContext);
				return Messages.reportContent(proposeAdoptContentContext.getContent(),
												newContext);
			}
		}
		
		// See if there is a goal that should be abandoned or rejected
		
		Goal goalToAbandon = goalSelector.findGoalToAbandon(what, context);
		Goal proposalToReject = goalSelector.findProposalOrQuestionToReject(what, context);
		
		if (goalToAbandon == null && proposalToReject == null)
		{
			return Messages.missingGoalToModify(what, context);
		}
		
		KQMLList abandonContent = null;
		KQMLList contextToSend = new KQMLList();
		contextToSend.addAll(context);
		if (goalToAbandon != null && proposalToReject == null)
			abandonContent = Messages.abandonContent(goalToAbandon.getId(), 
				goalToAbandon.getVariableName());
		else if (goalToAbandon == null && proposalToReject != null)
			abandonContent = Messages.rejectContent(proposalToReject.getId(),
					proposalToReject.getVariableName());
		else
		{
			contextToSend.add(proposalToReject.getKQMLTerm());
			contextToSend.add(goalToAbandon.getKQMLTerm());
			KQMLList truncatedSpeechAct = new KQMLList();
			truncatedSpeechAct.add("ABANDON");
			truncatedSpeechAct.add(":CONTENT");
			truncatedSpeechAct.add(what);
			abandonContent = Messages.ambiguousAbandonContent(truncatedSpeechAct,
					proposalToReject.getId(), proposalToReject.getVariableName(),
					goalToAbandon.getId(), goalToAbandon.getVariableName());
		}
		contextToSend = KQMLUtilities.removedDuplicates(contextToSend);
		
		return Messages.reportContent(abandonContent, contextToSend);
	}

	private KQMLList handleAskIf(boolean conditional) {
		Goal currentAcceptedGoal = goalPlanner.getActiveGoal();
		if (currentAcceptedGoal != null)
			activeGoal = currentAcceptedGoal.getVariableName();

		String newId = IDHandler.getNewID();

		KQMLList askRelnContent = new KQMLList();
		askRelnContent.add("ASK-IF");
		askRelnContent.add(":ID");
		askRelnContent.add(id);

		// Change :QUERY to be the new conditional if conditional
		KQMLList conditionalContent = new KQMLList();

		if (conditional) {
			conditionalContent.add("ont::RELN");
			String newConditionalId = IDHandler.getNewID();
			conditionalContent.add(newConditionalId);
			conditionalContent.add(":instance-of");
			// TODO Determine if causal or conditional
			conditionalContent.add("ONT::COND");

			KQMLObject conditionObject = innerContent.getKeywordArg(":CONDITION");

			if (!KQMLUtilities.isKQMLNull(conditionObject)) {
				conditionalContent.add(":factor");
				String condition = conditionObject.stringValue();
				conditionalContent.add(condition);

			}

			conditionalContent.add(":OUTCOME");
			conditionalContent.add(what);

			askRelnContent.add(":QUERY");
			askRelnContent.add(newConditionalId);
		} else {
			askRelnContent.add(":QUERY");
			askRelnContent.add(what);
		}
		askRelnContent.add(":AS");

		KQMLList queryInContext = new KQMLList();
		queryInContext.add("QUERY-IN-CONTEXT");
		queryInContext.add(":goal");
		if (currentAcceptedGoal != null)
			queryInContext.add(currentAcceptedGoal.getId());
		else
			queryInContext.add(new KQMLToken("NIL"));

		askRelnContent.add(queryInContext);

		if (false) { // LG 2017/10/09 DISABLED -- this creates bogus goals in the planner!
			KQMLList queryGoalContent = new KQMLList();
			queryGoalContent.add("ont::RELN");
			queryGoalContent.add(newId);
			queryGoalContent.add(":instance-of");
			queryGoalContent.add("ONT::QUERY-MODEL");
			queryGoalContent.add(":neutral");
			queryGoalContent.add(what);

			if (activeGoal == null) {
				Goal newGoal = new Goal(queryGoalContent, (KQMLList) context);
				newGoal.addContext((KQMLList) context);
				goalPlanner.addGoal(newGoal);
			} else {
				Goal newGoal = new Goal(queryGoalContent, (KQMLList) context);
				newGoal.addContext((KQMLList) context);
				goalPlanner.addGoal(newGoal, activeGoal);
			}
		}

		KQMLList contextToSend = new KQMLList();
		contextToSend.addAll((KQMLList) context);
		if (conditional)
			contextToSend.add(conditionalContent);
		if (currentAcceptedGoal != null)
			contextToSend.addAll(currentAcceptedGoal.getAdditionalContext());

		return Messages.reportContent(askRelnContent, contextToSend);
	}

}
