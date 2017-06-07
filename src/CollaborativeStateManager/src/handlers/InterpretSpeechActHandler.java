package handlers;

import java.util.ArrayList;
import java.util.List;

import plans.GoalPlanner;
import states.ActType;
import states.Action;
import states.Elaboration;
import states.Goal;
import states.Query;
import utilities.KQMLUtilities;
import extractors.EventExtractor;
import extractors.OntologyReader;
import extractors.TermExtractor;
import handlers.IDHandler;
import TRIPS.CollaborativeStateManager.CollaborativeStateManager;
import TRIPS.KQML.*;

public class InterpretSpeechActHandler extends MessageHandler implements Runnable{

	
	KQMLList innerContent = null;
	
	String speechAct;
	String what;
	String query;
	String id;
	KQMLObject context;

	KQMLObject whatLF = null;
	String activeGoal = null;
	OntologyReader ontologyReader;
	GoalPlanner goalPlanner;

	public InterpretSpeechActHandler(KQMLPerformative msg, KQMLList content, ReferenceHandler referenceHandler,
										GoalPlanner goalPlanner, 
										OntologyReader ontologyReader,
										CollaborativeStateManager csm)
	{
		super(msg,content, referenceHandler, csm);
		
		this.ontologyReader = ontologyReader;
		this.goalPlanner = goalPlanner;
		
	}
	
	@Override
	public void run() {
		// TODO Auto-generated method stub
		
	}
	
	public KQMLList process()
	{
		KQMLObject innerContentObj = content.getKeywordArg(":content");
		innerContent = null;
		
		if (innerContentObj instanceof KQMLList)
			innerContent = (KQMLList)innerContentObj;
		
		speechAct = innerContent.get(0).stringValue();
		what = innerContent.getKeywordArg(":content").stringValue();
		KQMLObject tempId = innerContent.getKeywordArg(":id");
		
		if (tempId != null)
			id = tempId.stringValue();
		else
			id = IDHandler.getNewID();
		
		context = innerContent.getKeywordArg(":context");
		KQMLObject queryObject = innerContent.getKeywordArg(":QUERY");
		if (queryObject != null)
			query = queryObject.stringValue();
		
		KQMLObject activeGoalObject = innerContent.getKeywordArg(":active-goal");
		
		// Disabled for now, active-goal isn't helpful
//		if ((activeGoalObject != null) &&
//		    !activeGoalObject.stringValue().equalsIgnoreCase("nil") &&
//		    !activeGoalObject.stringValue().equals("-"))
//		{
//			activeGoal = activeGoalObject.stringValue();
//		}
		if (KQMLUtilities.isKQMLNull(context))
			context = new KQMLList();
		
		for (KQMLObject lfTerm : (KQMLList)context)
		{
			if (((KQMLList)lfTerm).get(1).stringValue().equalsIgnoreCase(what))
				whatLF = lfTerm;
		}
		
		
		switch (speechAct.toLowerCase())
		{
		case "propose":
			return handlePropose();
		case "answer":
			return handleAnswer();
		case "ont::ask-what-is":
			return handleWhatIs();
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
	
	private KQMLList handleAnswer()
	{
		Goal currentAcceptedGoal = goalPlanner.getActiveGoal();
		if (currentAcceptedGoal != null)
		{
			activeGoal = currentAcceptedGoal.getVariableName();
		}

		if (activeGoal == null && currentAcceptedGoal == null)
		{
			return missingActiveGoal(ActType.ANSWER);
		}
		
		Query queryToAnswer = null;
		for (Query q : goalPlanner.getQueries())
		{
			if (q.getParent() != null && q.getParent().equals(currentAcceptedGoal))
			{
				queryToAnswer = q;
				break;
			}
		}
		
		if (queryToAnswer == null)
			return missingQueryToAnswer();
		
		KQMLList eventTerm = null;
		if (!KQMLUtilities.isKQMLNull(context))
			eventTerm = TermExtractor.extractTerm(what, (KQMLList)context);
		
		if (eventTerm == null && goalPlanner.hasGoal(what))
		{
			Goal g = goalPlanner.getGoal(what);
			if (g != null)
				eventTerm = g.getKQMLTerm();
		}
		if (eventTerm != null)
		{
			KQMLObject typeObject = eventTerm.getKeywordArg(":INSTANCE-OF");
			KQMLObject neutralObject = eventTerm.getKeywordArg(":NEUTRAL");
			KQMLObject agentObject = eventTerm.getKeywordArg(":AGENT");
			KQMLObject refersToObject = eventTerm.getKeywordArg(":REFERS-TO");
		}
		
		String initiativeAgent = null;
		
//		if (agentObject != null)
//		{
//			initiativeAgent = agentObject.stringValue();
//		}
//		else if (neutralObject != null)
//		{
//			initiativeAgent = neutralObject.stringValue();
//		}

//		System.out.println("Agent: " + initiativeAgent);
        
		//Goal replacementGoal = new Goal(currentAcceptedGoal);
		Elaboration elaboration = null;
		if (eventTerm != null)
		{
			elaboration = new Elaboration(eventTerm, (KQMLList)context);
			referenceHandler.addReference(elaboration.getKQMLTerm());
		}

        //referenceHandler.addReference(replacementGoal.getKQMLTerm());
		//replacementGoal.setInitiativeAgent(initiativeAgent, (KQMLList)context);
		//KQMLList answer = goalPlanner.modify(elaboration,
			//	currentAcceptedGoal.getVariableName());
		KQMLList answerContent = queryToAnswer.answerContent(what, (KQMLList)context);
		
		
		KQMLList newContext = new KQMLList();
		newContext.addAll((KQMLList)context);
		if (currentAcceptedGoal != null)
			newContext.addAll(currentAcceptedGoal.getAdditionalContext());
//        if (replacementGoal != null)
//        {
//            
//            KQMLList replacementContext = referenceHandler.generateContextForTerm(replacementGoal.getKQMLTerm());
//            
//            newContext.addAll(replacementContext);
//            
//            newContext.addAll(replacementGoal.getOriginalContext());
//        }
		
        if (elaboration != null)
        {
            
            KQMLList replacementContext = referenceHandler.generateContextForTerm(elaboration.getKQMLTerm());
            
            newContext.addAll(replacementContext);
            
            newContext.addAll(elaboration.getOriginalContext());
        }
        
        newContext.addAll(queryToAnswer.getOriginalContext());
		return reportContent(answerContent, newContext);

	}
	
	private KQMLList handleNotAcceptable() {
		// TODO Auto-generated method stub
		return null;
	}

	private KQMLList handleAcceptable() {
		// TODO Auto-generated method stub
		return null;
	}

	private KQMLList missingActiveGoal(String attemptedGoalType, ActType actType)
	{
		KQMLList failureReason = new KQMLList();
		failureReason.add("MISSING-ACTIVE-GOAL");
		
		List<String> parentActGoalTypes = ontologyReader.getParentGoalsOfActType(actType);
		List<String> parentGoalTypes = ontologyReader.getParentGoals(attemptedGoalType);
		List<Goal> possibleSolutionGoals = null;
		
		// Try act-specific goals first
		if (parentActGoalTypes != null && !parentActGoalTypes.isEmpty())
			possibleSolutionGoals =
				goalPlanner.generatePossibleGoals(parentActGoalTypes);
			
		if (parentGoalTypes == null)
			return missingActiveGoal(actType);
		
		if (possibleSolutionGoals == null)
			possibleSolutionGoals =
				goalPlanner.generatePossibleGoals(parentGoalTypes);
		
		KQMLList newContext = new KQMLList();
		newContext.addAll((KQMLList)context);
		KQMLList adoptContentList = new KQMLList();
		for (Goal possibleSolutionGoal : possibleSolutionGoals)
		{
			System.out.println("Possible solution: " + possibleSolutionGoal.getKQMLTerm());
			System.out.println("Possible solution goal: " + possibleSolutionGoal);
			newContext.add(possibleSolutionGoal.getKQMLTerm());
			
			KQMLList adoptContent = adoptContent(possibleSolutionGoal.getId(),possibleSolutionGoal.getVariableName(),
					"GOAL",null);
			System.out.println("ID: " + possibleSolutionGoal.getId());
			System.out.println("What: " + possibleSolutionGoal.getVariableName());
			System.out.println("Content: " + adoptContent);
			adoptContentList.add(adoptContent);
			// Temporary? Only add one 
			break;
		}
		
		return failureMessage(what, newContext,failureReason, adoptContentList);
	}
	
	private KQMLList missingActiveGoal(ActType actType)
	{
		KQMLList failureReason = new KQMLList();
		failureReason.add("MISSING-ACTIVE-GOAL");
		List<Goal> possibleSolutionGoals;
		List<String> parentActGoalTypes = ontologyReader.getParentGoalsOfActType(actType);
		
		// See if there are act-specific goals first
		if (parentActGoalTypes != null && !parentActGoalTypes.isEmpty())
			possibleSolutionGoals = goalPlanner.generatePossibleGoals(parentActGoalTypes);
		else
			possibleSolutionGoals =
				goalPlanner.generatePossibleGoals(ontologyReader.getRootGoals());
		
		KQMLList newContext = new KQMLList();
		newContext.addAll((KQMLList)context);
		KQMLList adoptContentList = new KQMLList();
		for (Goal possibleSolutionGoal : possibleSolutionGoals)
		{
			System.out.println("Possible solution: " + possibleSolutionGoal.getKQMLTerm());
			newContext.add(possibleSolutionGoal.getKQMLTerm());
			KQMLList adoptContent = adoptContent(possibleSolutionGoal.getId(),possibleSolutionGoal.getVariableName(),
					"GOAL",null);
			System.out.println("Content: " + adoptContent);
			adoptContentList.add(adoptContent);
			// Temporary? Only add one 
			break;
		}
		
		return failureMessage(what, newContext,failureReason, adoptContentList);
	}
	
	private KQMLList missingGoal()
	{
		KQMLList failureReason = new KQMLList();
		failureReason.add("MISSING-GOAL");
	
		return failureMessage(what, context,failureReason);
	}
	
	private KQMLList missingQueryToAnswer()
	{
		KQMLList failureReason = new KQMLList();
		failureReason.add("MISSING-QUERY-TO-ANSWER");
	
		return failureMessage(what, context,failureReason);
	}
	
	private KQMLList missingGoal(String otherWhat)
	{
		KQMLList failureReason = new KQMLList();
		failureReason.add("MISSING-GOAL");
	
		return failureMessage(otherWhat, context,failureReason);
	}
	
	private KQMLList noEventsInContext()
	{
		KQMLList failureReason = new KQMLList();
		failureReason.add("NO-EVENTS-IN-CONTEXT");
		
		return failureMessage(what,context,failureReason);
	}

	
	private KQMLList handleAssertion()
	{
		
		Goal currentAcceptedGoal = goalPlanner.getActiveGoal();
		if (currentAcceptedGoal != null)
		{
			activeGoal = currentAcceptedGoal.getVariableName();
		}

		if (activeGoal == null && currentAcceptedGoal == null)
		{
			return missingActiveGoal(ActType.ASSERTION);
		}
		
		System.out.println("Active goal: " + activeGoal + " at time of assertion");
		
		EventExtractor ee = new EventExtractor(ontologyReader);
		ee.apply((KQMLList)context);
		if (ee.getEventIDsInContext().isEmpty())
		{
			return noEventsInContext();
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
		assertionEventContext.addAll((KQMLList)context);
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

		if (contributesToGoal instanceof Action)
		{
			System.out.println("Checking for better goal to contribute to");
			contributesToGoal = 
					goalPlanner.getNonActionAncestor(activeGoal);
			System.out.println("ContributesTo: " + contributesToGoal);
		}
		if (contributesToGoal != null)
		{
			assertionContent.add(":as");
			contributesList.add("CONTRIBUTES-TO");
			contributesList.add(":goal");
			contributesList.add(contributesToGoal.getId());
			assertionContent.add(contributesList);
		}
		
		Action action = new Action(assertionRelnContent, new KQMLList());
		action.setId(ee.getId());
		action.setActionType("ASSERTION");
		if (contributesToGoal != null)
			action.setContributesTo(contributesToGoal);
		goalPlanner.addGoal(action,contributesToGoal.getVariableName());
		
		return reportContent(assertionContent, assertionEventContext);
		
	}
	
	private KQMLList handleEvaluateResult()
	{
		Goal currentAcceptedGoal = goalPlanner.getActiveGoal();
		if (currentAcceptedGoal != null)
			activeGoal = currentAcceptedGoal.getVariableName();
		
		if (activeGoal == null && currentAcceptedGoal == null)
		{
			return missingActiveGoal(ActType.EVALUATE);
		}
		
		// TODO: Get test parameter
		
		String evaluateId = IDHandler.getNewID();
		String causeEffectId = IDHandler.getNewID();
		String goalId = IDHandler.getNewID();
		KQMLList evaAdoptContent = adoptContent(goalId,evaluateId,"SUBGOAL",currentAcceptedGoal.getId());
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
		//causeReln.add()
		

		causeReln.add(":result");
		causeReln.add(what);
		
		KQMLList evaReportContext = new KQMLList();
		evaReportContext.add(evaReln);
		evaReportContext.add(causeReln);
		evaReportContext.addAll((KQMLList)context);
		if (currentAcceptedGoal != null)
			evaReportContext.addAll(currentAcceptedGoal.getAdditionalContext());

		return reportContent(evaAdoptContent, evaReportContext);
		
	}
	
	// TODO: Deal with edge cases
	private KQMLList handlePropose()
	{
		Goal currentAcceptedGoal = goalPlanner.getActiveGoal();
		if (currentAcceptedGoal != null)
			activeGoal = currentAcceptedGoal.getVariableName();
		KQMLList proposeAdoptContent = null;
        Goal replacementGoal = null;
		
		// The system wants to rollback a goal
		if (what != null)
		{
			KQMLList term = TermExtractor.extractTerm(what, (KQMLList)context);
            System.out.println("Checking goal " + what);
			if (term != null)
			{
				if (term.getKeywordArg(":INSTANCE-OF").stringValue().equalsIgnoreCase("ONT::ROLLBACK"))
				{
					goalPlanner.rollback();
				}
				else if (term.getKeywordArg(":INSTANCE-OF").stringValue().equalsIgnoreCase("ONT::RESTART"))
				{
					goalPlanner.startOver();
				}
				else if (term.getKeywordArg(":INSTANCE-OF").stringValue().equalsIgnoreCase("ONT::EXECUTE"))
				{
					// Now we'll just return it as an elaboration of the goal under discussion
					
//                    System.out.println("Execute message found. Changing initiative.");
//					KQMLObject agentSymbolObject = term.getKeywordArg(":AGENT");
//                    
//					if (agentSymbolObject != null)
//					{
//						String agentSymbol = agentSymbolObject.stringValue();
//						System.out.println("Agent: " + agentSymbol);
//                        Goal goalToModify = goalPlanner.getGoalUnderDiscussion();
//						replacementGoal = new Goal(goalToModify);
//                        referenceHandler.addReference(replacementGoal.getKQMLTerm());
//						replacementGoal.setInitiativeAgent(agentSymbol, (KQMLList)context);
//						proposeAdoptContent = goalPlanner.modify(replacementGoal,goalToModify.getVariableName());
//					}
					if (goalPlanner.getGoalUnderDiscussion() == null)
						return missingActiveGoal(ActType.PROPOSE);
					proposeAdoptContent = adoptContent(IDHandler.getNewID(),what,"ELABORATION",goalPlanner.getGoalUnderDiscussion().getId());
				}
			}
			
		}
		if (proposeAdoptContent == null)
		{
			KQMLObject asObject = innerContent.getKeywordArg(":AS");
			KQMLList asList = null;
			KQMLList term = TermExtractor.extractTerm(what, (KQMLList)context);
			
			// It has an :AS parameter
			if (asObject != null && asObject instanceof KQMLList)
			{
				asList = (KQMLList)asObject;
				String asType = asList.get(0).stringValue().toUpperCase();
				boolean strictModify = false;
				switch (asType)
				{
				
				case "MODIFICATION":
				case "MODIFY":
					strictModify = true;
				case "ELABORATION":
					if (activeGoal == null && currentAcceptedGoal == null && 
					goalPlanner.getGoalUnderDiscussion() == null)
					{
						return missingActiveGoal(ActType.PROPOSE);
					}
					
					String ofSymbol = null;
					if (term.getKeywordArg(":MOD") != null)
					{
						String modSymbol = term.getKeywordArg(":MOD").stringValue();
						KQMLList modTerm = TermExtractor.extractTerm(modSymbol, (KQMLList)context);
						if (modTerm.getKeywordArg(":INSTANCE-OF") != null && 
							modTerm.getKeywordArg(":INSTANCE-OF").stringValue().
									equalsIgnoreCase("ONT::CHOICE-OPTION"))
						{
							strictModify = true;
						}
					}

					if (asList.size() > 1)
						asType = asList.get(0).stringValue();
					if (asList.getKeywordArg(":OF") != null)
						ofSymbol = asList.getKeywordArg(":OF").stringValue();

					if (ofSymbol == null || !goalPlanner.hasGoal(ofSymbol))
						proposeAdoptContent = goalPlanner.modify(new Goal(what,(KQMLList)context),strictModify, false);
					else if (goalPlanner.hasGoal(ofSymbol))
						proposeAdoptContent = goalPlanner.modify(new Goal(what,(KQMLList)context),ofSymbol, strictModify);
					
						
					if (proposeAdoptContent == null)
					{
						return missingGoalToModify(what,context);
					}
					break;
				case "ANSWER":
					KQMLObject toObject = asList.getKeywordArg(":TO");
					if (toObject != null)
					{
						if (!goalPlanner.hasGoal(toObject.stringValue()))
							return missingGoal(toObject.stringValue());
						goalPlanner.setCompleted(goalPlanner.getGoal(toObject.stringValue()));
						proposeAdoptContent = answerContent(id,what, toObject.stringValue());
					}
					else if (activeGoal != null)
					{
						
						goalPlanner.setCompleted(goalPlanner.getActiveGoal());
						proposeAdoptContent = answerContent(id,what, currentAcceptedGoal.getId());
						
					}
					else
					{
						return missingActiveGoal(ActType.ANSWER);
					}
					break;
					
				}
			}
			// It has no :AS parameter, it's a top level goal
			else if (activeGoal == null || currentAcceptedGoal == null)
			{
				term = TermExtractor.extractTerm(what, (KQMLList)context);
				String goalType = term.getKeywordArg(":INSTANCE-OF").stringValue();
				if (!goalPlanner.hasAcceptedGoal && !ontologyReader.isRootGoal(goalType))
					return missingActiveGoal(goalType,ActType.PROPOSE);
				Goal newGoal = new Goal(what,(KQMLList)context);
				proposeAdoptContent = adoptContent(newGoal.getId(),what,"GOAL",null);
				goalPlanner.addGoal(newGoal);
			}
			else if (currentAcceptedGoal.isFailed())
			{
				proposeAdoptContent = goalPlanner.modify(new Goal(what,(KQMLList)context), true, false);
				if (proposeAdoptContent == null)
				{
					return missingGoalToModify(what,context);
				}
			}
			else // Currently adds as subgoal by default
			{
				Goal newGoal = new Goal(what,(KQMLList)context);
				proposeAdoptContent = adoptContent(newGoal.getId(),what,"SUBGOAL",currentAcceptedGoal.getId());
				goalPlanner.addGoal(newGoal, activeGoal);
			}
		}

		KQMLList newContext = new KQMLList();
		newContext.addAll((KQMLList)context);
		if (currentAcceptedGoal != null)
			newContext.addAll(currentAcceptedGoal.getAdditionalContext());
        if (replacementGoal != null)
        {
            
            KQMLList replacementContext = referenceHandler.generateContextForTerm(replacementGoal.getKQMLTerm());
            
            newContext.addAll(replacementContext);
            newContext.addAll(replacementGoal.getOriginalContext());
        }
		return reportContent(proposeAdoptContent, newContext);
	}
	
	private KQMLList handleWhatIs()
	{
		Goal currentAcceptedGoal = goalPlanner.getActiveGoal();
		if (currentAcceptedGoal != null)
			activeGoal = currentAcceptedGoal.getVariableName();
		
//		if (activeGoal == null && currentAcceptedGoal == null)
//		{
//			return missingActiveGoal();
//		}
		
		String newId = IDHandler.getNewID();
		KQMLList askAdoptContent;
//		if (((KQMLList)whatLF).getKeywordArg(":instance-of").stringValue().
//				equalsIgnoreCase("ONT:MEDICATION"))
//			askAdoptContent = adoptContent(newId,"SUBGOAL",activeGoal);
//		else
		
		
		
    	KQMLList askRelnContent = new KQMLList();
    	askRelnContent.add("ont::RELN");
    	askRelnContent.add(newId);
    	askRelnContent.add(":instance-of");
    	askRelnContent.add("ONT::IDENTIFY");
    	askRelnContent.add(":neutral");
    	askRelnContent.add(what);

    	if (activeGoal == null)
    	{
    		Goal newGoal = new Goal(askRelnContent,(KQMLList)context);
    		newGoal.addContext((KQMLList)context);
    		goalPlanner.addGoal(newGoal);
    		askAdoptContent = adoptContent(newGoal.getId(),newId, "GOAL", null);
    		
    	}
    	else
    	{
    		Goal newGoal = new Goal(askRelnContent,(KQMLList)context);
    		newGoal.addContext((KQMLList)context);
    		goalPlanner.addGoal(newGoal, activeGoal);
    		askAdoptContent = adoptContent(newGoal.getId(),newId, "SUBGOAL", currentAcceptedGoal.getId());
    	}
    	
    	KQMLList whatTerm = TermExtractor.extractTerm(what, (KQMLList)context);
    	String suchThat = null;
    	if (innerContent.getKeywordArg(":SUCHTHAT") != null)
    		suchThat = innerContent.getKeywordArg(":SUCHTHAT").stringValue();
    	else if (whatTerm != null && whatTerm.getKeywordArg(":SUCHTHAT") != null)
    		suchThat = whatTerm.getKeywordArg(":SUCHTHAT").stringValue();
    		
    	KQMLList askWhatIsContent = new KQMLList();
    	askWhatIsContent.add("ASK-WH");
    	askWhatIsContent.add(":ID");
    	askWhatIsContent.add(id);
    	askWhatIsContent.add(":WHAT");
    	askWhatIsContent.add(what);
    	if (suchThat != null)
    	{
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
    	//contextToSend.add(askRelnContent);
    	contextToSend.addAll((KQMLList)context);
    	if (currentAcceptedGoal != null)
    		contextToSend.addAll(currentAcceptedGoal.getAdditionalContext());
		//result.add(reportContent(askWhatIsContent, contextToSend));
		return reportContent(askWhatIsContent, contextToSend);
	}
	
	// TODO Change event to conditional
	private KQMLList handleAskConditionalIf()
	{
		Goal currentAcceptedGoal = goalPlanner.getActiveGoal();
		if (currentAcceptedGoal != null)
			activeGoal = currentAcceptedGoal.getVariableName();
		
		String newId = IDHandler.getNewID();
		KQMLList askAdoptContent;

		if (activeGoal != null)
			askAdoptContent = adoptContent(IDHandler.getNewID(),newId, "SUBGOAL", currentAcceptedGoal.getId());
		else
			askAdoptContent = adoptContent(IDHandler.getNewID(),newId, "GOAL", null);
		
		
    	KQMLList conditionalContent = new KQMLList();
    	conditionalContent.add("ont::RELN");
    	String newConditionalId = IDHandler.getNewID();
    	conditionalContent.add(newConditionalId);
    	conditionalContent.add(":instance-of");
    	// TODO Determine if causal or conditional
    	conditionalContent.add("ONT::COND");
    	

    	KQMLObject conditionObject = innerContent.getKeywordArg(":CONDITION");
    	
    	if (!KQMLUtilities.isKQMLNull(conditionObject))
    	{
    		conditionalContent.add(":factor");
    		String condition = conditionObject.stringValue();
    		conditionalContent.add(condition);
    		
    	}
    	
    	conditionalContent.add(":OUTCOME");
    	conditionalContent.add(what);
    	
    	KQMLList queryGoalContent = new KQMLList();
    	queryGoalContent.add("ont::RELN");
    	queryGoalContent.add(newId);
    	queryGoalContent.add(":instance-of");
    	queryGoalContent.add("ONT::EVALUATE");
    	queryGoalContent.add(":neutral");
    	queryGoalContent.add(newConditionalId);

    	if (activeGoal == null)
    	{
    		Goal newGoal = new Goal(queryGoalContent,(KQMLList)context);
    		newGoal.addContext((KQMLList)context);
    		goalPlanner.addGoal(newGoal);
    	}
    	else
    	{
    		Goal newGoal = new Goal(queryGoalContent,(KQMLList)context);
    		newGoal.addContext((KQMLList)context);
    		goalPlanner.addGoal(newGoal, activeGoal);
    	}
    	
    	KQMLList contextToSend = new KQMLList();
    	contextToSend.add(queryGoalContent);
    	contextToSend.add(conditionalContent);
    	contextToSend.addAll((KQMLList)context);
    	if (currentAcceptedGoal != null)
    		contextToSend.addAll(currentAcceptedGoal.getAdditionalContext());
		//result.add(reportContent(askRelnContent, contextToSend));
		
		
		return reportContent(askAdoptContent, contextToSend);
	}
	
	private KQMLList handleAskIf(boolean conditional)
	{
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
    	
    	if (conditional)
    	{
        	conditionalContent.add("ont::RELN");
        	String newConditionalId = IDHandler.getNewID();
        	conditionalContent.add(newConditionalId);
        	conditionalContent.add(":instance-of");
        	// TODO Determine if causal or conditional
        	conditionalContent.add("ONT::COND");
        	
        	KQMLObject conditionObject = innerContent.getKeywordArg(":CONDITION");
        	
        	if (!KQMLUtilities.isKQMLNull(conditionObject))
        	{
        		conditionalContent.add(":factor");
        		String condition = conditionObject.stringValue();
        		conditionalContent.add(condition);
        		
        	}
        	
        	conditionalContent.add(":OUTCOME");
        	conditionalContent.add(what);
        	
        	askRelnContent.add(":QUERY");
        	askRelnContent.add(newConditionalId);
    	}
    	else
    	{
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

    	
    	KQMLList queryGoalContent = new KQMLList();
    	queryGoalContent.add("ont::RELN");
    	queryGoalContent.add(newId);
    	queryGoalContent.add(":instance-of");
    	queryGoalContent.add("ONT::QUERY-MODEL");
    	queryGoalContent.add(":neutral");
    	queryGoalContent.add(what);

    	if (activeGoal == null)
    	{
    		Goal newGoal = new Goal(queryGoalContent,(KQMLList)context);
    		newGoal.addContext((KQMLList)context);
    		//newGoal.setId(id);
    		goalPlanner.addGoal(newGoal);
    	}
    	else
    	{
    		Goal newGoal = new Goal(queryGoalContent,(KQMLList)context);
    		newGoal.addContext((KQMLList)context);
    		//newGoal.setId(id);
    		goalPlanner.addGoal(newGoal, activeGoal);
    	}

    	
    	KQMLList contextToSend = new KQMLList();
    	//contextToSend.add(queryGoalContent);
    	contextToSend.addAll((KQMLList)context);
    	if (conditional)
    		contextToSend.add(conditionalContent);
    	if (currentAcceptedGoal != null)
    		contextToSend.addAll(currentAcceptedGoal.getAdditionalContext());
		//result.add(reportContent(askRelnContent, contextToSend));
		
		return reportContent(askRelnContent, contextToSend);
	}
	
    private KQMLList relnContent(String id, String instanceOf, String what, KQMLList context)
    {
    	KQMLList relnContent = new KQMLList();
    	relnContent.add("ont::RELN");
    	relnContent.add(id);
    	relnContent.add(":instance-of");
    	relnContent.add(instanceOf);
    	relnContent.add(":content");
    	return relnContent;
    }
    
    
    private KQMLList adoptContent(String id, String what, String goalType, String subgoalOf)
    {
    	KQMLList adopt = new KQMLList();
    	
    	adopt.add("ADOPT");
    	if (id != null)
    	{
	    	adopt.add(":ID");
	    	adopt.add(id);
    	}
    	adopt.add(":what");
    	adopt.add(what);
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
    
    private KQMLList answerContent(String id,String what, String query)
    {
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


}
