package states;

import extractors.TermExtractor;
import handlers.IDHandler;
import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLObject;

public class Query extends Goal {

	String queryWhat;
	String queryType;
	String querySymbol;
	KQMLList neutralTerm;
	String neutralSymbol;
	KQMLList originalAskTerm;
	boolean answered;
	
	public Query(KQMLList term, KQMLList context) {
		this(term, null, context);

		
		// TODO Auto-generated constructor stub
	}

	public Query(KQMLList term, Goal parent, KQMLList context) {
		super(term, parent, context);
		
		originalAskTerm = term;
		String newId = IDHandler.getNewID();
		answered = false;

	    	KQMLList askRelnContent = new KQMLList();
	    	askRelnContent.add("ont::RELN");
	    	askRelnContent.add(newId);
	    	askRelnContent.add(":instance-of");
	    	askRelnContent.add("ONT::IDENTIFY");
		KQMLObject whatObject = term.getKeywordArg(":WHAT");
		KQMLObject queryObject = term.getKeywordArg(":QUERY");
		
		if (queryObject != null)
			querySymbol = queryObject.stringValue();
		else
			querySymbol = parent.getId();
		
		String what;
		queryWhat = null;
		if (whatObject != null)
		{
			what = whatObject.stringValue(); 
	    	askRelnContent.add(":neutral");
	    	askRelnContent.add(what);
	    	queryWhat = what;
	    	queryType = null;
	    	neutralTerm = TermExtractor.extractTerm(what,context);
	    	KQMLObject neutralType = null;
	    	if (neutralTerm != null)
	    		neutralType = neutralTerm.getKeywordArg(":INSTANCE-OF");
	    	if (neutralType != null)
	    		queryType = neutralType.stringValue();
	    	if (neutralTerm == null)
	    		neutralTerm = new KQMLList();
		}
		this.term = askRelnContent;
		// TODO Auto-generated constructor stub
	}

	public Query(Goal toCopy) {
		super(toCopy);
		// TODO Auto-generated constructor stub
	}
	
	public KQMLList answerContent(String answer, KQMLList context)
	{
		KQMLList toReturn = new KQMLList();
		toReturn.add("ANSWER");
		
		if (id != null)
		{
			toReturn.add(":TO");
			toReturn.add(id);
		}
		
		if (queryWhat != null)
		{
			toReturn.add(":WHAT");
			toReturn.add(queryWhat);
		}
		
		if (querySymbol != null)
		{
			toReturn.add(":QUERY");
			toReturn.add(querySymbol);
		}
		
		if (answer != null)
		{
			toReturn.add(":VALUE");
			toReturn.add(answer);
			
		}
		
		//toReturn.add(":GOAL");
		//toReturn.add(getVariableName());
		
		
		return toReturn;
	}
	
	

	public KQMLList getOriginalAskTerm() {
		return originalAskTerm;
	}

	public boolean isAnswered() {
		return answered;
	}

	public void setAnswered(boolean answered) {
		this.answered = answered;
	}

	
	
}
