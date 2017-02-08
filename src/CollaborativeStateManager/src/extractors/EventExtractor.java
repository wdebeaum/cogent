package extractors;

import handlers.IDHandler;
import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLObject;

public class EventExtractor {

	String what;
	String id;
	KQMLList eventContext;
	KQMLList relnEventContext;
	KQMLList originalContext;
	KQMLList eventIDs;
	OntologyReader ontologyReader;
	
	
	public EventExtractor(OntologyReader ontologyReader)
	{
		eventContext = new KQMLList();
		relnEventContext = new KQMLList();
		eventIDs = new KQMLList();
		this.ontologyReader = ontologyReader;
	}
	
	public void apply(KQMLList context)
	{
		originalContext = context;
		what = IDHandler.getNewID();
		id = IDHandler.getNewID();
		for (KQMLObject term : context)
		{
			if (!(term instanceof KQMLList))
				continue;
			KQMLList termList = (KQMLList)term;
			
			if (ontologyReader.isEvent(termList.getKeywordArg(":INSTANCE-OF")
																.stringValue()))
			{
				eventIDs.add(termList.get(1));
				eventContext.add(term);
			}
		}
	}
	
	public KQMLList getEventsInContext()
	{
		return eventContext;
	}
	
	public KQMLList getOriginalContext()
	{
		return originalContext;
	}
	
	public KQMLList getEventIDsInContext()
	{
		return eventIDs;
	}
	
	public String getWhat()
	{
		return what;
	}
	
	public String getId()
	{
		return id;
	}
	
	
}
