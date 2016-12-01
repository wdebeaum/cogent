package handlers;

import java.util.HashMap;

import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLObject;

public class ReferenceHandler {

	HashMap<String,KQMLList> references = new HashMap<String,KQMLList>();
	
	public ReferenceHandler() {
		references = new HashMap<String, KQMLList>();
	}
	
	public void addReference(KQMLList reference)
	{
		references.put(reference.get(1).stringValue(), reference);
		System.out.println("Adding term " + reference.get(1).stringValue());
	}
	
	public void addReferences(KQMLList references)
	{
		for (KQMLObject term : references)
		{
			if (term instanceof KQMLList)
				addReference((KQMLList)term);
		}
	}
	
	public KQMLList getReference(String variable)
	{
		return references.get(variable);
	}
	
	public KQMLList generateContextForTerm(KQMLList term)
	{
		System.out.println("Generating context for " + term.stringValue());
		KQMLList contextToReturn = new KQMLList();
		for (KQMLObject element : term)
		{
			System.out.println("Checking element " + element);
			String stringValue = element.stringValue();
			if (stringValue.charAt(0) != ':')
			{
				if (references.containsKey(stringValue))
				{
					contextToReturn.add(references.get(stringValue));
					System.out.println("Adding " + references.get(stringValue));
				}
			}
		}
		
		return contextToReturn;
	}
	

}
