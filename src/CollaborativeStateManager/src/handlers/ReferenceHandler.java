package handlers;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;

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
		//System.out.println("Adding term " + reference.get(1).stringValue());
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
		HashSet<KQMLList> contextElementsToAdd = new HashSet<KQMLList>();
		List<KQMLList> contextAdditionHistory = new ArrayList<KQMLList>();
		contextAdditionHistory.add(term);
		int newElementsAdded = 1;
		int currentIndex = 0;
		KQMLList currentTerm = term;
		while (newElementsAdded != 0)
		{
			newElementsAdded = 0;
			currentTerm = contextAdditionHistory.get(currentIndex);
			for (KQMLObject element : currentTerm)
			{
				//System.out.println("Checking element " + element);
				String stringValue = element.stringValue();
				if (stringValue.charAt(0) != ':')
				{
					if (references.containsKey(stringValue) && 
							!contextElementsToAdd.contains(references.get(stringValue)))
					{
						newElementsAdded++;
						contextElementsToAdd.add(references.get(stringValue));
						contextAdditionHistory.add(references.get(stringValue));
						//contextToReturn.add(references.get(stringValue));
						//System.out.println("Adding " + references.get(stringValue));
					}
				}
			}
			currentIndex++;
		}
		
		for (KQMLList addingTerm : contextElementsToAdd)
			contextToReturn.add(addingTerm);
		
		return contextToReturn;
	}
	

}
