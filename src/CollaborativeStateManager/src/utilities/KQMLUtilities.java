package utilities;

import java.io.IOException;
import java.util.HashSet;

import extractors.TermExtractor;
import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLObject;

public class KQMLUtilities {

	
	public static KQMLList findTermInKQMLList(String variableName,KQMLList list)
	{
		for (KQMLObject term : list)
		{
			if (term instanceof KQMLList)
			{
				KQMLList termList = (KQMLList)term;
				if (termList.get(1).stringValue().equalsIgnoreCase(variableName))
					return termList;
			}
		}
		return null;
	}
	
	public static boolean isKQMLNull(KQMLObject kqmlObject)
	{
		if (kqmlObject == null)
			return true;
		if (kqmlObject.stringValue().equalsIgnoreCase("NIL") ||
				kqmlObject.stringValue().equals("-"))
			return true;
		
		return false;
	}
	
	public static KQMLList removedDuplicates(KQMLList list)
	{
		HashSet<String> set = new HashSet<String>();
		for (KQMLObject obj : list)
			set.add(obj.stringValue());
		
		KQMLList toReturn = new KQMLList();
		for (String obj : set)
		{
			KQMLList fromString;
			try {
				fromString = KQMLList.fromString(obj);
			} catch (IOException e) {
				continue;
			}
			toReturn.add(fromString);
		}
		
		return toReturn;
	}
	
	public static KQMLObject getLinkedArguments(KQMLList term, KQMLList context, String ...args)
	{
		KQMLList currentTerm = term;
		
		for (int i = 0; i < args.length; i++)
		{
			if (currentTerm.getKeywordArg(args[i]) == null)
				return null;

			KQMLObject variableValue = currentTerm.getKeywordArg(args[i]);

			// Reached the end, return the value
			if (i == args.length - 1)
			{
				return variableValue;
			}
			
			currentTerm = TermExtractor.extractTerm(variableValue.stringValue(),
													context);
			
			if (currentTerm == null)
				return null;
			
		}
		return null;
	}
	
	public static String getLinkedArgumentsAsString(KQMLList term, KQMLList context, String ...args)
	{
		KQMLObject result = getLinkedArguments(term, context, args);
		if (result == null)
			return null;
		return result.stringValue();
	}
	
	public static String getArgumentAsString(KQMLObject term, String argument)
	{
		if (!(term instanceof KQMLList))
			return null;
		
		KQMLList termList = (KQMLList)term;
		
		if (termList.getKeywordArg(argument) == null)
			return null;
		
		return termList.getKeywordArg(argument).stringValue();
	}
	
}
