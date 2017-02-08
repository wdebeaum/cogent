package utilities;

import java.io.IOException;
import java.util.HashSet;

import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLObject;

public class KQMLUtilities {

	
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
}
