package utilities;

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
}
