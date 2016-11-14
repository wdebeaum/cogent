package extractors;

import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLObject;

public class TermExtractor {

	public static KQMLList extractTerm(String variable, KQMLList context)
	{
		for (KQMLObject termObject : context)
		{
			if (termObject instanceof KQMLList)
			{
				KQMLList term = (KQMLList)termObject;
				if (term.get(1).stringValue().equalsIgnoreCase(variable))
					return term;
			}
		}
		return null;
	}
	
}
