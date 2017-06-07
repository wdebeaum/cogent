package handlers;

public class IDHandler {
	static int currentID = 0;
	
	public static synchronized String getNewID()
	{
		currentID++;
		return "C" + String.format("%05d", currentID);

	}
	
	public static void resetIDs()
	{
		currentID = 0;
	}
}
