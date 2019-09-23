package extractors;

import java.util.ArrayList;
import java.util.HashMap;

public class GoalSpecification {
	public String id;
	public String ontType = null;
	public GoalSpecification parent = null;
	public boolean eventual = false;
	public ArrayList<String> implicitAcceptIfFollowedBy;
	public String speechAct = null;
	public String suggestedSpeechAct = null;
	public String parentId = null;
	public HashMap<String, ArrayList<String>> args;
	
	public GoalSpecification(String id)
	{
		this.id = id;
		args = new HashMap<String, ArrayList<String>>();
		implicitAcceptIfFollowedBy = new ArrayList<String>();
	}
}
