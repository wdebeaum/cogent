package extractors;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.json.simple.parser.*;

import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLObject;
import states.ActType;

public class JSONOntologyReader extends OntologyReader {

	private HashMap<String,GoalSpecification> ontTypeToGoalSpecification;
	private HashMap<String,GoalSpecification> idToGoalSpecification;
	private HashMap<String,List<GoalSpecification>> speechActToGoalSpecification;
	private HashMap<String,List<GoalSpecification>> speechActToImplicitlyAcceptedGoalSpecifications;
	private HashMap<String,String> events; // Event type -> parent
	
	public JSONOntologyReader()
	{
		events = new HashMap<String, String>();
		ontTypeToGoalSpecification = new HashMap<String, GoalSpecification>();
		idToGoalSpecification = new HashMap<String, GoalSpecification>();
		speechActToGoalSpecification = new HashMap<String, List<GoalSpecification>>();
		speechActToImplicitlyAcceptedGoalSpecifications = new HashMap<String, List<GoalSpecification>>();
	}
	
	public void readEventGoalJSONFile(String filename)
	{
		InputStream in = null;
		JSONParser parser = new JSONParser();
		JSONObject document;
		try {
			in = new FileInputStream(filename);
		} catch (FileNotFoundException e1) {
			// TODO Auto-generated catch block
			System.err.println("Could not open goal file " + filename.toString() + " for reading.");
			e1.printStackTrace();
		}
		try {
				
				BufferedReader reader = new BufferedReader(new InputStreamReader(in));
				document = (JSONObject) parser.parse(reader);
				updateFromJSONObject(document);
		} catch (IOException ioe)
		{
			System.err.println("Could not open goal file " + filename.toString() + " for reading.");
			ioe.printStackTrace();
		} catch (Exception e) {
			System.err.println("Could not parse goal file " + filename.toString() + " for reading.");
			e.printStackTrace();
		} finally {
			try {
				if (in != null)
					in.close();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}		
	}
	
	private void updateFromJSONObject(JSONObject document)
	{
		JSONArray eventArray = (JSONArray)(document.get("events"));
		Iterator<String> eventIterator = eventArray.iterator();
        while (eventIterator.hasNext()) {
            events.put(eventIterator.next(), null);
        }
        
		JSONArray goalArray = (JSONArray)(document.get("goals"));
		Iterator<JSONObject> goalIterator = goalArray.iterator();
        while (goalIterator.hasNext()) {
        	JSONObject goal = goalIterator.next();
            String id = (String)goal.get("id");
            GoalSpecification gs = new GoalSpecification(id);
            
            if (goal.containsKey("ontType"))
            {
            	gs.ontType = (String)goal.get("ontType");
            	ontTypeToGoalSpecification.put(gs.ontType,gs);
            }
            
            if (goal.containsKey("eventual"))
            	gs.eventual = (boolean)goal.get("eventual");
            
            if (goal.containsKey("parent"))
            	gs.parentId = (String)goal.get("parent");
            
            if (goal.containsKey("implicitAcceptIfFollowedBy"))
            {
            	Iterator<String> implicitAcceptIterator = 
            			((JSONArray)goal.get("implicitAcceptIfFollowedBy")).iterator();
            	
            	while (implicitAcceptIterator.hasNext())
            	{
            		String value = implicitAcceptIterator.next();
            		gs.implicitAcceptIfFollowedBy.add(value);
            		
            		if (!speechActToImplicitlyAcceptedGoalSpecifications.containsKey(value))
            			speechActToImplicitlyAcceptedGoalSpecifications.put(value, new ArrayList<GoalSpecification>());
            		
            		speechActToImplicitlyAcceptedGoalSpecifications.get(value).add(gs);
            	}
            }
            
            if (goal.containsKey("speechAct"))
            {
            	gs.speechAct = (String)goal.get("speechAct");
            	if (!speechActToGoalSpecification.containsKey(gs.speechAct))
            		speechActToGoalSpecification.put(gs.speechAct, new ArrayList<GoalSpecification>());
            	speechActToGoalSpecification.get(gs.speechAct).add(gs);
            }
            
            if (goal.containsKey("args"))
            {
            	Map readMap = (Map)goal.get("args");
            	
            	for (Object key : readMap.keySet())
            	{
            		String keyString = (String)key;
            		JSONArray value = (JSONArray)readMap.get(key);
            		gs.args.put(keyString, new ArrayList<String>());
            		gs.args.get(keyString).addAll(value);
            	}
            }
            
            idToGoalSpecification.put(id, gs);
            
        }
        
        for (GoalSpecification gs : idToGoalSpecification.values())
        {
        	if (gs.parentId != null && idToGoalSpecification.containsKey(gs.parentId))
        		gs.parent = idToGoalSpecification.get(gs.parentId);
        		
        }
        System.out.println("Events read:");
        for (String event: events.keySet())
        	System.out.println(event);
	}
	
	public List<String> getRootEvents()
	{
		List<String> result = new ArrayList<String>();
		for (Entry<String,String> e : events.entrySet())
		{
			if (e.getValue() == null)
				result.add(e.getKey());
		}
		
		return result;
	}
	
	public List<String> getRootGoals()
	{
		List<String> result = new ArrayList<String>();
		for (GoalSpecification gs : idToGoalSpecification.values())
		{
			if (gs.parent == null && gs.ontType != null)
				result.add(gs.ontType);
		}
		
		return result;
	}
	
	public List<String> getParentGoals(String goalType)
	{
		List<String> results = new ArrayList<String>();
		if (ontTypeToGoalSpecification.containsKey(goalType))
		{
			GoalSpecification gs = ontTypeToGoalSpecification.get(goalType);
			if (gs.parent != null && gs.parent.ontType != null)
				results.add(gs.parent.ontType);
		}
		return results;
	}
	
	public List<String> getImplicitAcceptsOfActType(ActType actType)
	{
		if (!speechActToImplicitlyAcceptedGoalSpecifications.containsKey(actType.toString()))
			return new ArrayList<String>();
		List<GoalSpecification> goalsToImplicitlyAccept = speechActToImplicitlyAcceptedGoalSpecifications.get(actType.toString());
		List<String> goalTypes = new ArrayList<String>();
		for (GoalSpecification gs : goalsToImplicitlyAccept)
		{
			if (gs.ontType != null)
				goalTypes.add(gs.ontType);
		}
		return goalTypes;
	}
	
	public List<String> getParentGoalsOfActType(ActType actType)
	{
		List<String> results = new ArrayList<String>();
		for (GoalSpecification gs : idToGoalSpecification.values())
		{
			if (gs.suggestedSpeechAct != null && gs.ontType != null &&
					gs.suggestedSpeechAct.equalsIgnoreCase(actType.toString()))
				results.add(gs.ontType);
		}
		return results;
	}
	
	public boolean isEvent(String term)
	{
		return events.containsKey(term.toUpperCase());
	}
	
	public boolean isGoalWithArgument(String goalType, String argument)
	{
		// TODO: Too generic, doesn't use argument for ont goals
		if (ontTypeToGoalSpecification.containsKey(goalType.toUpperCase()) &&
				!ontTypeToGoalSpecification.get(goalType.toUpperCase()).args.isEmpty())
			return true;
		
		if (speechActToGoalSpecification.containsKey(goalType.toUpperCase()))
		{
		
			for (GoalSpecification gs : speechActToGoalSpecification.get(goalType.toUpperCase()))
			{
				// Only look at neutrals for now
				if (!gs.args.containsKey("neutral"))
				{
					System.out.println("No neutral args found");
					continue;
				}
				for (String neutralArg : gs.args.get("neutral"))
					if (neutralArg.equalsIgnoreCase(argument))
						return true;
				
				System.out.println("No matching args found");
			}
		}
		return false;
	}
	
	
	public boolean isGoal(String goalType)
	{
		return ontTypeToGoalSpecification.containsKey(goalType.toUpperCase());
	}
	
	public boolean isRootGoal(String goalType, KQMLList context)
	{
		if (getRootGoals().contains(goalType))
		{
			GoalSpecification gs = ontTypeToGoalSpecification.get(goalType);
			if (gs.args.isEmpty())
				return true;
			else
			{
				if (gs.args.containsKey(":NEUTRAL"))
				{
					for (String argument : gs.args.get(":NEUTRAL"))
					{
						for (KQMLObject term : context)
						{
							KQMLList termAsList = (KQMLList)term;
							if (termAsList.getKeywordArg(":INSTANCE-OF").stringValue().equalsIgnoreCase(argument))
								return true;
						}
					}
				}
			}
		}
		
		return false;
	}
	
	public boolean isEventualGoal(String goalType)
	{
		if (ontTypeToGoalSpecification.containsKey(goalType.toUpperCase()))
		{
			GoalSpecification gs = ontTypeToGoalSpecification.get(goalType.toUpperCase());
			return gs.eventual;
				
		}
		return false;
	}
	
	@Override
	public boolean supportsEventualGoals()
	{
		return true;
	}
	
}
