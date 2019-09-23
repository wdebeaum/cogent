package extractors;

import java.io.*;
import java.util.*;
import java.util.Map.*;
import java.util.regex.Pattern;

import TRIPS.KQML.KQMLList;
import TRIPS.KQML.KQMLObject;
import states.ActType;
import org.json.simple.parser.*;
import org.json.simple.*;


public class OntologyReader {

	private HashMap<String,String> events; // Event type -> parent
	private HashMap<String,ArrayList<String>> goals;
	private HashMap<String,ArrayList<String>> actGoalSuggestionMapping;
	private HashMap<String,ArrayList<String>> implicitActGoalMapping;
	private HashMap<String,ArrayList<String>> goalArguments;
	private HashSet<String> models;
	
	public OntologyReader()
	{
		events = new HashMap<String, String>();
		goals = new HashMap<String, ArrayList<String>>();
		actGoalSuggestionMapping = new HashMap<String, ArrayList<String>>();
		implicitActGoalMapping = new HashMap<String, ArrayList<String>>();
		goalArguments = new HashMap<String,ArrayList<String>>();
		models = new HashSet<String>();
	}
	

	
	public void readGoalOntologyFromFile(String filename)
	{
		InputStream in = null;
		try {
			in = new FileInputStream(filename);
		} catch (FileNotFoundException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		try {
				
				BufferedReader reader = new BufferedReader(new InputStreamReader(in));
				String line = null;
				System.out.println("Reading goals...");
				
			    while ((line = reader.readLine()) != null) {
			    	if (line.trim().startsWith("#") || line.trim().startsWith(";"))
			    		continue;
			    	
			    	if (line.trim().length() < 2)
			    		continue;
			    	String[] isaSplit = line.split("=>");
			    	
			    	
			    	String goal;
			    	if (isaSplit[0].contains("|"))
			    	{
			    		String[] goalArgSplit = isaSplit[0].split(Pattern.quote("|"));
			    		System.out.println("goalArgSplit[0]=" + goalArgSplit[0]);
			    		System.out.println("goalArgSplit[1]=" + goalArgSplit[1]);
			    		goal = goalArgSplit[0].trim();
			    		System.out.println("goal=" + goal);
			    		String[] arguments = goalArgSplit[1].trim().split(",");
			    		if (arguments.length > 0)
					    	goalArguments.put(goal.toUpperCase(), new ArrayList<String>());
				    	for (String argument : arguments)
				    	{
				    		String trimmedArg = argument.trim().toUpperCase();
				    		System.out.println("trimmedArg=" + trimmedArg );
				    		goalArguments.get(goal).add(trimmedArg);
				    	}
			    	}
			    	else
			    		goal = isaSplit[0].trim();

			    	
			    	if (isaSplit.length > 1)
			    	{
				    	String parents = isaSplit[1].trim();
				    	
				    	ArrayList<String> parentList = new ArrayList<String>();
				    	
				    	for (String parent : parents.split(","))
				    	{
				    		if (parent.trim().length() == 0)
				    			continue;
				    		
				    		if (parent.trim().charAt(0) == '@')
				    		{
				    			if (!actGoalSuggestionMapping.containsKey(parent))
				    				actGoalSuggestionMapping.put(parent,new ArrayList<String>());
				    			actGoalSuggestionMapping.get(parent).add(goal);
				    		}
				    		else if (parent.charAt(0) == '^')
				    		{
				    			if (!implicitActGoalMapping.containsKey(parent))
				    				implicitActGoalMapping.put(parent,new ArrayList<String>());
				    			implicitActGoalMapping.get(parent).add(goal);		    			
				    		}
				    		else
				    			parentList.add(parent.toUpperCase().trim());
				    	}
				    	
				    	if (parentList.size() == 0)
				    	{
				    		goals.put(goal.toUpperCase(), null);
				    		System.out.println(goal.toUpperCase());
				    	}
				    	else
				    	{
					    	goals.put(goal.toUpperCase(), parentList);
					    	System.out.print(goal.toUpperCase() + ":" );
					    	for (String parent : parentList)
					    		System.out.print(parent + ",");
					    	System.out.println();
				    	}
			    	}
			    	else
			    	{
			    		goals.put(goal.toUpperCase(),null);
			    		System.out.println(goal.toUpperCase());
			    	}
			    }
			    in.close();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				System.err.println("Could not open goal file " + filename.toString() + " for reading.");
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
	
	public void readModelOntologyFromFile(String filename)
	{
		InputStream in = null;
		try {
			in = new FileInputStream(filename);
		} catch (FileNotFoundException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		try {
				
				BufferedReader reader = new BufferedReader(new InputStreamReader(in));
				String line = null;
				System.out.println("Reading models...");
				
			    while ((line = reader.readLine()) != null) {
			    	if (line.trim().startsWith("#") || line.trim().startsWith(";"))
			    		continue;
			    	
			    	if (line.trim().length() < 2)
			    		continue;
			    	
			    	String model = line;
			    	if (model.trim().length() > 0)
			    		models.add(model);
			    }
			    in.close();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				System.err.println("Could not open events file " + filename.toString() + " for reading.");
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
	
	public List<String> getRootEvents()
	{
		List<String> result = new ArrayList<String>();
		for (Entry<String,String> e : events.entrySet())
		{
			if (e.getValue().equals("1"))
				result.add(e.getKey());
		}
		
		return result;
	}
	
	public List<String> getRootGoals()
	{
		List<String> result = new ArrayList<String>();
		for (Entry<String,ArrayList<String>> e : goals.entrySet())
		{
			if (e.getValue() == null)
				result.add(e.getKey());
		}
		
		return result;
	}
	
	public List<String> getParentGoals(String goalType)
	{
		return goals.get(goalType);
	}
	
	public List<String> getParentGoalsOfActType(ActType actType)
	{
		String actTypeString = "@" + actType.toString();
		
		return actGoalSuggestionMapping.get(actTypeString);
	}
	
	public List<String> getImplicitAcceptsOfActType(ActType actType)
	{
		String actTypeString = "^" + actType.toString();
		
		return implicitActGoalMapping.get(actTypeString);
	}
	
	public void readEventOntologyFromFile(String filename)
	{
		InputStream in = null;
		try {
			in = new FileInputStream(filename);
		} catch (FileNotFoundException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		try {
				
				BufferedReader reader = new BufferedReader(new InputStreamReader(in));
				String line = null;
				System.out.println("Reading events...");
				
			    while ((line = reader.readLine()) != null) {
			    	if (line.trim().startsWith("#") || line.trim().startsWith(";"))
			    		continue;
			    	if (line.trim().length() < 2)
			    		continue;
			    	String[] isaSplit = line.split("=>");
			    	String event = isaSplit[0].trim();
			    	String parent = isaSplit[1].trim();

			    	
			    	events.put(event.toUpperCase(), parent.toUpperCase());
			    	System.out.println(event.toUpperCase() + ":" + parent.toUpperCase());
			    }
			    in.close();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				System.err.println("Could not open events file " + filename.toString() + " for reading.");
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
	
	public boolean isEvent(String term)
	{
		return events.containsKey(term.toUpperCase());
	}
	
	public boolean isGoalWithArgument(String goalType, String argument)
	{

		return goalArguments.containsKey(goalType.toUpperCase()) &&
				goalArguments.get(goalType.toUpperCase()).contains(argument.toUpperCase());
	}
	
	public boolean isKnownModel(String term)
	{
		return models.contains(term);
	}
	
	public boolean isGoal(String goalType)
	{
		return goals.containsKey(goalType.toUpperCase());
	}
	
	public boolean isRootGoal(String goalType, KQMLList context)
	{
		if (getRootGoals().contains(goalType))
		{
			if (!goalArguments.containsKey(goalType))
				return true;
			else
			{
				for (String argument : goalArguments.get(goalType))
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
		
		return false;
	}
}
