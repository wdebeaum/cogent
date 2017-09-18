package extractors;

import java.io.*;
import java.util.*;
import java.util.Map.*;

import states.ActType;

public class OntologyReader {

	private HashMap<String,String> events; // Event type -> parent
	private HashMap<String,ArrayList<String>> goals;
	private ArrayList<String> goalOrdering;
	private HashMap<String,ArrayList<String>> actGoalSuggestionMapping;
	private HashSet<String> models;
	
	public OntologyReader()
	{
		events = new HashMap<String, String>();
		goals = new HashMap<String, ArrayList<String>>();
		actGoalSuggestionMapping = new HashMap<String, ArrayList<String>>();
		goalOrdering = new ArrayList<String>();
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
			    	String goal = isaSplit[0].trim();
			    	if (isaSplit.length > 1)
			    	{
				    	String parents = isaSplit[1].trim();
				    	
				    	ArrayList<String> parentList = new ArrayList<String>();
				    	
				    	for (String parent : parents.split(","))
				    	{
				    		if (parent.trim().length() > 0 && parent.charAt(0) != '@')
				    			parentList.add(parent.toUpperCase().trim());
				    		else if (parent.trim().length() > 0)
				    		{
				    			if (!actGoalSuggestionMapping.containsKey(parent))
				    				actGoalSuggestionMapping.put(parent,new ArrayList<String>());
				    			actGoalSuggestionMapping.get(parent).add(goal);
				    		}
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
	
	public boolean isKnownModel(String term)
	{
		return models.contains(term);
	}
	
	public boolean isRootGoal(String goalType)
	{
		return getRootGoals().contains(goalType);
	}
}
