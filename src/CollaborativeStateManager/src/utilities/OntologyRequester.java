package utilities;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import TRIPS.CollaborativeStateManager.*;
import TRIPS.KQML.*;

public class OntologyRequester {

	public OntologyRequester() {
		// TODO Auto-generated constructor stub
	}
	
	CollaborativeStateManager csm;
	HashMap<String,List<String>> hierarchyPaths; // Hierarchy paths sorted leaf -> root
	
	
	public OntologyRequester(CollaborativeStateManager csm)
	{
		this.csm = csm;
		this.hierarchyPaths = new HashMap<String, List<String>>();
	}
	
	// Prolly broken
	public void getOntologicalParent(KQMLList term)
	{
		if (term.getKeywordArg(":INSTANCE-OF") == null)
			return;
		
		String termTypeString = term.getKeywordArg(":INSTANCE-OF").stringValue();
		
		KQMLPerformative message = null;
		try {
			message = 
					KQMLPerformative.fromString("(request :receiver ONTOLOGYMANAGER :content (get-parent " + 
																	termTypeString + "))");
					
		} catch (IOException e) {
			// TODO Auto-generated catch block
			System.err.println("Error generating KQMLPerformative for ontological parent request");
			e.printStackTrace();
		}
		
		csm.sendKQMLPerformative(message);
		
	}
	public void getOntologicalParents(KQMLList term)
	{
		if (term.getKeywordArg(":INSTANCE-OF") == null)
			return;
		
		String termTypeString = term.getKeywordArg(":INSTANCE-OF").stringValue();
		
		KQMLPerformative message = null;
		try {
			message = 
					KQMLPerformative.fromString("(request :receiver ONTOLOGYMANAGER :content (get-parents " + 
																	termTypeString +
																	"))");
					
		} catch (IOException e) {
			// TODO Auto-generated catch block
			System.err.println("Error generating KQMLPerformative for ontological parent request");
			e.printStackTrace();
		}
		
		csm.sendKQMLPerformative(message);
		
	}
	
	// Prolly broken
	public void getOntologicalPath(KQMLList term)
	{

		if (term.getKeywordArg(":INSTANCE-OF") == null)
			return;
		
		String termTypeString = term.getKeywordArg(":INSTANCE-OF").stringValue();
		
		KQMLPerformative message = null;
		try {
			message = 
					KQMLPerformative.fromString("(request :content (eval-path :word " + 
																	termTypeString +
																	" :path " +
																	"(" + termTypeString + " (repeat 1 nil >inherit))))");
		} catch (IOException e) {
			// TODO Auto-generated catch block
			System.err.println("Error generating KQMLPerformative for ontological parent request");
			e.printStackTrace();
		}
		
		csm.sendKQMLPerformative(message);

	}
	
	public void receivedSenseResponse(KQMLList responseContent)
	{

	}
	
	public void receivedPathResponse(KQMLList response)
	{
		KQMLList pathExpression = (KQMLList)response.getKeywordArg(":PATH-EXPRESSION");
		KQMLList word = (KQMLList)response.getKeywordArg(":word");
		ArrayList<String> path = new ArrayList<String>();
		for (KQMLObject concept: pathExpression)
		{
			path.add(((KQMLList)concept).get(1).stringValue());
		}
		hierarchyPaths.put(word.stringValue(), path);
	}
	
	/**
	 * Parses the path list response from DeepSemLex, and sorts the hierarchy 
	 * from most specific to the least specific (leaf -> root)
	 * 
	 * @param response
	 * @return The utterance number for the message received
	 */
	public int receivedPathListResponse(KQMLList response)
	{
		KQMLList paths = (KQMLList)response.getKeywordArg(":paths");
		KQMLObject uttnum = response.getKeywordArg(":uttnum");
		
		String currentWord = "";
		for (KQMLObject conceptTupleObject : paths)
		{
			KQMLList conceptTuple = (KQMLList)conceptTupleObject;
			currentWord = conceptTuple.get(0).stringValue();
			if (conceptTuple.get(1).stringValue().equals("NIL"))
			{
				hierarchyPaths.put(currentWord, null);
				continue;
			}
			KQMLList pathExpression = ((KQMLList)conceptTuple.get(1));
			List<String> path = parsePathExpression(pathExpression);
			hierarchyPaths.put(currentWord, path);
		}
		for (Entry<String,List<String>> entry : hierarchyPaths.entrySet())
		{
			System.out.println(entry.getKey() + ": ");
			if (entry.getValue() == null)
				continue;
			for (String concept: entry.getValue())
				System.out.print(concept + " -> ");
		}
		return Integer.parseInt(uttnum.stringValue());
	}
	
	private List<String> parsePathExpression(KQMLList pathExpression)
	{
		ArrayList<String> path = new ArrayList<String>();
		Map<String,String> inheritanceLinks = new HashMap<String,String>();
		Map<String,String> reverseInheritanceLinks = new HashMap<String,String>();
		String root = null;
		// Extract all of the pointers from the elements returned
		for (KQMLObject concept: pathExpression)
		{
			KQMLList conceptList = (KQMLList)concept;

			String node = conceptList.get(1).stringValue();
			if (conceptList.size() > 3) //Has inherit element
			{
				String parent = ((KQMLList)conceptList.get(3)).get(1).stringValue();
				inheritanceLinks.put(node, parent);
				reverseInheritanceLinks.put(parent, node);
			}
			else //Doesn't, must be a root node
			{
				root = node;
			}
		}
		
		String currentNode = root;
		while(currentNode != null)
		{
			path.add(currentNode);
			if (reverseInheritanceLinks.containsKey(currentNode))
				currentNode = reverseInheritanceLinks.get(currentNode);
			else
				break;
		}
		Collections.reverse(path);
		return path;
	}
	
	public void receivedConceptResponse(KQMLList response)
	{
		
	}


}
