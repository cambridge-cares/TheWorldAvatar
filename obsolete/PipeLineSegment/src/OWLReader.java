

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public class OWLReader {
	
	
	
	/****
	 * The class that stores all the infomation of a node within OWL file
	 * @param name --> the name of the node (the string after #)
	 * @param type   --> the nodetype, for exampe numericalValue
	 * @param value  --> the value of a numerical value
	 * @param unit    --> the unit of the value 
	 */
	public static class OWLfileNode
	{
		OWLfileNode(String name, String type, String value, String unit, String parent) {
			NodeName = name;
			NodeType = type;
			NodeValue = value;
			ValueUnit = unit;
			ParentNodeName = parent;
		}
		
		public  String NodeName;
		public  String NodeType;
		public  String NodeValue;
		public  String ValueUnit;
		public  String ParentNodeName;
		public boolean CarryData = false;
	}	
	
	
public static NodeList individuals;	
public static ArrayList<String> nodelist = new ArrayList<String>();


public static Map<String,Node> nodemap = new HashMap<>();
public static Set<OWLfileNode> theNodeList = new HashSet<>(); // the raw nodelist that stores every node connected the the target
public static Set<OWLfileNode> theFinalNodeList = new HashSet<>(); // the filterd nodelist that returns the nodes that carries data

public static ArrayList<String> name_list = new ArrayList<String>();
public static ArrayList<String> value_list = new ArrayList<String>();
public static ArrayList<String> relationships = new ArrayList<String>();

public static double x;
public static double y; 


 
	public static ArrayList<String> read_owl_file (String filename, String device) throws Exception, IOException {

		nodelist.clear();
		name_list.clear();
		value_list.clear();
		relationships.clear();
		nodemap.clear();
		theNodeList.clear();
		theFinalNodeList.clear();
		

		
		
		
		
		

		if(filename == null)
		{
		 //	filename = "buildingmodif2.owl";	 
			filename = "BiodieselPlant3.owl";
		//	filename = "storageTest.owl";
		} 
		
		
		
		
	   File inputFile = new File(filename);
       DocumentBuilderFactory dbFactory  = DocumentBuilderFactory.newInstance();
       DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
       Document doc = dBuilder.parse(inputFile);
       doc.getDocumentElement().normalize();
       Element root = doc.getDocumentElement();
       individuals = root.getElementsByTagName("owl:NamedIndividual");
       
       for(int i = 0 ; i < individuals.getLength() ; i ++)
       {
    	   String name = individuals.item(i).getAttributes().item(0).getNodeValue();
    	   name = name.substring(name.indexOf("#") + 1 );
    	   nodelist.add(name);
    	   nodemap.put(name, individuals.item(i));
       }
       
       
       
		
		if(device == null)
		{
			return nodelist;
		}
       
       
       
       
       
       OWLfileNode startnode = new OWLfileNode(device,null,null,null,null);
      
       
       expand(startnode);
       

       
       
       
       // ------------------------------------------
       for(OWLfileNode node : theNodeList)
       {
    	//   System.out.println("Name:  " + node.NodeName + "|| Type: " + node.NodeType + 	"|| Value: " + node.NodeValue + "||Unit: " + node.ValueUnit + "|| Parent: " +  node.ParentNodeName ); 
   		if(node.CarryData)
   		{
   			theFinalNodeList.add(node);
   		}
       }
       
       
       
       
       
       
       for(OWLfileNode node: theFinalNodeList)
       {
     //    System.out.println("Name:  " + node.NodeName + "*** Type: " + node.NodeType + 	"*** Value: " + node.NodeValue + "*** Unit: " + node.ValueUnit + "*** Parent: " +  node.ParentNodeName ); 
         node.NodeName = node.NodeName.replaceAll("-", "_");
        
      //   System.out.println("-----------> " +  node.NodeName);
         name_list.add(node.NodeName);
         value_list.add(node.NodeValue);
         if(node.ValueUnit!=null)
         {
        	 name_list.add(node.NodeName + "_Unit" );
        	 value_list.add(node.ValueUnit);
         }
         
       }
       
		for(int i = 0; i < name_list.size(); i ++)
		{
			String item = name_list.get(i);
			if(item.contains("_x_") && !(item.contains("Unit")))
			{
				 x = Double.parseDouble(value_list.get(i));
			}
			
			if(item.contains("_y_") && !(item.contains("Unit")))
			{
				 y = Double.parseDouble(value_list.get(i));
			}
			
		}
        return null;
       
    }
	
 
	
	  public static void expand(OWLfileNode node)
	{
		
		if(nodemap.get(node.NodeName)!=null)
		{
		Node targetnode = nodemap.get(node.NodeName);
		NodeList childnodes = targetnode.getChildNodes();
		for(int i = 0; i < childnodes.getLength(); i++)
		{
			if(childnodes.item(i).getNodeType() ==  Node.ELEMENT_NODE) // make sure the node is a node that stores data
			{
			String nodename = childnodes.item(i).getAttributes().item(0).getNodeValue();// find the value of NamedIndividual , extract the name after #
			String nodetype = childnodes.item(i).getNodeName();
			String nodevalue = null;
			if(!(nodename.lastIndexOf("#") == nodename.length() - 1) &&!(nodetype.contains("rdf:type")))// check whether it is empty after #
				{ 
					nodename = nodename.split("#")[1];
					if(nodetype.toLowerCase().contains("unit"))
					{
				//		System.out.println("This is a unit --> " + nodename);
 						node.ValueUnit = nodename;
					}
					else
					{
					if(nodetype.contains("numerical"))
					{
					nodevalue =  childnodes.item(i).getTextContent();
					node.NodeValue = nodevalue;
					node.CarryData = true;
					}
					if(nodetype.contains("realize"))
					{
		//				System.out.println("Here is a realizes -- > " +  nodename);
						
						getRelationships( nodemap.get(nodename));
						
					}
					
					
					
					OWLfileNode newNode = new OWLfileNode(nodename,nodetype,"","",node.NodeName);
				//	System.out.println("Name:  " + newNode.NodeName + " Type: " + newNode.NodeType +
				//			" Value: " + newNode.NodeValue + " Unit: " + newNode.ValueUnit + " Parent: " +  newNode.ParentNodeName ); 
					theNodeList.add(newNode);
					if(nodemap.get(nodename)!=null) // check whether such node exists
					{
						expand(newNode);
					}
					 
					}
				}
			
			}
		}
		
		theNodeList.add(node);
		
		
	}
	 
	}

	  public static void getRelationships(Node relationship)
	  { 
	//	  System.out.println("Running relationship ---> ");
		  relationships = new ArrayList<String>();
		  NodeList list = relationship.getChildNodes();
	//	  System.out.println("list size --  " + list.getLength());
		  for(int i = 0; i < list.getLength();i++)
		  {
			  if((list.item(i).getNodeType() == Node.ELEMENT_NODE) && (list.item(i).getNodeName().contains("topology"))){
			   String temp =  list.item(i).getAttributes().item(0).getNodeValue();
	//		   System.out.println("relationship ---> " + temp);
			   if(!relationships.contains(temp.split("#")[1]))
			   {
				   
				   relationships.add(temp.split("#")[1]);
			   }
			    
			  }
		  }
		   removeDuplicates(relationships);
	  }
 	
	  public static void removeDuplicates(ArrayList<String> arraylist)
	  {
 		   for(int i = 0; i < arraylist.size(); i++)
		   {
 
			   for(int j = 0; j < arraylist.size(); j++)
			   {
				   if(arraylist.get(i).equals(arraylist.get(j))&& (i!=j))
				   {
					   arraylist.remove(j);
				   }
			   }
		   }
		  
	  }
 	
 	
 	
 	
 	
 	
 	
 	
 	
 	
 	
 	
 	
}
