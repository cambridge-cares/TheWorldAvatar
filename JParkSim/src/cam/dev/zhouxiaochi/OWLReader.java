package cam.dev.zhouxiaochi;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.esri.map.ArcGISFeatureLayer;

public class OWLReader {
	 public static NodeList individuals;
	 public static boolean flag = false;
	 
	 public static double x;
	 public static double y;
	 
	 public static ArrayList<String> data_list;
	 public static ArrayList<String> value_list;
	 public static ArrayList<String> name_list;
	 public static ArrayList<String> raw_name_list;
	 public static ArrayList<String> unit_list;
	 public static ArrayList<String> relationships;
	 public static String name;
	 
	 public static String[] targets;
	 
	 
	 public static Map<String,String> comb;
	 
	 
	 /**
	  * 
	  * @param filename the name of the owl file you want to read 
	  * @param device the device name that you want to read the data from 
	  * @throws Exception
	  * @throws IOException
	  */
	public static void read_owl_file (String filename, String device) throws Exception, IOException {
		
		readlist();
		

		if(filename == null)
		{
	//		filename = "BiodieselPlant3.owl";
			filename = "BiodieselPlant3.owl";		 
		}
		
		name = device;
		
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
    	   name = name.substring(name.indexOf("#"));
     
       }
    
       
       data_list = new ArrayList<String>();
       value_list = new ArrayList<String>();
       name_list = new ArrayList<String>();
       raw_name_list = new ArrayList<String>();
       unit_list = new ArrayList<String>();
	   relationships = new ArrayList<String>();
       
       
       data_list.add("####################");
       data_list.add("####################");
       data_list.add("####################");
       
        
       System.out.println(device);
       expand(find_node(device));
      
       
       
       System.out.println();
        pack_up();
         
 
    
 
        
        comb = new HashMap<String, String>();
        for(int i = 0; i < name_list.size();i++)
        {
        	comb.put(name_list.get(i),value_list.get(i));
        }
        
        
        Set<String> new_list = new HashSet<>();
        
        new_list.addAll(name_list);
        
        name_list.clear();
        
        
        Set<String> new_list_raw = new HashSet<>();
        
        new_list_raw.addAll(raw_name_list);
        
        raw_name_list.clear();
        
         
        
        Object[] temp_array = new String[new_list.size()];
        
        temp_array =  new_list.toArray();
        
        
        
        Object[] temp_array_raw = new String[new_list_raw.size()];
        
        temp_array_raw =  new_list_raw.toArray();
        
        
        value_list.clear();
        
        
        for(Object item : temp_array)
        {
        	name_list.add((String) item);
        	
        	String value = comb.get((String) item);
        	 value_list.add(value);
        }
        
        for(Object itemx : temp_array_raw)
        {
        	raw_name_list.add((String) itemx);
        }
      

	}
	
	
	public static void expand(Node father_node)
	{
		
		NodeList list = father_node.getChildNodes();
		for(int i = 2 ; i < list.getLength() ; i++)
		{
			if(list.item(i).getNodeType() == Node.ELEMENT_NODE)    
			{
			String value = list.item(i).getAttributes().item(0).getNodeValue();
			String name = value.split("#")[1];
			
			String node_name = list.item(i).getNodeName();
			String node_value = list.item(i).getTextContent();
	 
			if(node_name.contains("realize"))
			{
		//	System.out.println("we found one --- > "   + list.item(i).getNodeName() );
			
				Node relationship_node = find_node(name);
		     	getRelationships(relationship_node);
				System.out.println("LOOK---------->" +  relationship_node.getAttributes().item(0).getNodeValue());
				
 			}
			
			if(node_value != "")
			{
				data_list.add(name + "/" + node_name +"/" + node_value);
	//  	System.out.println(node_name + " --- "+ name + "---" + node_value) ;
			}
			else
			{
	// 			System.out.println(node_name + " --- "+ name) ;
     			data_list.add(name + "/" + node_name );
			}
			

			// numerical value  +  the node _ name
			
			
			if(find_node(name)!=null)
			{
			expand(find_node(name));
		 
			}
			}
			
		}
		data_list.add("####################");
		 
//	 	System.out.println("---------------------------------" );

	}
	
	
	public static Node  find_node(String node_name)
	{
		Node result = null;
		  for(int i = 0; i < individuals.getLength();i++)
	       {
	    	   String xxx = individuals.item(i).getAttributes().item(0).getNodeValue();
	    	   String name = xxx.split("#")[1];
	    	   if(name.contentEquals(node_name))	
	    	   {
	    		    result = individuals.item(i);
	    		 //   System.out.println(name);
	    	   }
	    	  
	       }
		 
		return result;
	}

	
	public static void pack_up() throws IOException
	{
		for(String item: data_list)
		{
			
			
		
			
			
			if(item.contains("numericalValue"))
			{
			    String value = "nil";
				String name = "nil";
				String unit = "nil";
				String raw_name = "nil";
				
			 
				value = item.split("/")[2]; 
			
				int index = data_list.indexOf(item);
			 
				while(!((data_list.get(index-1).contains("#"))&&(data_list.get(index-2).contains("#"))))
				{
					if(data_list.get(index-1).contains("#"))
					{
						
					}
					else
					{ 
						String data = data_list.get(index - 1);
				//	 	System.out.println(data);
					 	if(data.contains("ValueOf"))
					 	{
					// 		System.out.println("This is a value name ---" + data);
					 		raw_name =data.split("/")[0];
					 		name = data.split("/")[0].substring(7);
					 		name = name.replaceAll("-", "_");
		 
					 	}
					 	if(data.contains("hasUnitOf"))
					 	{
				//	 		System.out.println("This is a value unit ---" + data);
					 		unit = data.split("/")[0];
					 	}
					 	 
					}
					index = index - 1;
					
				} 
				
				value_list.add(value);
				raw_name_list.add(raw_name);
				name_list.add(name);
				unit_list.add(unit);
				  
			}
	
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
			
	//		System.out.println("-----------------------------------");
		}
	//	System.in.read();
		for(String item: name_list)
		{
			
			if(item.contains("_x_"))
			{
				int index = name_list.indexOf(item);
				String x_string = value_list.get(index);
				x = Double.parseDouble(x_string);
			}
			
			if(item.contains("_y_"))
			{
				int index = name_list.indexOf(item);
				String y_string = value_list.get(index);
				y = Double.parseDouble(y_string);
			}
			
		}
		
		
		
		
		
		
		
	}
	
	  public static void readlist()
	  {
		  
		  ArrayList<String> temp = new ArrayList<String>();
		  try (BufferedReader br = new BufferedReader(new FileReader("map.txt"))) {

				String sCurrentLine;

				while ((sCurrentLine = br.readLine()) != null) {
					temp.add(sCurrentLine);
				}

			} catch (IOException e) {
				e.printStackTrace();
			}
		  
		  targets = new String[temp.size()];
 		  int counter = 0; 
		  for(String item : temp)
		  {
		 
			 targets[counter] = item.split("#")[0];
			  counter++;
		  }
		  
		  
	  }
	
	  public static void getRelationships(Node relationship)
	  { 
		  relationships = new ArrayList<String>();
		  NodeList list = relationship.getChildNodes();
		  for(int i = 0; i < list.getLength();i++)
		  {
			  if((list.item(i).getNodeType() == Node.ELEMENT_NODE) && (list.item(i).getNodeName().contains("topology"))){
			   String temp =  list.item(i).getAttributes().item(0).getNodeValue();
 		      
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
































