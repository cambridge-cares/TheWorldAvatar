package cam.dev.zhouxiaochi;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public class OWLReader {
	 public static NodeList individuals;
	 public static boolean flag = false;
	 
	 public static double x;
	 public static double y;
	 
	 public static ArrayList<String> data_list;
	 public static ArrayList<String> value_list;
	 public static ArrayList<String> name_list;
	 public static ArrayList<String> unit_list;
	  
	 
	 /**
	  * 
	  * @param filename the name of the owl file you want to read 
	  * @param device the device name that you want to read the data from 
	  * @throws Exception
	  * @throws IOException
	  */
	public static void read_owl_file (String filename, String device) throws Exception, IOException {
		
		if(filename == null)
		{
			filename = "BiodieselPlant3.owl";
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
    	   name = name.substring(name.indexOf("#"));
    	   System.out.println(name);
    	   
    	   
       }
    
       
       data_list = new ArrayList<String>();
       value_list = new ArrayList<String>();
       name_list = new ArrayList<String>();
       unit_list = new ArrayList<String>();
       
       
       
       data_list.add("####################");
       data_list.add("####################");
       data_list.add("####################");
       
        
       
       expand(find_node(device));
      
       
       
       System.out.println();
        pack_up();
        
        System.out.println(value_list);
        System.out.println(name_list);
        System.out.println(unit_list);

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
	 

			if(node_value != "")
			{
				data_list.add(name + "-" + node_name +"-" + node_value);
	//  	System.out.println(node_name + " --- "+ name + "---" + node_value) ;
			}
			else
			{
	// 			System.out.println(node_name + " --- "+ name) ;
     			data_list.add(name + "-" + node_name );
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

	
	public static void pack_up()
	{
		for(String item: data_list)
		{
			if(item.contains("numericalValue"))
			{
				 		String value = "nil";
				String name = "nil";
				String unit = "nil";
				
				value = item.split("-")[2]; 
			//	System.out.println(item);
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
					 		name = data.split("-")[0].substring(7);
					 	}
					 	if(data.contains("hasUnitOf"))
					 	{
				//	 		System.out.println("This is a value unit ---" + data);
					 		unit = data.split("-")[0];
					 	}
					 	 
					}
					index = index - 1;
					
				} 
				
				value_list.add(value);
				name_list.add(name);
				unit_list.add(unit);
				
				
				
			}
	//		System.out.println("-----------------------------------");
		}
		
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
	
}
