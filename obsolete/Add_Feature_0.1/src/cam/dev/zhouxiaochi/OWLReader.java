package cam.dev.zhouxiaochi;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import com.esri.core.geometry.Point;

import cam.dev.zhouxiaochi.EntityTree.TreeNode;


public class OWLReader {
	  final static Logger logger = LoggerFactory.getLogger(OWLReader.class);
      final static String TOP_LEVEL_OWL_LOCATION = "owl/JurongIsland.owl";
      final static String TOP_LEVEL_OWL_NODE_NAME = "JurongIsland";

	
	/****
	 * The class that stores all the infomation of a node within OWL file
	 * @param name --> the name of the node (the string after #)
	 * @param type   --> the nodetype, for exampe numericalValue
	 * @param value  --> the value of a numerical value
	 * @param unit    --> the unit of the value 
	 */
	
    public static class Device // Such class describes a device 
	{
		public  String Id;     			// store the id of the device
		public  Point Point;			// store the coordinates of the device
		public  String[] Name_list;		// store the attributes names of the device
		public  String[] Value_list; 	// store the attributes values of teh device

	}	
	
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

public static Map<String,OWLfileNode> owlnodemap = new HashMap<>();//map links name to node info
public static Map<String,Node> nodemap = new HashMap<>();//map links name to actual node object
public static Set<OWLfileNode> theNodeList = new LinkedHashSet<>(); // the raw nodelist that stores every node connected the the target
public static Set<OWLfileNode> theFinalNodeList = new LinkedHashSet<>(); // the filterd nodelist that returns the nodes that carries data
public static Set<String> theNameList = new LinkedHashSet<>();


public static ArrayList<String> name_list = new ArrayList<String>();
public static ArrayList<String> value_list = new ArrayList<String>();
public static ArrayList<String> relationships = new ArrayList<String>();
public static ArrayList<String> usednames = new ArrayList<String>();


private static ArrayList<String> deviceNameList = new ArrayList<String>();
private static EntityTree entityNameTree = null;
private static List<EntityInfo> entityNameList = null;

private static Map<String, String> resourceLocationMap = new HashMap<String, String>();
public static double x = 0;
public static double y = 0; 


  // for storing coordinates of a batch of devices  





public static ArrayList<String> read_owl_file(String filename, String deviceName) throws IOException, Exception{
	
	return read_owl_file(filename, deviceName, false);
}


 
	public static ArrayList<String> read_owl_file (String filename, String deviceName, boolean expandOneLevelOnly) throws Exception, IOException {

		nodelist.clear();
		name_list.clear();
		value_list.clear();
		relationships.clear();
		nodemap.clear();
		theNodeList.clear();
		theFinalNodeList.clear();
		theNameList.clear();
		owlnodemap.clear();
		resourceLocationMap.clear();


		if(filename == null)
		{
			logger.warn("owl file location not defined. Function read owl file terminated");
		// System.out.println("WARNING: owl file location not defined. Function read owl file terminated");
		return null;
		} 
		
		
		
		
	   File inputFile = new File(filename);
	   if(!inputFile.exists()){
		  
			logger.info("owl file location "+filename+"not exists. Function read owl file terminated");

		   return null;
	   }
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
       
       
		//if no specific device return full nodelist
		if(deviceName == null)
		{
			return nodelist;
		}
       
       
       
       
       
       OWLfileNode startnode = new OWLfileNode(deviceName,null,null,null,"JesusChrist");
      
       
       expand(startnode, expandOneLevelOnly);
       

       
       
       
       // ------------------------------------------
       for(OWLfileNode node : theNodeList)
       {
    	//   System.out.println("Name:  " + node.NodeName + "|| Type: " + node.NodeType + 	"|| Value: " + node.NodeValue + "||Unit: " + node.ValueUnit + "|| Parent: " +  node.ParentNodeName ); 
   		if(node.CarryData)
   		{
   			theNameList.add(node.NodeName);
   			owlnodemap.put(node.NodeName, node);
   		}
       }
       
       
       
       /**
       
       
       for(OWLfileNode node: theFinalNodeList)
       {
     //    System.out.println("Name:  " + node.NodeName + "*** Type: " + node.NodeType + 	"*** Value: " + node.NodeValue + "*** Unit: " + node.ValueUnit + "*** Parent: " +  node.ParentNodeName ); 
         node.NodeName = node.NodeName.replaceAll("-", "_").trim();
        
         System.out.println("-----------> " +  node.NodeName);
         name_list.add(node.NodeName.trim());
         value_list.add(node.NodeValue.trim());
         if(node.ValueUnit!=null)
         {
        	 name_list.add(node.NodeName + "_Unit" );
        	 value_list.add(node.ValueUnit);
         }
         
         
       }
       
       **/
       usednames.clear();
       
       for(String name : theNameList)
       { 
    	   int counter = 0;
    	   OWLfileNode node = owlnodemap.get(name);
    	   node.NodeName = node.NodeName.replaceAll("-", "_").trim();
    	   if(node.NodeValue!="")
    	   {
    	   System.out.println("-----------> " +  node.NodeName);
    	   if(node.ValueUnit!=null)
           {
          	 name_list.add(node.NodeName + "_Unit" );
          	 value_list.add(node.ValueUnit);
           }
    	   }
    	   else if(node.NodeType!=null)
    	   {
    	   System.out.println("-----------> " +  node.NodeName + "-- has no value" + node.NodeType);
    	   String temp = node.NodeName;
    	   node.NodeName = node.NodeType;
    	   node.NodeValue = temp;
    	   
    	   node.NodeName = node.NodeName.replace(":", "_");
    	   node.NodeValue = temp.replace(":", "_");    	
    	   
    	   String temp2 = node.NodeName;
    	   

    		   for(String usedname : usednames)
    		   {
    			    if(usedname.contentEquals(temp2))
    			    {
    			    	counter++;
    			    }
    		   }
    		   
    	   usednames.add(temp2);
    	   
    	   }
    
    	   if(counter!=0)
    	   {
    		   name_list.add(node.NodeName.trim()  + counter);
    	   }
    	   else
    	   {
    		   name_list.add(node.NodeName.trim());
    	   }
    	   
    	   
           value_list.add(node.NodeValue.trim());
           
           
       }
       
 
		for(int i = 0; i < name_list.size(); i ++)
		{
			String item = name_list.get(i);
			if(item.contains("_x_") && !(item.contains("Unit")))
			{
				 x = Double.parseDouble(value_list.get(i));
			//	 logger.info("x---> " + x);
				 
			}
			
			if(item.contains("_y_") && !(item.contains("Unit")))
			{
				 y = Double.parseDouble(value_list.get(i));
				// logger.info("y---> " + y);

			}
			
		}
		
        return null;
       
    }
	
	
	
	
	
	  public static void expand(OWLfileNode node){
		  expand(node, false);
	  }

	//TODO: Modified so that if has attribute sameAs, search the sameAs node instead
	  public static void expand(OWLfileNode node, Boolean expandOneLevel)
	{
		
		if(nodemap.get(node.NodeName)!=null)///only run if node do exist
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
			//logger.info("++++Orgin Unfilterd  "+nodename);

			if(!(nodename.lastIndexOf("#") == nodename.length() - 1) )// check whether it is empty after #
				{
				if(!nodetype.contains("rdf:type") &&nodename.split("#").length > 1){
					nodename = nodename.split("#")[1];
					nodename= nodename.replace('.', '_');//replacing illegal char//TODO: do a full check
					logger.info("++++after replace.  "+nodename);
				if(nodetype.contains("owl:sameAs")) {//contains allias?
					 logger.info("a same as allias:"+nodename);
					OWLfileNode newNode = new OWLfileNode(nodename,nodetype,"","",node.NodeName);
					expand(newNode, false);//expand allias node 
				}
				else {
					//logger.info(nodename);
					//logger.info("Node Type --> : " + nodetype);
					
					//logger.info("---------------------------------------");
					
					
					if(nodetype.toLowerCase().contains("unit"))
					{
				//		System.out.println("This is a unit --> " + nodename);
 						node.ValueUnit = nodename;
					}
					else
					{
				//	if(nodetype.contains("numerical"))
	
					nodevalue =  childnodes.item(i).getTextContent();
					node.NodeValue = nodevalue;
					//logger.info("Node Value --> : " + nodevalue);

					node.CarryData = true;
					
				
					/**
					if(nodetype.contains("realize"))
					{
		//				System.out.println("Here is a realizes -- > " +  nodename);
						
						getRelationships( nodemap.get(nodename));
						
					}
					**/
					
					
					OWLfileNode newNode = new OWLfileNode(nodename,nodetype,"","",node.NodeName);
				//	System.out.println("Name:  " + newNode.NodeName + " Type: " + newNode.NodeType +
				//			" Value: " + newNode.NodeValue + " Unit: " + newNode.ValueUnit + " Parent: " +  newNode.ParentNodeName ); 
					theNodeList.add(newNode); //  && 
					if(nodemap.get(nodename)!=null   && !(newNode.NodeName.contentEquals(node.ParentNodeName)) &&!expandOneLevel) // check whether such node exists
					{
						if(!(nodetype.contains("topology:enters")||nodetype.contains("topology:leaves")||nodetype.contains("isConnectedTo")))
						{
						expand(newNode,false);
						}
					}
					}
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
 	

	  /***
	   * Return entites read from owl as a list of entityInfo.
	   * using getEntityListFromOWLAsTree funciton.
	   * @return
	   * @throws IOException
	   * @throws Exception
	   */
 	public static List<EntityInfo>  getEntityFromOWL() throws IOException, Exception{
 		if(entityNameList == null){//lazy init
 			if(entityNameTree == null){
 				getEntityListFromOWLAsTree();
 			}
 			
 			entityNameList = entityNameTree.getAllLeafNodeData();
 			
 			
 		} 
 		return entityNameList;
 		
 	}
 	
 	///main for testing getEntityFromOWL();
 	public static void main(String[] args){
 		
 		try {
			List<EntityInfo>  entityList = getEntityFromOWL();
			logger.warn("leaf num:"+entityList.size());
			for(EntityInfo entity:entityList){
				logger.info("leaf node layer++++"+entity.getName()+" from "+entity.getOwlSource());
			}
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
 		
 		
 	}
 	
 	/***
 	 * Read entity hierarchy starting from tope-node owl , returned in tree structure
 	 * Note that currently some layers are not structured in owl, so they are manually added
 	 * @return EntityTree, entity info(name, owlSource) stored in Tree structure 
 	 * @throws IOException
 	 * @throws Exception
 	 */
 	 public static EntityTree getEntityListFromOWLAsTree() throws IOException, Exception{
 		 if(entityNameTree == null){//lazy initiation 
 		 
 		ArrayList<String> secondLevelEntities = new ArrayList<String>();	 
 	 	read_owl_file(TOP_LEVEL_OWL_LOCATION, TOP_LEVEL_OWL_NODE_NAME, true);//read owl of top node to get subsystem list
	 		for(OWLfileNode node :theNodeList){
  	 			if(node.NodeType!= null && node.NodeType.contains("hasSubsystem")){//is this attri shows a subsystem?
  	 				//>YES! Then it is a second level owl node
  	 				secondLevelEntities.add(node.NodeName);
  	 			}  	 			
  	 		}
	 		
	 		//check if indeed has second level entities
	 		if(secondLevelEntities.size() < 1){
	 			logger.error("Reading Second level entity from top level owl:"+TOP_LEVEL_OWL_LOCATION+" Failed with length 0.");
	 		
	 		return null;
	 		}
	 		
	 		//construct name data tree with top node name as root data	
	  	   entityNameTree = new EntityTree(new EntityInfo(TOP_LEVEL_OWL_NODE_NAME,TOP_LEVEL_OWL_LOCATION) );

	  	   ///////extract location of second level entities from top node owl
	  	   TreeNode root =entityNameTree.getRoot();//get root node
	 		for(String entityName : secondLevelEntities){//loop through all subsystem got 
	 	 	 	read_owl_file(TOP_LEVEL_OWL_LOCATION, entityName, false);//read owl location of subsystem
	               String secondLevelOwlLocation = null ;

		 		for(OWLfileNode node :theNodeList){//TODO: should be isLocateAt, but currently owlreader is dismatched by one level
	  	 			if(node.NodeType!= null && node.NodeType.contains("hasConceptualModel")){//is this attri a location?
	  	 				secondLevelOwlLocation = node.NodeValue;
	  	 				logger.info("location of "+entityName+" :" + secondLevelOwlLocation);
	  	 				break;
	  	 			}  	 			
	  	 		}
	 	 	 	
		 		if(secondLevelOwlLocation ==null){//check if location string is not null
	 	 			 logger.info("Fail to read owl locations of second level entity: "+entityName+" in top level owl file:"+TOP_LEVEL_OWL_LOCATION); 		 
	 	 	 		return null;
		 		}

		 		////expand subsystem list on subsystem/////
               TreeNode secondLevelNode =   root.addChild(new EntityInfo(entityName, secondLevelOwlLocation));//construct second level node
         		 expandEntityListOneLevel(secondLevelNode);//expand on second level to get its own subsystem to leaf		 

	 		}
	 		
	        //TODO: Manually construct tree to accomodate some layers not in the structure yet, to be deleted later
	        TreeNode root1 = entityNameTree.getRoot();
	        root1.addChild(new EntityInfo("waterline",""));
	        root1.addChild(new EntityInfo("gasline",""));
	        root1.addChild(new EntityInfo("materialline",""));
	        root1.addChild(new EntityInfo("airline",""));
	        
	        TreeNode subTranNode = entityNameTree.getNodeVName("SubTransmissionLines");//TODO: TESTING!
	        System.out.println(subTranNode.getEntityInfo().getName());
	        subTranNode.addChild(new EntityInfo("EHTLines","owl/updated electrical network.owl" ));
	        subTranNode.addChild(new EntityInfo("HTLines","owl/updated electrical network.owl" ));
	        TreeNode disNode = entityNameTree.getNodeVName("DistributionLines");
	        disNode.addChild(new EntityInfo("TLPlant(22kV-11kV)","owl/updated electrical network.owl" ));
	        disNode.addChild(new EntityInfo("TLPlant(22kV-3.4kV)","owl/updated electrical network.owl" ));
	        disNode.addChild(new EntityInfo("TLPlant(3.4kV-3kV)","owl/updated electrical network.owl" ));
	        disNode.addChild(new EntityInfo("TLPlant(3kV-0.4kV)","owl/updated electrical network.owl" ));
	        disNode.addChild(new EntityInfo("TLPlant(main-22kV)","owl/updated electrical network.owl" ));

	        TreeNode ElectricalGrid = entityNameTree.getNodeVName("ElectricalSystem"); 
	        ElectricalGrid.addChild(new EntityInfo("Bus Coupler","owl/updated electrical network.owl" ));

	        
	        
	        TreeNode Substations = entityNameTree.getNodeVName("Substations"); 
	        Substations.addChild(new EntityInfo("EHT_Stations","owl/updated electrical network.owl" ));
	        Substations.addChild(new EntityInfo("UHT_Stations","owl/updated electrical network.owl" ));
	        Substations.addChild(new EntityInfo("HT_Stations","owl/updated electrical network.owl" ));
	        Substations.addChild(new EntityInfo("LT_Stations","owl/updated electrical network.owl" ));
	        
	        TreeNode transpNode =  entityNameTree.getNodeVName("TransportationSystem");
	        transpNode.deleteAllChildren();
 		 
 		    TreeNode buildingNode =  entityNameTree.getNodeVName("BuildingLayer");
 		    buildingNode.deleteAllChildren();
 		    
 		    TreeNode sNode =  entityNameTree.getNodeVName("StorageTankSystem");
 		    sNode.deleteAllChildren();
 		    TreeNode lNode =  entityNameTree.getNodeVName("LandlotsLayer");
 		    lNode.deleteAllChildren();
 		 } 
 		 logger.info("Now return tree");
 		 return entityNameTree;
 	 }
 	 
 	
/*  PointObjectsGenerator.layer_factory(0,"Load",null,"Load_Point",true); // Load Points
         PointObjectsGenerator.layer_factory(0,"Coupler",null,"Bus_Coupler",false); // Load Points
         PointObjectsGenerator.layer_factory(0,"Transformer","^.*EHT.*$","EHT_Station",true);
         PointObjectsGenerator.layer_factory(0,"Transformer","^.*UHT.*$","UHT_Station_2",true);
         PointObjectsGenerator.layer_factory(0,"Transformer","^.*HT.*$","HT_Station",true);
         PointObjectsGenerator.layer_factory(0,"Transformer","^.*LT.*$","LT_Station",true);             
*/
 	 /***
 	  * 
 	  * @param parentNode
 	  * @throws IOException
 	  * @throws Exception
 	  */
 	 
 	 
 	 private static void   expandEntityListOneLevel(TreeNode parentNode) throws IOException, Exception{

 			EntityInfo parentNodeInfo = parentNode.getEntityInfo();
 			String entityName = parentNodeInfo.getName();
 			String owlLocation = parentNodeInfo.getOwlSource();
  	 		read_owl_file(owlLocation, entityName, true);//read owl of top node to get one level of node lists
  	 		ArrayList<OWLfileNode> tempNodeList = new ArrayList<OWLfileNode>();
  	 		if(theNodeList.size() < 1){
  	 			logger.warn("Read no data for entity:" + entityName);
  	 			return;
  	 		}
  	 		///////////copy the nodelist into templist//////////////////////////////////////////////
  	 		for(OWLfileNode node : theNodeList)
  	 		{
  	 			tempNodeList.add(node);
  	 		}
  	 		
  	 		for(OWLfileNode node :tempNodeList){
  	 			logger.info("Expand Entity:"+node.NodeName+"  "+node.NodeType+"  "+node.NodeValue);
  	 			if(node.NodeType!=null && node.NodeType.contains("hasSubsystem")){//is this attri shows a subsystem?
  	 				//>YES! Then it is a device
  	 				//add it to parent node  	 			
  	 				TreeNode newNode = parentNode.addChild(new EntityInfo(node.NodeName, owlLocation));
  	 				//search entity on child
  	 				expandEntityListOneLevel(newNode);
  	 			}  	 			
  	 		}
 	return;	 
 	 }
 	 
 		
 	 
 	 
 	 
 	/****
 	 * This function is used to expand certain type of equipment in batch. 
 	 * @param filename --> the name of the owl file
 	 * @param type     --> the type of device to be expanded
 	 * @throws Exception 
 	 */
 	 
 	public static ArrayList<Device> BatchOperationForPoint(String filename, String type, Boolean filterOn, String filter) throws Exception
 	{
 	   ArrayList<Device> device_list = new ArrayList<Device>();
 	   File inputFile = new File(filename);
       DocumentBuilderFactory dbFactory  = DocumentBuilderFactory.newInstance();
       DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
       Document doc = dBuilder.parse(inputFile);
       doc.getDocumentElement().normalize();
       Element root = doc.getDocumentElement();
       individuals = root.getElementsByTagName("owl:NamedIndividual");
       for(int i = 0 ; i < individuals.getLength() ; i++)
       {
     	   
    	   if(individuals.item(i).hasChildNodes())
			{
				Node childnode = individuals.item(i).getChildNodes().item(1);
				if(childnode.hasAttributes())
				{
					 String type_name = childnode.getAttributes().item(0).getNodeValue().split("#")[1];
					 if(type_name.contentEquals(type))
					 {
						 String node_name = individuals.item(i).getAttributes().item(0).getNodeValue().split("#")[1];
						 
						
						 if(filterOn)
						 {
							 
							 if( node_name.matches(filter))
							 {
								 logger.info("Node name -->" + node_name);
								 name_list.clear();
								 value_list.clear();
								 read_owl_file(filename,node_name,false);	 
							
								 Device new_device = new Device();
								 new_device.Name_list = name_list.toArray(new String[name_list.size()]);
								 new_device.Value_list = value_list.toArray(new String[value_list.size()]);
								 
								 new_device.Point = new Point(x,y);
								 new_device.Id = node_name;

								 device_list.add(new_device);
	 
							 }
						 }
						 else
						 {
							logger.info("Node name -->" + node_name);
							 name_list.clear();
							 value_list.clear();
							 read_owl_file(filename,node_name,false);	 
						
							 Device new_device = new Device();
							 new_device.Name_list = name_list.toArray(new String[name_list.size()]);
							 new_device.Value_list = value_list.toArray(new String[value_list.size()]);
							 
							 new_device.Point = new Point(x,y);
							 new_device.Id = node_name;

							 device_list.add(new_device);
 
						 }
					   
					 }
 				}
			}
       }
 
       return device_list;
       
 	}
 	  
 	
 	
 	 
 	 
 	 
 	/****
 	 * This function is used to expand certain type of equipment in batch. 
 	 * @param filename --> the name of the owl file
 	 * @param type     --> the type of device to be expanded
 	 * @throws Exception 
 	 */
 
 	  
 	
 	
 	
 	
}
