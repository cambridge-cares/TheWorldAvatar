package arcgistoowl;

import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.esri.core.geodatabase.GeodatabaseFeatureServiceTable;
import com.esri.core.io.EsriSecurityException;
import com.esri.core.io.UserCredentials;
import com.esri.core.map.Feature;
import com.esri.core.map.FeatureResult;
import com.esri.core.map.Graphic;
import com.esri.core.tasks.query.QueryParameters;
import com.esri.core.tasks.query.QueryTask;
import com.esri.map.ArcGISFeatureLayer;
import com.esri.map.GraphicsLayer;

import edu.stanford.smi.protegex.owl.ProtegeOWL;
import edu.stanford.smi.protegex.owl.jena.JenaOWLModel;
import edu.stanford.smi.protegex.owl.model.OWLDatatypeProperty;
import edu.stanford.smi.protegex.owl.model.OWLIndividual;
import edu.stanford.smi.protegex.owl.model.OWLNamedClass;
import edu.stanford.smi.protegex.owl.model.RDFIndividual;

/*import edu.stanford.smi.protegex.owl.model.OWLNamedClass;
import edu.stanford.smi.protegex.owl.model.OWLObjectProperty;
import edu.stanford.smi.protegex.owl.model.RDFIndividual;
import edu.stanford.smi.protegex.owl.model.RDFResource;*/


public class arcgistoowl {
	
//	public static Map<Integer, String> OBJECTIDtoHXB1 = new HashMap<>(); 
	
	public static String a,b,c;
	public static ArcGISFeatureLayer PlantReactorlayer;
	public static String[] layer ={"Landlots","Building","Storage","TLPlantmain","road","Generators","UHTLines",
			"UHT_substations","EHT_Lines","EHT_substation","HTLines","HTSubstation_22to11","HTSubstation_22to3_4",
			"LTSubstation_3_4to3","LTSubstation_3to0_4","Load_points","bus_couplers","Working_Fluid","heater_cooler",
			"Gas_line","airline","EnergyStream","Material_line","water_line","TLPlant2","TLPlant3","TLPlant2a","TLPlant4",
			"Reactor","Decanter","Extractor","Flashdrum","Mixer","RadFrac","exchanger","Pump","blower","valve","splitter",
			"vessel","Filter","expander","compressor","steam_interplants","waterpoint","WaterNetwork","RoadIntersection"};
	public static String[] listfieldname, listfieldtype, container, container2;
	public static ArrayList<Map<String, Object>> attributeslist_reactor = new ArrayList<Map<String, Object>>();
	
	public static void getinput() throws Throwable{
		
		 // additional ArrayList for reactor
		
	UserCredentials user = new UserCredentials();
	user.setUserAccount("kleinelanghorstmj", "h3OBhT0gR4u2k22XZjQltp");

	
	int numberoffeature = 1000; //need to be changed later with the amount of feature with function
	
	String[] f= new String[numberoffeature];
	String[] g= new String[numberoffeature];
	
	GeodatabaseFeatureServiceTable ReactorTable = new GeodatabaseFeatureServiceTable("http://services5.arcgis.com/9i99ftvHsa6nxRGj/arcgis/rest/services/"+layer[28]+"/FeatureServer",user, 0);
	ReactorTable.setFeatureRequestMode(GeodatabaseFeatureServiceTable.FeatureRequestMode.MANUAL_CACHE);
	ReactorTable.initialize();
	System.out.println(ReactorTable.getStatus());
	ReactorTable.getInitializationError();
	
	//long y= ReactorTable.getNumberOfFeatures();
	//int y= ReactorTable.getAttributeValue("OBJECTID").intValue();
	//long y=ReactorTable.getFields();
	//System.out.println("amount of data="+ReactorTable.getNumberOfFeatures());
	
	String field=ReactorTable.getFields().toString();
	String[] fieldname = field.split("name=");
	String[] fieldtype = field.split("type=");
	listfieldname= new String[ReactorTable.getFields().size()];
	listfieldtype= new String[ReactorTable.getFields().size()];
	container = new String[ReactorTable.getFields().size()];
	/*int count = 0;
    while (count >=0 &&  ) {
        System.out.println("Count is: " + count);
        count++;
    }*/
	
	for (int b=1; b<100000; b++) { //numberoffeature+1
		
			QueryParameters qParameter_HX = new QueryParameters();  // create an instance  of QueryParameters to be used  for querying  ArcGIS database for predefined data
 			qParameter_HX.setWhere("OBJECTID='" + b + "'");                            // define FID address of an ArcGIS element
			qParameter_HX.setOutFields(new String[] { "*" });                            // fetch all  attributes of an ArcGIS element using *
			QueryTask qTask_HX = null;                                                   // create an instance of QueryTask to store URL address of appropriate database and user credentials necessary for accessing it
			Feature graphic_HX = null;                                                   // create an instance of Feature to store an ArcGIS element

			qTask_HX = new QueryTask( "http://services5.arcgis.com/9i99ftvHsa6nxRGj/ArcGIS/rest/services/"+layer[28]+"/FeatureServer/0", user); // store URL address of appropriate databaseand user credentials
		
			FeatureResult fResult_HX = qTask_HX.execute(qParameter_HX);   // FeatureResult is used to store information from ArcGIS database requested using qParameter_LP  and qTask_LP
			
			
			if(qTask_HX.execute(qParameter_HX) != null && fResult_HX.featureCount()>0 )
			{
				//System.out.println("---->" + fResult_HX.featureCount() + "counter : " + b);
				graphic_HX = (Feature) fResult_HX.iterator().next();  	
				attributeslist_reactor.add(graphic_HX.getAttributes());  
			}
			else
			{
				break;
			}
		                          // append information about the  element in graphic_LP to ArrayList attributeslist_LP
			
		   
		                      // define FID address of an ArcGIS element

		
		for (int lo=1; lo<=ReactorTable.getFields().size(); lo++)
		{
		String namelist = fieldname[lo];
		String typelist = fieldtype[lo];
		String[] correctlist = namelist.split(",");
		String[] correctlisttype = typelist.split(",");
		String name=correctlist[0];
		String type=correctlisttype[0];
		listfieldname[lo-1]=name;
		listfieldtype[lo-1]=type;
		}


		
 	String r= String.valueOf(attributeslist_reactor.get(b-1).get(listfieldname[2]));//long
 	//System.out.println("size: " + f.length + "  index: " +  (b-1) );
 	f[b-1]=r;
 	String s=String.valueOf(attributeslist_reactor.get(b-1).get(listfieldname[3]));//lat
 	//
 	g[b-1]=s;
 	//System.out.println("output long"+b+"=" +r);
 	//System.out.println("output lat"+b+"=" +s);
 	//System.out.println("element after split="+listfieldname[b-1]);
 	
 	}
	System.out.println("element of field="+ReactorTable.getFields().toString());
	System.out.println("size="+ReactorTable.getFields().size());
	//System.out.println("element after split="+listfieldname[1]); //index here from 0-119
 	
	
	
	
						 a= String.valueOf(attributeslist_reactor.get(0).get(listfieldname[2]));// (0,2)
						 b= String.valueOf(attributeslist_reactor.get(0).get(listfieldname[3]));//(0,3)
						 c= String.valueOf(attributeslist_reactor.get(0).get(listfieldname[0]));
						 System.out.println("output1=" +a);
							System.out.println("output2=" +b);
							System.out.println("output3=" +c);
							//PlantReactorlayer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/ArcGIS/rest/services/Reactor/FeatureServer/0", user);
								
							
							    		
							
	}
	
	 public static String[] RenameDuplicate(String[] array)
	 {
	  //String[] new_array = new String[array.length];
	  
	  int inner_counter = 0;
	  
	  for(String individual: array)
	  {
	    int counter = 1;
	    inner_counter = 0;
	  for(String individual2 : array)
	  {
	   
	    if(individual.equalsIgnoreCase(individual2)&&listfieldtype[inner_counter].equals("esriFieldTypeString"))
	    {
	     array[inner_counter] = individual2 + "__" + counter;
	     	          
	     	     counter++;
	    }
	    inner_counter++;
	  }
	  
	  } 
	  
	  int i = 0;
	  for(String item: array)
	  {
		  if(listfieldtype[i].equals("esriFieldTypeString"))
		  {
	   array[i] = item.substring(0,item.length()-3);
	   
		  }i++;
	  }
	  
	  return array;
	 }	
	
		
	public static void main(String[] args) throws Throwable {
		   // TODO Auto-generated method stub

		
		   //get model from an owl file
		   String filePath = "D://OntoCAPE/testingontology.owl";
		   FileInputStream inFile= new FileInputStream(filePath);
		   Reader in = new InputStreamReader(inFile,"UTF-8");
		   JenaOWLModel jenaOwlModel = ProtegeOWL.createJenaOWLModelFromReader(in);
		   
		   String filePath2 = "D://OntoCAPE/updatedBiodieselPlant.owl";
		   FileInputStream inFile2= new FileInputStream(filePath2);
		   Reader in2 = new InputStreamReader(inFile2,"UTF-8");
		   JenaOWLModel jenaOwlModel2 = ProtegeOWL.createJenaOWLModelFromReader(in2);
		   //get an class from the model
		  /* RDFResource oneClass=jenaOwlModel.getRDFResource("http://www.jparksimulator.com/BiodieselPlant.owl#Material");
		   
		   RDFResource oneDataProperty=jenaOwlModel.getRDFResource("http://www.jparksimulator.com/BiodieselPlant.owl#hasTrayNumber");
		   
		   //delete the resource
		   oneClass.delete();
		   oneDataProperty.delete();
		   
		   //make new class
		   
		   OWLNamedClass personClass = jenaOwlModel.createOWLNamedClass("http://www.jparksimulator.com/BiodieselPlant.owl#person");

		   // Create subclass (complicated version)
		   OWLNamedClass brotherClass = jenaOwlModel.createOWLNamedClass("http://www.jparksimulator.com/BiodieselPlant.owl#brother");
		   brotherClass.addSuperclass(personClass);
		   brotherClass.removeSuperclass(jenaOwlModel.getOWLThingClass());
		   
		   //set data property
		   OWLDatatypeProperty ageProperty = jenaOwlModel.createOWLDatatypeProperty("http://www.jparksimulator.com/BiodieselPlant.owl#hasAge");
		   ageProperty.setRange(jenaOwlModel.getXSDint());
		   ageProperty.setDomain(personClass);

		 //set object property
		   OWLObjectProperty childrenProperty = jenaOwlModel.createOWLObjectProperty("http://www.jparksimulator.com/BiodieselPlant.owl#hasChildren");
		   childrenProperty.setRange(personClass);
		   childrenProperty.setDomain(personClass);
		   
		 //set individual
		   RDFIndividual darwin = personClass.createRDFIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#Darwin");
		   darwin.setPropertyValue(ageProperty, new Integer(0));
		   
		   RDFIndividual holgi = personClass.createRDFIndividual("http://www.jparksimulator.com/BiodieselPlant.owl#Holger");
		   holgi.setPropertyValue(childrenProperty, darwin);
		   holgi.setPropertyValue(ageProperty, new Integer(73));
		   
		   */
		//update the value of an individual
		   
		   getinput();
		   
		   /*OWLIndividual xR101 = jenaOwlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant1WOWHR.owl#ValueOf_x_R-101");
		   OWLIndividual yR101 = jenaOwlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant1WOWHR.owl#ValueOf_y_R-101");
		   OWLDatatypeProperty value = jenaOwlModel.getOWLDatatypeProperty("file:/C:/OntoCAPE/OntoCAPE/upper_level/system.owl#numericalValue");
		   
		   xR101.setPropertyValue(value, new Float(Float.parseFloat(String.valueOf(attributeslist_reactor.get(0).get(listfieldname[2])))));
		   //yR101.setPropertyValue(value, new Float(Float.parseFloat(b)));
		   yR101.setPropertyValue(value, new Float(Float.parseFloat(String.valueOf(attributeslist_reactor.get(0).get(listfieldname[3])))));
		   */
		 
		   OWLNamedClass personClass = jenaOwlModel.createOWLNamedClass("file:///D:/OntoCAPE/updatedBiodieselPlant.owl#test");
			
		  
			
		   for (int y=0 ; y<=listfieldname.length-1 ; y++)
		   {
			   container[y]= String.valueOf(attributeslist_reactor.get(0).get(listfieldname[y]));
			   
			  
				 
				   
				   
				   
		    //RDFIndividual stream2 = personClass.createRDFIndividual("http://www.jparksimulator.com/BiodieselPlant1WOWHR.owl#item_"+y);  
			  //stream2.delete();
			   
			   }
		   
		   container= RenameDuplicate(container);
		   ArrayList<String> container2 = new ArrayList<String>();
		   ArrayList<String> container3 = new ArrayList<String>();
		   int counter = 0 ;
		   for(String item: container)
		   {

			   if(listfieldtype[counter].equals("esriFieldTypeString")&&listfieldname[counter].equals("Long")==false && listfieldname[counter].equals("Lat")==false )
			   {
			//   RDFIndividual stream = personClass.createRDFIndividual("http://www.jparksimulator.com/BiodieselPlant1WOWHR.owl#"+ item);
				   if(item.contains("__"))
				   {
					   container2.add(item);
				   }
				   else
				   {
					   container3.add(item);
				   }  
			   System.out.println("------->"+item);
			   }
			   
			   
			   
			   counter++;
		   
		   }
		    
		  for(String item : container3)
		  {
			  System.out.println("----------->>>>>>" + item);
			  RDFIndividual stream2 = personClass.createRDFIndividual("http://www.jparksimulator.com/BiodieselPlant1WOWHR.owl#"+item); 
			//OWLIndividual deleted = jenaOwlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant1WOWHR.owl#"+ item+"2");
			//deleted.delete(); 
		  }
		   
		   //save the model to another owl file
		  URI file=URI.create("file:///D:/OntoCAPE/updatedBiodieselPlant.owl");
		   jenaOwlModel.save(file);
		 System.out.println(file);
		 // jenaOwlModel2.save(URI.create("file:///D:/OntoCAPE/updatedBiodieselPlant2.owl"));
		  //System.out.println(URI.create("file:///D:/OntoCAPE/updatedBiodieselPlant2.owl"));
		   //System.out.println(oneClass);

		}
	
	public static String[] removeDuplicates(String[] arr) {
	    return (String[]) Arrays.stream(arr)
	            .distinct()
	            .toArray();
	}
	
}
