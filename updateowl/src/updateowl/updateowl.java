package updateowl;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URI;
import java.util.ArrayList;
import java.util.Map;

import com.esri.core.geodatabase.GeodatabaseFeatureServiceTable;
import com.esri.core.io.UserCredentials;
import com.esri.core.map.Feature;
import com.esri.core.map.FeatureResult;
import com.esri.core.tasks.query.QueryParameters;
import com.esri.core.tasks.query.QueryTask;
import com.esri.map.ArcGISFeatureLayer;

//import org.semanticweb.owl.model.OWLObjectProperty;


import edu.stanford.smi.protegex.owl.ProtegeOWL;
import edu.stanford.smi.protegex.owl.jena.JenaOWLModel;
import edu.stanford.smi.protegex.owl.model.OWLDatatypeProperty;
import edu.stanford.smi.protegex.owl.model.OWLIndividual;
import edu.stanford.smi.protegex.owl.model.OWLObjectProperty;
import edu.stanford.smi.protegex.owl.model.OWLNamedClass;
import edu.stanford.smi.protegex.owl.model.RDFIndividual;
import edu.stanford.smi.protegex.owl.model.RDFProperty;
import edu.stanford.smi.protegex.owl.model.RDFResource;
import edu.stanford.smi.protegex.owl.model.RDFSNamedClass;

public class updateowl {
	
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
							
							PlantReactorlayer = new ArcGISFeatureLayer("http://services5.arcgis.com/9i99ftvHsa6nxRGj/ArcGIS/rest/services/Reactor/FeatureServer/0", user);
								System.out.println("checking= "+PlantReactorlayer.isAllowGeometryUpdates());
							
							    		
							
	}



public static void main(String[] args) throws Exception {
	
	
   // TODO Auto-generated method stub

   //get model from an owl file
   String filePath = "D:\\OntoCAPE/BiodieselPlant1WOWHR.owl";
   FileInputStream inFile= new FileInputStream(filePath);
   Reader in = new InputStreamReader(inFile,"UTF-8");
   JenaOWLModel jenaOwlModel = ProtegeOWL.createJenaOWLModelFromReader(in);
   
   //get an class from the model
   OWLNamedClass processstreamClass = jenaOwlModel.getOWLNamedClass("file:/C:/OntoCAPE/OntoCAPE/chemical_process_system/CPS_function/process.owl#ProcessStream");
   OWLNamedClass rawmaterialClass = jenaOwlModel.getOWLNamedClass("file:/C:/OntoCAPE/OntoCAPE/chemical_process_system/CPS_function/process.owl#RawMaterial");
   OWLNamedClass coreproductClass = jenaOwlModel.getOWLNamedClass("file:/C:/OntoCAPE/OntoCAPE/chemical_process_system/CPS_function/process.owl#CoreProduct");
   OWLNamedClass splittingClass = jenaOwlModel.getOWLNamedClass("file:/C:/OntoCAPE/OntoCAPE/chemical_process_system/CPS_function/process.owl#Splitting");
   OWLNamedClass pressurechangeClass = jenaOwlModel.getOWLNamedClass("file:/C:/OntoCAPE/OntoCAPE/chemical_process_system/CPS_function/process.owl#PressureChangeOfGas");
   OWLNamedClass generalClass = jenaOwlModel.getOWLNamedClass("file:/C:/OntoCAPE/OntoCAPE/chemical_process_system/CPS_behavior/behavior.owl#GeneralizedAmount");
   OWLNamedClass continuousmaterialamountClass = jenaOwlModel.getOWLNamedClass("file:/C:/OntoCAPE/OntoCAPE/chemical_process_system/CPS_behavior/behavior.owl#ContinuousMaterialAmount");
   OWLNamedClass convectivemassflowrateClass = jenaOwlModel.getOWLNamedClass("file:/C:/OntoCAPE/OntoCAPE/chemical_process_system/CPS_behavior/behavior.owl#ConvectiveMassFlowrate");
   OWLNamedClass machineClass = jenaOwlModel.getOWLNamedClass("file:/C:/OntoCAPE/OntoCAPE/chemical_process_system/CPS_realization/plant.owl#Machine");
   OWLNamedClass apparatusClass = jenaOwlModel.getOWLNamedClass("file:/C:/OntoCAPE/OntoCAPE/chemical_process_system/CPS_realization/plant.owl#Apparatus");
   
   
   OWLNamedClass molecularweightClass = jenaOwlModel.getOWLNamedClass("file:/C:/OntoCAPE/OntoCAPE/material/substance/substance.owl#MolecularWeight");
   OWLNamedClass molecularentityClass = jenaOwlModel.getOWLNamedClass("file:/C:/OntoCAPE/OntoCAPE/material/substance/substance.owl#MolecularEntity");
   OWLNamedClass chemicalspeciesClass = jenaOwlModel.getOWLNamedClass("file:/C:/OntoCAPE/OntoCAPE/material/substance/substance.owl#ChemicalSpecies");
   OWLNamedClass mixtureClass = jenaOwlModel.getOWLNamedClass("file:/C:/OntoCAPE/OntoCAPE/material/substance/substance.owl#Mixture");
   OWLNamedClass materialClass = jenaOwlModel.getOWLNamedClass("file:/C:/OntoCAPE/OntoCAPE/material/material.owl#Material");
   OWLNamedClass singlephaseClass = jenaOwlModel.getOWLNamedClass("file:/C:/OntoCAPE/OntoCAPE/material/phase_system/phase_system.owl#SinglePhase");
   OWLNamedClass temperatureClass = jenaOwlModel.getOWLNamedClass("file:/C:/OntoCAPE/OntoCAPE/material/phase_system/phase_system.owl#Temperature");
   OWLNamedClass compositionClass = jenaOwlModel.getOWLNamedClass("file:/C:/OntoCAPE/OntoCAPE/material/phase_system/phase_system.owl#Composition");
   OWLNamedClass molefractionClass = jenaOwlModel.getOWLNamedClass("file:/C:/OntoCAPE/OntoCAPE/material/phase_system/phase_system.owl#MoleFraction");
   OWLNamedClass pressureClass = jenaOwlModel.getOWLNamedClass("file:/C:/OntoCAPE/OntoCAPE/material/phase_system/phase_system.owl#Pressure");
   OWLNamedClass coordinatevalueClass = jenaOwlModel.getOWLNamedClass("file:/C:/OntoCAPE/OntoCAPE/upper_level/coordinate_system.owl#CoordinateValue");
   OWLNamedClass scalarvalueClass = jenaOwlModel.getOWLNamedClass("file:/C:/OntoCAPE/OntoCAPE/upper_level/system.owl#ScalarValue");
   OWLNamedClass fixedvaluesetClass = jenaOwlModel.getOWLNamedClass("file:/C:/OntoCAPE/OntoCAPE/upper_level/system.owl#FixedValueSet");
   
   OWLNamedClass siderivedunitClass = jenaOwlModel.getOWLNamedClass("file:/C:/OntoCAPE/OntoCAPE/supporting_concepts/SI_unit/SI_unit.owl#SI_DerivedUnit"); //needed if there is new units which are not exist in ontocape
   OWLNamedClass straightcoordinateClass = jenaOwlModel.getOWLNamedClass("file:/C:/OntoCAPE/OntoCAPE/supporting_concepts/space_and_time/space_and_time.owl#StraightCoordinate");
   OWLNamedClass giscoordinatesysClass = jenaOwlModel.getOWLNamedClass("file:/C:/OntoCAPE/OntoCAPE/supporting_concepts/space_and_time/space_and_time_extended.owl#GISCoordinateSystem");
   
   
   
   
   
   OWLObjectProperty realizes = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/upper_level/technical_system.owl#realizes");
   
   OWLObjectProperty referstoaxis = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/upper_level/coordinate_system.owl#refersToAxis");
   
   OWLObjectProperty hassubsystem = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/upper_level/system.owl#hasSubsystem");
   OWLObjectProperty hasproperty = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/upper_level/system.owl#hasProperty");
   OWLObjectProperty hasvalue = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/upper_level/system.owl#hasValue");
   OWLObjectProperty hasunit = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/upper_level/system.owl#hasUnitOfMeasure");
   OWLObjectProperty containsdirectly = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/upper_level/system.owl#containsDirectly");
   OWLObjectProperty comprisesdirectly = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/upper_level/system.owl#comprisesDirectly");
   OWLObjectProperty isrelatedto = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/upper_level/system.owl#isRelatedTo");
   OWLObjectProperty hasDimension = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/upper_level/system.owl#hasDimension");
   OWLObjectProperty hasCharacteristic = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/upper_level/system.owl#hasCharacteristic");
   
   OWLDatatypeProperty numericalvalue = jenaOwlModel.getOWLDatatypeProperty("file:/C:/OntoCAPE/OntoCAPE/upper_level/system.owl#numericalValue");
   OWLDatatypeProperty hasreference = jenaOwlModel.createOWLDatatypeProperty("http://www.jparksimulator.com/ChemicalPlants.owl#hasReference");
   hasreference.setRange(jenaOwlModel.getXSDstring());
   OWLDatatypeProperty CAS = jenaOwlModel.getOWLDatatypeProperty("file:/C:/OntoCAPE/OntoCAPE/material/substance/substance.owl#CAS_RegistryNumber");
   OWLDatatypeProperty molecularformula = jenaOwlModel.getOWLDatatypeProperty("file:/C:/OntoCAPE/OntoCAPE/material/substance/substance.owl#molecularFormula");
   
   OWLObjectProperty hasmolecularstructure = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/material/substance/substance.owl#hasMolecularStructure");
   OWLObjectProperty hasmacroscopicappearance = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/material/substance/substance.owl#hasMacroscopicAppearance");
   
   OWLObjectProperty referstomaterial = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/chemical_process_system/CPS_behavior/behavior.owl#refersToMaterial");
   OWLObjectProperty referstogeneralizedamount = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/chemical_process_system/chemical_process_system.owl#refersToGeneralizedAmount");
   OWLObjectProperty hasoutletpress = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/chemical_process_system/CPS_realization/plant_equipment/machine.owl#hasOutletPressure");
   
   OWLObjectProperty intrinsiccharacteristics = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/material/material.owl#intrinsicCharacteristics");
   OWLObjectProperty thermodynamicbehavior = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/material/material.owl#thermodynamicBehavior");
   
   OWLObjectProperty hascomposition = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/material/phase_system/phase_system.owl#has_composition");
   OWLObjectProperty hastemperature = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/material/phase_system/phase_system.owl#has_temperature");
   OWLObjectProperty haspressure = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/material/phase_system/phase_system.owl#has_pressure");
      
   OWLObjectProperty hasprojectedcoordinatex = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/supporting_concepts/space_and_time/space_and_time_extended.owl#hasProjectedCoordinate_x");
   OWLObjectProperty hasprojectedcoordinatey = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/supporting_concepts/space_and_time/space_and_time_extended.owl#hasProjectedCoordinate_y");
   
   OWLObjectProperty hasinput = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/meta_model/topology/topology.owl#hasInput");
   OWLObjectProperty hasoutput = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/meta_model/topology/topology.owl#hasOutput");
   
   
   
   
      
   
   
   /*
   RDFResource oneClass=jenaOwlModel.getRDFResource("file:/C:/OntoCAPE/OntoCAPE/material/material.owl#Material");
   RDFResource generalizedamountClass=jenaOwlModel.getRDFResource("file:/C:/OntoCAPE/OntoCAPE/chemical_process_system/CPS_behavior/behavior.owl#GeneralizedAmount");
   
   RDFResource oneDataProperty=jenaOwlModel.getRDFResource("http://www.jparksimulator.com/BiodieselPlant1WOWHR.owl#hasTrayNumber");
   RDFResource hasSubsystem=jenaOwlModel.getRDFResource("file:/C:/OntoCAPE/OntoCAPE/upper_level/system.owl#hasSubsystem");
   
   RDFResource individu=jenaOwlModel.getRDFResource("http://www.jparksimulator.com/BiodieselPlant1WOWHR.owl#carbon_dioxide");
   
   //delete the resource
  // oneClass.delete();
   //oneDataProperty.delete();
   //individu.delete();
   
   //make new class
   
   OWLNamedClass personClass = jenaOwlModel.createOWLNamedClass("http://www.jparksimulator.com/BiodieselPlant1WOWHR.owl#person");
   

   // Create subclass (complicated version)
   OWLNamedClass brotherClass = jenaOwlModel.createOWLNamedClass("http://www.jparksimulator.com/BiodieselPlant1WOWHR.owl#brother");
   brotherClass.addSuperclass(personClass);
   brotherClass.removeSuperclass(jenaOwlModel.getOWLThingClass());
   
   //set data property
   OWLDatatypeProperty ageProperty = jenaOwlModel.createOWLDatatypeProperty("http://www.jparksimulator.com/BiodieselPlant1WOWHR.owl#hasAge");
   ageProperty.setRange(jenaOwlModel.getXSDint());
   ageProperty.setDomain(personClass);

 //set object property
   OWLObjectProperty childrenProperty = jenaOwlModel.createOWLObjectProperty("http://www.jparksimulator.com/BiodieselPlant1WOWHR.owl#hasChildren");
   childrenProperty.setRange(personClass);
   childrenProperty.setDomain(personClass);
   
   
   
 //set individual
   RDFIndividual darwin = ((RDFSNamedClass) oneClass).createRDFIndividual("http://www.jparksimulator.com/BiodieselPlant1WOWHR.owl#Darwin");
   darwin.setPropertyValue(ageProperty, new Integer(0));
   
   RDFIndividual holgi = personClass.createRDFIndividual("http://www.jparksimulator.com/BiodieselPlant1WOWHR.owl#Holger");
   holgi.setPropertyValue(childrenProperty, darwin);
   holgi.setPropertyValue(ageProperty, new Integer(73));
   
  */
   
   //mixture1= STY, cyclohex (10,13)
   //mixture2= STY, cyclohex,SBR (11,7,8,9)
   //mixture3 = STY,BD,Buli6, cyclohex (4,5,6)
   
   
   RDFIndividual mixture1 = mixtureClass.createRDFIndividual("http://www.jparksimulator.com/BiodieselPlant1WOWHR.owl#mixture_1");
   RDFIndividual mixture2 = mixtureClass.createRDFIndividual("http://www.jparksimulator.com/BiodieselPlant1WOWHR.owl#mixture_2");
   RDFIndividual mixture3 = mixtureClass.createRDFIndividual("http://www.jparksimulator.com/BiodieselPlant1WOWHR.owl#mixture_3");
   
      
   RDFIndividual unitskmolhr = jenaOwlModel.getOWLIndividual("file:/C:/OntoCAPE/OntoCAPE/supporting_concepts/SI_unit/derived_SI_units.owl#kmol_per_hr");
   RDFIndividual unitsC = jenaOwlModel.getOWLIndividual("file:/C:/OntoCAPE/OntoCAPE/supporting_concepts/SI_unit/derived_SI_units.owl#Celsius");
   RDFIndividual unitsbar = jenaOwlModel.getOWLIndividual("file:/C:/OntoCAPE/OntoCAPE/supporting_concepts/SI_unit/derived_SI_units.owl#bar");
  
   RDFIndividual bd = chemicalspeciesClass.createRDFIndividual("http://www.jparksimulator.com/BiodieselPlant1WOWHR.owl#butadiene");
   RDFIndividual sty = chemicalspeciesClass.createRDFIndividual("http://www.jparksimulator.com/BiodieselPlant1WOWHR.owl#styrene");
   RDFIndividual buli6 = chemicalspeciesClass.createRDFIndividual("http://www.jparksimulator.com/BiodieselPlant1WOWHR.owl#diisooctyl_phthalate");
   RDFIndividual cyclohex = chemicalspeciesClass.createRDFIndividual("http://www.jparksimulator.com/BiodieselPlant1WOWHR.owl#cyclohexene");
   RDFIndividual bdstructure = molecularentityClass.createRDFIndividual("http://www.jparksimulator.com/BiodieselPlant1WOWHR.owl#butadiene");
   RDFIndividual stystructure = molecularentityClass.createRDFIndividual("http://www.jparksimulator.com/BiodieselPlant1WOWHR.owl#styrene");
   RDFIndividual buli6structure = molecularentityClass.createRDFIndividual("http://www.jparksimulator.com/BiodieselPlant1WOWHR.owl#diisooctyl_phthalate");
   RDFIndividual cyclohexstructure = molecularentityClass.createRDFIndividual("http://www.jparksimulator.com/BiodieselPlant1WOWHR.owl#cyclohexene");
   
  for (int x=0 ; x<13;x++)
  {
	 int y= x+1; 
	 
	 RDFIndividual streamtipe = null;
	 
	 if(y==1||y==2||y==3||y==12)
	 {	 
	 RDFIndividual streamtype = rawmaterialClass.createRDFIndividual("http://www.jparksimulator.com/BiodieselPlant1WOWHR.owl#RawMaterial_4-"+y); 
	 streamtipe = streamtype;
	 }
	 else if(y==11)
	 {RDFIndividual streamtype = coreproductClass.createRDFIndividual("http://www.jparksimulator.com/BiodieselPlant1WOWHR.owl#CoreProduct_4-"+y); 
	 streamtipe = streamtype;
	 }
	 else
	 {
		 RDFIndividual streamtype = processstreamClass.createRDFIndividual("http://www.jparksimulator.com/BiodieselPlant1WOWHR.owl#ProcessStream_4-"+y); 
		 streamtipe = streamtype;
	 }
	 
	  
   RDFIndividual stream = generalClass.createRDFIndividual("http://www.jparksimulator.com/BiodieselPlant1WOWHR.owl#GeneralizedAmount_4-"+y);
   RDFIndividual materialamount = continuousmaterialamountClass.createRDFIndividual("http://www.jparksimulator.com/BiodieselPlant1WOWHR.owl#MaterialAmount_4-"+y);
   RDFIndividual material = materialClass.createRDFIndividual("http://www.jparksimulator.com/BiodieselPlant1WOWHR.owl#Material_4-"+y);
   RDFIndividual name = singlephaseClass.createRDFIndividual("http://www.jparksimulator.com/BiodieselPlant1WOWHR.owl#4-"+y);
   
   RDFIndividual temp = temperatureClass.createRDFIndividual("http://www.jparksimulator.com/BiodieselPlant1WOWHR.owl#temp_4-"+y);
   RDFIndividual tempvalue = scalarvalueClass.createRDFIndividual("http://www.jparksimulator.com/BiodieselPlant1WOWHR.owl#ValueOf_temp_4-"+y);
   
   RDFIndividual press = pressureClass.createRDFIndividual("http://www.jparksimulator.com/BiodieselPlant1WOWHR.owl#pres_4-"+y);
   RDFIndividual pressvalue = scalarvalueClass.createRDFIndividual("http://www.jparksimulator.com/BiodieselPlant1WOWHR.owl#ValueOf_pres_4-"+y);
   
   
   RDFIndividual flow = convectivemassflowrateClass.createRDFIndividual("http://www.jparksimulator.com/BiodieselPlant1WOWHR.owl#4-"+y+"_MolarFlowRate");
   RDFIndividual flowvalue = scalarvalueClass.createRDFIndividual("http://www.jparksimulator.com/BiodieselPlant1WOWHR.owl#ValueOf_4-"+y+"_MolarFlowRate");
   
   
   streamtipe.setPropertyValue(referstogeneralizedamount, stream);
   stream.setPropertyValue(hassubsystem, materialamount);
   materialamount.setPropertyValue(hasproperty, flow);
   materialamount.setPropertyValue(referstomaterial, material);
   
   if (y==10||y==13)
   {
   material.setPropertyValue(intrinsiccharacteristics, mixture1);
   }
   else if(y==4||y==5||y==6)
   {
	   material.setPropertyValue(intrinsiccharacteristics, mixture3);
      }
   else if(y==7||y==8||y==9||y==11)
   {
	   material.setPropertyValue(intrinsiccharacteristics, mixture2);
      }
   else if(y==1)
   {
	   material.setPropertyValue(intrinsiccharacteristics, bd);
      }
   else if(y==2)
   {
	   material.setPropertyValue(intrinsiccharacteristics, sty);
      }
   else if(y==3)
   {
	   material.setPropertyValue(intrinsiccharacteristics, buli6);
      }
   else 
   {
	   material.setPropertyValue(intrinsiccharacteristics, cyclohex);
      }
   
   
   material.setPropertyValue(thermodynamicbehavior, name);
   name.setPropertyValue(hastemperature, temp);
   name.setPropertyValue(haspressure, press);
   
   
  
   flow.setPropertyValue(hasvalue, flowvalue);
   flowvalue.setPropertyValue(hasunit, unitskmolhr);
   flowvalue.setPropertyValue(numericalvalue, new Float(Float.parseFloat(a[x])));//change value to variable
   //flowvalue.setPropertyValue(numericalvalue, new Float(30.4));//change value to variable
   
   temp.setPropertyValue(hasvalue, tempvalue);
   tempvalue.setPropertyValue(hasunit, unitsC);
   tempvalue.setPropertyValue(numericalvalue, new Float(Float.parseFloat(b[x])));//change value to variable
   
   press.setPropertyValue(hasvalue, pressvalue);
   pressvalue.setPropertyValue(hasunit, unitsbar);
  pressvalue.setPropertyValue(numericalvalue, new Float(Float.parseFloat(c[x])));//change value to variable
   
  }
   
//update the value of an individual
   
   /*OWLIndividual pump = jenaOwlModel.getOWLIndividual("http://www.jparksimulator.com/BiodieselPlant1WOWHR.owl#P-101");
   OWLDatatypeProperty hasName = jenaOwlModel.getOWLDatatypeProperty("file:/C:/OntoCAPE/OntoCAPE/upper_level/system.owl#hasPumpHead");
   pump.setPropertyValue(hasName, new String("10P05normal"));*/
   
   //save the model to another owl file
   URI file=URI.create("file:///D:/OntoCAPE/BiodieselPlantchange2.owl");
   
   jenaOwlModel.save(file);
   System.out.println(file);
   //System.out.println(oneClass);

}

}