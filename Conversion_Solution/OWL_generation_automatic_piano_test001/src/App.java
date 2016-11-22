import java.awt.List;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.apache.commons.lang.ArrayUtils;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import edu.stanford.smi.protege.exception.OntologyLoadException;
import edu.stanford.smi.protegex.owl.ProtegeOWL;
import edu.stanford.smi.protegex.owl.jena.JenaOWLModel;
import edu.stanford.smi.protegex.owl.model.OWLDatatypeProperty;
import edu.stanford.smi.protegex.owl.model.OWLNamedClass;
import edu.stanford.smi.protegex.owl.model.OWLObjectProperty;
import edu.stanford.smi.protegex.owl.model.RDFIndividual;
import edu.stanford.smi.protegex.owl.model.RDFResource;

public class App {
	public static ArrayList<Object> individual_list = new ArrayList<Object>();
	public static ArrayList<Object> class_list = new ArrayList<Object>();
	public static Node previous_node;
	public static JenaOWLModel jenaOwlModel;
	public static RDFIndividual previous_individual = null;
	public static Map<String, Object> map = new HashMap<String, Object>();
	
	
	public static void main(String[] args) throws Exception {
		// TODO Auto-generated method stub
		
		
		 URI file=URI.create("file:///C:/OntoCAPE/generated.owl");
		
		File inputFile = new File("Script.xml");
        DocumentBuilderFactory dbFactory  = DocumentBuilderFactory.newInstance();
        DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
        Document doc = dBuilder.parse(inputFile);
        doc.getDocumentElement().normalize();
        
        Node root = doc.getDocumentElement();
        previous_node = root;
        Node next_node = null;
        
        
        jenaOwlModel =  initiate_owl();
        
        recursion(root,next_node);
       	jenaOwlModel.save(file);
        	    	
	}
	
	
	public static Object getRelationship(int index,JenaOWLModel jenaOwlModel ) throws IOException, OntologyLoadException
	{
		 
		
		   OWLDatatypeProperty numericalvalue = jenaOwlModel.getOWLDatatypeProperty("file:/C:/OntoCAPE/OntoCAPE/upper_level/system.owl#numericalValue");
		// 0
		   OWLObjectProperty hasunit = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/upper_level/system.owl#hasUnitOfMeasure");
		// 1 
		   OWLObjectProperty hasvalue = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/upper_level/system.owl#hasValue");
		// 2
		   OWLObjectProperty referstoaxis = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/upper_level/coordinate_system.owl#refersToAxis"); 
		// 3
		   OWLObjectProperty hasDimension = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/upper_level/system.owl#hasDimension");
		// 4
		   OWLObjectProperty hasproperty = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/upper_level/system.owl#hasProperty");
		// 5
		   OWLObjectProperty realizes = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/upper_level/technical_system.owl#realizes");   
		// 6
		   OWLObjectProperty hasoutletpress = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/chemical_process_system/CPS_realization/plant_equipment/machine.owl#hasOutletPressure");
		// 7
		   OWLObjectProperty hasinput = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/meta_model/topology/topology.owl#hasInput");
		// 8
		   OWLObjectProperty hasoutput = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/meta_model/topology/topology.owl#hasOutput");
		// 9
		   OWLObjectProperty referstogeneralizedamount = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/chemical_process_system/chemical_process_system.owl#refersToGeneralizedAmount");
		// 10
		   OWLObjectProperty hassubsystem = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/upper_level/system.owl#hasSubsystem");
		// 11
		   OWLObjectProperty referstomaterial = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/chemical_process_system/CPS_behavior/behavior.owl#refersToMaterial");
		// 12
		   OWLObjectProperty thermodynamicbehavior = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/material/material.owl#thermodynamicBehavior");
		// 13
		   OWLObjectProperty hascomposition = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/material/phase_system/phase_system.owl#has_composition");
		// 14
		   OWLObjectProperty haspressure = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/material/phase_system/phase_system.owl#has_pressure");
		// 15
		   OWLObjectProperty hastemperature = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/material/phase_system/phase_system.owl#has_temperature");
		// 16
		   OWLObjectProperty comprisesdirectly = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/upper_level/system.owl#comprisesDirectly");
		// 17
		   OWLObjectProperty intrinsiccharacteristics = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/material/material.owl#intrinsicCharacteristics");
		// 18
		   OWLObjectProperty containsdirectly = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/upper_level/system.owl#containsDirectly");
		// 19
		   OWLObjectProperty hasmacroscopicappearance = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/material/substance/substance.owl#hasMacroscopicAppearance");
		// 20
		   OWLDatatypeProperty molecularformula = jenaOwlModel.getOWLDatatypeProperty("file:/C:/OntoCAPE/OntoCAPE/material/substance/substance.owl#molecularFormula");
		// 21
		   OWLDatatypeProperty CAS = jenaOwlModel.getOWLDatatypeProperty("file:/C:/OntoCAPE/OntoCAPE/material/substance/substance.owl#CAS_RegistryNumber");
		// 22
		   OWLObjectProperty hasmolecularstructure = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/material/substance/substance.owl#hasMolecularStructure");
		// 23
		   OWLObjectProperty hasCharacteristic = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/upper_level/system.owl#hasCharacteristic");
		// 24
		   OWLObjectProperty hasprojectedcoordinatex = jenaOwlModel.getOWLObjectProperty("file:/C/OntoCAPE/OntoCAPE/supporting_concepts/space_and_time/space_and_time_extended.owl#hasProjectedCoordinate_x");
		// 25
		   OWLObjectProperty hasprojectedcoordinatey = jenaOwlModel.getOWLObjectProperty("file:/C/OntoCAPE/OntoCAPE/supporting_concepts/space_and_time/space_and_time_extended.owl#hasProjectedCoordinate_y");
		// 26
		//   OWLDatatypeProperty hasreference = jenaOwlModel.createOWLDatatypeProperty("http://www.jparksimulator.com/ChemicalPlants.owl#hasReference");
		//  hasreference.setRange(jenaOwlModel.getXSDstring());
		// 27
		   OWLObjectProperty isrelatedto = jenaOwlModel.getOWLObjectProperty("file:/C:/OntoCAPE/OntoCAPE/upper_level/system.owl#isRelatedTo");
		// 28

		   individual_list.add(numericalvalue);
		   individual_list.add(hasunit);
		   individual_list.add(hasvalue);
		   individual_list.add(referstoaxis);
		   individual_list.add(hasDimension);
		   individual_list.add(hasproperty);
		   individual_list.add(realizes);
		   individual_list.add(hasoutletpress);
		   individual_list.add(hasinput);
		   individual_list.add(hasoutput);
		   individual_list.add(referstogeneralizedamount);
		   individual_list.add(hassubsystem);
		   individual_list.add(referstomaterial);
		   individual_list.add(thermodynamicbehavior);
		   individual_list.add(hascomposition);
		   individual_list.add(haspressure);
		   individual_list.add(hastemperature);
		   individual_list.add(comprisesdirectly);
		   individual_list.add(intrinsiccharacteristics);
		   individual_list.add(containsdirectly);
		   individual_list.add(hasmacroscopicappearance);
		   individual_list.add(molecularformula);
		   individual_list.add(CAS);
		   individual_list.add(hasmolecularstructure);
		   individual_list.add(hasCharacteristic);
		   individual_list.add(hasprojectedcoordinatex);
		   individual_list.add(hasprojectedcoordinatey);
		//   individual_list.add(hasreference);
		   individual_list.add(isrelatedto);
		   
		   return individual_list.get(index);
	}
	
	
	public static Object getClass(int index, JenaOWLModel jenaOwlModel) throws IOException, OntologyLoadException
	{		   
		
		 
 		   // Apparatus 
		   
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
		   OWLNamedClass siderivedunitClass = jenaOwlModel.getOWLNamedClass("file:/C/OntoCAPE/OntoCAPE/supporting_concepts/SI_unit/SI_unit.owl#SI_DerivedUnit"); //needed if there is new units which are not exist in ontocape
		   OWLNamedClass straightcoordinateClass = jenaOwlModel.getOWLNamedClass("file:/C/OntoCAPE/OntoCAPE/supporting_concepts/space_and_time/space_and_time.owl#StraightCoordinate");
		   OWLNamedClass giscoordinatesysClass = jenaOwlModel.getOWLNamedClass("file:/C/OntoCAPE/OntoCAPE/supporting_concepts/space_and_time/space_and_time_extended.owl#CartesianCoordinateSystem");
	
		  
		   class_list.add(processstreamClass);
		   class_list.add(rawmaterialClass);
		   class_list.add(coreproductClass);
		   class_list.add(splittingClass);
		   class_list.add(pressurechangeClass);
		   class_list.add(generalClass);
		   class_list.add(continuousmaterialamountClass);
		   class_list.add(convectivemassflowrateClass);
		   class_list.add(machineClass);
		   class_list.add(apparatusClass);
		   class_list.add(molecularweightClass);
		   class_list.add(molecularentityClass);
		   class_list.add(chemicalspeciesClass);
		   class_list.add(mixtureClass);
		   class_list.add(materialClass);
		   class_list.add(singlephaseClass);
		   class_list.add(temperatureClass);
		   class_list.add(compositionClass);
		   class_list.add(molefractionClass);
		   class_list.add(pressureClass);
		   class_list.add(coordinatevalueClass);
		   class_list.add(scalarvalueClass);
		   class_list.add(fixedvaluesetClass);
		   class_list.add(siderivedunitClass);
		   class_list.add(straightcoordinateClass);
		   class_list.add(giscoordinatesysClass);
		 
		   
		   return class_list.get(index);

		
	}
	
	
	public static JenaOWLModel initiate_owl() throws IOException, OntologyLoadException
	{
		
		   String filePath = "C:\\OntoCAPE/test.owl";
		   FileInputStream inFile= new FileInputStream(filePath);
		   Reader in = new InputStreamReader(inFile,"UTF-8");
		   JenaOWLModel jenaOwlModel = ProtegeOWL.createJenaOWLModelFromReader(in);
		 
		   return jenaOwlModel;
		
	}
	
	
	public static ArrayList<Node> get_child_nodes(Node node)
	{
		ArrayList<Node> list = new ArrayList<Node>();
		
		for(int i = 0; i < node.getChildNodes().getLength();i++)
		{
			Node temp = node.getChildNodes().item(i);
			if(temp.getNodeType() == Node.ELEMENT_NODE)
			{
				list.add(temp);
			}
		}
		 
		return list;
	}

	public static void recursion(Node previous_node , Node next_node) throws IOException, OntologyLoadException
	{
		 
        for(int i = 0; i < get_child_nodes(previous_node).size(); i++)
        {
        	next_node = get_child_nodes(previous_node).get(i);
        	System.out.println(previous_node.getNodeName() + "--------->" + next_node.getNodeName());
        	System.out.println("Previous_node : " + get_all_attributes(previous_node));
        	System.out.println();
        	System.out.println("Next_node :     " + get_all_attributes(next_node)  );
         	
         	CreateOWL(jenaOwlModel, get_all_attributes(previous_node),get_all_attributes(next_node));
         	// MARK THIS IS WHERE YOU PUT THE CODE TO CREATE THE OWL FILE 
        	recursion(next_node,null);
        }
		
        
      
	}

	public static ArrayList<String> get_all_attributes(Node node)
	{
		ArrayList<String> result = new ArrayList<String>();
		NamedNodeMap map = node.getAttributes();
			for(int j = 0; j<map.getLength();j++)
			{
			 
				String name = map.item(j).getNodeName();
				String value = map.item(j).getNodeValue();
				if(name!=null)
				{
					 result.add(name+":"+value);
					
				}
				
			}
		return result;
	}
	
	public static void CreateOWL(JenaOWLModel jenaOwlModel,ArrayList<String> previous,ArrayList<String> next ) throws IOException, OntologyLoadException
	{
		System.out.println("This size of the previous is -- " + previous.size());
		System.out.println(previous.size() != 0);


     	
     	
     	//==========================================================================================================
     	String temp = next.get(1);
		String individual_name = temp.substring(temp.indexOf(':') + 1);
		temp = next.get(0);
		System.out.println(temp);
		String index =  temp.substring(temp.indexOf(':') + 1);
		int class_index = Integer.parseInt(index);
	 	OWLNamedClass temp_class_1 = (OWLNamedClass) getClass(class_index,jenaOwlModel); 
	 	RDFIndividual individual_1 = temp_class_1.createRDFIndividual("http://www.jparksimulator.com/generated.owl#" + individual_name);
	 	
	 	
	 	
     	//==========================================================================================================
	 
	 	map.put(individual_name, individual_1);
	 
	 	
 
     	if(previous.size()!=0)
     	{
     		 
     			System.out.println("Relationship -- " + next.get(2).substring(next.get(2).indexOf(':')+ 1));
     			String index_string = next.get(2).substring(next.get(2).indexOf(':')+ 1);
     			int property_index = Integer.parseInt(index_string);
     			
     			
     			 
     			String previous_name= previous.get(1);
     			previous_name = previous_name.substring(previous_name.indexOf(':')+1);
     			
     			System.out.println("------->>>" + previous_name);
     			//previous_individual =  (RDFIndividual) map.get(previous_name);
     		 	System.out.println(((RDFIndividual) map.get(previous_name)).getName() + "---" + individual_1.getName() + "---" + property_index);
     			System.out.println( (OWLObjectProperty) getRelationship(property_index,jenaOwlModel));
     		 	((RDFResource) map.get(previous_name)).
     		 	addPropertyValue((OWLObjectProperty) getRelationship(property_index,jenaOwlModel), individual_1);
     		 
     		 	 
     	}
     	
     	
     	
     	System.out.println("----------------------------------------------------------------------------------------");
     	System.out.println("");
     	
     	
     	
     	
		
	}
	
	public static boolean contains(final int[] array, final int key) {     
	    return ArrayUtils.contains(array, key);
	}
	
}
