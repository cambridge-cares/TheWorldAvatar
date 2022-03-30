package kbcreator;
 // esp for the engine

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;


import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import javax.xml.transform.TransformerFactoryConfigurationError;

import org.apache.jena.ontology.DatatypeProperty;
import org.apache.jena.ontology.Individual;
import org.apache.jena.ontology.ObjectProperty;
import org.apache.jena.ontology.OntClass;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.rdf.model.ModelFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

public class EngineKnowledgeBaseCreator {

	public static String baseURL2 = "D:\\KBDev-git/irp3-JPS-KBDev-git/Server Ontology Configuration Root/kb/deu/berlin/powerplants/";
	public static String baseURL = "D:\\KBDev-git/irp3-JPS-KBDev-git/Server Ontology Configuration Root/kb/temporary/";
	public static String ontologymainiri="http://www.theworldavatar.com/ontology/ontoengine/OntoEngine.owl#"; 
	
	private OntClass engineclass = null;
	private OntClass cylinderclass = null;
	private OntClass invalveclass = null;
	private OntClass exvalveclass = null;
	private OntClass boreclass = null;
	private OntClass scalarvalueclass = null;
	
	
	private OntClass compresionratioclass = null;
	private OntClass conrodclass = null;
	private OntClass displacementvolclass = null;
	private OntClass enginespeedclass = null;
	private OntClass exeventheightclass = null;
	private OntClass exvalveclosingclass = null;
	private OntClass exvalvediameterclass = null;
	private OntClass exvalveopeningclass = null;
	private OntClass ineventheightclass = null;
	private OntClass invalveareaclass = null;
	private OntClass invalveclosingclass = null;
	
	private OntClass invalvediameterclass = null;
	private OntClass invalveopeningclass = null;
	private OntClass nozzlediameterclass = null;
	private OntClass nozzleclass = null;
	private OntClass ronclass = null;
	
	
	private OntClass strokedistanceclass = null;
	private OntClass pinoffsetclass = null;
	private OntClass afrclass = null;
	private OntClass afrstoichclass = null;
	private OntClass farclass = null;
	private OntClass ctohratioclass = null;
	private OntClass extegrclass = null;
	private OntClass integrclass = null;
	private OntClass fuelclass = null;
	private OntClass processclass = null;
	private OntClass processtreamclass = null;
	private OntClass convectivemassclass=null;
	private OntClass matamountclass = null;
	private OntClass genamountclass=null;
	private OntClass pressureclass = null;
	private OntClass temperatureclass=null;
	private OntClass phaseclass=null;
	private OntClass mixtureclass = null;
	private OntClass materialclass=null;
	private OntClass Cpclass=null;
	
	private ObjectProperty hasAirFuelRatio = null;
	private ObjectProperty hasBore = null;
	private ObjectProperty hascoordinatesystem = null;
	private ObjectProperty hasx = null;
	private ObjectProperty hasy = null;
	private ObjectProperty hasvalue = null;
	private ObjectProperty hasunit = null;
	private ObjectProperty hasBowlDepth = null;
	private ObjectProperty hasBowlRadius = null;
	private ObjectProperty refertogenamount=null;
	private ObjectProperty hasClosingConditionAngle = null;
	private ObjectProperty hasSubsystem = null;
	private ObjectProperty hasProperty = null;
	private ObjectProperty hasCompressionRatio = null;
	private ObjectProperty realizes = null;
	private ObjectProperty referstomaterial = null;
	
	
	private ObjectProperty hasConnectionRodLength = null;
	private ObjectProperty hasCtoHRatio = null;
	private ObjectProperty hasDisplacementVolume = null;
	private ObjectProperty hasEngineSpeed = null;
	private ObjectProperty hasExhaustEventHeight = null;
	private ObjectProperty hasExhaustValveDiameter = null;
	private ObjectProperty hasExternalEGR = null;
	private ObjectProperty hasFuel = null;
	private ObjectProperty hasInitialFuelAirRatio = null;
	private ObjectProperty hasIntakeEventHeight = null;
	private ObjectProperty hasIntakeValveArea = null;
	private ObjectProperty hasIntakeValveDiameter = null;
	private ObjectProperty hasStrokeDistance = null;
	private ObjectProperty hasInternalEGR = null;
	private ObjectProperty hasInput = null;
	private ObjectProperty hasOutput = null;
	private ObjectProperty hasWristPinOffset = null;
	private ObjectProperty hasStoichiometricAirFuelRatio = null;
	private ObjectProperty hasRON = null;
	private ObjectProperty hasOpeningConditionAngle = null;
	private ObjectProperty contains = null;
	private ObjectProperty hasNozzleDiameter = null;
	private ObjectProperty hastemperature = null;
	private ObjectProperty haspressure = null;
	private ObjectProperty intcharacteristic = null;
	private ObjectProperty thermbehavior = null;
	
	private DatatypeProperty numberOfCyclinder = null;
	private DatatypeProperty numberOfExhaustValve = null;
	private DatatypeProperty numberOfIntakevalve = null;
	private DatatypeProperty hasname = null;
	private DatatypeProperty numberOfStroke = null;
	private DatatypeProperty numberOfNozzle = null;
	private DatatypeProperty numval = null;
	
	static Individual mm;
	static Individual m2;
	static Individual kgperh;
	static Individual K;
	static Individual C;
	static Individual bar;
	static Individual gpers;
	static Individual g;
	static Individual jperkgk;
	static Individual degree;
	static Individual cm3;
	
	HashMap<String, String> hmap = new HashMap<String, String>();
	
	public void savefile(OntModel jenaOwlModel, String filePath2) throws URISyntaxException, FileNotFoundException {

		FileOutputStream out = new FileOutputStream(filePath2);
		
		Collection errors = new ArrayList();
		jenaOwlModel.write(out, "RDF/XML-ABBREV");

		
		System.out.println("File saved with " + errors.size() + " errors.");
	}
	
	public void initOWLClasses(OntModel jenaOwlModel) {
		engineclass = jenaOwlModel.getOntClass(ontologymainiri+"CompressionIgnitionEngine");
		cylinderclass = jenaOwlModel.getOntClass(ontologymainiri+"#Cylinder");
		invalveclass = jenaOwlModel.getOntClass(
				ontologymainiri+"#IntakeValve");
		exvalveclass = jenaOwlModel.getOntClass(
				ontologymainiri+"#ExhaustValve");
		fuelclass = jenaOwlModel.getOntClass(ontologymainiri+"#Fuel");
		processtreamclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_function/process.owl#ProcessStream");
		
		boreclass = jenaOwlModel.getOntClass(
				ontologymainiri+"#Bore");
		
		scalarvalueclass = jenaOwlModel
				.getOntClass("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#ScalarValue");	
		
		convectivemassclass=jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#ConvectiveMassFlowrate");	
		pressureclass=jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#Pressure");
		temperatureclass=jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#Temperature");
		
		compresionratioclass = jenaOwlModel
				.getOntClass(ontologymainiri+"#CompressionRatio");
		conrodclass = jenaOwlModel.getOntClass(ontologymainiri+"#ConnectingRodLength");
		displacementvolclass = jenaOwlModel.getOntClass(ontologymainiri+"#DisplacementVolume");
		enginespeedclass = jenaOwlModel.getOntClass(ontologymainiri+"#EngineSpeed");
		exeventheightclass = jenaOwlModel.getOntClass(ontologymainiri+"#ExhaustEventHeight");
		
		processclass = jenaOwlModel.getOntClass(ontologymainiri+"#CombustionProcess");
		
		exvalveclosingclass = jenaOwlModel
				.getOntClass(ontologymainiri+"#ExhaustValveClosingPosition");
		exvalvediameterclass = jenaOwlModel.getOntClass(ontologymainiri+"#ExhaustValveDiameter");
		exvalveopeningclass = jenaOwlModel.getOntClass(ontologymainiri+"#ExhaustValveOpeningPosition");
		ineventheightclass = jenaOwlModel.getOntClass(ontologymainiri+"#IntakeEventHeight");
		invalveareaclass = jenaOwlModel.getOntClass(ontologymainiri+"#IntakeValveArea");
		
		invalveclosingclass = jenaOwlModel.getOntClass(ontologymainiri+"#IntakeValveClosingPosition");
		invalvediameterclass = jenaOwlModel.getOntClass(ontologymainiri+"#IntakeValveDiameter");
		invalveopeningclass = jenaOwlModel.getOntClass(ontologymainiri+"#IntakeValveOpeningPosition");
		nozzlediameterclass = jenaOwlModel.getOntClass(ontologymainiri+"#NozzleDiameter");
		
		phaseclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#SinglePhase");
		mixtureclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/material/substance/substance.owl#Mixture");
		materialclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/material/material.owl#Material");
		
		nozzleclass = jenaOwlModel.getOntClass(ontologymainiri+"#Nozzle");
		ronclass = jenaOwlModel.getOntClass(ontologymainiri+"#RON");		
		
		strokedistanceclass = jenaOwlModel.getOntClass(ontologymainiri+"#StrokeDistance");
		pinoffsetclass = jenaOwlModel.getOntClass(ontologymainiri+"#WristPinOffset");
		afrclass = jenaOwlModel.getOntClass(ontologymainiri+"#AFR");
		afrstoichclass = jenaOwlModel.getOntClass(ontologymainiri+"#AFRstoich");
		ctohratioclass = jenaOwlModel.getOntClass(ontologymainiri+"#CtoHRatio");
		extegrclass = jenaOwlModel.getOntClass(ontologymainiri+"#ExternalEGR");		
		integrclass = jenaOwlModel.getOntClass(ontologymainiri+"#InternalEGR");		
		farclass = jenaOwlModel.getOntClass(ontologymainiri+"#FuelAirEquivalentRatio");
		matamountclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#MaterialAmount");
		genamountclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#GeneralizedAmount");
		Cpclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#ThermodynamicStateProperty");
		
		refertogenamount = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/chemical_process_system.owl#refersToGeneralizedAmount");
		contains = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#contains");
		hasInput = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#hasInput");
		hasOutput= jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#hasOutput");
		hasSubsystem = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasSubsystem");
		hasProperty = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasProperty");
		realizes = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#realizes");
		hasAirFuelRatio = jenaOwlModel.getObjectProperty(ontologymainiri+"#hasAirFuelRatio");
		hasBore= jenaOwlModel.getObjectProperty(ontologymainiri+"#hasBore");
		hascoordinatesystem = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasGISCoordinateSystem");
		hasx = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasProjectedCoordinate_x");
		hasy = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasProjectedCoordinate_y");
		hasvalue = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue");
		hasunit = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasUnitOfMeasure");
		referstomaterial=jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#refersToMaterial");
		hasBowlDepth = jenaOwlModel.getObjectProperty(ontologymainiri+"#hasBowlDepth");
		hasBowlRadius = jenaOwlModel.getObjectProperty(ontologymainiri+"#hasBowlRadius");
		hastemperature = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#has_temperature");
		haspressure = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#has_pressure");
		intcharacteristic = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/material/material.owl#intrinsicCharacteristics");
		thermbehavior = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/material/material.owl#thermodynamicBehavior");
		
		hasClosingConditionAngle = jenaOwlModel.getObjectProperty(ontologymainiri+"#hasClosingConditionAngle");
		hasCompressionRatio = jenaOwlModel.getObjectProperty(ontologymainiri+"#hasCompressionRatio");
		hasConnectionRodLength = jenaOwlModel.getObjectProperty(ontologymainiri+"#hasConnectingRodLength");
		hasCtoHRatio = jenaOwlModel.getObjectProperty(ontologymainiri+"#hasCtoHRatio");
		hasDisplacementVolume = jenaOwlModel.getObjectProperty(ontologymainiri+"#hasDisplacementVolume");
		hasEngineSpeed = jenaOwlModel.getObjectProperty(ontologymainiri+"#hasEngineSpeed");
		
		hasExhaustEventHeight = jenaOwlModel.getObjectProperty(ontologymainiri+"#hasExhaustEventHeight");
		hasExhaustValveDiameter = jenaOwlModel.getObjectProperty(ontologymainiri+"#hasExhaustValveDiameter");
		hasExternalEGR = jenaOwlModel.getObjectProperty(ontologymainiri+"#hasExternalEGR");
		hasFuel = jenaOwlModel.getObjectProperty(ontologymainiri+"#hasFuel");
		hasInitialFuelAirRatio = jenaOwlModel.getObjectProperty(ontologymainiri+"#hasInitialFuelAirRatio");
		hasIntakeEventHeight = jenaOwlModel.getObjectProperty(ontologymainiri+"#hasIntakeEventHeight");
		
		hasIntakeValveArea = jenaOwlModel.getObjectProperty(ontologymainiri+"#hasIntakeValveArea");
				hasIntakeValveDiameter = jenaOwlModel.getObjectProperty(ontologymainiri+"#hasIntakeValveDiameter");
				hasStrokeDistance = jenaOwlModel.getObjectProperty(ontologymainiri+"#hasStrokeDistance");
				hasInternalEGR = jenaOwlModel.getObjectProperty(ontologymainiri+"#hasInternalEGR");
				hasWristPinOffset = jenaOwlModel.getObjectProperty(ontologymainiri+"#hasWristPinOffset");
				hasStoichiometricAirFuelRatio= jenaOwlModel.getObjectProperty(ontologymainiri+"#hasStoichiometricAirFuelRatio");
		
				hasRON = jenaOwlModel.getObjectProperty(ontologymainiri+"#hasRON");
				hasOpeningConditionAngle = jenaOwlModel.getObjectProperty(ontologymainiri+"#hasOpeningConditionAngle");
				hasNozzleDiameter = jenaOwlModel.getObjectProperty(ontologymainiri+"#hasNozzleDiameter");
				
				hasFuel=jenaOwlModel.getObjectProperty(ontologymainiri+"#hasFuel");

		
		numval = jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");
		numberOfCyclinder = jenaOwlModel.getDatatypeProperty(ontologymainiri+"#numberOfCylinder");
		numberOfNozzle  = jenaOwlModel.getDatatypeProperty(ontologymainiri+"#numberOfNozzle");
		numberOfStroke = jenaOwlModel.getDatatypeProperty(ontologymainiri+"#numberOfStroke");
		numberOfIntakevalve = jenaOwlModel.getDatatypeProperty(ontologymainiri+"#numberOfIntakeValve");
		numberOfExhaustValve = jenaOwlModel.getDatatypeProperty(ontologymainiri+"#numberOfExhaustValve");
		


		mm=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#mm");
		m2=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#m.m");
		bar=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#bar");
		K=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/SI_unit.owl#K");
		C=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#Celsius");
		gpers=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#g_per_s");
		g=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#g");
		degree=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#degree");
		cm3=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#cm.cm.cm");
		kgperh=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#kg_per_hr");
		jperkgk=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#J_per_kg.K");
		//xsdDouble = jenaOwlModel.getRDFDatatypeByName("xsd:double");
		
	}
	
	public HashMap<String, String> editXML(String filepath1) throws TransformerFactoryConfigurationError {
		try {

			// String filepath = "/JPS/workingdir/ADMS/InputParams.xml";
			// String filepath2 = AgentLocator.getPathToJpsWorkingDir() +
			// "/JPS/SRM/InputParams.xml";
			

			DocumentBuilderFactory docFactory = DocumentBuilderFactory.newInstance();
			DocumentBuilder docBuilder = docFactory.newDocumentBuilder();
			Document doc = docBuilder.parse(filepath1);
			
			NodeList propgroup=doc.getElementsByTagName("property");
						
			for (int b = 0; b < propgroup.getLength(); b++) {
				if (propgroup.item(b).getNodeType() == Node.ELEMENT_NODE) {
					Node node = propgroup.item(b);
					Element eElement = (Element) node;

					String a = eElement.getAttribute("xsi:type");

					System.out.println("a= "+a);
//					if (a.equals("No_int_val")) {

						Node value = eElement.getChildNodes().item(1);
						System.out.println("the node is= "+value.getNodeName());
						System.out.println("the value is= "+value.getTextContent());
						hmap.put(a, value.getTextContent());
//					}


				}
			}
			


		} catch (ParserConfigurationException pce) {
			pce.printStackTrace();
		}  catch (IOException ioe) {
			ioe.printStackTrace();
		} catch (SAXException sae) {
			sae.printStackTrace();
		}
		return hmap;
	}
	
	
	
	
	public void startConversion() throws Exception {


	            	
	            	

	                //System.out.println("Country [code= " + country[4] + " , name=" + country[5] + "]");
	            	
	            	String filePath = baseURL + "EngineTemplate.owl"; // the empty owl file
	            	


	    				
	    				String filePath2 = baseURL2 + "DieselEngine-001.owl"; // the result of written owl file
	    				
	    				FileInputStream inFile = new FileInputStream(filePath);
		    			Reader in = new InputStreamReader(inFile, "UTF-8");
		    				    			
		    			OntModel jenaOwlModel = ModelFactory.createOntologyModel();
		    			jenaOwlModel.read(in, null);

		    			initOWLClasses(jenaOwlModel);
		    			
	    				doConversion(jenaOwlModel); //plant,country,owner,fuel,tech,x,y,emission,cost,anngen,capa,age	
	    				
	    				/** save the updated model file */
		    			savefile(jenaOwlModel, filePath2);
	    			
	            	


	           // }

	//        } catch (IOException e) {
	//            e.printStackTrace();
	//        }
	
	}
	
	
	public void doConversion(OntModel jenaOwlModel){
		String filepath1 = "C:/JPS_DATA/workingdir/JPS/SRM/InputEngineML.xml";
		EngineKnowledgeBaseCreator xmlreader = new EngineKnowledgeBaseCreator();
		hmap=xmlreader.editXML(filepath1);//read the xml and store to hashmap
		
		System.out.println ("testformapping= "+hmap.size());
		Individual engine = engineclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#DieselEngine-001");
		engine.setPropertyValue(numberOfCyclinder, jenaOwlModel.createTypedLiteral(hmap.get("No_cyl")));
		engine.setPropertyValue(numberOfExhaustValve, jenaOwlModel.createTypedLiteral(hmap.get("No_exh_val")));
		engine.setPropertyValue(numberOfIntakevalve, jenaOwlModel.createTypedLiteral(hmap.get("No_int_val")));
		engine.setPropertyValue(numberOfStroke, jenaOwlModel.createTypedLiteral(hmap.get("Strokes")));
		engine.setPropertyValue(numberOfNozzle, jenaOwlModel.createTypedLiteral("6"));
		
		Individual enginespeed = enginespeedclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#EngineSpeedOfDieselEngine-001");
		Individual valueenginespeed = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#V_EngineSpeedOfIntakeValve-001");
		engine.addProperty(hasEngineSpeed, enginespeed);
		enginespeed.addProperty(hasvalue,valueenginespeed);
		valueenginespeed.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(hmap.get("RPM")));
		//valueenginespeed.setPropertyValue(hasunit, rpm);
		
		//intake valve
		//==============================================================================
		Individual intakevalve = invalveclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#IntakeValve-001");
		
		Individual ineventheight = ineventheightclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#EventHeightOfIntakeValve-001");
		Individual valueineventheight = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#V_EventHeightOfIntakeValve-001");
		ineventheight.addProperty(hasvalue,valueineventheight);
		valueineventheight.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(hmap.get("Int_event_height")));
		valueineventheight.addProperty(hasunit, mm);
		
		Individual intakeopeningposition = invalveopeningclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#OpeningPositionOfIntakeValve-001");
		Individual valueintakeopeningposition = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#V_OpeningPositionOfIntakeValve-001");
		intakeopeningposition.addProperty(hasvalue,valueintakeopeningposition);
		valueintakeopeningposition.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(hmap.get("IVO")));
		valueintakeopeningposition.setPropertyValue(hasunit, degree);

		Individual intakeclosingposition = invalveclosingclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#ClosingPositionOfIntakeValve-001");
		Individual valueintakeclosingposition = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#V_ClosingPositionOfIntakeValve-001");
		intakeclosingposition.addProperty(hasvalue,valueintakeclosingposition);
		valueintakeclosingposition.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(hmap.get("IVC")));
		valueintakeclosingposition.setPropertyValue(hasunit, degree);
		
		Individual invalvearea = invalveareaclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#AreaOfIntakeValve-001");
		Individual valueinvalvearea = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#V_AreaOfIntakeValve-001");
		invalvearea.addProperty(hasvalue,valueinvalvearea);
		//valueinvalvearea.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(ynumval));
		valueinvalvearea.addProperty(hasunit, m2);
		
		Individual invalvediameter = invalvediameterclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#DiameterOfIntakeValve-001");
		Individual valueinvalvediameter = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#V_DiameterOfIntakeValve-001");
		invalvediameter.addProperty(hasvalue,valueinvalvediameter);
		valueinvalvediameter.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(hmap.get("Int_dia")));
		valueinvalvediameter.addProperty(hasunit, mm);
		
		intakevalve.addProperty(hasIntakeEventHeight, ineventheight);
		intakevalve.addProperty(hasIntakeValveDiameter, invalvediameter);
		intakevalve.addProperty(hasIntakeValveArea, invalvearea);
		intakevalve.addProperty(hasOpeningConditionAngle, intakeopeningposition);
		intakevalve.addProperty(hasClosingConditionAngle, intakeclosingposition);
		
		
		engine.addProperty(hasSubsystem, intakevalve);
		//==============================================================================
		
		//exhaust valve
		//==============================================================================
		Individual exhaustvalve = exvalveclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#ExhaustValve-001");
		
		Individual exeventheight = exeventheightclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#EventHeightOfExhaustValve-001");
		Individual valueexeventheight = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#V_EventHeightOfExhaustValve-001");
		exeventheight .addProperty(hasvalue,valueexeventheight );
		valueexeventheight.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(hmap.get("Exh_event_height")));
		valueexeventheight.addProperty(hasunit, mm);

		
		Individual exvalvediameter = exvalvediameterclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#DiameterOfExhaustValve-001");
		Individual valueexvalvediameter = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#V_DiameterOfExhaustValve-001");
		exvalvediameter.addProperty(hasvalue,valueexvalvediameter);
		valueexvalvediameter.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(hmap.get("Exh_dia")));
		valueexvalvediameter.addProperty(hasunit, mm);
		
		Individual exhaustopeningposition = exvalveopeningclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#OpeningPositionOfExhaustValve-001");
		Individual valueexhaustopeningposition = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#V_OpeningPositionOfExhaustValve-001");
		exhaustopeningposition.addProperty(hasvalue,valueexhaustopeningposition);
		valueexhaustopeningposition.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(hmap.get("EVO")));
		valueexhaustopeningposition.addProperty(hasunit, degree);

		Individual exhaustclosingposition = exvalveclosingclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#ClosingPositionOfExhaustValve-001");
		Individual valueexhaustclosingposition = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#V_ClosingPositionOfExhaustValve-001");
		exhaustclosingposition.addProperty(hasvalue,valueexhaustclosingposition);
		valueexhaustclosingposition.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(hmap.get("EVC")));
		valueexhaustclosingposition.addProperty(hasunit, degree);

		exhaustvalve.addProperty(hasExhaustEventHeight, exeventheight);
		exhaustvalve.addProperty(hasExhaustValveDiameter, exvalvediameter);
		exhaustvalve.addProperty(hasOpeningConditionAngle, exhaustopeningposition);
		exhaustvalve.addProperty(hasClosingConditionAngle, exhaustclosingposition);
		
		
		engine.addProperty(hasSubsystem, exhaustvalve);
		//==============================================================================
		
		//cylinder
		//==============================================================================
		for(int n=1;n<=1;n++){
		Individual cylinder = cylinderclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#Cylinder-00"+n);
				
		Individual bore = boreclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#BoreOfCylinder-00"+n);
		Individual valuebore = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#V_BoreOfCylinder-00"+n);
		bore.addProperty(hasvalue,valuebore);
		valuebore.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(hmap.get("Bore")));
		valuebore.addProperty(hasunit, mm);
		
		Individual CR = compresionratioclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#CompressionRatioOfCylinder-00"+n);
		Individual valueCR = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#V_CompressionRatioOfCylinder-00"+n);
		CR.addProperty(hasvalue,valueCR);
		valueCR.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(hmap.get("CR")));
		
		Individual ConRod = conrodclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#ConnectingRodLengthOfCylinder-00"+n);
		Individual valueConRod = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#V_ConnectingRodLengthOfCylinder-00"+n);
		ConRod.addProperty(hasvalue,valueConRod);
		valueConRod.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(hmap.get("Con_rod")));
		valueConRod.addProperty(hasunit, mm);
		
		Individual dispvol = displacementvolclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#DisplacementVolumeOfCylinder-00"+n);
		Individual valuedispvol = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#V_DisplacementVolumeOfCylinder-00"+n);
		dispvol.addProperty(hasvalue,valuedispvol);
		valuedispvol.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(hmap.get("Eng_displ_vol")));
		valuedispvol.addProperty(hasunit, cm3);
		
		Individual extegr = extegrclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#ExternalEGROfCylinder-00"+n);
		Individual valueextegr = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#V_ExternalEGROfCylinder-00"+n);
		extegr.addProperty(hasvalue,valueextegr);
		valueextegr.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(hmap.get("extEGR")));
		
		Individual integr = integrclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#InternalEGROfCylinder-00"+n);
		Individual valueintegr = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#V_InternalEGROfCylinder-00"+n);
		integr.addProperty(hasvalue,valueintegr);
		valueintegr.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(hmap.get("intEGR")));
		
		Individual strokedist = strokedistanceclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#StrokeDistanceOfCylinder-00"+n);
		Individual valuestrokedist = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#V_StrokeDistanceOfCylinder-00"+n);
		strokedist.addProperty(hasvalue,valuestrokedist);
		valuestrokedist.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(hmap.get("Stroke")));
		valuestrokedist.addProperty(hasunit, mm);
		
		Individual wristpin = pinoffsetclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#WristPinOffSetOfCylinder-00"+n);
		Individual valuewristpin = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#V_WristPinOffSetOfCylinder-00"+n);
		wristpin.addProperty(hasvalue,valuewristpin);
		valuewristpin.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(hmap.get("Wrist_pin_offset")));
		valuewristpin.addProperty(hasunit, mm);
		
		cylinder.addProperty(hasBore, bore);
		cylinder.addProperty(hasCompressionRatio, CR);
		cylinder.addProperty(hasConnectionRodLength, ConRod);
		cylinder.addProperty(hasDisplacementVolume, dispvol);
		cylinder.addProperty(hasExternalEGR, extegr);
		cylinder.addProperty(hasInternalEGR, integr);
		cylinder.addProperty(hasStrokeDistance, strokedist);
		cylinder.addProperty(hasWristPinOffset, wristpin);
		
		engine.addProperty(hasSubsystem, cylinder);
		}
		//==============================================================================
		
		
		//fuel
		//==============================================================================
		
		Individual fuelofengine = fuelclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#FuelOfDieselEngine-001");
		
		Individual ronfuel = ronclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#RONOfFuelOfDieselEngine-001");
		Individual valueronfuel = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#V_RONOfFuelOfDieselEngine-001");
		ronfuel.addProperty(hasvalue,valueronfuel);
		valueronfuel.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(hmap.get("RON")));
		
		Individual chratio = ctohratioclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#CtoHRatioOfFuelOfDieselEngine-001");
		Individual valuechratio = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#V_CtoHRatioOfFuelOfDieselEngine-001");
		chratio.addProperty(hasvalue,valuechratio);
		valuechratio.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(hmap.get("C_to_H")));
		
		//Resource isooctane = jenaOwlModel.createResource("http://dbpedia.org/resource/" + isooctane);
		fuelofengine.addProperty(hasRON, ronfuel);
		fuelofengine.addProperty(hasCtoHRatio, chratio);
		//fuelofengine.addProperty(contains, isooctane);
		
		engine.addProperty(hasFuel, fuelofengine);
		//==============================================================================
		
		//process
		//==============================================================================
		Individual combustion = processclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#CombustionProcessOfDieselEngine-001");
		
		Individual initfuelairratio = farclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#InitialFuelAirRatioOfDieselEngine-001");
		Individual valueinitfuelairratio = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#V_InitialFuelAirRatioOfDieselEngine-001");
		initfuelairratio.addProperty(hasvalue,valueinitfuelairratio);
		valueinitfuelairratio.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(hmap.get("IniFuelAirEquivRatio")));
		
		Individual airfuelratio = afrclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#AirFuelRatioOfDieselEngine-001");
		Individual valueairfuelratio = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#V_AirFuelRatioOfDieselEngine-001");
		airfuelratio.addProperty(hasvalue,valueairfuelratio);
		valueairfuelratio.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(hmap.get("AFR")));
		
		Individual stoichairfuelratio = afrstoichclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#StoichiometricAirFuelRatioOfDieselEngine-001");
		Individual valuestoichairfuelratio = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#V_StoichiometricAirFuelRatioOfDieselEngine-001");
		stoichairfuelratio.addProperty(hasvalue,valuestoichairfuelratio);
		valuestoichairfuelratio.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(hmap.get("AFRstoich")));
		
		combustion.addProperty(hasInitialFuelAirRatio, initfuelairratio);
		combustion.addProperty(hasAirFuelRatio, airfuelratio);
		combustion.addProperty(hasStoichiometricAirFuelRatio, stoichairfuelratio);

		engine.addProperty(realizes, combustion);
		//------------------------------------------------------------------------------
		
		Individual AirStream = processtreamclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#AirStreamOfDieselEngine-001");
		Individual FuelStream = processtreamclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#FuelStreamOfDieselEngine-001");
		Individual WasteStream = processtreamclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#WasteStreamOfDieselEngine-001");
		combustion.addProperty(hasInput, AirStream);
		combustion.addProperty(hasInput, FuelStream);
		combustion.addProperty(hasOutput, WasteStream);
		
		FuelStream.addProperty(contains, fuelofengine);
		
		Individual GenAmountAirStream = genamountclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#GeneralizedAmount_AirStreamOfDieselEngine-001");
		Individual GenAmountFuelStream = genamountclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#GeneralizedAmount_FuelStreamOfDieselEngine-001");
		Individual GenAmountWasteStream = genamountclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#GeneralizedAmount_WasteStreamOfDieselEngine-001");
		AirStream.addProperty(refertogenamount, GenAmountAirStream);
		FuelStream.addProperty(refertogenamount, GenAmountFuelStream);
		WasteStream.addProperty(refertogenamount, GenAmountWasteStream);
		
		Individual MaterialAmountAirStream = matamountclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#MaterialAmount_AirStreamOfDieselEngine-001");
		Individual MaterialAmountFuelStream = matamountclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#MaterialAmount_FuelStreamOfDieselEngine-001");
		Individual MaterialAmountWasteStream = matamountclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#MaterialAmount_WasteStreamOfDieselEngine-001");
		GenAmountAirStream.addProperty(hasSubsystem, MaterialAmountAirStream);
		GenAmountFuelStream.addProperty(hasSubsystem, MaterialAmountFuelStream);
		GenAmountWasteStream.addProperty(hasSubsystem, MaterialAmountWasteStream);
		
		Individual MassFAirStream = convectivemassclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#MassFlowRate_AirStreamOfDieselEngine-001");
		MaterialAmountAirStream.addProperty(hasProperty, MassFAirStream);
		Individual valueMassFAirStream = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#V_MassFlowRate_AirStreamOfDieselEngine-001");
		MassFAirStream.addProperty(hasvalue,valueMassFAirStream);
		valueMassFAirStream.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(hmap.get("amfr")));
		valueMassFAirStream.addProperty(hasunit, kgperh);
		
		Individual MaterialAirStream = materialclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#Material_AirStreamOfDieselEngine-001");
		Individual MaterialFuelStream = materialclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#Material_FuelStreamOfDieselEngine-001");
		Individual MaterialWasteStream = materialclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#Material_WasteStreamOfDieselEngine-001");
		MaterialAmountAirStream.addProperty(referstomaterial, MaterialAirStream);
		MaterialAmountFuelStream.addProperty(referstomaterial, MaterialFuelStream);
		MaterialAmountWasteStream.addProperty(referstomaterial, MaterialWasteStream);
		
		Individual MixtureAmountAirStream = mixtureclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#Mixture_AirStreamOfDieselEngine-001");
		Individual MixtureAmountFuelStream = mixtureclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#Mixture_FuelStreamOfDieselEngine-001");
		Individual MixtureAmountWasteStream = mixtureclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#Mixture_WasteStreamOfDieselEngine-001");
		MaterialAirStream.addProperty(intcharacteristic,MixtureAmountAirStream);
		MaterialFuelStream.addProperty(intcharacteristic,MixtureAmountFuelStream);
		MaterialWasteStream.addProperty(intcharacteristic,MixtureAmountWasteStream);
		
		
		
		//==================================================================waste stream componenents================================================================
		
		Individual componentflow1WasteStream = convectivemassclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#Unburned_Hydrocarbon_EmissionRate");
		Individual componentflow2WasteStream = convectivemassclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#Carbon_monoxide_EmissionRate");
		Individual componentflow3WasteStream = convectivemassclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#Carbon_dioxide_EmissionRate");
		Individual componentflow4WasteStream = convectivemassclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#Nitrogen_dioxide_EmissionRate");
		Individual componentflow5WasteStream = convectivemassclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#Nitrogen_oxides_EmissionRate");
		Individual combinedmwWasteStream = convectivemassclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#Combined_MolecularMass_WasteStream");

		MixtureAmountWasteStream.addProperty(hasProperty, componentflow1WasteStream);
		MixtureAmountWasteStream.addProperty(hasProperty, componentflow2WasteStream);
		MixtureAmountWasteStream.addProperty(hasProperty, componentflow3WasteStream);
		MixtureAmountWasteStream.addProperty(hasProperty, componentflow4WasteStream);
		MixtureAmountWasteStream.addProperty(hasProperty, componentflow5WasteStream);
		MixtureAmountWasteStream.addProperty(hasProperty, combinedmwWasteStream);
		
		Individual valuecomponentflow1WasteStream = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#V_Unburned_Hydrocarbon_EmissionRate");
		Individual valuecomponentflow2WasteStream = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#V_Carbon_monoxide_EmissionRate");
		Individual valuecomponentflow3WasteStream = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#V_Carbon_dioxide_EmissionRate");
		Individual valuecomponentflow4WasteStream = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#V_Nitrogen_dioxide_EmissionRate");
		Individual valuecomponentflow5WasteStream = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#V_Nitrogen_oxides_EmissionRate");
		Individual valuecombinedmwWasteStream = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#V_Combined_MolecularMass_WasteStream");
		
		componentflow1WasteStream.addProperty(hasvalue,valuecomponentflow1WasteStream);
		componentflow2WasteStream.addProperty(hasvalue,valuecomponentflow2WasteStream);
		componentflow3WasteStream.addProperty(hasvalue,valuecomponentflow3WasteStream);
		componentflow4WasteStream.addProperty(hasvalue,valuecomponentflow4WasteStream);
		componentflow5WasteStream.addProperty(hasvalue,valuecomponentflow5WasteStream);
		combinedmwWasteStream.addProperty(hasvalue,valuecombinedmwWasteStream);
		
		//valuecomponentflow1WasteStream.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(ynumval));
		valuecomponentflow1WasteStream.addProperty(hasunit, gpers);
		//valuecomponentflow2WasteStream.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(ynumval));
		valuecomponentflow2WasteStream.addProperty(hasunit, gpers);
		//valuecomponentflow3WasteStream.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(ynumval));
		valuecomponentflow3WasteStream.addProperty(hasunit, gpers);
		//valuecomponentflow4WasteStream.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(ynumval));
		valuecomponentflow4WasteStream.addProperty(hasunit, gpers);
		//valuecomponentflow5WasteStream.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(ynumval));
		valuecomponentflow5WasteStream.addProperty(hasunit, gpers);
		//valuecombinedmwWasteStream.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(ynumval));
		valuecombinedmwWasteStream.addProperty(hasunit, g);
		//===========================================================================================================================================================	
		
		Individual PhaseAirStream = phaseclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#SinglePhase_AirStreamOfDieselEngine-001");
		Individual PhaseFuelStream = phaseclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#SinglePhase_FuelStreamOfDieselEngine-001");
		Individual PhaseWasteStream = phaseclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#SinglePhase_WasteStreamOfDieselEngine-001");
		MaterialAirStream.addProperty(thermbehavior,PhaseAirStream);
		MaterialFuelStream.addProperty(thermbehavior,PhaseFuelStream);
		MaterialWasteStream.addProperty(thermbehavior,PhaseWasteStream);
		
		Individual TemperatureAirStream = temperatureclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#Temperature_AirStreamOfDieselEngine-001");
		Individual TemperatureFuelStream = temperatureclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#Temperature_FuelStreamOfDieselEngine-001");
		Individual TemperatureWasteStream = temperatureclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#Temperature_WasteStreamOfDieselEngine-001");
		PhaseAirStream.addProperty(hastemperature,TemperatureAirStream);
		PhaseFuelStream.addProperty(hastemperature,TemperatureFuelStream);
		PhaseWasteStream.addProperty(hastemperature,TemperatureWasteStream);
		
		Individual valueTemperatureAirStream = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#V_Temperature_AirStreamOfDieselEngine-001");
		Individual valueTemperatureFuelStream = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#V_Temperature_FuelStreamOfDieselEngine-001");
		Individual valueTemperatureWasteStream = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#V_Temperature_WasteStreamOfDieselEngine-001");
		TemperatureAirStream.addProperty(hasvalue,valueTemperatureAirStream);
		TemperatureFuelStream.addProperty(hasvalue,valueTemperatureFuelStream);
		TemperatureWasteStream.addProperty(hasvalue,valueTemperatureWasteStream);
		valueTemperatureAirStream.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(hmap.get("Tman")));
		valueTemperatureAirStream.addProperty(hasunit, K);
		//valueTemperatureFuelStream.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(ynumval));
		valueTemperatureFuelStream.addProperty(hasunit, K);
		valueTemperatureWasteStream.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(hmap.get("Tex")));
		valueTemperatureWasteStream.addProperty(hasunit, C);
		
		Individual PressureAirStream = pressureclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#Pressure_AirStreamOfDieselEngine-001");
		Individual PressureFuelStream = pressureclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#Pressure_FuelStreamOfDieselEngine-001");
		Individual PressureWasteStream = pressureclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#Pressure_WasteStreamOfDieselEngine-001");
		PhaseAirStream.addProperty(haspressure,PressureAirStream);
		PhaseFuelStream.addProperty(haspressure,PressureFuelStream);
		PhaseWasteStream.addProperty(haspressure,PressureWasteStream);
		
		Individual valuePressureAirStream = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#V_Pressure_AirStreamOfDieselEngine-001");
		Individual valuePressureFuelStream = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#V_Pressure_FuelStreamOfDieselEngine-001");
		Individual valuePressureWasteStream = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#V_Pressure_WasteStreamOfDieselEngine-001");
		PressureAirStream.addProperty(hasvalue,valuePressureAirStream);
		PressureFuelStream.addProperty(hasvalue,valuePressureFuelStream);
		PressureWasteStream.addProperty(hasvalue,valuePressureWasteStream);
		
		valuePressureAirStream.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(hmap.get("Pman")));
		valuePressureAirStream.addProperty(hasunit, bar);
		//valuePressureFuelStream.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(ynumval));
		valuePressureFuelStream.addProperty(hasunit, bar);
		valuePressureWasteStream.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(hmap.get("Pex")));
		valuePressureWasteStream.addProperty(hasunit, bar);
		
		
		
		
		
		Individual CpAirStream = Cpclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#Cp_AirStreamOfDieselEngine-001");
		Individual CpFuelStream = Cpclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#Cp_FuelStreamOfDieselEngine-001");
		Individual CpWasteStream = Cpclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#Cp_WasteStreamOfDieselEngine-001");
		PhaseAirStream.addProperty(hasProperty,CpAirStream);
		PhaseFuelStream.addProperty(hasProperty,CpFuelStream);
		PhaseWasteStream.addProperty(hasProperty,CpWasteStream);
		
		Individual valueCpAirStream = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#V_Cp_AirStreamOfDieselEngine-001");
		Individual valueCpFuelStream = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#V_Cp_FuelStreamOfDieselEngine-001");
		Individual valueCpWasteStream = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/deu/berlin/powerplants/DieselEngine-001.owl#V_Cp_WasteStreamOfDieselEngine-001");
		CpAirStream.addProperty(hasvalue,valueCpAirStream);
		CpFuelStream.addProperty(hasvalue,valueCpFuelStream);
		CpWasteStream.addProperty(hasvalue,valueCpWasteStream);
		
		//valueCpAirStream.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(ynumval));
		valueCpAirStream.addProperty(hasunit, jperkgk);
		//valueCpFuelStream.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(ynumval));
		valueCpFuelStream.addProperty(hasunit, jperkgk);
		//valueCpWasteStream.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(ynumval));
		valueCpWasteStream.addProperty(hasunit, jperkgk);
	}

	public static void main(String[] args) throws Exception {
		
		 String filepath1 = "C:/JPS_DATA/workingdir/JPS/SRM/InputEngineML.xml";
		System.out.println("Starting Process");
		EngineKnowledgeBaseCreator converter = new EngineKnowledgeBaseCreator();
		converter.startConversion();
		
//		KnowledgeBaseCreator xmlreader = new KnowledgeBaseCreator();
//		xmlreader.editXML(filepath1);
	}
}
