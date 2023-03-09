package uk.ac.cam.cares.jps.ship;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Collection;

import org.apache.jena.ontology.DatatypeProperty;
import org.apache.jena.ontology.Individual;
import org.apache.jena.ontology.ObjectProperty;
import org.apache.jena.ontology.OntClass;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.rdf.model.ModelFactory;
import org.apache.jena.rdf.model.Resource;

public class ShipKbCreator {
	public static String baseURL2 = "C:/JPS_DATA/workingdir/JPS/SHIP/output/"; // directory of output file
	public static String baseURL = "C:/JPS_DATA/workingdir/JPS/SHIP/input/"; // location of template file which is empty; file located in C:\Users\WE\Dropbox (Cambridge CARES)\IRP3 CAPRICORN shared folder\_JPS Development\workingdir\JPS_SHIP
	public static String ontologymainiri="http://www.theworldavatar.com/ontology/ontoship/OntoShip.owl#"; 
	
	private OntClass scalarvalueclass = null;
	private OntClass coordinatesystemclass = null;
	private OntClass coordinateclass = null;
	private OntClass tempcoordinateclass = null;
	private OntClass coordinatevalueclass = null;
	private OntClass temporalcoordinatesystemclass = null;
	private OntClass pressureclass = null;
	private OntClass temperatureclass=null;
	private OntClass convectivemassclass=null;
	private OntClass phaseclass=null;
	private OntClass mixtureclass=null;
	private OntClass materialclass=null;
	private OntClass Cpclass=null;
	private OntClass matamountclass = null;
	private OntClass genamountclass=null;
	private OntClass wastestreamclass=null;
	private OntClass processclass=null;
	private OntClass Pipeclass=null;
	
	private OntClass IMOclass = null;
	private OntClass MMSIclass = null;
	private OntClass COGclass = null;
	private OntClass SOGclass = null;
	private OntClass Bowclass = null;
	private OntClass Portclass = null;
	private OntClass Starboardclass = null;
	private OntClass Sternclass = null;
	private OntClass Draughtclass = null;
	private OntClass Headingclass = null;
	private OntClass NaviStatusclass = null;
	private OntClass Positioningdevclass = null;
	private OntClass RoTclass = null;
	private OntClass ShipTypeclass = null;
	private OntClass Destinationclass = null;
	private OntClass Shipclass = null;
	private OntClass ETAclass = null;
	

	
	private ObjectProperty hasMMSI = null;
	private ObjectProperty hasIMONumber = null;
	private ObjectProperty hasBowLength = null;
	private ObjectProperty hasCOG = null;
	private ObjectProperty hasDraught = null;
	private ObjectProperty hasHeading = null;
	private ObjectProperty hasNavStatus = null;
	private ObjectProperty hasPortLength = null;
	private ObjectProperty hasSOG = null;
	private ObjectProperty hasPosDevType = null;
	private ObjectProperty hasROT = null;
	private ObjectProperty hasShipType = null;
	private ObjectProperty hasStarboardLength = null;
	private ObjectProperty hasSternLength = null;
	private ObjectProperty hasDestination = null;
	private ObjectProperty hasETA = null;
	
	private ObjectProperty intcharacteristic = null;
	private ObjectProperty thermbehavior = null;
	private ObjectProperty hasSubsystem = null;
	private ObjectProperty hascoordinatesystem = null;
	private ObjectProperty hasx = null;
	private ObjectProperty hasy = null;
	private ObjectProperty hasvalue = null;
	private ObjectProperty hasunit = null;
	private ObjectProperty hastimestamp = null;
	private ObjectProperty hastemporalcoordinate = null;
	private ObjectProperty realizes = null;
	private ObjectProperty hasProperty = null;
	private ObjectProperty hastemperature = null;
	private ObjectProperty haspressure = null;
	private ObjectProperty hasOutput = null;
	private ObjectProperty referstomaterial = null;
	private ObjectProperty refertogenamount = null;
	private ObjectProperty hasHeight = null;
	private ObjectProperty hasInsideDiameter = null;
	
	private DatatypeProperty hasPAC = null;
	private DatatypeProperty hasCallSign = null;
	private DatatypeProperty numval = null;
	private DatatypeProperty hasShipname = null;

	static Individual m;
	static Individual knot;
	static Individual degree;
	static Individual C;
	static Individual bar;
	static Individual gpers;
	static Individual jperkgk;
	static Individual g;
	static Individual kgperh;
	
	public void initOWLClasses(OntModel jenaOwlModel) {
		Shipclass = jenaOwlModel.getOntClass(ontologymainiri+"Ship");
		IMOclass = jenaOwlModel.getOntClass(ontologymainiri+"IMOIdentificationNumber");
		MMSIclass = jenaOwlModel.getOntClass(ontologymainiri+"MMSI");
		COGclass = jenaOwlModel.getOntClass(ontologymainiri+"CourseOverGround");
		Bowclass = jenaOwlModel.getOntClass(ontologymainiri+"DimensionOfBow");
		Portclass = jenaOwlModel.getOntClass(ontologymainiri+"DimensionOfPort");
		Starboardclass = jenaOwlModel.getOntClass(ontologymainiri+"DimensionOfStarboard");
		Sternclass = jenaOwlModel.getOntClass(ontologymainiri+"DimensionOfStern");
		Draughtclass = jenaOwlModel.getOntClass(ontologymainiri+"Draught");
		Headingclass = jenaOwlModel.getOntClass(ontologymainiri+"Heading");
		NaviStatusclass = jenaOwlModel.getOntClass(ontologymainiri+"NavigationalStatus");
		Positioningdevclass = jenaOwlModel.getOntClass(ontologymainiri+"PositioningDeviceType");
		RoTclass = jenaOwlModel.getOntClass(ontologymainiri+"RateOfTurn");
		ShipTypeclass = jenaOwlModel.getOntClass(ontologymainiri+"ShipType");
		SOGclass = jenaOwlModel.getOntClass(ontologymainiri+"SpeedOverGround");
		Destinationclass = jenaOwlModel.getOntClass(ontologymainiri+"Destination");
		ETAclass = jenaOwlModel.getOntClass(ontologymainiri+"EstimatedTimeOfArrival");
		
				
		scalarvalueclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#ScalarValue");	
		coordinatesystemclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#ProjectedCoordinateSystem");
		temporalcoordinatesystemclass =jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#TemporalCoordinateSystem");
		coordinateclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#AngularCoordinate");
		tempcoordinateclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#TemporalCoordinate");
		coordinatevalueclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl#CoordinateValue");
		convectivemassclass=jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#ConvectiveMassFlowrate");	
		pressureclass=jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#Pressure");
		temperatureclass=jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#Temperature");
		phaseclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#SinglePhase");
		mixtureclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/material/substance/substance.owl#Mixture");
		materialclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/material/material.owl#Material");
		Cpclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#ThermodynamicStateProperty");
		matamountclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#MaterialAmount");
		genamountclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#GeneralizedAmount");
		wastestreamclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_function/process.owl#NonReusableWasteProduct");
		processclass=jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_function/process.owl#ReleaseEmission");
		Pipeclass=jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#Pipe");
		
		hasMMSI = jenaOwlModel.getObjectProperty(ontologymainiri+"hasMMSI");
		hasIMONumber = jenaOwlModel.getObjectProperty(ontologymainiri+"hasIMONumber");
		hasETA = jenaOwlModel.getObjectProperty(ontologymainiri+"hasETA");
		hasBowLength = jenaOwlModel.getObjectProperty(ontologymainiri+"hasBowLength");
		hasCOG = jenaOwlModel.getObjectProperty(ontologymainiri+"hasCOG");
		hasDraught = jenaOwlModel.getObjectProperty(ontologymainiri+"hasDraught");
		hasHeading = jenaOwlModel.getObjectProperty(ontologymainiri+"hasHeading");
		hasNavStatus = jenaOwlModel.getObjectProperty(ontologymainiri+"hasNavigationalStatus");
		hasPortLength = jenaOwlModel.getObjectProperty(ontologymainiri+"hasPortLength");
		hasPosDevType = jenaOwlModel.getObjectProperty(ontologymainiri+"hasPositioningDeviceType");
		hasROT = jenaOwlModel.getObjectProperty(ontologymainiri+"hasRateOfTurn");
		hasShipType = jenaOwlModel.getObjectProperty(ontologymainiri+"hasShipType");
		hasSOG = jenaOwlModel.getObjectProperty(ontologymainiri+"hasSOG");
		hasStarboardLength = jenaOwlModel.getObjectProperty(ontologymainiri+"hasStarboardLength");
		hasSternLength = jenaOwlModel.getObjectProperty(ontologymainiri+"hasSternLength");
		hasDestination = jenaOwlModel.getObjectProperty(ontologymainiri+"hasDestination");
		
		hascoordinatesystem = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasGISCoordinateSystem");
		hasx = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasProjectedCoordinate_x");
		hasy = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasProjectedCoordinate_y");
		hasvalue = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue");
		hasunit = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasUnitOfMeasure");
		hastimestamp=jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasTimestamp");
		hastemporalcoordinate=jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasTemporalCoordinate");
		hasSubsystem = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasSubsystem");
		realizes = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#realizes");
		hasOutput= jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#hasOutput");
		hasProperty = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasProperty");
		hastemperature = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#has_temperature");
		haspressure = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#has_pressure");
		intcharacteristic = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/material/material.owl#intrinsicCharacteristics");
		thermbehavior = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/material/material.owl#thermodynamicBehavior");
		refertogenamount = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/chemical_process_system.owl#refersToGeneralizedAmount");
		referstomaterial=jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#refersToMaterial");
		hasHeight = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#hasHeight");
		hasInsideDiameter=jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#hasInsideDiameter");
		
		numval = jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");
		hasPAC = jenaOwlModel.getDatatypeProperty(ontologymainiri+"hasPAC");
		hasCallSign  = jenaOwlModel.getDatatypeProperty(ontologymainiri+"hasCallSign");
		hasShipname = jenaOwlModel.getDatatypeProperty(ontologymainiri+"hasShipName");
		
		m=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/SI_unit.owl#m");
		knot=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#knot");
		degree=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#degree");
		bar=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#bar");
		jperkgk=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#J_per_kg.K");
		C=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#Celsius");
		gpers=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#g_per_s");
		g=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#g");
		kgperh=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#kg_per_hr");
		//xsdDouble = jenaOwlModel.getRDFDatatypeByName("xsd:double");
		
	}
	
	public void doConversion(OntModel jenaOwlModel, String shipinstance){
		//ship instance is like Ship-001
		//String filepath1 = "C:/JPS_DATA/workingdir/JPS/SRM/InputEngineML.xml";
		//KnowledgeBaseCreator xmlreader = new KnowledgeBaseCreator();
		//hmap=xmlreader.editXML(filepath1);//read the xml and store to hashmap
		
		String chimneyinstance="Chimney-1"; //still hard-coded for the sample
		
		
		Individual ship = Shipclass.createIndividual("http://www.theworldavatar.com/kb/ships/"+shipinstance+".owl#"+shipinstance);
		System.out.println(ship.getURI());
		ship.setPropertyValue(hasCallSign, jenaOwlModel.createTypedLiteral("0"));
		ship.setPropertyValue(hasShipname, jenaOwlModel.createTypedLiteral("0"));
		ship.setPropertyValue(hasPAC, jenaOwlModel.createTypedLiteral("0"));
		//TODO tomorrow : contains directly 
		
		Resource engine = jenaOwlModel.createResource("http://www.theworldavatar.com/kb/ships/Engine-001.owl#Engine-001");
		ship.addProperty(hasSubsystem,engine);
		
		Individual chimney = Pipeclass.createIndividual("http://www.theworldavatar.com/kb/ships/"+chimneyinstance+".owl#"+chimneyinstance);
		ship.addProperty(hasSubsystem,chimney);
		
		
		

		
		Individual bow = Bowclass.createIndividual("http://www.theworldavatar.com/kb/ships/"+shipinstance+".owl#BowLengthOf"+shipinstance);
		Individual vbow = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/ships/"+shipinstance+".owl#V_BowLengthOf"+shipinstance);
		bow.addProperty(hasvalue, vbow);
		vbow.setPropertyValue(numval, jenaOwlModel.createTypedLiteral("0"));
		vbow.addProperty(hasunit, m);
		ship.addProperty(hasBowLength, bow);
		
		Individual port = Portclass.createIndividual("http://www.theworldavatar.com/kb/ships/"+shipinstance+".owl#PortLengthOf"+shipinstance);
		Individual vport = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/ships/"+shipinstance+".owl#V_PortLengthOf"+shipinstance);
		port.addProperty(hasvalue, vport);
		vport.setPropertyValue(numval, jenaOwlModel.createTypedLiteral("0"));
		vport.addProperty(hasunit, m);
		ship.addProperty(hasPortLength, port);
		
		Individual Stern = Sternclass.createIndividual("http://www.theworldavatar.com/kb/ships/"+shipinstance+".owl#SternLengthOf"+shipinstance);
		Individual vStern = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/ships/"+shipinstance+".owl#V_SternLengthOf"+shipinstance);
		Stern.addProperty(hasvalue, vStern);
		vStern.setPropertyValue(numval, jenaOwlModel.createTypedLiteral("0"));
		vStern.addProperty(hasunit, m);
		ship.addProperty(hasSternLength, Stern);
		
		Individual Starboard = Starboardclass.createIndividual("http://www.theworldavatar.com/kb/ships/"+shipinstance+".owl#StarboardLengthOf"+shipinstance);
		Individual vStarboard = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/ships/"+shipinstance+".owl#V_StarboardLengthOf"+shipinstance);
		Starboard.addProperty(hasvalue, vStarboard);
		vStarboard.setPropertyValue(numval, jenaOwlModel.createTypedLiteral("0"));
		vStarboard.addProperty(hasunit, m);
		ship.addProperty(hasStarboardLength, Starboard);
		
		Individual Draught = Draughtclass.createIndividual("http://www.theworldavatar.com/kb/ships/"+shipinstance+".owl#DraughtLengthOf"+shipinstance);
		Individual vDraught = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/ships/"+shipinstance+".owl#V_DraughtLengthOf"+shipinstance);
		Draught.addProperty(hasvalue, vDraught);
		vDraught.setPropertyValue(numval, jenaOwlModel.createTypedLiteral("0"));
		vDraught.addProperty(hasunit, m);
		ship.addProperty(hasDraught, Draught);
		
		Individual Heading = Headingclass.createIndividual("http://www.theworldavatar.com/kb/ships/"+shipinstance+".owl#HeadingOf"+shipinstance);
		Individual vHeading = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/ships/"+shipinstance+".owl#V_HeadingOf"+shipinstance);
		Heading.addProperty(hasvalue, vHeading);
		vHeading.setPropertyValue(numval, jenaOwlModel.createTypedLiteral("0"));
		vHeading.addProperty(hasunit, degree);
		ship.addProperty(hasHeading, Heading);
		
		Individual RateofTurn = RoTclass.createIndividual("http://www.theworldavatar.com/kb/ships/"+shipinstance+".owl#RateOfTurnOf"+shipinstance);
		Individual vRateofTurn = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/ships/"+shipinstance+".owl#V_RateOfTurnOf"+shipinstance);
		RateofTurn.addProperty(hasvalue, vRateofTurn);
		vRateofTurn.setPropertyValue(numval, jenaOwlModel.createTypedLiteral("0"));
		ship.addProperty(hasROT, RateofTurn);
		
		Individual SOG = SOGclass.createIndividual("http://www.theworldavatar.com/kb/ships/"+shipinstance+".owl#SOGOf"+shipinstance);
		Individual vSOG = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/ships/"+shipinstance+".owl#V_SOGOf"+shipinstance);
		SOG.addProperty(hasvalue, vSOG);
		vSOG.setPropertyValue(numval, jenaOwlModel.createTypedLiteral("0"));
		//vSOG.addProperty(hasunit, knot);
		ship.addProperty(hasSOG, SOG);
		
		Individual COG = COGclass.createIndividual("http://www.theworldavatar.com/kb/ships/"+shipinstance+".owl#COGOf"+shipinstance);
		Individual vCOG = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/ships/"+shipinstance+".owl#V_COGhOf"+shipinstance);
		COG.addProperty(hasvalue, vCOG);
		vCOG.setPropertyValue(numval, jenaOwlModel.createTypedLiteral("0"));
		//vCOG.addProperty(hasunit, degree);
		ship.addProperty(hasCOG, COG);
		
		Individual ShipType = ShipTypeclass.createIndividual("http://www.theworldavatar.com/kb/ships/"+shipinstance+".owl#ShipTypeOf"+shipinstance);
		Individual vShipType = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/ships/"+shipinstance+".owl#V_ShipTypeOf"+shipinstance);
		ShipType.addProperty(hasvalue, vShipType);
		vShipType.setPropertyValue(numval, jenaOwlModel.createTypedLiteral("0"));
		ship.addProperty(hasShipType, ShipType);
		
		Individual NavigationalStatus = NaviStatusclass.createIndividual("http://www.theworldavatar.com/kb/ships/"+shipinstance+".owl#NavigationalStatusOf"+shipinstance);
		Individual vNavigationalStatus = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/ships/"+shipinstance+".owl#V_NavigationalStatusOf"+shipinstance);
		NavigationalStatus.addProperty(hasvalue, vNavigationalStatus);
		vNavigationalStatus.setPropertyValue(numval, jenaOwlModel.createTypedLiteral("0"));
		ship.addProperty(hasNavStatus, NavigationalStatus);
		
		Individual PositioningDeviceType = Positioningdevclass.createIndividual("http://www.theworldavatar.com/kb/ships/"+shipinstance+".owl#PositioningDeviceTypeOf"+shipinstance);
		Individual vPositioningDeviceType = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/ships/"+shipinstance+".owl#V_PositioningDeviceTypeOf"+shipinstance);
		PositioningDeviceType.addProperty(hasvalue, vPositioningDeviceType);
		vPositioningDeviceType.setPropertyValue(numval, jenaOwlModel.createTypedLiteral("0"));
		ship.addProperty(hasPosDevType, PositioningDeviceType);
		
		Resource Dest = jenaOwlModel.createResource("http://dbpedia.org/resource/Singapore");
		ship.addProperty(hasDestination, Dest);
		
		Individual shiptemporalcoordinate = temporalcoordinatesystemclass.createIndividual("http://www.theworldavatar.com/kb/ships/"+shipinstance+".owl#TemporalCoordinateSystemOf"+shipinstance);
		Individual shipcoordinate = coordinatesystemclass.createIndividual("http://www.theworldavatar.com/kb/ships/"+shipinstance+".owl#CoordinateSystemOf"+shipinstance);
		Individual xcoordinate = coordinateclass.createIndividual("http://www.theworldavatar.com/kb/ships/"+shipinstance+".owl#xCoordinateOf"+shipinstance);
		Individual ycoordinate = coordinateclass.createIndividual("http://www.theworldavatar.com/kb/ships/"+shipinstance+".owl#yCoordinateOf"+shipinstance);
		Individual tcoordinate = tempcoordinateclass.createIndividual("http://www.theworldavatar.com/kb/ships/"+shipinstance+".owl#timestampOf"+shipinstance);
		Individual xcoordinatevalue = coordinatevalueclass.createIndividual("http://www.theworldavatar.com/kb/ships/"+shipinstance+".owl#V_xCoordinateOf"+shipinstance);
		Individual ycoordinatevalue = coordinatevalueclass.createIndividual("http://www.theworldavatar.com/kb/ships/"+shipinstance+".owl#V_yCoordinateOf"+shipinstance);
		Individual tcoordinatevalue = coordinatevalueclass.createIndividual("http://www.theworldavatar.com/kb/ships/"+shipinstance+".owl#V_timestampOf"+shipinstance);
		shipcoordinate.addProperty(hasx,xcoordinate);
		shipcoordinate.addProperty(hasy,ycoordinate);
		shiptemporalcoordinate.addProperty(hastemporalcoordinate,tcoordinate);
		xcoordinate.addProperty(hasvalue,xcoordinatevalue);
		ycoordinate.addProperty(hasvalue,ycoordinatevalue);
		tcoordinate.addProperty(hasvalue,tcoordinatevalue);
		xcoordinatevalue.setPropertyValue(numval,jenaOwlModel.createTypedLiteral("0"));
		ycoordinatevalue.setPropertyValue(numval,jenaOwlModel.createTypedLiteral("0"));
		tcoordinatevalue.setPropertyValue(numval,jenaOwlModel.createTypedLiteral("0"));
		xcoordinatevalue.addProperty(hasunit, degree);
		ycoordinatevalue.addProperty(hasunit, degree);
		ship.addProperty(hastimestamp, shiptemporalcoordinate);
		ship.addProperty(hascoordinatesystem, shipcoordinate);
		
		Individual MMSI = MMSIclass.createIndividual("http://www.theworldavatar.com/kb/ships/"+shipinstance+".owl#MMSIOf"+shipinstance);
		Individual vMMSI = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/ships/"+shipinstance+".owl#V_MMSIOf"+shipinstance);
		MMSI.addProperty(hasvalue, vMMSI);
		vMMSI.setPropertyValue(numval, jenaOwlModel.createTypedLiteral("0"));
		ship.addProperty(hasMMSI, MMSI);
		
		Individual ETA = ETAclass.createIndividual("http://www.theworldavatar.com/kb/ships/"+shipinstance+".owl#ETAOf"+shipinstance);
		Individual vETA = coordinatevalueclass.createIndividual("http://www.theworldavatar.com/kb/ships/"+shipinstance+".owl#V_ETAOf"+shipinstance);
		ETA.addProperty(hasvalue, vETA);
		vETA.setPropertyValue(numval, jenaOwlModel.createTypedLiteral("0"));
		//vETA.addProperty(hasunit, UTM);
		ship.addProperty(hasETA, ETA);
		
		Individual IMONumber = IMOclass.createIndividual("http://www.theworldavatar.com/kb/ships/"+shipinstance+".owl#IMONumberOf"+shipinstance);
		Individual vIMONumber = scalarvalueclass.createIndividual("http://www.theworldavatar.com/kb/ships/"+shipinstance+".owl#V_IMONumberOf"+shipinstance);
		IMONumber.addProperty(hasvalue, vIMONumber);
		vIMONumber.setPropertyValue(numval, jenaOwlModel.createTypedLiteral("0"));
		ship.addProperty(hasIMONumber, IMONumber);
	}
	
	public void startConversion() throws Exception {
    	
    	String filePath = baseURL + "ShipTemplate.owl"; // the empty owl file
    	
    	int shipamount= 10;
    	

    	//System.out.println ("check="+jenaOwlModel.toString();
    	
    	for (int i=1;i<=shipamount;i++)
    	{
    		FileInputStream inFile = new FileInputStream(filePath);
        	Reader in = new InputStreamReader(inFile, "UTF-8");
    	    			
        	OntModel jenaOwlModel = ModelFactory.createOntologyModel();
        	jenaOwlModel.read(in, null);
    		initOWLClasses(jenaOwlModel);
			String shipinstance="Ship-"+i;
			String filePath2 = baseURL2 +shipinstance+".owl"; // the result of written owl file
			System.out.println("it is processed= "+shipinstance);
			doConversion(jenaOwlModel,shipinstance); //plant,country,owner,fuel,tech,x,y,emission,cost,anngen,capa,age	
			
			/** save the updated model file */
			savefile(jenaOwlModel, filePath2);
		
    	}   	

}
	
	public void savefile(OntModel jenaOwlModel, String filePath2) throws URISyntaxException, FileNotFoundException {

		FileOutputStream out = new FileOutputStream(filePath2);
		
		Collection errors = new ArrayList();
		jenaOwlModel.write(out, "RDF/XML-ABBREV");

		
		System.out.println("File saved with " + errors.size() + " errors.");
	}
	
	public static void main(String[] args) throws Exception {
		
		
		System.out.println("Starting Process");
		ShipKbCreator converter = new ShipKbCreator();
		converter.startConversion();
		

	}
	
	
}

