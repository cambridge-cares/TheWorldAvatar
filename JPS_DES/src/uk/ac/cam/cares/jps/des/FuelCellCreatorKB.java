package uk.ac.cam.cares.jps.des;

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


public class FuelCellCreatorKB {
	public static String baseURL2 = "C:/TOMCAT/webapps/ROOT/kb/temporary/"; // directory of output file
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
	private OntClass propertyclass = null;
	private OntClass efficiencyclass = null;
	private OntClass noofcellsclass = null;
	private OntClass Starboardclass = null;
	private OntClass Sternclass = null;
	private OntClass Draughtclass = null;
	private OntClass Headingclass = null;
	private OntClass NaviStatusclass = null;
	private OntClass Positioningdevclass = null;
	private OntClass RoTclass = null;
	private OntClass ShipTypeclass = null;
	private OntClass Destinationclass = null;
	private OntClass mainobjclass = null;
	private OntClass ETAclass = null;
	

	
//	private ObjectProperty hasMMSI = null;
//	private ObjectProperty hasIMONumber = null;
	private ObjectProperty hasEfficiency = null;
//	private ObjectProperty hasCOG = null;
//	private ObjectProperty hasDraught = null;
//	private ObjectProperty hasHeading = null;
//	private ObjectProperty hasNavStatus = null;
	private ObjectProperty hasNumberOfCells = null;
//	private ObjectProperty hasSOG = null;
//	private ObjectProperty hasPosDevType = null;
//	private ObjectProperty hasROT = null;
//	private ObjectProperty hasShipType = null;
//	private ObjectProperty hasStarboardLength = null;
//	private ObjectProperty hasSternLength = null;
//	private ObjectProperty hasDestination = null;
//	private ObjectProperty hasETA = null;
	
//	private ObjectProperty intcharacteristic = null;
//	private ObjectProperty thermbehavior = null;
//	private ObjectProperty hasSubsystem = null;
//	private ObjectProperty hascoordinatesystem = null;
//	private ObjectProperty hasx = null;
//	private ObjectProperty hasy = null;
	private ObjectProperty hasvalue = null;
	private ObjectProperty hasunit = null;
//	private ObjectProperty hastimestamp = null;
//	private ObjectProperty hastemporalcoordinate = null;
	private ObjectProperty realizes = null;
	private ObjectProperty hasProperty = null;
	private ObjectProperty hastemperature = null;
//	private ObjectProperty haspressure = null;
//	private ObjectProperty hasOutput = null;
//	private ObjectProperty referstomaterial = null;
//	private ObjectProperty refertogenamount = null;
//	private ObjectProperty hasHeight = null;
//	private ObjectProperty hasInsideDiameter = null;
	
	private DatatypeProperty upperlimit = null;
	private DatatypeProperty lowerlimit = null;
	private DatatypeProperty numval = null;
	//private DatatypeProperty hasShipname = null;

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
		mainobjclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#FuelCell");
		
//		IMOclass = jenaOwlModel.getOntClass(ontologymainiri+"IMOIdentificationNumber");
//		MMSIclass = jenaOwlModel.getOntClass(ontologymainiri+"MMSI");
//		COGclass = jenaOwlModel.getOntClass(ontologymainiri+"CourseOverGround");
		efficiencyclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#Efficiency");
		noofcellsclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#NumberOfCells");
//		Starboardclass = jenaOwlModel.getOntClass(ontologymainiri+"DimensionOfStarboard");
//		Sternclass = jenaOwlModel.getOntClass(ontologymainiri+"DimensionOfStern");
//		Draughtclass = jenaOwlModel.getOntClass(ontologymainiri+"Draught");
//		Headingclass = jenaOwlModel.getOntClass(ontologymainiri+"Heading");
//		NaviStatusclass = jenaOwlModel.getOntClass(ontologymainiri+"NavigationalStatus");
//		Positioningdevclass = jenaOwlModel.getOntClass(ontologymainiri+"PositioningDeviceType");
//		RoTclass = jenaOwlModel.getOntClass(ontologymainiri+"RateOfTurn");
//		ShipTypeclass = jenaOwlModel.getOntClass(ontologymainiri+"ShipType");
//		SOGclass = jenaOwlModel.getOntClass(ontologymainiri+"SpeedOverGround");
//		Destinationclass = jenaOwlModel.getOntClass(ontologymainiri+"Destination");
//		ETAclass = jenaOwlModel.getOntClass(ontologymainiri+"EstimatedTimeOfArrival");
		
		propertyclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#Property");			
		scalarvalueclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#ScalarValue");	
//		coordinatesystemclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#ProjectedCoordinateSystem");
//		temporalcoordinatesystemclass =jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#TemporalCoordinateSystem");
//		coordinateclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#AngularCoordinate");
//		tempcoordinateclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#TemporalCoordinate");
//		coordinatevalueclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl#CoordinateValue");
//		convectivemassclass=jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#ConvectiveMassFlowrate");	
//		pressureclass=jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#Pressure");
		temperatureclass=jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#Temperature");
//		phaseclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#SinglePhase");
//		mixtureclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/material/substance/substance.owl#Mixture");
//		materialclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/material/material.owl#Material");
//		Cpclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#ThermodynamicStateProperty");
//		matamountclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#MaterialAmount");
//		genamountclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#GeneralizedAmount");
//		wastestreamclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_function/process.owl#NonReusableWasteProduct");
		processclass=jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontopowsys/PowSysFunction.owl#PowerConsumption");
//		Pipeclass=jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#Pipe");
		
//		hasMMSI = jenaOwlModel.getObjectProperty(ontologymainiri+"hasMMSI");
//		hasIMONumber = jenaOwlModel.getObjectProperty(ontologymainiri+"hasIMONumber");
//		hasETA = jenaOwlModel.getObjectProperty(ontologymainiri+"hasETA");
		hasEfficiency = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#hasEfficiency");
		
//		hasCOG = jenaOwlModel.getObjectProperty(ontologymainiri+"hasCOG");
//		hasDraught = jenaOwlModel.getObjectProperty(ontologymainiri+"hasDraught");
//		hasHeading = jenaOwlModel.getObjectProperty(ontologymainiri+"hasHeading");
//		hasNavStatus = jenaOwlModel.getObjectProperty(ontologymainiri+"hasNavigationalStatus");
		hasNumberOfCells = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#hasNumberOfCells");
		
//		hasPosDevType = jenaOwlModel.getObjectProperty(ontologymainiri+"hasPositioningDeviceType");
//		hasROT = jenaOwlModel.getObjectProperty(ontologymainiri+"hasRateOfTurn");
//		hasShipType = jenaOwlModel.getObjectProperty(ontologymainiri+"hasShipType");
//		hasSOG = jenaOwlModel.getObjectProperty(ontologymainiri+"hasSOG");
//		hasStarboardLength = jenaOwlModel.getObjectProperty(ontologymainiri+"hasStarboardLength");
//		hasSternLength = jenaOwlModel.getObjectProperty(ontologymainiri+"hasSternLength");
//		hasDestination = jenaOwlModel.getObjectProperty(ontologymainiri+"hasDestination");
		
//		hascoordinatesystem = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasGISCoordinateSystem");
//		hasx = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasProjectedCoordinate_x");
//		hasy = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasProjectedCoordinate_y");
		hasvalue = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue");
		hasunit = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasUnitOfMeasure");
		hasProperty=jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasProperty");
//		hastemporalcoordinate=jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasTemporalCoordinate");
//		hasSubsystem = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasSubsystem");
		realizes = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#realizes");
//		hasOutput= jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#hasOutput");
//		hasProperty = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasProperty");
		hastemperature = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#has_temperature");
//		haspressure = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#has_pressure");
//		intcharacteristic = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/material/material.owl#intrinsicCharacteristics");
//		thermbehavior = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/material/material.owl#thermodynamicBehavior");
//		refertogenamount = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/chemical_process_system.owl#refersToGeneralizedAmount");
//		referstomaterial=jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#refersToMaterial");
//		hasHeight = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#hasHeight");
//		hasInsideDiameter=jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#hasInsideDiameter");
		
		numval = jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");
		upperlimit = jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#upperLimit");
		lowerlimit = jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#lowerLimit");
		
		//hasShipname = jenaOwlModel.getDatatypeProperty(ontologymainiri+"hasShipName");
		
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
	
	public void doConversion(OntModel jenaOwlModel, String mainobjectname){

		

		String Prefix="http://www.theworldavatar.com/kb/sgp/singapore/singaporeelectricalnetwork/";
		//ObjectProperty[] relationobject= {hastemperature};
		
		Individual mainobjinst = mainobjclass.createIndividual(Prefix+mainobjectname+".owl#"+mainobjectname);
		Individual efficiency = efficiencyclass.createIndividual(Prefix+mainobjectname+".owl#EfficiencyOf"+mainobjectname);
		Individual vefficiency = scalarvalueclass.createIndividual(Prefix+mainobjectname+".owl#V_EfficiencyOf"+mainobjectname);
		efficiency.addProperty(hasvalue, vefficiency);
		vefficiency.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double (0.7)));
		//vefficiency.addProperty(hasunit, m);
		mainobjinst.addProperty(hasEfficiency, efficiency);
		
		Individual nocells = noofcellsclass.createIndividual(Prefix+mainobjectname+".owl#NumberOfCellsOf"+mainobjectname);
		Individual vnoofcells = scalarvalueclass.createIndividual(Prefix+mainobjectname+".owl#V_NumberOfCellsOf"+mainobjectname);
		nocells.addProperty(hasvalue, vnoofcells);
		vnoofcells.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Integer(35)));
		//vnoofcells.addProperty(hasunit, m);
		mainobjinst.addProperty(hasNumberOfCells, nocells);
		
		Individual reactionprocess= processclass.createIndividual(Prefix+mainobjectname+".owl#ElectrolysisOf"+mainobjectname);
		mainobjinst.addProperty(realizes, reactionprocess);
		Individual electrontransferprocess= propertyclass.createIndividual(Prefix+mainobjectname+".owl#ElectronTransferperReactionOf"+mainobjectname);
		reactionprocess.addProperty(hasProperty, electrontransferprocess);
		Individual valueelectrontransferprocess= scalarvalueclass.createIndividual(Prefix+mainobjectname+".owl#V_ElectronTransferperReactionOf"+mainobjectname);
		electrontransferprocess.addProperty(hasvalue, valueelectrontransferprocess);
		valueelectrontransferprocess.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Integer(2)));
		
		Individual temperature = temperatureclass.createIndividual(Prefix+mainobjectname+".owl#OperatingTemperatureOf"+mainobjectname);
		Individual vtemp = scalarvalueclass.createIndividual(Prefix+mainobjectname+".owl#V_OperatingTemperatureOf"+mainobjectname);
		temperature.addProperty(hasvalue, vtemp);
		vtemp.setPropertyValue(upperlimit, jenaOwlModel.createTypedLiteral(new Double (72.0)));
		vtemp.setPropertyValue(lowerlimit, jenaOwlModel.createTypedLiteral(new Double (24.0)));
		vtemp.addProperty(hasunit, C);
		mainobjinst.addProperty(hastemperature, temperature);
		
		
		
		
//		Resource engine = jenaOwlModel.createResource("http://www.theworldavatar.com/kb/ships/Engine-001.owl#Engine-001");
//		mainobjinst.addProperty(hasSubsystem,engine);
		
	}
	
	public void startConversion() throws Exception {
    	
    	String filePath = baseURL2 + "planttemplatekb2.owl"; // the empty owl file
    	
    	//int shipamount= 10;

		//for (int i = 1; i <= shipamount; i++) {
			FileInputStream inFile = new FileInputStream(filePath);
			Reader in = new InputStreamReader(inFile, "UTF-8");

			OntModel jenaOwlModel = ModelFactory.createOntologyModel();
			jenaOwlModel.read(in, null);
			initOWLClasses(jenaOwlModel);
			String mainobjectname="FuelCell-001"; //still hard-coded for the sample
			String filePath2 = baseURL2 + mainobjectname + ".owl"; // the result of written owl file
			System.out.println("it is processed= " + mainobjectname);
			doConversion(jenaOwlModel, mainobjectname); // plant,country,owner,fuel,tech,x,y,emission,cost,anngen,capa,age

			/** save the updated model file */
			savefile(jenaOwlModel, filePath2);

		//}  	

}
	
	public void savefile(OntModel jenaOwlModel, String filePath2) throws URISyntaxException, FileNotFoundException {

		FileOutputStream out = new FileOutputStream(filePath2);
		
		Collection errors = new ArrayList();
		jenaOwlModel.write(out, "RDF/XML-ABBREV");

		
		System.out.println("File saved with " + errors.size() + " errors.");
	}
	
	public static void main(String[] args) throws Exception {
		
		
		System.out.println("Starting Process");
		FuelCellCreatorKB converter = new FuelCellCreatorKB();
		converter.startConversion();
		

	}

}
