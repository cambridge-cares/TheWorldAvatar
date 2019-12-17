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

import uk.ac.cam.cares.jps.base.config.AgentLocator;

public class WeatherTimeStampKB {
	private OntClass mainobjclass = null;
	private OntClass mainobj2class = null;
	private OntClass tempclass = null;
	private OntClass scalarvalueclass = null;
	private OntClass timeinstanceclass = null;
	private OntClass outsidepropclass = null;
	
	private ObjectProperty hasvalue = null;
	private ObjectProperty hasunit = null;
	private ObjectProperty observes = null;
	private ObjectProperty hastime = null;
	private DatatypeProperty timexsdvalue = null;
	private DatatypeProperty numval = null;
	static Individual C;
	static Individual Wperm2;
	
	
	
	
	public void initOWLClasses(OntModel jenaOwlModel) {
			
		outsidepropclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#OutsideAirProperties");
		mainobjclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#T-Sensor");
		mainobj2class=jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#Q-Sensor");
		tempclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#OutsideAirTemperature");		
		scalarvalueclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#ScalarValue");	
		timeinstanceclass=jenaOwlModel.getOntClass("http://www.w3.org/2006/time#Instant");
		
		
		//		coordinatesystemclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#ProjectedCoordinateSystem");
//		temporalcoordinatesystemclass =jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#TemporalCoordinateSystem");
//		coordinateclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#AngularCoordinate");
//		tempcoordinateclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#TemporalCoordinate");
//		coordinatevalueclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl#CoordinateValue");
//		convectivemassclass=jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#ConvectiveMassFlowrate");	
//		pressureclass=jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#Pressure");
		
		observes = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#observes");
		
//		hasCOG = jenaOwlModel.getObjectProperty(ontologymainiri+"hasCOG");
//		hasDraught = jenaOwlModel.getObjectProperty(ontologymainiri+"hasDraught");
//		hasHeading = jenaOwlModel.getObjectProperty(ontologymainiri+"hasHeading");
//		hasNavStatus = jenaOwlModel.getObjectProperty(ontologymainiri+"hasNavigationalStatus");
		hastime = jenaOwlModel.getObjectProperty("http://www.w3.org/2006/time#hasTime");
			
//		hascoordinatesystem = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasGISCoordinateSystem");
//		hasx = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasProjectedCoordinate_x");
//		hasy = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasProjectedCoordinate_y");
		hasvalue = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue");
		hasunit = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasUnitOfMeasure");

//		hasProperty = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasProperty");
		numval = jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");
		timexsdvalue = jenaOwlModel.getDatatypeProperty("http://www.w3.org/2006/time#inXSDDateTimeStamp");

//		degree=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#degree");
//		bar=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#bar");
//		jperkgk=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#J_per_kg.K");
		C=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#Celsius");
		Wperm2=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#W_per_m.m");
			
	}
	public void doConversiontempsensor(OntModel jenaOwlModel, String mainobjectname) throws FileNotFoundException, URISyntaxException{

		//String mainobjectname= SGTemperatureSensor-001

		String Prefix="http://www.theworldavatar.com/kb/sgp/singapore/";
		//ObjectProperty[] relationobject= {hastemperature};
		
		Individual mainobjinst = mainobjclass.createIndividual(Prefix+mainobjectname+".owl#"+mainobjectname);
		
		Individual outsidetemp = tempclass.createIndividual(Prefix+mainobjectname+".owl#MeasuredTemperatureOf"+mainobjectname);
		mainobjinst.addProperty(observes, outsidetemp);
		for(int x=1;x<=48;x++) {
			Individual voutsidetemp = scalarvalueclass.createIndividual(Prefix+mainobjectname+".owl#V_MeasuredTemperatureOf"+mainobjectname+"_"+x);
			Individual timestamptemp = scalarvalueclass.createIndividual(Prefix+mainobjectname+".owl#TimeOfMeasuredTemperatureOf"+mainobjectname+"_"+x);
			outsidetemp.addProperty(hasvalue, voutsidetemp);
			voutsidetemp.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double (0.7)));
			voutsidetemp.addProperty(hasunit, C);
			
			voutsidetemp.addProperty(hastime, timestamptemp);
			timestamptemp.setPropertyValue(timexsdvalue, jenaOwlModel.createTypedLiteral(new Double (0.7))); //value need to be changed later
			
		}
		String baseURL2 = AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/";
		String filePath1 = baseURL2 + mainobjectname + ".owl"; // the result of written owl file
	}
	
	public void doConversionirradiationsensor(OntModel jenaOwlModel, String mainobjectname2) throws FileNotFoundException, URISyntaxException{

		//String mainobjectname= SGSolarIrradiationSensor-001

		String Prefix="http://www.theworldavatar.com/kb/sgp/singapore/";
		//ObjectProperty[] relationobject= {hastemperature};
		
		Individual mainobjinst = mainobj2class.createIndividual(Prefix+mainobjectname2+".owl#"+mainobjectname2);
		
		Individual outsideirradiation = outsidepropclass.createIndividual(Prefix+mainobjectname2+".owl#MeasuredIrradiationOf"+mainobjectname2);
		mainobjinst.addProperty(observes, outsideirradiation);
		for(int x=1;x<=48;x++) {
			Individual voutsideirradiation = scalarvalueclass.createIndividual(Prefix+mainobjectname2+".owl#V_MeasuredIrradiationOf"+mainobjectname2+"_"+x);
			Individual timestampirradiation = scalarvalueclass.createIndividual(Prefix+mainobjectname2+".owl#TimeOfMeasuredIrradiationOf"+mainobjectname2+"_"+x);
			outsideirradiation.addProperty(hasvalue, voutsideirradiation);
			voutsideirradiation.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double (0.7)));
			voutsideirradiation.addProperty(hasunit, Wperm2);
			
			voutsideirradiation.addProperty(hastime, timestampirradiation);
			timestampirradiation.setPropertyValue(timexsdvalue, jenaOwlModel.createTypedLiteral(new Double (0.7))); //value need to be changed later
			
		}
		String baseURL2 = AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/";
		String mainobject2name = "SGSolarIrradiationSensor-001"; // still hard-coded for the sample
		String filePath2 = baseURL2 + mainobject2name + ".owl"; // the result of written owl file
		
	}

	public void startConversion() throws Exception {
		String baseURL2 = AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/";
		String filePath = baseURL2 + "SensorTemp.owl"; // the empty owl file
		
		// int shipamount= 10;

		// for (int i = 1; i <= shipamount; i++) {
		FileInputStream inFile = new FileInputStream(filePath);
		Reader in = new InputStreamReader(inFile, "UTF-8");

		OntModel jenaOwlModel = ModelFactory.createOntologyModel();
		jenaOwlModel.read(in, null);
		initOWLClasses(jenaOwlModel);

		String mainobjectname = "SGTemperatureSensor-001"; // still hard-coded for the sample
		String mainobject2name = "SGSolarIrradiationSensor-001"; // still hard-coded for the sample
		String filePath1 = baseURL2 + mainobjectname + ".owl"; // the result of written owl file
		String filePath2 = baseURL2 + mainobject2name + ".owl"; // the result of written owl file
		System.out.println("it is processed= " + mainobjectname);
		//doConversiontempsensor(jenaOwlModel, mainobjectname); // plant,country,owner,fuel,tech,x,y,emission,cost,anngen,capa,age
		doConversionirradiationsensor(jenaOwlModel, mainobject2name);
		savefile(jenaOwlModel, filePath2);
		//savefile(jenaOwlModel, filePath1);
		

	}
	public void savefile(OntModel jenaOwlModel, String filePath2) throws URISyntaxException, FileNotFoundException {

		FileOutputStream out = new FileOutputStream(filePath2);
		
		Collection errors = new ArrayList();
		jenaOwlModel.write(out, "RDF/XML-ABBREV");

		
		System.out.println("File saved with " + errors.size() + " errors.");
	}
	
	public static void main(String[] args) throws Exception {
		
		
		System.out.println("Starting Process");
		WeatherTimeStampKB converter = new WeatherTimeStampKB();
		converter.startConversion();
		

	}
	

}
