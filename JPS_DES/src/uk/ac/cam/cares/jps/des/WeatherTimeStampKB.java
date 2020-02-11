package uk.ac.cam.cares.jps.des;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URISyntaxException;
import java.util.List;

import org.apache.jena.ontology.DatatypeProperty;
import org.apache.jena.ontology.Individual;
import org.apache.jena.ontology.ObjectProperty;
import org.apache.jena.ontology.OntClass;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.rdf.model.ModelFactory;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;

public class WeatherTimeStampKB {
	private OntClass mainobjclass = null;
	private OntClass mainobj2class = null;
	private OntClass mainobj3class = null;
	private OntClass tempclass = null;
	private OntClass scalarvalueclass = null;
	private OntClass timeinstanceclass = null;
	private OntClass outsidepropclass = null;
	private OntClass outsidewindspeedclass= null;
	
	private ObjectProperty hasvalue = null;
	private ObjectProperty hasunit = null;
	private ObjectProperty observes = null;
	private ObjectProperty hastime = null;
	private DatatypeProperty timexsdvalue = null;
	private DatatypeProperty numval = null;
	static Individual C;
	static Individual Wperm2;
	static Individual mpers;
	
	
	
	
	public void initOWLClasses(OntModel jenaOwlModel) {
			
		outsidepropclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#OutsideAirProperties");
		outsidewindspeedclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#OutsideWindSpeed");
		mainobjclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#T-Sensor");
		mainobj2class=jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#Q-Sensor");
		mainobj3class=jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#F-Sensor");
		tempclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#OutsideAirTemperature");		
		scalarvalueclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#ScalarValue");	
		timeinstanceclass=jenaOwlModel.getOntClass("http://www.w3.org/2006/time#Instant");	
		observes = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#observes");
		hastime = jenaOwlModel.getObjectProperty("http://www.w3.org/2006/time#hasTime");
		hasvalue = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue");
		hasunit = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasUnitOfMeasure");
		numval = jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");
		timexsdvalue = jenaOwlModel.getDatatypeProperty("http://www.w3.org/2006/time#inXSDDateTimeStamp");
		C=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#Celsius");
		Wperm2=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#W_per_m.m");
		mpers=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#m_per_s");
			
	}
	public void doConversiontempsensor(OntModel jenaOwlModel, String mainobjectname,String Prefix,List<String[]> readingFromCSV) throws FileNotFoundException, URISyntaxException{
		System.out.println("it is processed= " + mainobjectname);
		//String mainobjectname= SGTemperatureSensor-001

		
		
		Individual mainobjinst = mainobjclass.createIndividual(Prefix+mainobjectname+".owl#"+mainobjectname);
		
		Individual outsidetemp = tempclass.createIndividual(Prefix+mainobjectname+".owl#MeasuredTemperatureOf"+mainobjectname);
		mainobjinst.addProperty(observes, outsidetemp);
		for(int x=1;x<=49;x++) {
			String tempvalue=readingFromCSV.get(x-1)[4];
			String year=readingFromCSV.get(x-1)[0];
			String month=readingFromCSV.get(x-1)[1].split("-")[1]; //need to map from june
			String date=readingFromCSV.get(x-1)[1].split("-")[0];
			String time=readingFromCSV.get(x-1)[2];
			String timestampvalue=year+"-"+month+"-"+String.format("%02d", Integer.valueOf(date))+"T"+time+"+08:00";
			
			Individual voutsidetemp = scalarvalueclass.createIndividual(Prefix+mainobjectname+".owl#V_MeasuredTemperatureOf"+mainobjectname+"_"+x);
			Individual timestamptemp = timeinstanceclass.createIndividual(Prefix+mainobjectname+".owl#TimeOfMeasuredTemperatureOf"+mainobjectname+"_"+x);
			outsidetemp.addProperty(hasvalue, voutsidetemp);
			voutsidetemp.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double (tempvalue)));
			voutsidetemp.addProperty(hasunit, C);
			
			voutsidetemp.addProperty(hastime, timestamptemp);
			timestamptemp.setPropertyValue(timexsdvalue, jenaOwlModel.createTypedLiteral(new String (timestampvalue))); //value need to be changed later
			
		}
	}
	
	public void doConversionirradiationsensor(OntModel jenaOwlModel, String mainobjectname2,String Prefix,List<String[]> readingFromCSV) throws FileNotFoundException, URISyntaxException{

		System.out.println("it is processed= " + mainobjectname2);
		
		Individual mainobjinst = mainobj2class.createIndividual(Prefix+mainobjectname2+".owl#"+mainobjectname2);
		
		Individual outsideirradiation = outsidepropclass.createIndividual(Prefix+mainobjectname2+".owl#MeasuredIrradiationOf"+mainobjectname2);
		mainobjinst.addProperty(observes, outsideirradiation);
		for(int x=1;x<=49;x++) {
			String irradiationvalue=readingFromCSV.get(x-1)[8];
			//System.out.println("irradvalue= "+irradiationvalue);
			String year=readingFromCSV.get(x-1)[0];
			String month=readingFromCSV.get(x-1)[1].split("-")[1]; 
			String date=readingFromCSV.get(x-1)[1].split("-")[0];
			String time=readingFromCSV.get(x-1)[2];
			String timestampvalue=year+"-"+month+"-"+String.format("%02d", Integer.valueOf(date))+"T"+time+"+08:00";
			Individual voutsideirradiation = scalarvalueclass.createIndividual(Prefix+mainobjectname2+".owl#V_MeasuredIrradiationOf"+mainobjectname2+"_"+x);
			Individual timestampirradiation = timeinstanceclass.createIndividual(Prefix+mainobjectname2+".owl#TimeOfMeasuredIrradiationOf"+mainobjectname2+"_"+x);
			outsideirradiation.addProperty(hasvalue, voutsideirradiation);
			voutsideirradiation.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double (irradiationvalue)));
			voutsideirradiation.addProperty(hasunit, Wperm2);
			
			voutsideirradiation.addProperty(hastime, timestampirradiation);
			timestampirradiation.setPropertyValue(timexsdvalue, jenaOwlModel.createTypedLiteral(new String(timestampvalue))); //value need to be changed later
			
		}
		
	}
	
	public void doConversionWindSpeedsensor(OntModel jenaOwlModel, String mainobjectname3,String Prefix,List<String[]> readingFromCSV) throws FileNotFoundException, URISyntaxException{

		System.out.println("it is processed= " + mainobjectname3);
		
		Individual mainobjinst = mainobj3class.createIndividual(Prefix+mainobjectname3+".owl#"+mainobjectname3);
		
		Individual outsidewindspeed = outsidewindspeedclass.createIndividual(Prefix+mainobjectname3+".owl#MeasuredWindSpeedOf"+mainobjectname3);
		mainobjinst.addProperty(observes, outsidewindspeed);
		for(int x=1;x<=49;x++) {
			String winspeedvalue=readingFromCSV.get(x-1)[6];
			String year=readingFromCSV.get(x-1)[0];
			String month=readingFromCSV.get(x-1)[1].split("-")[1]; 
			String date=readingFromCSV.get(x-1)[1].split("-")[0];
			String time=readingFromCSV.get(x-1)[2];
			String timestampvalue=year+"-"+month+"-"+String.format("%02d", Integer.valueOf(date))+"T"+time+"+08:00";
			Individual voutsidewindspeed = scalarvalueclass.createIndividual(Prefix+mainobjectname3+".owl#V_MeasuredWindSpeedOf"+mainobjectname3+"_"+x);
			Individual timestampwindpseed = timeinstanceclass.createIndividual(Prefix+mainobjectname3+".owl#TimeOfMeasuredWindSpeedOf"+mainobjectname3+"_"+x);
			outsidewindspeed.addProperty(hasvalue, voutsidewindspeed);
			voutsidewindspeed.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double (winspeedvalue)));
			voutsidewindspeed.addProperty(hasunit, mpers);
			
			voutsidewindspeed.addProperty(hastime, timestampwindpseed);
			timestampwindpseed.setPropertyValue(timexsdvalue, jenaOwlModel.createTypedLiteral(new String(timestampvalue))); //value need to be changed later
			
		}
		
	}

	public String startConversion(List<String[]> readingFromCSV,String flag) throws Exception {
		String baseURL = AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/";
		String filePath = baseURL + "SensorTemp.owl"; // the empty owl file

		FileInputStream inFile = new FileInputStream(filePath);
		Reader in = new InputStreamReader(inFile, "UTF-8");

		OntModel jenaOwlModel = ModelFactory.createOntologyModel();
		jenaOwlModel.read(in, null);
		initOWLClasses(jenaOwlModel);
		
		String mainobjectname = "SGTemperatureSensor-001"; // still hard-coded for the sample
		String mainobject2name = "SGSolarIrradiationSensor-001"; // still hard-coded for the sample
		String mainobject3name = "SGWindSpeedSensor-001"; // still hard-coded for the sample
		String Prefix="http://www.theworldavatar.com/kb/sgp/singapore/";
		String filePath1 = Prefix + mainobjectname + ".owl#"+ mainobjectname; // the result of written owl file
		String filePath2 = Prefix + mainobject2name + ".owl#"+ mainobject2name; // the result of written owl file
		String filePath3 = Prefix + mainobject3name + ".owl#"+ mainobject3name; // the result of written owl file
		
		if (flag.toLowerCase().contains("temperature")) {
			System.out.println("creating temperature");
			doConversiontempsensor(jenaOwlModel, mainobjectname,Prefix,readingFromCSV); 
			String content = JenaHelper.writeToString(jenaOwlModel);
			new QueryBroker().putOld(Prefix+mainobjectname+".owl", content);
			return filePath1;
		}
		else if (flag.toLowerCase().contains("irradiation")) {
			System.out.println("creating irradiation");
			doConversionirradiationsensor(jenaOwlModel, mainobject2name,Prefix,readingFromCSV);
			String content = JenaHelper.writeToString(jenaOwlModel);
			new QueryBroker().putOld(Prefix+mainobject2name+".owl", content);
			return filePath2;
		}
		else {
			System.out.println("creating speed");
			doConversionWindSpeedsensor(jenaOwlModel, mainobject3name,Prefix,readingFromCSV);
			String content = JenaHelper.writeToString(jenaOwlModel);
			new QueryBroker().putOld(Prefix+mainobject3name+".owl", content);
			return filePath3;
		}

	}
	
	public void executeConversion() throws Exception {
		System.out.println("Starting Process");
		WeatherTimeStampKB converter = new WeatherTimeStampKB();
		String csv = new QueryBroker().readFileLocal(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/Weather.csv");
		List<String[]> readingFromCSV = MatrixConverter.fromCsvToArray(csv);
		//String baseURL2 = AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/";
		converter.startConversion(readingFromCSV,"temperature");
		converter.startConversion(readingFromCSV,"irradiation");
		converter.startConversion(readingFromCSV,"windspeed");
		
		
	}
	public String startConversionForecast(List<String[]> readingFromCSV,String flag) throws Exception {
		String baseURL = AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/";
		String filePath = baseURL + "SensorTemp.owl"; // the empty owl file

		FileInputStream inFile = new FileInputStream(filePath);
		Reader in = new InputStreamReader(inFile, "UTF-8");

		OntModel jenaOwlModel = ModelFactory.createOntologyModel();
		jenaOwlModel.read(in, null);
		initOWLClasses(jenaOwlModel);
		
		String Prefix="http://www.theworldavatar.com/kb/sgp/singapore/";
		
		if (flag.toLowerCase().contains("temperature")) {
			System.out.println("creating temperature");
			String mainobjectname = "SGTemperatureForecast-001"; // still hard-coded for the sample
			String filePath1 = Prefix + mainobjectname + ".owl#"+ mainobjectname; // the result of written owl file
			doConversionforecast(jenaOwlModel, mainobjectname,Prefix,readingFromCSV, "Temperature", C); 
			String content = JenaHelper.writeToString(jenaOwlModel);
			new QueryBroker().putOld(Prefix+mainobjectname+".owl", content);
			return filePath1;
		}
		else if (flag.toLowerCase().contains("irradiation")) {
			System.out.println("creating irradiation");
			String mainobject2name = "SGSolarIrradiationForecast-001"; // still hard-coded for the sample
			String filePath2 = Prefix + mainobject2name + ".owl#"+ mainobject2name; // the result of written owl file
			doConversionforecast(jenaOwlModel, mainobject2name,Prefix,readingFromCSV, "Irradiation", Wperm2);
			String content = JenaHelper.writeToString(jenaOwlModel);
			new QueryBroker().putOld(Prefix+mainobject2name+".owl", content);
			return filePath2;
		}
		else {
			System.out.println("creating speed");
			String mainobject3name = "SGWindSpeedForecast-001"; // still hard-coded for the sample
			String filePath3 = Prefix + mainobject3name + ".owl#"+ mainobject3name; // the result of written owl file
			doConversionforecast(jenaOwlModel, mainobject3name,Prefix,readingFromCSV,"WindSpeed", mpers );
			String content = JenaHelper.writeToString(jenaOwlModel);
			new QueryBroker().putOld(Prefix+mainobject3name+".owl", content);
			return filePath3;
		}
	}
	public void doConversionforecast(OntModel jenaOwlModel, String mainobjectname2,String Prefix,List<String[]> readingFromCSV, String typeOfW, Individual uniq) throws FileNotFoundException, URISyntaxException{

		System.out.println("it is processed= " + mainobjectname2);

		Individual mainobjinst = mainobj2class.createIndividual(Prefix+mainobjectname2+".owl#"+mainobjectname2);
		
		Individual outsideirradiation = outsidepropclass.createIndividual(Prefix+mainobjectname2+".owl#Measured"+typeOfW+"Of"+mainobjectname2);
		mainobjinst.addProperty(observes, outsideirradiation);
		for(int x=1;x<=24;x++) {
			String irradiationvalue=readingFromCSV.get(x-1)[8];
			String year=readingFromCSV.get(x-1)[0];
			String month=readingFromCSV.get(x-1)[1].split("-")[1]; 
			String date=readingFromCSV.get(x-1)[1].split("-")[0];
			String time=readingFromCSV.get(x-1)[2];
			String timestampvalue=year+"-"+month+"-"+String.format("%02d", Integer.valueOf(date))+"T"+time+"+08:00";
			Individual voutsideirradiation = scalarvalueclass.createIndividual(Prefix+mainobjectname2+".owl#V_Measured"+typeOfW+"Of"+mainobjectname2+"_"+x);
			Individual timestampirradiation = timeinstanceclass.createIndividual(Prefix+mainobjectname2+".owl#TimeOfMeasured"+typeOfW+"Of"+mainobjectname2+"_"+x);
			outsideirradiation.addProperty(hasvalue, voutsideirradiation);
			voutsideirradiation.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double (irradiationvalue)));
			voutsideirradiation.addProperty(hasunit, uniq);
			
			voutsideirradiation.addProperty(hastime, timestampirradiation);
			timestampirradiation.setPropertyValue(timexsdvalue, jenaOwlModel.createTypedLiteral(new String(timestampvalue))); //value need to be changed later
			
		}
	}
	
	public static void main(String[] args) throws Exception {
		
		new WeatherTimeStampKB().executeConversion();


	}
	

}
