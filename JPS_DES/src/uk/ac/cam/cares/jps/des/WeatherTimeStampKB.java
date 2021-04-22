package uk.ac.cam.cares.jps.des;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URISyntaxException;
import java.util.List;

import org.apache.jena.datatypes.xsd.XSDDatatype;
import org.apache.jena.ontology.DatatypeProperty;
import org.apache.jena.ontology.Individual;
import org.apache.jena.ontology.ObjectProperty;
import org.apache.jena.ontology.OntClass;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.rdf.model.ModelFactory;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;
/** Old method no longer used by WeatherIrradiationRetriever and Forecast Agent. 
 * Use only when IrradiationSensor/Forecast is gone
 * 
 *
 *
 */
public class WeatherTimeStampKB { //control which location from
	private OntClass mainobjclass = null;
	private OntClass mainobj2class = null;
	private OntClass mainobj3class = null;
	private OntClass tempclass = null;
	private OntClass scalarvalueclass = null;
	private OntClass timeinstanceclass = null;
	private OntClass outsidepropclass = null;
	private OntClass outsidewindspeedclass= null;
	private OntClass outsidewinddirectionclass= null;
	private OntClass outsiderelativehumidityclass= null;
	private OntClass outsideprecipitationclass= null;
	private OntClass outsidepressureclass= null;
	private OntClass outsidecloudcoverclass= null;
	private OntClass coordinatesystemclass = null;
	private OntClass coordinateclass = null;
	private OntClass coordinatevalueclass = null;
	
	private ObjectProperty hasvalue = null;
	private ObjectProperty hasunit = null;
	private ObjectProperty observes = null;
	private ObjectProperty hastime = null;
	private ObjectProperty hascoordinatesystem = null;
	private ObjectProperty hasx = null;
	private ObjectProperty hasy = null;
	private ObjectProperty hasz = null;
	private DatatypeProperty timexsdvalue = null;
	private DatatypeProperty numval = null;
	static Individual C;
	static Individual Wperm2;
	static Individual mpers;
	static Individual mm;
	static Individual m;
	static Individual mBar;
	static Individual degree;
	
	
	public void initOWLClasses(OntModel jenaOwlModel) {
		coordinateclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#AngularCoordinate");
		coordinatevalueclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl#CoordinateValue");
		coordinatesystemclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#ProjectedCoordinateSystem");
		outsidepropclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#OutsideAirProperties");
		outsidewindspeedclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#OutsideWindSpeed");
		outsidewinddirectionclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#OutsideWindDirection");
		outsidecloudcoverclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#OutsideAirCloudCover");
		outsidepressureclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#OutsideAirPressure");
		outsideprecipitationclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#OutsideAirPrecipitation");
		outsiderelativehumidityclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#OutsideAirRelativeHumidity");
		mainobjclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#T-Sensor");
		mainobj2class=jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#Q-Sensor");
		mainobj3class=jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#F-Sensor");
		tempclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#OutsideAirTemperature");		
		scalarvalueclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#ScalarValue");	
		timeinstanceclass=jenaOwlModel.getOntClass("http://www.w3.org/2006/time#Instant");	
		observes = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#observes");
		hastime = jenaOwlModel.getObjectProperty("http://www.w3.org/2006/time#hasTime");
		hascoordinatesystem = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasGISCoordinateSystem");
		hasx = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasProjectedCoordinate_x");
		hasy = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasProjectedCoordinate_y");
		hasz = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasProjectedCoordinate_z");
		hasvalue = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue");
		hasunit = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasUnitOfMeasure");
		numval = jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");
		timexsdvalue = jenaOwlModel.getDatatypeProperty("http://www.w3.org/2006/time#inXSDDateTime");
		C=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#Celsius");
		Wperm2=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#W_per_m.m");
		mpers=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#m_per_s");
		mm=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#mm");
		mBar=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#mBar");
		degree=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#degree");
		m=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/SI_unit.owl#m");
	}
	
	public void executeConversion() throws Exception { //control which location to be created
		System.out.println("Starting Process");
		WeatherTimeStampKB converter = new WeatherTimeStampKB();
		String csv = new QueryBroker().readFileLocal(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/Weather.csv");
		List<String[]> readingFromCSV = MatrixConverter.fromCsvToArray(csv);

		
		//for SG:
//		int numberofstn=14;
//		String locationid="SG";
		
		//for Berlin:
//		int numberofstn=1;
//		String locationid="DE";
		
		//for THeHague:
		int numberofstn=1;
		String locationid="NL";
			
		
		//for HK:
//		String csvhk = new QueryBroker().readFileLocal(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/1hrweatherhistory.csv");
//		List<String[]> readingFromCSVHK = MatrixConverter.fromCsvToArray(csvhk);
//		readingFromCSVHK.remove(0);//move the header
//		int numberofstn=readingFromCSVHK.size();
//		String locationid="HK";
		
//		for(int d=1;d<=numberofstn;d++) {
//			String number="00"+d;
//			if(d>9&&d<100) {
//				number="0"+d;
//			}else if(d>99) {
//				number=""+d;
//			}
//			converter.startConversion(readingFromCSV,"relativehumidity",number,locationid);
//			converter.startConversion(readingFromCSV,"windspeed",number,locationid);
//			converter.startConversion(readingFromCSV,"winddirection",number,locationid);
//			converter.startConversion(readingFromCSV,"precipitation",number,locationid);
//			converter.startConversion(readingFromCSV,"temperature",number,locationid);
//			converter.startConversion(readingFromCSV,"cloudcover",number,locationid);
//			converter.startConversion(readingFromCSV,"pressure",number,locationid);
//		}
		converter.startConversion(readingFromCSV,"temperature","015","SG");
		//converter.startConversion(readingFromCSV,"irradiation","001");
		converter.startConversion(readingFromCSV,"windspeed","015","SG");
		converter.startConversion(readingFromCSV,"winddirection","015","SG");
		converter.startConversion(readingFromCSV,"precipitation","015","SG");
		converter.startConversion(readingFromCSV,"pressure","015","SG");
		converter.startConversion(readingFromCSV,"relativehumidity","015","SG");
		converter.startConversion(readingFromCSV,"cloudcover","015","SG");
	}
	
	public void doConversiontempsensor(OntModel jenaOwlModel, String mainobjectname,String Prefix,List<String[]> readingFromCSV,String[]location) throws FileNotFoundException, URISyntaxException{
		System.out.println("it is processed= " + mainobjectname);
		//String mainobjectname= SGTemperatureSensor-001

		
		
		Individual mainobjinst = mainobjclass.createIndividual(Prefix+mainobjectname+".owl#"+mainobjectname);
		
		Individual outsidetemp = tempclass.createIndividual(Prefix+mainobjectname+".owl#MeasuredTemperatureOf"+mainobjectname);
		mainobjinst.addProperty(observes, outsidetemp);
		
		Individual fccoordinate = coordinatesystemclass.createIndividual(Prefix+mainobjectname+".owl#CoordinateSystemOf"+mainobjectname);
		Individual xcoordinate = coordinateclass.createIndividual(Prefix+mainobjectname+".owl#xCoordinateOf"+mainobjectname);
		Individual ycoordinate = coordinateclass.createIndividual(Prefix+mainobjectname+".owl#yCoordinateOf"+mainobjectname);
		Individual zcoordinate = coordinateclass.createIndividual(Prefix+mainobjectname+".owl#zCoordinateOf"+mainobjectname);
		Individual zcoordinatevalue = coordinatevalueclass.createIndividual(Prefix+mainobjectname+".owl#V_zCoordinateOf"+mainobjectname);
		Individual xcoordinatevalue = coordinatevalueclass.createIndividual(Prefix+mainobjectname+".owl#V_xCoordinateOf"+mainobjectname);
		Individual ycoordinatevalue = coordinatevalueclass.createIndividual(Prefix+mainobjectname+".owl#V_yCoordinateOf"+mainobjectname);
		fccoordinate.addProperty(hasx,xcoordinate);
		fccoordinate.addProperty(hasy,ycoordinate);
		fccoordinate.addProperty(hasz,zcoordinate);
		xcoordinate.addProperty(hasvalue,xcoordinatevalue);
		ycoordinate.addProperty(hasvalue,ycoordinatevalue);
		zcoordinate.addProperty(hasvalue,zcoordinatevalue);
		xcoordinatevalue.setPropertyValue(numval,jenaOwlModel.createTypedLiteral(location[0]));
		ycoordinatevalue.setPropertyValue(numval,jenaOwlModel.createTypedLiteral(location[1]));
		zcoordinatevalue.setPropertyValue(numval,jenaOwlModel.createTypedLiteral(location[2]));
		xcoordinatevalue.addProperty(hasunit, degree);
		ycoordinatevalue.addProperty(hasunit, degree);
		zcoordinatevalue.addProperty(hasunit, m);
		mainobjinst.addProperty(hascoordinatesystem, fccoordinate);
		
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
			timestamptemp.setPropertyValue(timexsdvalue, jenaOwlModel.createTypedLiteral(timestampvalue,XSDDatatype.XSDdateTime)); //value need to be changed later
			
		}
	}
	
	public void doConversionirradiationsensor(OntModel jenaOwlModel, String mainobjectname2,String Prefix,List<String[]> readingFromCSV,String[]location) throws FileNotFoundException, URISyntaxException{

		System.out.println("it is processed= " + mainobjectname2);
		
		Individual mainobjinst = mainobj2class.createIndividual(Prefix+mainobjectname2+".owl#"+mainobjectname2);
		
		Individual outsideirradiation = outsidepropclass.createIndividual(Prefix+mainobjectname2+".owl#MeasuredIrradiationOf"+mainobjectname2);
		mainobjinst.addProperty(observes, outsideirradiation);
		Individual fccoordinate = coordinatesystemclass.createIndividual(Prefix+mainobjectname2+".owl#CoordinateSystemOf"+mainobjectname2);
		Individual xcoordinate = coordinateclass.createIndividual(Prefix+mainobjectname2+".owl#xCoordinateOf"+mainobjectname2);
		Individual ycoordinate = coordinateclass.createIndividual(Prefix+mainobjectname2+".owl#yCoordinateOf"+mainobjectname2);
		Individual zcoordinate = coordinateclass.createIndividual(Prefix+mainobjectname2+".owl#zCoordinateOf"+mainobjectname2);
		Individual zcoordinatevalue = coordinatevalueclass.createIndividual(Prefix+mainobjectname2+".owl#V_zCoordinateOf"+mainobjectname2);
		Individual xcoordinatevalue = coordinatevalueclass.createIndividual(Prefix+mainobjectname2+".owl#V_xCoordinateOf"+mainobjectname2);
		Individual ycoordinatevalue = coordinatevalueclass.createIndividual(Prefix+mainobjectname2+".owl#V_yCoordinateOf"+mainobjectname2);
		fccoordinate.addProperty(hasx,xcoordinate);
		fccoordinate.addProperty(hasy,ycoordinate);
		fccoordinate.addProperty(hasz,zcoordinate);
		xcoordinate.addProperty(hasvalue,xcoordinatevalue);
		ycoordinate.addProperty(hasvalue,ycoordinatevalue);
		zcoordinate.addProperty(hasvalue,zcoordinatevalue);
		xcoordinatevalue.setPropertyValue(numval,jenaOwlModel.createTypedLiteral(location[0]));
		ycoordinatevalue.setPropertyValue(numval,jenaOwlModel.createTypedLiteral(location[1]));
		zcoordinatevalue.setPropertyValue(numval,jenaOwlModel.createTypedLiteral(location[2]));
		xcoordinatevalue.addProperty(hasunit, degree);
		ycoordinatevalue.addProperty(hasunit, degree);
		zcoordinatevalue.addProperty(hasunit, m);
		mainobjinst.addProperty(hascoordinatesystem, fccoordinate);
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
			timestampirradiation.setPropertyValue(timexsdvalue, jenaOwlModel.createTypedLiteral(timestampvalue,XSDDatatype.XSDdateTime)); //value need to be changed later
			
		}
		
	}
	
	public void doConversionWindSpeedsensor(OntModel jenaOwlModel, String mainobjectname3,String Prefix,List<String[]> readingFromCSV,String[]location) throws FileNotFoundException, URISyntaxException{

		System.out.println("it is processed= " + mainobjectname3);
		
		Individual mainobjinst = mainobj3class.createIndividual(Prefix+mainobjectname3+".owl#"+mainobjectname3);
		
		Individual outsidewindspeed = outsidewindspeedclass.createIndividual(Prefix+mainobjectname3+".owl#MeasuredWindSpeedOf"+mainobjectname3);
		mainobjinst.addProperty(observes, outsidewindspeed);
		Individual fccoordinate = coordinatesystemclass.createIndividual(Prefix+mainobjectname3+".owl#CoordinateSystemOf"+mainobjectname3);
		Individual xcoordinate = coordinateclass.createIndividual(Prefix+mainobjectname3+".owl#xCoordinateOf"+mainobjectname3);
		Individual ycoordinate = coordinateclass.createIndividual(Prefix+mainobjectname3+".owl#yCoordinateOf"+mainobjectname3);
		Individual zcoordinate = coordinateclass.createIndividual(Prefix+mainobjectname3+".owl#zCoordinateOf"+mainobjectname3);
		Individual zcoordinatevalue = coordinatevalueclass.createIndividual(Prefix+mainobjectname3+".owl#V_zCoordinateOf"+mainobjectname3);
		Individual xcoordinatevalue = coordinatevalueclass.createIndividual(Prefix+mainobjectname3+".owl#V_xCoordinateOf"+mainobjectname3);
		Individual ycoordinatevalue = coordinatevalueclass.createIndividual(Prefix+mainobjectname3+".owl#V_yCoordinateOf"+mainobjectname3);
		fccoordinate.addProperty(hasx,xcoordinate);
		fccoordinate.addProperty(hasy,ycoordinate);
		fccoordinate.addProperty(hasz,zcoordinate);
		xcoordinate.addProperty(hasvalue,xcoordinatevalue);
		ycoordinate.addProperty(hasvalue,ycoordinatevalue);
		zcoordinate.addProperty(hasvalue,zcoordinatevalue);
		xcoordinatevalue.setPropertyValue(numval,jenaOwlModel.createTypedLiteral(location[0]));
		ycoordinatevalue.setPropertyValue(numval,jenaOwlModel.createTypedLiteral(location[1]));
		zcoordinatevalue.setPropertyValue(numval,jenaOwlModel.createTypedLiteral(location[2]));
		xcoordinatevalue.addProperty(hasunit, degree);
		ycoordinatevalue.addProperty(hasunit, degree);
		zcoordinatevalue.addProperty(hasunit, m);
		mainobjinst.addProperty(hascoordinatesystem, fccoordinate);
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
			timestampwindpseed.setPropertyValue(timexsdvalue, jenaOwlModel.createTypedLiteral(timestampvalue,XSDDatatype.XSDdateTime)); //value need to be changed later
			
		}
		
	}
	
	public void doConversionWeathersensor(OntModel jenaOwlModel, String mainobjectname,String Prefix,List<String[]> readingFromCSV,String propertyname,OntClass propclass,String[]location) throws FileNotFoundException, URISyntaxException{
		System.out.println("it is processed= " + mainobjectname);
		//String mainobjectname= SGTemperatureSensor-001 (sample)

		Individual mainobjinst = mainobj2class.createIndividual(Prefix+mainobjectname+".owl#"+mainobjectname);
		
		Individual measuredproperty = propclass.createIndividual(Prefix+mainobjectname+".owl#Measured"+propertyname+"Of"+mainobjectname);
		mainobjinst.addProperty(observes, measuredproperty);
		
		Individual fccoordinate = coordinatesystemclass.createIndividual(Prefix+mainobjectname+".owl#CoordinateSystemOf"+mainobjectname);
		Individual xcoordinate = coordinateclass.createIndividual(Prefix+mainobjectname+".owl#xCoordinateOf"+mainobjectname);
		Individual ycoordinate = coordinateclass.createIndividual(Prefix+mainobjectname+".owl#yCoordinateOf"+mainobjectname);
		Individual zcoordinate = coordinateclass.createIndividual(Prefix+mainobjectname+".owl#zCoordinateOf"+mainobjectname);
		Individual zcoordinatevalue = coordinatevalueclass.createIndividual(Prefix+mainobjectname+".owl#V_zCoordinateOf"+mainobjectname);
		Individual xcoordinatevalue = coordinatevalueclass.createIndividual(Prefix+mainobjectname+".owl#V_xCoordinateOf"+mainobjectname);
		Individual ycoordinatevalue = coordinatevalueclass.createIndividual(Prefix+mainobjectname+".owl#V_yCoordinateOf"+mainobjectname);
		fccoordinate.addProperty(hasx,xcoordinate);
		fccoordinate.addProperty(hasy,ycoordinate);
		fccoordinate.addProperty(hasz,zcoordinate);
		xcoordinate.addProperty(hasvalue,xcoordinatevalue);
		ycoordinate.addProperty(hasvalue,ycoordinatevalue);
		zcoordinate.addProperty(hasvalue,zcoordinatevalue);
		xcoordinatevalue.setPropertyValue(numval,jenaOwlModel.createTypedLiteral(location[0]));
		ycoordinatevalue.setPropertyValue(numval,jenaOwlModel.createTypedLiteral(location[1]));
		zcoordinatevalue.setPropertyValue(numval,jenaOwlModel.createTypedLiteral(location[2]));
		xcoordinatevalue.addProperty(hasunit, degree);
		ycoordinatevalue.addProperty(hasunit, degree);
		zcoordinatevalue.addProperty(hasunit, m);
		mainobjinst.addProperty(hascoordinatesystem, fccoordinate);
		
		for(int x=1;x<=49;x++) {
			String propvalue=readingFromCSV.get(x-1)[3]; //assume must be in col 3
			String year=readingFromCSV.get(x-1)[0];
			String month=readingFromCSV.get(x-1)[1].split("-")[1]; //need to map from june
			String date=readingFromCSV.get(x-1)[1].split("-")[0];
			String time=readingFromCSV.get(x-1)[2];
			String timestampvalue=year+"-"+month+"-"+String.format("%02d", Integer.valueOf(date))+"T"+time+"+08:00";
			
			Individual valueofproperty = scalarvalueclass.createIndividual(Prefix+mainobjectname+".owl#V_Measured"+propertyname+"Of"+mainobjectname+"_"+x);
			Individual timestamp = timeinstanceclass.createIndividual(Prefix+mainobjectname+".owl#TimeOfMeasured"+propertyname+"Of"+mainobjectname+"_"+x);
			measuredproperty.addProperty(hasvalue, valueofproperty);
			valueofproperty.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double (propvalue)));
			if(propertyname.toLowerCase().contains("pressure")) {
				valueofproperty.addProperty(hasunit, mBar); //need to be changed according to the propertyname
			}
			else if (propertyname.toLowerCase().contains("precipitation")) {
				valueofproperty.addProperty(hasunit, mm); 
			}
			else if (propertyname.toLowerCase().contains("direction")) {
				valueofproperty.addProperty(hasunit, degree); 
			}
			else if (propertyname.toLowerCase().contains("humidity")||propertyname.toLowerCase().contains("cloud")) {
				// for humidity no unit but when written to csv must be in percent; here just use decimal 
				// for cloudcover no unit; just use decimal
			}
			valueofproperty.addProperty(hastime, timestamp);
			timestamp.setPropertyValue(timexsdvalue, jenaOwlModel.createTypedLiteral(timestampvalue,XSDDatatype.XSDdateTime)); //value need to be changed later
			
		}
		System.out.println("owl file created");
	}

	public String startConversion(List<String[]> readingFromCSV,String flag,String idOfStation,String locationID) throws Exception {
		String baseURL = AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/";
		String filePath = baseURL + "SensorTemp.owl"; // the empty owl file

		FileInputStream inFile = new FileInputStream(filePath);
		Reader in = new InputStreamReader(inFile, "UTF-8");
		String countryname="singapore";
		String inputRef="";
		String Prefix="http://www.theworldavatar.com/kb/sgp/"+countryname+"/";
		OntModel jenaOwlModel = ModelFactory.createOntologyModel();
		jenaOwlModel.read(in, null);
		initOWLClasses(jenaOwlModel);
		if(locationID.contentEquals("SG")) {
			countryname="singapore";
			inputRef=new QueryBroker().readFileLocal(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/sensor weather reference.json");
			Prefix="http://www.theworldavatar.com/kb/sgp/"+countryname+"/";
		}else if(locationID.contentEquals("HK")) {
			countryname="hongkong";
			inputRef=new QueryBroker().readFileLocal(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/1hrweatherhistory.csv");
			Prefix="http://www.theworldavatar.com/kb/hkg/"+countryname+"/";
		}else if(locationID.contentEquals("NL")) {
			countryname="thehague";
			inputRef=new QueryBroker().readFileLocal(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/TheHagueTemplate.csv");
			Prefix="http://www.theworldavatar.com/kb/nld/"+countryname+"/";
		}else if(locationID.contentEquals("DE")) {
			countryname="berlin";
			inputRef=new QueryBroker().readFileLocal(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/BerlinTemplate.csv");
			Prefix="http://www.theworldavatar.com/kb/deu/"+countryname+"/";
		}
		
		String mainobjectname = locationID+"TemperatureSensor-"+idOfStation; // still hard-coded for the sample
		String mainobject2name = locationID+"SolarIrradiationSensor-"+idOfStation; // still hard-coded for the sample
		String mainobject3name = locationID+"WindSpeedSensor-"+idOfStation; // still hard-coded for the sample
		String mainobject4name = locationID+"WindDirectionSensor-"+idOfStation; // still hard-coded for the sample
		String mainobject5name = locationID+"RelativeHumiditySensor-"+idOfStation; // still hard-coded for the sample
		String mainobject6name = locationID+"PrecipitationSensor-"+idOfStation; // still hard-coded for the sample
		String mainobject7name = locationID+"CloudCoverSensor-"+idOfStation; // still hard-coded for the sample
		String mainobject8name = locationID+"PressureSensor-"+idOfStation; // still hard-coded for the sample
		
		String filePath1 = Prefix + mainobjectname + ".owl#"+ mainobjectname; // the result of written owl file
		String filePath2 = Prefix + mainobject2name + ".owl#"+ mainobject2name; // the result of written owl file
		String filePath3 = Prefix + mainobject3name + ".owl#"+ mainobject3name; // the result of written owl file
		String filePath4 = Prefix + mainobject4name + ".owl#"+ mainobject4name; // the result of written owl file
		String filePath5 = Prefix + mainobject5name + ".owl#"+ mainobject5name; // the result of written owl file
		String filePath6 = Prefix + mainobject6name + ".owl#"+ mainobject6name; // the result of written owl file
		String filePath7 = Prefix + mainobject7name + ".owl#"+ mainobject7name; // the result of written owl file
		String filePath8 = Prefix + mainobject8name + ".owl#"+ mainobject8name; // the result of written owl file
		
		String[] location = extractLocationofStation(idOfStation, inputRef,locationID);
		
		if (flag.toLowerCase().contains("temperature")) {
			System.out.println("creating temperature");
			doConversiontempsensor(jenaOwlModel, mainobjectname,Prefix,readingFromCSV,location); 
			String content = JenaHelper.writeToString(jenaOwlModel);
			new QueryBroker().putOld(Prefix+mainobjectname+".owl", content);
			return filePath1;
		}
		else if (flag.toLowerCase().contains("irradiation")) {
			System.out.println("creating irradiation");
			doConversionirradiationsensor(jenaOwlModel, mainobject2name,Prefix,readingFromCSV,location);
			String content = JenaHelper.writeToString(jenaOwlModel);
			new QueryBroker().putOld(Prefix+mainobject2name+".owl", content);
			return filePath2;
		}
		else if(flag.toLowerCase().contains("windspeed")) {
			System.out.println("creating speed");
			doConversionWindSpeedsensor(jenaOwlModel, mainobject3name,Prefix,readingFromCSV,location);
			String content = JenaHelper.writeToString(jenaOwlModel);
			new QueryBroker().putOld(Prefix+mainobject3name+".owl", content);
			return filePath3;
		}
		else if (flag.toLowerCase().contains("winddirection")) {
			System.out.println("creating "+flag);
			doConversionWeathersensor(jenaOwlModel, mainobject4name,Prefix,readingFromCSV,flag,outsidewinddirectionclass,location);
			String content = JenaHelper.writeToString(jenaOwlModel);
			new QueryBroker().putOld(Prefix+mainobject4name+".owl", content);
			return filePath4;
		}
		else if (flag.toLowerCase().contains("relativehumidity")) {
			System.out.println("creating "+flag);
			doConversionWeathersensor(jenaOwlModel, mainobject5name,Prefix,readingFromCSV,flag,outsiderelativehumidityclass,location);
			String content = JenaHelper.writeToString(jenaOwlModel);
			new QueryBroker().putOld(Prefix+mainobject5name+".owl", content);
			return filePath5;
		}
		else if (flag.toLowerCase().contains("precipitation")) {
			System.out.println("creating "+flag);
			doConversionWeathersensor(jenaOwlModel, mainobject6name,Prefix,readingFromCSV,flag,outsideprecipitationclass,location);
			String content = JenaHelper.writeToString(jenaOwlModel);
			new QueryBroker().putOld(Prefix+mainobject6name+".owl", content);
			return filePath6;
		}
		else if (flag.toLowerCase().contains("cloudcover")) {
			System.out.println("creating "+flag);
			doConversionWeathersensor(jenaOwlModel, mainobject7name,Prefix,readingFromCSV,flag,outsidecloudcoverclass,location);
			String content = JenaHelper.writeToString(jenaOwlModel);
			new QueryBroker().putOld(Prefix+mainobject7name+".owl", content);
			return filePath7;
		}
		else if (flag.toLowerCase().contains("pressure")) {
			System.out.println("creating "+flag);
			doConversionWeathersensor(jenaOwlModel, mainobject8name,Prefix,readingFromCSV,flag,outsidepressureclass,location);
			String content = JenaHelper.writeToString(jenaOwlModel);
			new QueryBroker().putOld(Prefix+mainobject8name+".owl", content);
			return filePath8;
		}
		return filePath;
	}
	
	private String[] extractLocationofStation(String id, String refDirFile,String locationID) {
		String []location=new String[3];
		int index=Integer.valueOf(id)-1;
		if(locationID.contains("SG")) {
			if(index<14) {
				JSONObject current= new JSONObject(refDirFile);
				int []indexchosen= {0,1,2,3,4,5,6,7,8,9,10,11,12,13}; //based on json object file because stn 24 is ignored
				String lat1 = current.getJSONObject("metadata").getJSONArray("stations").getJSONObject(indexchosen[index])
						.getJSONObject("location").get("latitude").toString();
				String long1 = current.getJSONObject("metadata").getJSONArray("stations").getJSONObject(indexchosen[index])
						.getJSONObject("location").get("longitude").toString();
				String height1= current.getJSONObject("metadata").getJSONArray("stations").getJSONObject(indexchosen[index])
						.getJSONObject("location").get("height").toString();
				
				location[0]=long1;
				location[1]=lat1;
				location[2]=height1;
			}else if(index==14) {
				//it is for singapore weather station taken from Accuweather data; THIS IS ARBITRARY LOCATION
				location[0]="103.786231";
				location[1]="1.280979";
				location[2]="10";
			}

		}else if(locationID.contains("HK")) {
			List<String[]> readingFromCSV = MatrixConverter.fromCsvToArray(refDirFile);
			readingFromCSV.remove(0);//remove header
			String lat1=readingFromCSV.get(index)[1];
			String long1=readingFromCSV.get(index)[2];
			String height1=readingFromCSV.get(index)[3];
			location[0]=long1;
			location[1]=lat1;
			location[2]=height1;
		}else if(locationID.contains("DE")) {
			List<String[]> readingFromCSV = MatrixConverter.fromCsvToArray(refDirFile);
			readingFromCSV.remove(0);//remove header
			String lat1=readingFromCSV.get(index)[1];
			String long1=readingFromCSV.get(index)[2];
			String height1=readingFromCSV.get(index)[3];
			location[0]=long1;
			location[1]=lat1;
			location[2]=height1;
		}else if(locationID.contains("NL")) {
			List<String[]> readingFromCSV = MatrixConverter.fromCsvToArray(refDirFile);
			readingFromCSV.remove(0);//remove header
			String lat1=readingFromCSV.get(index)[1];
			String long1=readingFromCSV.get(index)[2];
			String height1=readingFromCSV.get(index)[3];
			location[0]=long1;
			location[1]=lat1;
			location[2]=height1;
		}
		return location;
	}
		
	public String startConversionForecast(List<String[]> readingFromCSV,String flag, Integer position) throws Exception {
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
			doConversionforecast(mainobjclass,jenaOwlModel, mainobjectname,Prefix,readingFromCSV, "Temperature", position,C); //4
			String content = JenaHelper.writeToString(jenaOwlModel);
			new QueryBroker().putOld(Prefix+mainobjectname+".owl", content);
			return filePath1;
		}
		else if (flag.toLowerCase().contains("irradiation")) {
			System.out.println("creating irradiation");
			String mainobject2name = "SGSolarIrradiationForecast-001"; // still hard-coded for the sample
			String filePath2 = Prefix + mainobject2name + ".owl#"+ mainobject2name; // the result of written owl file
			doConversionforecast(mainobj2class,jenaOwlModel, mainobject2name,Prefix,readingFromCSV, "Irradiation", position, Wperm2);//8
			String content = JenaHelper.writeToString(jenaOwlModel);
			new QueryBroker().putOld(Prefix+mainobject2name+".owl", content);
			return filePath2;
		}
		else {
			System.out.println("creating speed");
			String mainobject3name = "SGWindSpeedForecast-001"; // still hard-coded for the sample
			String filePath3 = Prefix + mainobject3name + ".owl#"+ mainobject3name; // the result of written owl file
			doConversionforecast(mainobj3class,jenaOwlModel, mainobject3name,Prefix,readingFromCSV,"WindSpeed",position, mpers );//6
			String content = JenaHelper.writeToString(jenaOwlModel);
			new QueryBroker().putOld(Prefix+mainobject3name+".owl", content);
			return filePath3;
		}
	}
	
	public void doConversionforecast(OntClass typeofSensor, OntModel jenaOwlModel, String mainobjectname2,String Prefix,List<String[]> readingFromCSV, String typeOfW, Integer positon,Individual uniq) throws FileNotFoundException, URISyntaxException{

		System.out.println("it is processed= " + mainobjectname2);

		Individual mainobjinst = typeofSensor.createIndividual(Prefix+mainobjectname2+".owl#"+mainobjectname2);
		
		Individual outsideirradiation = outsidepropclass.createIndividual(Prefix+mainobjectname2+".owl#Measured"+typeOfW+"Of"+mainobjectname2);
		mainobjinst.addProperty(observes, outsideirradiation);
		for(int x=1;x<=24;x++) {
			String irradiationvalue=readingFromCSV.get(x-1)[positon];
			String date=readingFromCSV.get(x-1)[4];
			String time=readingFromCSV.get(x-1)[3];
			String timestampvalue= date+"T"+time+"+08:00";
			Individual voutsideirradiation = scalarvalueclass.createIndividual(Prefix+mainobjectname2+".owl#V_Measured"+typeOfW+"Of"+mainobjectname2+"_"+x);
			Individual timestampirradiation = timeinstanceclass.createIndividual(Prefix+mainobjectname2+".owl#TimeOfMeasured"+typeOfW+"Of"+mainobjectname2+"_"+x);
			outsideirradiation.addProperty(hasvalue, voutsideirradiation);
			voutsideirradiation.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double (irradiationvalue)));
			voutsideirradiation.addProperty(hasunit, uniq);
			
			voutsideirradiation.addProperty(hastime, timestampirradiation);
			timestampirradiation.setPropertyValue(timexsdvalue, jenaOwlModel.createTypedLiteral(timestampvalue,XSDDatatype.XSDdateTime)); //value need to be changed later
			
		}
	}
	
	public static void main(String[] args) throws Exception {
		
		new WeatherTimeStampKB().executeConversion();


	}
	

}
