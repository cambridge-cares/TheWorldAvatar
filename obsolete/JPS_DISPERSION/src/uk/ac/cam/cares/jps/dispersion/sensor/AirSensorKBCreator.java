package uk.ac.cam.cares.jps.dispersion.sensor;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URISyntaxException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;

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


public class AirSensorKBCreator {
	
	public  AirSensorKBCreator() {
		
		AirSensorConfig config= new AirSensorConfig();
		numberofdataslot=config.getNumberOfDataSlot();
		xloc=config.getSensorXLocation();
		yloc=config.getSensorYLocation();
		zloc= config.getSensorZLocation();
	}
	private OntClass mainobjclass = null;
	private OntClass mainobj2class = null;
	private OntClass mainobj3class = null;
	private OntClass tempclass = null;
	private OntClass scalarvalueclass = null;
	private OntClass timeinstanceclass = null;
	private OntClass timeentityclass = null;
	private OntClass outsidewindspeedclass= null;
	private OntClass outsidewinddirectionclass= null;
	private OntClass outsiderelativehumidityclass= null;
	private OntClass outsideprecipitationclass= null;
	private OntClass outsidepressureclass= null;
	private OntClass outsidecloudcoverclass= null;
	private OntClass coordinatesystemclass = null;
	private OntClass coordinateclass = null;
	private OntClass coordinatevalueclass = null;
	private OntClass outsideco2class = null;
	private OntClass outsidecoclass = null;
	private OntClass outsideno2class = null;
	private OntClass outsidenoclass = null;
	private OntClass outsidenoxclass = null;
	private OntClass outsideso2class = null;
	private OntClass outsideo3class = null;
	private OntClass outsidepm1class= null;
	private OntClass outsidepm10class = null;
	private OntClass outsidepm25class= null;
	private OntClass outsidehcclass=null;
	
	private ObjectProperty hasvalue = null;
	private ObjectProperty hasunit = null;
	private ObjectProperty hasBeginning = null;
	private ObjectProperty hasEnd = null;
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
	static Individual ugperm3;
	static Individual degree;
	private DatatypeProperty gasProtocolVersion = null;
	private DatatypeProperty particleProtocolVersion = null;
	private DatatypeProperty locationName = null;
	private DatatypeProperty gasState = null;
	private DatatypeProperty particleState = null;
	private DatatypeProperty podSerialNumber = null;
	private DatatypeProperty scalednumval = null;
	private DatatypeProperty prescalednumval = null;
	private DatatypeProperty hasMeasuredPropertyMean = null;
	private DatatypeProperty hasMeasuredPropertyMin = null;
	private DatatypeProperty hasMeasuredPropertyMax = null;
	private DatatypeProperty hasMeasuredPropertyVariance = null;
	private DatatypeProperty hasPSI = null;
	
	private int numberofdataslot;
	private String xloc;
	private String yloc;
	private String zloc;
	
	
	
	public void initOWLClasses(OntModel jenaOwlModel) {
		coordinateclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#AngularCoordinate");
		coordinatevalueclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl#CoordinateValue");
		coordinatesystemclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#ProjectedCoordinateSystem");
		outsideco2class = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#OutsideCO2Concentration");
		outsidecoclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#OutsideCOConcentration");
		outsideno2class = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#OutsideNO2Concentration");
		outsidenoclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#OutsideNOConcentration");
		outsidenoxclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#OutsideNOxConcentration");
		outsideso2class = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#OutsideSO2Concentration");
		outsideo3class = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#OutsideO3Concentration");
		outsidehcclass= jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#OutsideHCConcentration");
		outsidepm1class = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#OutsidePM1Concentration");
		outsidepm10class = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#OutsidePM10Concentration");
		outsidepm25class = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#OutsidePM25Concentration");
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
		timeentityclass=jenaOwlModel.getOntClass("http://www.w3.org/2006/time#TemporalEntity");	
		observes = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#observes");
		hastime = jenaOwlModel.getObjectProperty("http://www.w3.org/2006/time#hasTime");
		hasBeginning = jenaOwlModel.getObjectProperty("http://www.w3.org/2006/time#hasBeginning");
		hasEnd = jenaOwlModel.getObjectProperty("http://www.w3.org/2006/time#hasEnd");
		hascoordinatesystem = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasGISCoordinateSystem");
		hasx = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasProjectedCoordinate_x");
		hasy = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasProjectedCoordinate_y");
		hasz = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasProjectedCoordinate_z");
		hasvalue = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue");
		hasunit = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasUnitOfMeasure");
		numval = jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");
		timexsdvalue = jenaOwlModel.getDatatypeProperty("http://www.w3.org/2006/time#inXSDDateTimeStamp");
		
		gasProtocolVersion = jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#gasProtocolVersion");
		particleProtocolVersion = jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#particleProtocolVersion");
		locationName = jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#locationName");
		gasState = jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#gasState");
		particleState = jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#particleState");
		podSerialNumber = jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#podSerialNumber");
		scalednumval = jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#scaledNumValue");
		prescalednumval = jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#prescaledNumValue");
		hasMeasuredPropertyMin = jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#hasMeasuredPropertyMin");
		hasMeasuredPropertyMax = jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#hasMeasuredPropertyMax");
		hasMeasuredPropertyMean = jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#hasMeasuredPropertyMean");
		hasMeasuredPropertyVariance = jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#hasMeasuredPropertyVariance");
		hasPSI = jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#hasPSI");
		
		
		C=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#Celsius");
		Wperm2=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#W_per_m.m");
		mpers=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#m_per_s");
		mm=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#mm");
		ugperm3=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#ug_per_m.m.m");
		degree=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#degree");
		m=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/SI_unit.owl#m");
	}
	
	public void executeConversion() throws Exception { //control which location to be created
		System.out.println("Starting Process");
		AirSensorKBCreator converter = new AirSensorKBCreator();
		String csv = new QueryBroker().readFileLocal(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/Copy of aqmesh-location-data-Cares1-20200420020751.csv");
		List<String[]> readingFromCSV = MatrixConverter.fromCsvToArray(csv);

		
		//for SG:
		int numberofstn=1;
		String locationid = "SG";
//		String locationid="SGAQMesh"; //enable this when creating the AQMesh sensor. 
		
		//for Berlin:
//		int numberofstn=1;
//		String locationid="DE";
		
		//for THeHague:
//		int numberofstn=1;
//		String locationid="NL";
			
		
		//for HK:
//		String csvhk = new QueryBroker().readFileLocal(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/1hrweatherhistory.csv");
//		List<String[]> readingFromCSVHK = MatrixConverter.fromCsvToArray(csvhk);
//		readingFromCSVHK.remove(0);//move the header
//		int numberofstn=readingFromCSVHK.size();
//		String locationid="HK";
		
		for(int d=1;d<=numberofstn;d++) {
			String number="00"+d;
			if(d>9&&d<100) {
				number="0"+d;
			}else if(d>99) {
				number=""+d;
			}
			converter.startConversion(readingFromCSV,"OutsideCO2Concentration",number,locationid);
			converter.startConversion(readingFromCSV,"OutsideCOConcentration",number,locationid);
			converter.startConversion(readingFromCSV,"OutsideSO2Concentration",number,locationid);
			converter.startConversion(readingFromCSV,"OutsideNO2Concentration",number,locationid);
			converter.startConversion(readingFromCSV,"OutsideNOxConcentration",number,locationid);
			converter.startConversion(readingFromCSV,"OutsideNOConcentration",number,locationid);
			converter.startConversion(readingFromCSV,"OutsideO3Concentration",number,locationid);
			converter.startConversion(readingFromCSV,"OutsidePM1Concentration",number,locationid);
			converter.startConversion(readingFromCSV,"OutsidePM10Concentration",number,locationid);
			converter.startConversion(readingFromCSV,"OutsidePM25Concentration",number,locationid);
			converter.startConversion(readingFromCSV,"OutsideHCConcentration",number,locationid);
		}
	}
	
	
	public String formatTime(String datetime) {
		//String datetime="16/Apr/2020 12:00:00";
		String timeformatted="";

		SimpleDateFormat dateFormat = new SimpleDateFormat("dd/MMM/yyyy HH:mm:ss");
		
		Date date1;
		try {
			date1 = dateFormat.parse(datetime);
			timeformatted=new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss").format(date1)+"+08:00";
			System.out.println("new format= "+timeformatted);
		} catch (ParseException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}  
		
		return timeformatted;
		
	}
	//start there
	public void doConversionAirsensor(OntModel jenaOwlModel, String mainobjectname,String Prefix,List<String[]> readingFromCSV,String propertyname,OntClass propclass,String[]location) throws FileNotFoundException, URISyntaxException{
		String particleprotocol="V3.0";
		String gasprotocol="V5.1";
		//int numberofdataslots=24*7; //(assume now 1 week every hour) 1 day every minute then 1440
		int numberofdigit=(""+numberofdataslot).length();
		System.out.println("it is processed= " + mainobjectname);
		//String mainobjectname= SGTemperatureSensor-001 (sample)

		Individual mainobjinst = mainobj2class.createIndividual(Prefix+mainobjectname+".owl#"+mainobjectname);
		
		Individual measuredproperty = propclass.createIndividual(Prefix+mainobjectname+".owl#Measured"+propertyname+"Of"+mainobjectname);
		mainobjinst.addProperty(observes, measuredproperty);
		if(propertyname.contains("PM")) {
			mainobjinst.setPropertyValue(particleProtocolVersion, jenaOwlModel.createTypedLiteral(particleprotocol));
		}else {
			mainobjinst.setPropertyValue(gasProtocolVersion, jenaOwlModel.createTypedLiteral(gasprotocol));
		}
		measuredproperty.setPropertyValue(hasMeasuredPropertyMean, jenaOwlModel.createTypedLiteral(new Double ("0.0")));
		measuredproperty.setPropertyValue(hasMeasuredPropertyVariance, jenaOwlModel.createTypedLiteral(new Double ("0.0")));
		measuredproperty.setPropertyValue(hasMeasuredPropertyMin, jenaOwlModel.createTypedLiteral(new Double ("0.0")));
		measuredproperty.setPropertyValue(hasMeasuredPropertyMax, jenaOwlModel.createTypedLiteral(new Double ("0.0")));
		measuredproperty.setPropertyValue(hasPSI, jenaOwlModel.createTypedLiteral(new Double ("0.0")));
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
		
		for(int x=1;x<=numberofdataslot;x++) {
			String xindex=String.format("%0"+numberofdigit+"d", x);
			String prescaledvalue=""; 
			String scaledvalue=""; 
			int y=3;
			Individual valueofproperty = scalarvalueclass.createIndividual(Prefix+mainobjectname+".owl#V_Measured"+propertyname+"Of"+mainobjectname+"_"+xindex);
			if(propertyname.contains("COConc")) {
				valueofproperty.setPropertyValue(gasState, jenaOwlModel.createTypedLiteral(readingFromCSV.get(y)[5]));
				prescaledvalue=readingFromCSV.get(y)[6];
				scaledvalue=readingFromCSV.get(y)[7]; 
			}else if(propertyname.contains("CO2Conc")) {
				valueofproperty.setPropertyValue(gasState, jenaOwlModel.createTypedLiteral(readingFromCSV.get(y)[8]));
				prescaledvalue=readingFromCSV.get(y)[9];
				scaledvalue=readingFromCSV.get(y)[10]; 
			}else if(propertyname.contains("NOConc")) {
				valueofproperty.setPropertyValue(gasState, jenaOwlModel.createTypedLiteral(readingFromCSV.get(y)[11]));
				prescaledvalue=readingFromCSV.get(y)[12];
				scaledvalue=readingFromCSV.get(y)[13]; 
			}else if(propertyname.contains("NO2Conc")) {
				valueofproperty.setPropertyValue(gasState, jenaOwlModel.createTypedLiteral(readingFromCSV.get(y)[14]));
				prescaledvalue=readingFromCSV.get(y)[15];
				scaledvalue=readingFromCSV.get(y)[16]; 
			}else if(propertyname.contains("NOxConc")) {
				valueofproperty.setPropertyValue(gasState, jenaOwlModel.createTypedLiteral(readingFromCSV.get(y)[17]));
				prescaledvalue=readingFromCSV.get(y)[18];
				scaledvalue=readingFromCSV.get(y)[19]; 
			}else if(propertyname.contains("O3Conc")) {
				valueofproperty.setPropertyValue(gasState, jenaOwlModel.createTypedLiteral(readingFromCSV.get(y)[20]));
				prescaledvalue=readingFromCSV.get(y)[21];
				scaledvalue=readingFromCSV.get(y)[22]; 
			}else if(propertyname.contains("SO2Conc")) {
				valueofproperty.setPropertyValue(gasState, jenaOwlModel.createTypedLiteral(readingFromCSV.get(y)[23]));
				prescaledvalue=readingFromCSV.get(y)[24];
				scaledvalue=readingFromCSV.get(y)[25]; 
			}else if(propertyname.contains("PM1Conc")) {
				valueofproperty.setPropertyValue(particleState, jenaOwlModel.createTypedLiteral(readingFromCSV.get(y)[27]));
				prescaledvalue=readingFromCSV.get(y)[28];
				scaledvalue=readingFromCSV.get(y)[29]; 
			}else if(propertyname.contains("PM2")) {
				valueofproperty.setPropertyValue(particleState, jenaOwlModel.createTypedLiteral(readingFromCSV.get(y)[27]));
				prescaledvalue=readingFromCSV.get(y)[30];
				scaledvalue=readingFromCSV.get(y)[31]; 
			}else if(propertyname.contains("PM10Conc")) {
				valueofproperty.setPropertyValue(particleState, jenaOwlModel.createTypedLiteral(readingFromCSV.get(y)[27]));
				prescaledvalue=readingFromCSV.get(y)[32];
				scaledvalue=readingFromCSV.get(y)[33]; 
			}else if(propertyname.contains("HCConc")) {
				valueofproperty.setPropertyValue(gasState, jenaOwlModel.createTypedLiteral(readingFromCSV.get(y)[8]));
				prescaledvalue=readingFromCSV.get(y)[9];
				scaledvalue=readingFromCSV.get(y)[10]; 
			}
			String timestampstartvalue=formatTime(readingFromCSV.get(x)[2]);
			//String timestampendvalue=formatTime(readingFromCSV.get(x)[3]);
			
			
			//Individual timestamp = timeentityclass.createIndividual(Prefix+mainobjectname+".owl#TimeOfMeasured"+propertyname+"Of"+mainobjectname+"_"+xindex);
			Individual timestampstart = timeinstanceclass.createIndividual(Prefix+mainobjectname+".owl#TimeOfMeasured"+propertyname+"Of"+mainobjectname+"_"+xindex);
			//Individual timestampend = timeinstanceclass.createIndividual(Prefix+mainobjectname+".owl#EndTimeOfMeasured"+propertyname+"Of"+mainobjectname+"_"+xindex);
			measuredproperty.addProperty(hasvalue, valueofproperty);

			valueofproperty.setPropertyValue(prescalednumval, jenaOwlModel.createTypedLiteral(new Double (prescaledvalue)));
			valueofproperty.setPropertyValue(scalednumval, jenaOwlModel.createTypedLiteral(new Double (scaledvalue)));
			valueofproperty.addProperty(hasunit, ugperm3);
			
			valueofproperty.addProperty(hastime, timestampstart);
			timestampstart.setPropertyValue(timexsdvalue, jenaOwlModel.createTypedLiteral(new String (timestampstartvalue)));
//			timestamp.addProperty(hasBeginning, timestampstart);
//			timestamp.addProperty(hasEnd, timestampend);
//			timestampstart.setPropertyValue(timexsdvalue, jenaOwlModel.createTypedLiteral(new String (timestampstartvalue)));
//			timestampend.setPropertyValue(timexsdvalue, jenaOwlModel.createTypedLiteral(new String (timestampendvalue)));
			
			
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
		
		String mainobjectname = locationID+"CO2Sensor-"+idOfStation; // still hard-coded for the sample
		String mainobject2name = locationID+"COSensor-"+idOfStation; // still hard-coded for the sample
		String mainobject3name = locationID+"SO2Sensor-"+idOfStation; // still hard-coded for the sample
		String mainobject4name = locationID+"O3Sensor-"+idOfStation; // still hard-coded for the sample
		String mainobject5name = locationID+"NO2Sensor-"+idOfStation; // still hard-coded for the sample
		String mainobject6name = locationID+"NOSensor-"+idOfStation; // still hard-coded for the sample
		String mainobject7name = locationID+"NOxSensor-"+idOfStation; // still hard-coded for the sample
		String mainobject8name = locationID+"PM1Sensor-"+idOfStation; // still hard-coded for the sample
		String mainobject9name = locationID+"PM2.5Sensor-"+idOfStation; // still hard-coded for the sample
		String mainobject10name = locationID+"PM10Sensor-"+idOfStation; // still hard-coded for the sample
		String mainobject11name = locationID+"HCSensor-"+idOfStation; // still hard-coded for the sample
		
		String filePath1 = Prefix + mainobjectname + ".owl#"+ mainobjectname; // the result of written owl file
		String filePath2 = Prefix + mainobject2name + ".owl#"+ mainobject2name; // the result of written owl file
		String filePath3 = Prefix + mainobject3name + ".owl#"+ mainobject3name; // the result of written owl file
		String filePath4 = Prefix + mainobject4name + ".owl#"+ mainobject4name; // the result of written owl file
		String filePath5 = Prefix + mainobject5name + ".owl#"+ mainobject5name; // the result of written owl file
		String filePath6 = Prefix + mainobject6name + ".owl#"+ mainobject6name; // the result of written owl file
		String filePath7 = Prefix + mainobject7name + ".owl#"+ mainobject7name; // the result of written owl file
		String filePath8 = Prefix + mainobject8name + ".owl#"+ mainobject8name; // the result of written owl file
		String filePath9 = Prefix + mainobject9name + ".owl#"+ mainobject9name; // the result of written owl file
		String filePath10 = Prefix + mainobject10name + ".owl#"+ mainobject10name; // the result of written owl file
		String filePath11 = Prefix + mainobject11name + ".owl#"+ mainobject11name; // the result of written owl file
		
		//String[] location = extractLocationofStation(idOfStation, inputRef,locationID);
		String[]location= {xloc,yloc,zloc}; //assume the sg virtual sensor
		
		if (flag.contains("OutsideCO2Concentration")) {
			System.out.println("creating CO2");
			doConversionAirsensor(jenaOwlModel, mainobjectname,Prefix,readingFromCSV,flag,outsideco2class,location); 
			String content = JenaHelper.writeToString(jenaOwlModel);
			new QueryBroker().putOld(Prefix+mainobjectname+".owl", content);
			return filePath1;
		}
		else if (flag.contains("OutsideCOConcentration")) {
			System.out.println("creating CO");
			doConversionAirsensor(jenaOwlModel, mainobject2name,Prefix,readingFromCSV,flag,outsidecoclass,location);
			String content = JenaHelper.writeToString(jenaOwlModel);
			new QueryBroker().putOld(Prefix+mainobject2name+".owl", content);
			return filePath2;
		}
		else if(flag.contains("OutsideSO2Concentration")) {
			System.out.println("creating SO2");
			doConversionAirsensor(jenaOwlModel, mainobject3name,Prefix,readingFromCSV,flag,outsideso2class,location);
			String content = JenaHelper.writeToString(jenaOwlModel);
			new QueryBroker().putOld(Prefix+mainobject3name+".owl", content);
			return filePath3;
		}
		else if (flag.contains("OutsideNO2Concentration")) {
			System.out.println("creating "+flag);
			doConversionAirsensor(jenaOwlModel, mainobject5name,Prefix,readingFromCSV,flag,outsideno2class,location);
			String content = JenaHelper.writeToString(jenaOwlModel);
			new QueryBroker().putOld(Prefix+mainobject5name+".owl", content);
			return filePath5;
		}
		else if (flag.contains("OutsideNOxConcentration")) {
			System.out.println("creating "+flag);
			doConversionAirsensor(jenaOwlModel, mainobject7name,Prefix,readingFromCSV,flag,outsidenoxclass,location);
			String content = JenaHelper.writeToString(jenaOwlModel);
			new QueryBroker().putOld(Prefix+mainobject7name+".owl", content);
			return filePath7;
		}
		else if (flag.contains("OutsideNOConcentration")) {
			System.out.println("creating "+flag);
			doConversionAirsensor(jenaOwlModel, mainobject6name,Prefix,readingFromCSV,flag,outsidenoclass,location);
			String content = JenaHelper.writeToString(jenaOwlModel);
			new QueryBroker().putOld(Prefix+mainobject6name+".owl", content);
			return filePath6;
		}
		else if (flag.contains("OutsideO3Concentration")) {
			System.out.println("creating "+flag);
			doConversionAirsensor(jenaOwlModel, mainobject4name,Prefix,readingFromCSV,flag,outsideo3class,location);
			String content = JenaHelper.writeToString(jenaOwlModel);
			new QueryBroker().putOld(Prefix+mainobject4name+".owl", content);
			return filePath4;
		}
		else if (flag.contains("OutsidePM1Concentration")) {
			System.out.println("creating "+flag);
			doConversionAirsensor(jenaOwlModel, mainobject8name,Prefix,readingFromCSV,flag,outsidepm1class,location);
			String content = JenaHelper.writeToString(jenaOwlModel);
			new QueryBroker().putOld(Prefix+mainobject8name+".owl", content);
			return filePath8;
		}
		else if (flag.contains("OutsidePM10Concentration")) {
			System.out.println("creating "+flag);
			doConversionAirsensor(jenaOwlModel, mainobject10name,Prefix,readingFromCSV,flag,outsidepm10class,location);
			String content = JenaHelper.writeToString(jenaOwlModel);
			new QueryBroker().putOld(Prefix+mainobject10name+".owl", content);
			return filePath10;
		}
		else if (flag.contains("OutsidePM25Concentration")) {
			System.out.println("creating "+flag);
			doConversionAirsensor(jenaOwlModel, mainobject9name,Prefix,readingFromCSV,flag,outsidepm25class,location);
			String content = JenaHelper.writeToString(jenaOwlModel);
			new QueryBroker().putOld(Prefix+mainobject9name+".owl", content);
			return filePath9;
		}else if (flag.contains("OutsideHCConcentration")) {
			System.out.println("creating HC");
			doConversionAirsensor(jenaOwlModel, mainobject11name,Prefix,readingFromCSV,flag,outsidehcclass,location);
			String content = JenaHelper.writeToString(jenaOwlModel);
			new QueryBroker().putOld(Prefix+mainobject11name+".owl", content);
			return filePath11;
		}
//		else if (flag.contains("OutsideHCConcentration")) {
//			System.out.println("creating HC");
//			doConversionAirsensor(jenaOwlModel, mainobject11name,Prefix,readingFromCSV,flag,outsidehcclass,location);
//			String content = JenaHelper.writeToString(jenaOwlModel);
//			new QueryBroker().putOld(Prefix+mainobject11name+".owl", content);
//			return filePath11;
//		}
		return filePath;
	}
	
	private String[] extractLocationofStation(String id, String refDirFile,String locationID) {
		String []location=new String[3];
		int index=Integer.valueOf(id)-1;
		if(locationID.contains("SG")&&index<14) {
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
		
	
	public static void main(String[] args) throws Exception {
		
		new AirSensorKBCreator().executeConversion();


	}

}
