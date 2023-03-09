package kbcreator;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Paths;

import org.apache.jena.ontology.DatatypeProperty;
import org.apache.jena.ontology.Individual;
import org.apache.jena.ontology.ObjectProperty;
import org.apache.jena.ontology.OntClass;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.rdf.model.ModelFactory;

import com.opencsv.CSVReader;


public class WeatherStationKB {
	public static String baseURL2 = "D:\\KBDev-git/irp3-JPS-KBDev-git/Server Ontology Configuration Root/kb/hkg/";
	public static String baseURL = "D:\\KBDev-git/irp3-JPS-KBDev-git/Server Ontology Configuration Root/kb/temporary/";
	public static String ontologymainiri="http://www.theworldavatar.com/ontology/ontostation/OntoStation.owl#";
	public static String sensoriri="http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#";
	public static String weatheriri="https://www.auto.tuwien.ac.at/downloads/thinkhome/ontology/WeatherOntology.owl#";
	public static String kbmainweatherstationiri="http://www.theworldavatar.com/kb/hkg/hongkong/weatherstations/";
	public static String kbmainairstationiri="http://www.theworldavatar.com/kb/hkg/hongkong/airqualitystations/";
	public static String csvpath1="D:\\JParkSimulator-git-dev-database/KnowledgeBaseCreator/testres/weather_stations.csv";
	public static String csvpath2="D:\\JParkSimulator-git-dev-database/KnowledgeBaseCreator/testres/air_stations.csv";
	
	
	private OntClass weatherstationclass = null;
	private OntClass airstationclass = null;
	
	private OntClass Tsensorclass = null; //tempr
	private OntClass Psensorclass = null;//press
	private OntClass Qsensorclass = null;//conc,humidity
	private OntClass Fsensorclass = null; //windspeed,winddirection
	private OntClass Tempclass = null; //tempr
	private OntClass Pressclass = null;//press
	private OntClass windspeedclass = null;
	private OntClass winddirclass = null; 
	private OntClass humidityclass = null;
	private OntClass o3concclass = null; 
	private OntClass no2concclass = null;
	private OntClass So2concclass = null;
	private OntClass pm10concclass = null;
	private OntClass pm25concclass = null;
	
	private OntClass coordinatesystemclass = null;
	private OntClass coordinateclass = null;
	private OntClass coordinatevalueclass = null;
	private OntClass scalarvalueclass = null;
	
	private ObjectProperty observes=null;
	private ObjectProperty hasSubsystem=null;
	private ObjectProperty hascoordinatesystem = null;
	private ObjectProperty hasx = null;
	private ObjectProperty hasy = null;
	private ObjectProperty hasz = null;
	
	private ObjectProperty hasvalue = null;
	
	private DatatypeProperty numval = null;
	private ObjectProperty hasunit = null;
	static Individual degree;
	static Individual m;
	static Individual ugperm3;
	static Individual celsius;
	static Individual hPa;
	static Individual kmperhr;
	
	
	
	public void initspecOWLClasses(OntModel jenaOwlModel) {
		weatherstationclass = jenaOwlModel.getOntClass(ontologymainiri+"WeatherStation");
		airstationclass = jenaOwlModel.getOntClass(ontologymainiri+"AirQualityStation");
		Tsensorclass=jenaOwlModel.getOntClass(sensoriri+"T-Sensor");
		Psensorclass=jenaOwlModel.getOntClass(sensoriri+"P-Sensor");
		Fsensorclass=jenaOwlModel.getOntClass(sensoriri+"F-Sensor");
		Qsensorclass=jenaOwlModel.getOntClass(sensoriri+"Q-Sensor");
		
		Tempclass = jenaOwlModel.getOntClass(weatheriri+"Temperature");
		humidityclass = jenaOwlModel.getOntClass(weatheriri+"Humidity");
		Pressclass = jenaOwlModel.getOntClass(weatheriri+"AtmosphericPressure");
		windspeedclass = jenaOwlModel.getOntClass(ontologymainiri+"WindSpeed");
		winddirclass = jenaOwlModel.getOntClass(ontologymainiri+"WindDirection");
		
		no2concclass = jenaOwlModel.getOntClass(ontologymainiri+"NO2Concentration");
		So2concclass = jenaOwlModel.getOntClass(ontologymainiri+"SO2Concentration");
		o3concclass = jenaOwlModel.getOntClass(ontologymainiri+"O3Concentration");
		pm10concclass = jenaOwlModel.getOntClass(ontologymainiri+"PM10Concentration");
		pm25concclass = jenaOwlModel.getOntClass(ontologymainiri+"PM2.5Concentration");
		coordinatesystemclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#ProjectedCoordinateSystem");
		coordinateclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#AngularCoordinate");
		scalarvalueclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#ScalarValue");	
		coordinatevalueclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl#CoordinateValue");
		
		observes=jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#observes");
		hasSubsystem = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasSubsystem");
		hascoordinatesystem = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasGISCoordinateSystem");
		hasx = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasProjectedCoordinate_x");
		hasy = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasProjectedCoordinate_y");
		hasz = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasProjectedCoordinate_z");
		hasvalue = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue");
		hasunit = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasUnitOfMeasure");
		
		numval = jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");
		
		m=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/SI_unit.owl#m");
		
		degree=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#degree");
		ugperm3=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#ug_per_m.m.m"); 
		celsius=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#Celsius");
		
		hPa=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#hPa");
		kmperhr=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#km_per_hr");
		
	}
	
		public void startConversion() throws Exception {
	
			String filetemplatePath = baseURL + "stationtemplate.owl"; // the empty owl file
	    	
						doConversionForWeatherStation(filetemplatePath,csvpath1);
						doConversionForAirQualityStation(filetemplatePath,csvpath2);
						
					}
		
		
		public void doConversionForWeatherStation(String filetemplatePath, String csvpathfile) throws IOException, URISyntaxException {
			
	    	
	    	

						
			Reader reader = Files.newBufferedReader(Paths.get(csvpathfile));
            CSVReader csvReader = new CSVReader(reader);	
			int counter=0;
            String[] nextRecord;
            while ((nextRecord = csvReader.readNext()) != null) {
			 	if(counter==0) {
			            		
			       	}
            	else {
            		FileInputStream inFile = new FileInputStream(filetemplatePath);
					Reader in = new InputStreamReader(inFile, "UTF-8");
						    			
					OntModel jenaOwlModel = ModelFactory.createOntologyModel();
					jenaOwlModel.read(in, null);
					
					ShipKbCreator newkb= new ShipKbCreator();
		
					initspecOWLClasses(jenaOwlModel);
            		
            		String nameofstn=nextRecord[0].replace(" ", "_");
            		nameofstn=nameofstn.replace("'s", "s");
            		String filePath2 = baseURL2+"weatherstations/" +nameofstn+".owl"; // the result of written owl file
            	Individual stn= weatherstationclass.createIndividual(kbmainweatherstationiri+nameofstn+".owl#"+nameofstn);
            	Individual tsensor=Tsensorclass.createIndividual(kbmainweatherstationiri+nameofstn+".owl#"+"TemperatureSensorOf"+nameofstn);
            	Individual psensor=Psensorclass.createIndividual(kbmainweatherstationiri+nameofstn+".owl#"+"PressureSensorOf"+nameofstn);
            	Individual humsensor=Qsensorclass.createIndividual(kbmainweatherstationiri+nameofstn+".owl#"+"HumiditySensorOf"+nameofstn);
            	Individual windspeedsensor=Fsensorclass.createIndividual(kbmainweatherstationiri+nameofstn+".owl#"+"WindSpeedSensorOf"+nameofstn);
            	Individual winddirsensor=Fsensorclass.createIndividual(kbmainweatherstationiri+nameofstn+".owl#"+"WindDirectionSensorOf"+nameofstn);
            	
            	
            	stn.addProperty(hasSubsystem, tsensor);
               	stn.addProperty(hasSubsystem, psensor);
            	stn.addProperty(hasSubsystem, humsensor);
            	stn.addProperty(hasSubsystem, winddirsensor);
            	stn.addProperty(hasSubsystem, windspeedsensor);
            	
            	Individual stncoordsys= coordinatesystemclass.createIndividual(kbmainweatherstationiri+nameofstn+".owl#"+nameofstn+"_CoordinateSystem");
            	stn.addProperty(hascoordinatesystem, stncoordsys);
            	Individual xstn=coordinateclass.createIndividual(kbmainweatherstationiri+nameofstn+".owl#"+"x_"+nameofstn);
            	stncoordsys.addProperty(hasx, xstn);
            	Individual ystn=coordinateclass.createIndividual(kbmainweatherstationiri+nameofstn+".owl#"+"y_"+nameofstn);
            	stncoordsys.addProperty(hasy, ystn);
            	Individual zstn=coordinateclass.createIndividual(kbmainweatherstationiri+nameofstn+".owl#"+"z_"+nameofstn);
            	stncoordsys.addProperty(hasz, zstn);
            	Individual Vxstn=coordinatevalueclass.createIndividual(kbmainweatherstationiri+nameofstn+".owl#"+"V_x_"+nameofstn);
            	xstn.addProperty(hasvalue, Vxstn);
            	Individual Vystn=coordinatevalueclass.createIndividual(kbmainweatherstationiri+nameofstn+".owl#"+"V_y_"+nameofstn);
            	ystn.addProperty(hasvalue, Vystn);
            	Individual Vzstn=coordinatevalueclass.createIndividual(kbmainweatherstationiri+nameofstn+".owl#"+"V_z_"+nameofstn);
            	zstn.addProperty(hasvalue, Vzstn);
            	Vxstn.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(nextRecord[2])));
            	Vxstn.addProperty(hasunit, degree);
            	Vystn.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(nextRecord[1])));
            	Vystn.addProperty(hasunit, degree);
            	Vzstn.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(nextRecord[3])));
            	Vzstn.addProperty(hasunit, m);

            	Individual tempcondition= Tempclass.createIndividual(kbmainweatherstationiri+nameofstn+".owl#"+"TemperatureConditionIn_"+nameofstn);
            	Individual presscondition= Pressclass.createIndividual(kbmainweatherstationiri+nameofstn+".owl#"+"PressureConditionIn_"+nameofstn);
            	Individual humcondition= humidityclass.createIndividual(kbmainweatherstationiri+nameofstn+".owl#"+"HumidityConditionIn_"+nameofstn);
            	Individual windspeedcondition= windspeedclass.createIndividual(kbmainweatherstationiri+nameofstn+".owl#"+"WindSpeedConditionIn_"+nameofstn);
            	Individual winddircondition= winddirclass.createIndividual(kbmainweatherstationiri+nameofstn+".owl#"+"WindDirectionConditionIn_"+nameofstn);
            	tsensor.addProperty(observes, tempcondition);
            	psensor.addProperty(observes, presscondition);
            	humsensor.addProperty(observes, humcondition);
            	winddirsensor.addProperty(observes, winddircondition);
            	windspeedsensor.addProperty(observes, windspeedcondition);
            	
            	Individual Vtempcondition= scalarvalueclass.createIndividual(kbmainweatherstationiri+nameofstn+".owl#"+"V_TemperatureConditionIn_"+nameofstn);
            	Individual Vpresscondition= scalarvalueclass.createIndividual(kbmainweatherstationiri+nameofstn+".owl#"+"V_PressureConditionIn_"+nameofstn);
            	Individual Vhumcondition= scalarvalueclass.createIndividual(kbmainweatherstationiri+nameofstn+".owl#"+"V_HumidityConditionIn_"+nameofstn);
            	Individual Vwindspeedcondition= scalarvalueclass.createIndividual(kbmainweatherstationiri+nameofstn+".owl#"+"V_WindSpeedConditionIn_"+nameofstn);
            	Individual Vwinddircondition= scalarvalueclass.createIndividual(kbmainweatherstationiri+nameofstn+".owl#"+"V_WindDirectionConditionIn_"+nameofstn);
            	tempcondition.addProperty(hasvalue, Vtempcondition);
            	presscondition.addProperty(hasvalue, Vpresscondition);
            	humcondition.addProperty(hasvalue, Vhumcondition);
            	winddircondition.addProperty(hasvalue, Vwinddircondition);
            	windspeedcondition.addProperty(hasvalue, Vwindspeedcondition);
            
            	//TO DO :unit to be changed
            	Vtempcondition.addProperty(hasunit, celsius);
            	Vpresscondition.addProperty(hasunit, hPa);
            	//Vhumcondition.addProperty(hasunit, Vhumcondition); no unit
            	Vwinddircondition.addProperty(hasunit, degree);
            	Vwindspeedcondition.addProperty(hasunit, kmperhr);
            	
            	Vtempcondition.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(nextRecord[4])));
            	Vpresscondition.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(nextRecord[5])));
            	Vhumcondition.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(nextRecord[6])/100));
            	Vwinddircondition.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(nextRecord[8])));
            	Vwindspeedcondition.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double(nextRecord[7])));
            	
				/** save the updated model file */
				newkb.savefile(jenaOwlModel, filePath2);
            	}
			 	counter++;
            }
		}
		
		public void doConversionForAirQualityStation(String filetemplatePath, String csvpathfile) throws IOException, URISyntaxException {
			
			Reader reader = Files.newBufferedReader(Paths.get(csvpathfile));
            CSVReader csvReader = new CSVReader(reader);	
			
            String[] nextRecord;
            int counter=0;
            while ((nextRecord = csvReader.readNext()) != null) {
            	if(counter==0) {
            		
            	}
            	else {
            		FileInputStream inFile2 = new FileInputStream(filetemplatePath);
        			Reader in2 = new InputStreamReader(inFile2, "UTF-8");
        				    			
        			OntModel jenaOwlModel2 = ModelFactory.createOntologyModel();
        			jenaOwlModel2.read(in2, null);
        			
        			ShipKbCreator newkb2= new ShipKbCreator();

        			initspecOWLClasses(jenaOwlModel2);
            		
            		String nameofstn=nextRecord[0].replace(" ", "_");
            		String filePath2 = baseURL2+"airqualitystations/" +nameofstn+".owl"; // the result of written owl file
            	Individual stn= airstationclass.createIndividual(kbmainairstationiri+nameofstn+".owl#"+nameofstn);
            	Individual so2sensor=Qsensorclass.createIndividual(kbmainairstationiri+nameofstn+".owl#"+"SO2ConcSensorOf"+nameofstn);
            	Individual no2sensor=Qsensorclass.createIndividual(kbmainairstationiri+nameofstn+".owl#"+"NO2ConcSensorOf"+nameofstn);
            	Individual o3sensor=Qsensorclass.createIndividual(kbmainairstationiri+nameofstn+".owl#"+"O3ConcSensorOf"+nameofstn);
            	Individual pm10sensor=Qsensorclass.createIndividual(kbmainairstationiri+nameofstn+".owl#"+"PM10ConcsensorOf"+nameofstn);
            	Individual pm25sensor=Qsensorclass.createIndividual(kbmainairstationiri+nameofstn+".owl#"+"PM2.5ConcSensorOf"+nameofstn);
            	
            	stn.addProperty(hasSubsystem, so2sensor);
            	stn.addProperty(hasSubsystem, no2sensor);
            	stn.addProperty(hasSubsystem, o3sensor);
            	stn.addProperty(hasSubsystem, pm25sensor);
            	stn.addProperty(hasSubsystem, pm10sensor);
            	
            	Individual stncoordsys= coordinatesystemclass.createIndividual(kbmainairstationiri+nameofstn+".owl#"+nameofstn+"_CoordinateSystem");
            	stn.addProperty(hascoordinatesystem, stncoordsys);
            	Individual xstn=coordinateclass.createIndividual(kbmainairstationiri+nameofstn+".owl#"+"x_"+nameofstn);
            	stncoordsys.addProperty(hasx, xstn);
            	Individual ystn=coordinateclass.createIndividual(kbmainairstationiri+nameofstn+".owl#"+"y_"+nameofstn);
            	stncoordsys.addProperty(hasy, ystn);
            	Individual zstn=coordinateclass.createIndividual(kbmainairstationiri+nameofstn+".owl#"+"z_"+nameofstn);
            	stncoordsys.addProperty(hasz, zstn);
            	Individual Vxstn=coordinatevalueclass.createIndividual(kbmainairstationiri+nameofstn+".owl#"+"V_x_"+nameofstn);
            	xstn.addProperty(hasvalue, Vxstn);
            	Individual Vystn=coordinatevalueclass.createIndividual(kbmainairstationiri+nameofstn+".owl#"+"V_y_"+nameofstn);
            	ystn.addProperty(hasvalue, Vystn);
            	Individual Vzstn=coordinatevalueclass.createIndividual(kbmainairstationiri+nameofstn+".owl#"+"V_z_"+nameofstn);
            	zstn.addProperty(hasvalue, Vzstn);
            	Vxstn.setPropertyValue(numval, jenaOwlModel2.createTypedLiteral(new Double(nextRecord[2])));
            	Vxstn.addProperty(hasunit, degree);
            	Vystn.setPropertyValue(numval, jenaOwlModel2.createTypedLiteral(new Double(nextRecord[1])));
            	Vystn.addProperty(hasunit, degree);
            	Vzstn.setPropertyValue(numval, jenaOwlModel2.createTypedLiteral(new Double(nextRecord[3])));
            	Vzstn.addProperty(hasunit, m);
            	
            	Individual so2conc= So2concclass.createIndividual(kbmainairstationiri+nameofstn+".owl#"+"SO2concIn_"+nameofstn);
            	Individual no2conc= no2concclass.createIndividual(kbmainairstationiri+nameofstn+".owl#"+"NO2concIn_"+nameofstn);
            	Individual o3conc= o3concclass.createIndividual(kbmainairstationiri+nameofstn+".owl#"+"O3concIn_"+nameofstn);
            	Individual pm10conc= pm10concclass.createIndividual(kbmainairstationiri+nameofstn+".owl#"+"PM10concIn_"+nameofstn);
            	Individual pm25conc= pm25concclass.createIndividual(kbmainairstationiri+nameofstn+".owl#"+"PM2.5concIn_"+nameofstn);
            	so2sensor.addProperty(observes, so2conc);
            	no2sensor.addProperty(observes, no2conc);
            	o3sensor.addProperty(observes, o3conc);
            	pm25sensor.addProperty(observes, pm25conc);
            	pm10sensor.addProperty(observes, pm10conc);
            	
            	Individual Vso2conc= scalarvalueclass.createIndividual(kbmainairstationiri+nameofstn+".owl#"+"V_SO2concIn_"+nameofstn);
            	Individual Vno2conc= scalarvalueclass.createIndividual(kbmainairstationiri+nameofstn+".owl#"+"V_NO2concIn_"+nameofstn);
            	Individual Vo3conc= scalarvalueclass.createIndividual(kbmainairstationiri+nameofstn+".owl#"+"V_O3concIn_"+nameofstn);
            	Individual Vpm10conc= scalarvalueclass.createIndividual(kbmainairstationiri+nameofstn+".owl#"+"V_PM10concIn_"+nameofstn);
            	Individual Vpm25conc= scalarvalueclass.createIndividual(kbmainairstationiri+nameofstn+".owl#"+"V_PM2.5concIn_"+nameofstn);
            	so2conc.addProperty(hasvalue, Vso2conc);
            	no2conc.addProperty(hasvalue, Vno2conc);
            	o3conc.addProperty(hasvalue, Vo3conc);
            	pm25conc.addProperty(hasvalue, Vpm25conc);
            	pm10conc.addProperty(hasvalue, Vpm10conc);
            
            	//TO DO :unit to be changed
            	Vso2conc.addProperty(hasunit, ugperm3);
            	Vno2conc.addProperty(hasunit, ugperm3);
            	Vo3conc.addProperty(hasunit, ugperm3);
            	Vpm25conc.addProperty(hasunit, ugperm3);
            	Vpm10conc.addProperty(hasunit, ugperm3);
            	
            	Vso2conc.setPropertyValue(numval, jenaOwlModel2.createTypedLiteral(new Double(nextRecord[8])));
            	Vno2conc.setPropertyValue(numval, jenaOwlModel2.createTypedLiteral(new Double(nextRecord[5])));
            	Vo3conc.setPropertyValue(numval, jenaOwlModel2.createTypedLiteral(new Double(nextRecord[6])));
            	Vpm25conc.setPropertyValue(numval, jenaOwlModel2.createTypedLiteral(new Double(nextRecord[4])));
            	Vpm10conc.setPropertyValue(numval, jenaOwlModel2.createTypedLiteral(new Double(nextRecord[7])));
            	
            	/** save the updated model file */
				newkb2.savefile(jenaOwlModel2, filePath2);
            	}
            	counter++;
            	
            }
		}
		
		public static void main(String[] args) throws Exception {
			System.out.println("Starting Process");
			WeatherStationKB converter = new WeatherStationKB();
			converter.startConversion();

		}
}
