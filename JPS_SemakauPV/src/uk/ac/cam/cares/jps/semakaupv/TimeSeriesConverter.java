package uk.ac.cam.cares.jps.semakaupv;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;

import org.apache.jena.ontology.DatatypeProperty;
import org.apache.jena.ontology.Individual;
import org.apache.jena.ontology.ObjectProperty;
import org.apache.jena.ontology.OntClass;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.ModelFactory;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;


public class TimeSeriesConverter {
	
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
	static Individual MW;
	static Individual Mvar;
	static Individual degree;
	
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
		MW=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#MW");	
		Mvar=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#Mvar");
		degree=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#degree");
	}
	
	public void doConversionForTimeSeries(OntModel jenaOwlModel, String mainobjectname2,String Prefix,List<String[]> readingFromCSV, List<Individual> unit,String flag) throws FileNotFoundException, URISyntaxException{

		/**
		 * list of properties in 1 owl file 
		 * + list of values of them
		 * + list of units of them
		 * 
		 * check if the value properties is exist in the beginning, if yes, delete; if no direct create and link it to the properties
		 * how many time series
		 * 
		 */
		
		String genInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
				+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
				+ "SELECT ?Pg ?Qg "

				+ "WHERE {?entity  a  j1:PowerGenerator  ."
				+ "?entity   j2:isModeledBy ?model ."

				+ "?model   j5:hasModelVariable ?Pg ." 
				+ "?Pg  a  j3:Pg  ." 
				+ "?Pg  j2:hasValue ?vpg ."
				+ "?vpg   j2:numericalValue ?activepowervalue ." // pg

				+ "?model   j5:hasModelVariable ?Qg ." 
				+ "?Qg  a  j3:Qg  ." 
				+ "?Qg  j2:hasValue ?vqg ."
				+ "?vqg   j2:numericalValue ?reactivepowervalue ." // qg
				+ "}" ;
		
		
		String busInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
				+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
				+ "SELECT ?VM ?VA "

				+ "WHERE {?entity  a  j1:BusNode  ." 
				+ "?entity   j2:isModeledBy ?model ."

				+ "?model   j5:hasModelVariable ?VM ." 
				+ "?VM  a  j3:Vm  ." 
				+ "?VM  j2:hasValue ?vVM ."
				+ "?vVM   j2:numericalValue ?VoltMagvalue ." // Vm

				+ "?model   j5:hasModelVariable ?VA ." 
				+ "?VA  a  j3:Va  ." 
				+ "?VA  j2:hasValue ?vVA ."
				+ "?vVA   j2:numericalValue ?VoltAnglevalue ." // Va

//				+ "?model   j5:hasModelVariable ?BKV ." 
//				+ "?BKV  a  j3:baseKV  ." 
//				+ "?BKV  j2:hasValue ?vBKV ."
//				+ "?vBKV   j2:numericalValue ?BaseKVvalue ." // Base KV
				+ "}" ;
		ResultSet resultSet = JenaHelper.query(jenaOwlModel, busInfo);
		int[]indexcsv= {5,6};
		if(flag.contains("gen")) {
			 resultSet = JenaHelper.query(jenaOwlModel, genInfo);
			 indexcsv[0]=3;
			 indexcsv[1]=4;
		}
			
			String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
			String[] keys = JenaResultSetFormatter.getKeys(result);
			List<String[]> resultListiri = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
			

		for(int propnum=0;propnum<keys.length;propnum++) {
			Individual outsideirradiation = jenaOwlModel.getIndividual(resultListiri.get(0)[propnum]);
			System.out.println(resultListiri.get(0)[propnum]);
			//outsideirradiation.removeAll(hasvalue);
			for(int x=1;x<=readingFromCSV.size();x++) {
				String irradiationvalue=readingFromCSV.get(x-1)[indexcsv[propnum]]; //need to be changed
				String year=readingFromCSV.get(x-1)[0];
				String month=readingFromCSV.get(x-1)[1].split("-")[1]; 
				String date=readingFromCSV.get(x-1)[1].split("-")[0];
				String time=readingFromCSV.get(x-1)[2];
				String timestampvalue=year+"-"+month+"-"+String.format("%02d", Integer.valueOf(date))+"T"+time+"+08:00";
				Individual voutsideirradiation = scalarvalueclass.createIndividual(Prefix+mainobjectname2+".owl#V_Calculated"+keys[propnum]+"Of"+mainobjectname2+"_"+x);
				Individual timestampirradiation = timeinstanceclass.createIndividual(Prefix+mainobjectname2+".owl#TimeOfCalculated"+keys[propnum]+"Of"+mainobjectname2+"_"+x);
				outsideirradiation.addProperty(hasvalue, voutsideirradiation);
				voutsideirradiation.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double (irradiationvalue)));
				if(unit.get(propnum)!=null) {
				voutsideirradiation.addProperty(hasunit, unit.get(propnum));
				}
				voutsideirradiation.addProperty(hastime, timestampirradiation);
				timestampirradiation.setPropertyValue(timexsdvalue, jenaOwlModel.createTypedLiteral(new String(timestampvalue))); //value need to be changed later
				
			}
			
		}
		
	}
	
	public void startConversion(List<String[]> readingFromCSV,String flag,String tempfilename) throws Exception {
		String baseURL = AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/";
		String filePath = baseURL + tempfilename; // the empty owl file

		FileInputStream inFile = new FileInputStream(filePath);
		Reader in = new InputStreamReader(inFile, "UTF-8");

		OntModel jenaOwlModel = ModelFactory.createOntologyModel();
		jenaOwlModel.read(in, null);
		initOWLClasses(jenaOwlModel);
		List<Individual>unit1=new ArrayList<Individual>();
		unit1.add(MW);
		unit1.add(Mvar);
		List<Individual>unit2=new ArrayList<Individual>();
		unit2.add(null);
		unit2.add(degree);
		String Prefix="http://www.theworldavatar.com/kb/sgp/semakauisland/semakauelectricalnetwork/";
		if (flag.contains("gen")) {
			doConversionForTimeSeries(jenaOwlModel,"PV-001",Prefix , readingFromCSV, unit1,flag);
			String content = JenaHelper.writeToString(jenaOwlModel);
			new QueryBroker().putOld(Prefix+"PV-001.owl", content);
		}else {
			doConversionForTimeSeries(jenaOwlModel,"EBus-006", Prefix,readingFromCSV, unit2,flag);	
			String content = JenaHelper.writeToString(jenaOwlModel);
			new QueryBroker().putOld(Prefix+"EBus-006.owl", content);
		}
		
	}
	
	public void executeConversion() throws Exception {
		System.out.println("Starting Process");
		TimeSeriesConverter converter = new TimeSeriesConverter();
		String csv = new QueryBroker().readFileLocal(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/timeseriespropvalues.csv");
		List<String[]> readingFromCSV = MatrixConverter.fromCsvToArray(csv);
		//String baseURL2 = AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/";
		converter.startConversion(readingFromCSV,"gen","PV-001.owl");
		System.out.println("PV finished");
		converter.startConversion(readingFromCSV,"bus","EBus-006.owl");

			
	}
	public static void main(String[] args) throws Exception {
		
		new TimeSeriesConverter().executeConversion();
	}

}
