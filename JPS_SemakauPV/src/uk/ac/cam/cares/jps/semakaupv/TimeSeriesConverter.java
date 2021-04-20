package uk.ac.cam.cares.jps.semakaupv;

import java.io.FileNotFoundException;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.datatypes.xsd.XSDDatatype;
import org.apache.jena.ontology.DatatypeProperty;
import org.apache.jena.ontology.Individual;
import org.apache.jena.ontology.ObjectProperty;
import org.apache.jena.ontology.OntClass;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;

/** Ontology creator
 * 
 * @author KADITYA01
 *
 */
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
		timexsdvalue = jenaOwlModel.getDatatypeProperty("http://www.w3.org/2006/time#inXSDDateTime");
		C=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#Celsius");
		Wperm2=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#W_per_m.m");
		mpers=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#m_per_s");
		MW=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#MW");	
		Mvar=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#Mvar");
		degree=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#degree");
	}
	/** create Query for Solar Panel
	 * 
	 * @return String querystring for solar panel
	 */
	public static SelectBuilder createQueryForPowerGeneratorPV() {
		SelectBuilder sb = new SelectBuilder()
				.addPrefix("j1","http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#")
				.addPrefix("j2", "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#")
				.addPrefix("j3", "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#")
				.addPrefix("j5", "http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#")
				.addVar("?Pg").addVar("?Qg")
				.addWhere("?entity", "a", "j1:PowerGenerator")
				.addWhere("?entity", "j2:isModeledBy", "?model")
				
				.addWhere("?model", "j5:hasModelVariable" ,"?Pg")
				.addWhere("?Pg", "a" ,"j3:Pg")
				.addWhere("?Pg", "j2:hasValue" ,"?vpg")
				.addWhere("?vpg", "j2:numericalValue" ,"?activepowervalue")
				
				.addWhere("?model", "j5:hasModelVariable" ,"?Qg")
				.addWhere("?Qg", "a" ,"j3:Qg")
				.addWhere("?Qg", "j2:hasValue" ,"?vqg")
				.addWhere("?vqg", "j2:numericalValue" ,"?reactivepowervalue");
		return sb;
				
	}
	/** create Query for Electronic Bus
	 * 
	 * @return String querystring for Electronic Bus
	 */
	public static SelectBuilder createQueryForBus() {
		SelectBuilder sb = new SelectBuilder()
				.addPrefix("j1","http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#")
				.addPrefix("j2", "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#")
				.addPrefix("j3", "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#")
				.addPrefix("j5", "http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#")
				.addVar("?VM").addVar("?VA")
				.addWhere("?entity", "a", "j1:BusNode")
				.addWhere("?entity", "j2:isModeledBy", "?model")
				
				.addWhere("?model", "j5:hasModelVariable" ,"?VM")
				.addWhere("?VM", "a" ,"j3:Vm")
				.addWhere("?VM", "j2:hasValue" ,"?vVM")
				.addWhere("?vVM", "j2:numericalValue" ,"?VoltMagvalue")
				
				.addWhere("?model", "j5:hasModelVariable" ,"?VA")
				.addWhere("?VA", "a" ,"j3:Va")
				.addWhere("?VA", "j2:hasValue" ,"?vVA")
				.addWhere("?vVA", "j2:numericalValue" ,"?VoltAnglevalue");
		return sb;
				
	}
	/**
	 * list of properties in 1 owl file 
	 * + list of values of them
	 * + list of units of them
	 * 
	 * check if the value properties is exist in the beginning, if yes, delete; if no direct create and link it to the properties
	 * how many time series
	 * @param jenaOwlModel
	 * @param mainobjectname2
	 * @param Prefix
	 * @param readingFromCSV
	 * @param unit
	 * @param flag
	 * @throws FileNotFoundException
	 * @throws URISyntaxException
	 */
	public void doConversionForTimeSeries(OntModel jenaOwlModel, String mainobjectname2,String Prefix,List<String[]> readingFromCSV, List<Individual> unit,String flag) throws FileNotFoundException, URISyntaxException{

		String genInfo =createQueryForPowerGeneratorPV().buildString() ;
		String busInfo = createQueryForBus().buildString();
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
//			System.out.println(resultListiri.get(0)[propnum]);
			//outsideirradiation.removeAll(hasvalue);
			for(int x=1;x<readingFromCSV.size();x++) {
				String irradiationvalue=readingFromCSV.get(x)[indexcsv[propnum]]; //need to be changed
				String year=readingFromCSV.get(x)[0];
				String month=readingFromCSV.get(x)[1].split("-")[0]; 
				String date=readingFromCSV.get(x)[1].split("-")[1];
				String time=readingFromCSV.get(x)[2];
				String individualindex=String.format("%02d", x);
				String timestampvalue=year+"-"+month+"-"+String.format("%02d", Integer.valueOf(date))+"T"+time+"+08:00";
				Individual voutsideirradiation = scalarvalueclass.createIndividual(Prefix+mainobjectname2+".owl#V_Calculated"+keys[propnum]+"Of"+mainobjectname2+"_"+individualindex);
				Individual timestampirradiation = jenaOwlModel.getIndividual(Prefix+mainobjectname2+".owl#TimeOfCalculatedPropertiesOf"+mainobjectname2+"_"+individualindex);
				if(timestampirradiation==null) {
				timestampirradiation = timeinstanceclass.createIndividual(Prefix+mainobjectname2+".owl#TimeOfCalculatedPropertiesOf"+mainobjectname2+"_"+individualindex);
				}
				outsideirradiation.addProperty(hasvalue, voutsideirradiation);
				voutsideirradiation.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double (irradiationvalue)));
				if(unit.get(propnum)!=null) {
				voutsideirradiation.addProperty(hasunit, unit.get(propnum));
				}
				voutsideirradiation.addProperty(hastime, timestampirradiation);
				timestampirradiation.setPropertyValue(timexsdvalue, jenaOwlModel.createTypedLiteral(timestampvalue,XSDDatatype.XSDdateTime)); //value need to be changed later
				
			}
			
		}
		
	}
	/**Used to create OWL files
	 * 
	 * @param readingFromCSV {"year","monthdate","time","PGen","QGen","VmPu","Va"} format 
	 * @param flag gen Or bus
	 * @param Prefix http://www.theworldavatar.com/kb/sgp/semakauisland/semakauelectricalnetwork/ or any respective folder
	 * @param filename PVXX.owl or EBusXX.owl
	 * @throws Exception
	 */
	public void startConversion(List<String[]> readingFromCSV,String flag,String Prefix,String filename) throws Exception {
		OntModel jenaOwlModel = JenaHelper.createModel(Prefix+filename);
		initOWLClasses(jenaOwlModel);
		List<Individual>unit1=new ArrayList<Individual>();
		unit1.add(MW);
		unit1.add(Mvar);
		List<Individual>unit2=new ArrayList<Individual>();
		unit2.add(null);
		unit2.add(degree);
		//String Prefix="http://www.theworldavatar.com/kb/sgp/semakauisland/semakauelectricalnetwork/";
		if (flag.contains("gen")) {
//			System.out.println(filename.split(".owl")[0]);
			doConversionForTimeSeries(jenaOwlModel,filename.split(".owl")[0],Prefix , readingFromCSV, unit1,flag);
			String content = JenaHelper.writeToString(jenaOwlModel);
			new QueryBroker().putOld(Prefix+filename, content);
		}else {
			doConversionForTimeSeries(jenaOwlModel,filename.split(".owl")[0], Prefix,readingFromCSV, unit2,flag);	
			String content = JenaHelper.writeToString(jenaOwlModel);
			new QueryBroker().putOld(Prefix+filename, content);
		}
		
	}
	/** helper function to create preliminary PV, bus files
	 * 
	 * @throws Exception
	 */
	public void executeConversion() throws Exception {
//		System.out.println("Starting Process");
		TimeSeriesConverter converter = new TimeSeriesConverter();
		String csv = new QueryBroker().readFileLocal(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/timeseriespropvalues.csv");
		List<String[]> readingFromCSV = MatrixConverter.fromCsvToArray(csv);
		//String baseURL2 = AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/";
		converter.startConversion(readingFromCSV,"gen","http://www.theworldavatar.com/kb/sgp/semakauisland/semakauelectricalnetwork/","PV-002.owl");
//		System.out.println("PV finished");
		converter.startConversion(readingFromCSV,"bus","http://www.theworldavatar.com/kb/sgp/semakauisland/semakauelectricalnetwork/","EBus-006.owl");

			
	}
	/** main method to create preliminary OWL files for later updates
	 * 
	 * @param args
	 * @throws Exception
	 */
	public static void main(String[] args) throws Exception {
		
		new TimeSeriesConverter().executeConversion();
	}

}
