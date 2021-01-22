package uk.ac.cam.cares.jps.semakaupv;

import java.io.IOException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.cmclinnovations.mods.api.MoDSAPI;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;

@WebServlet(urlPatterns = { "/SemakauPV"})

public class SemakauPV extends JPSHttpServlet {
	private Logger logger = LoggerFactory.getLogger(SemakauPV.class);
	public static String root=AgentLocator.getProperty("absdir.root");
	String Sim4 = root+"/Sim_PV1"; // THIS SIMULATION NEED TO BE EXIST 

	/**
	 * @param request HttpServletrequest, should contain responses from DES Solar Irradiation collection agent
	 */
	protected void doGetJPS(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		
		JSONObject joforess = AgentCaller.readJsonParameter(request);
		String ENIRI=joforess.getString("electricalnetwork");
		String irradSensorIRI=joforess.getString("irradiationsensor");
		OntModel model = readModelGreedy(ENIRI);
		JSONObject res=runMODS(model,irradSensorIRI);
		JSONObject result=updateOWLValue(res,"http://www.theworldavatar.com/kb/sgp/semakauisland/semakauelectricalnetwork/","PV-002.owl","EBus-006.owl");
		//hardcoded at the moment the iri due to model restriction
		
		AgentCaller.printToResponse(result, response);
			
	}
	

	/** reads the topnode into an OntModel of all its subsystems. 
	 * @param iriofnetwork
	 * @return
	 */
	public static OntModel readModelGreedy(String iriofnetwork) { //model will get all the subsystems
		SelectBuilder sb = new SelectBuilder().addPrefix("j2","http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#" )
				.addWhere("?entity" ,"a", "j2:CompositeSystem").addWhere("?entity" ,"j2:hasSubsystem", "?component");
		String wasteInfo = sb.build().toString();

		QueryBroker broker = new QueryBroker();
		return broker.readModelGreedy(iriofnetwork, wasteInfo);
	}
	/** creates Query for PV variables
	 * 
	 * @return String Query
	 */
	public String getPVQuery() {
		SelectBuilder sb = new SelectBuilder()
				.addPrefix("j1","http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#")
				.addPrefix("j2", "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#")
				.addPrefix("j6", "http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#")
				.addPrefix("j9", "http://www.theworldavatar.com/ontology/meta_model/mereology/mereology.owl#")
				.addVar("?entity").addVar("?widthvalue").addVar("?lengthvalue").addVar("?effvalue").addVar("?panelnovalue")
				.addWhere("?obj", "a", "j1:PowerGenerator")
				.addWhere("?obj", "j9:isComposedOf", "?entity")
				
				.addWhere("?entity", "j1:hasPanelWidth", "?width")
				.addWhere( "?width", "j2:hasValue", "?vwidth")
				.addWhere( "?vwidth", "j2:numericalValue", "?widthvalue")
				
				.addWhere("?entity", "j1:hasPanelLength", "?length")
				.addWhere( "?length", "j2:hasValue", "?vlength")
				.addWhere( "?vlength", "j2:numericalValue", "?lengthvalue")
				
				.addWhere("?obj", "j6:hasEfficiency", "?eff")
				.addWhere( "?eff", "j2:hasValue", "?veff")
				.addWhere( "?veff", "j2:numericalValue", "?effvalue")
				
				.addWhere("?obj", "j1:hasNumberOfPanels", "?panelno")
				.addWhere( "?panelno", "j2:hasValue", "?vpanelno")
				.addWhere( "?vpanelno", "j2:numericalValue", "?panelnovalue");
		return sb.buildString();
	}
	/** get Query from Irradiation OWL
	 * @return String Query
	 */
	public String getSolarData() {
		WhereBuilder whereB = new WhereBuilder().addPrefix("j2", "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#")
	    			.addPrefix("j4", "http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#")
	    			.addPrefix("j5","http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#")
	    			.addPrefix("j6", "http://www.w3.org/2006/time#").addWhere("?entity", "j4:observes", "?prop")
	    			.addWhere("?prop", "j2:hasValue", "?vprop").addWhere("?vprop", "j2:numericalValue", "?propval")
	    			.addWhere("?vprop", "j6:hasTime", "?proptime").addWhere("?proptime", "j6:inXSDDateTime", "?proptimeval");
		SelectBuilder sensorIrrad = new SelectBuilder()
	    			.addPrefix("j5","http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#")
	    			.addVar("?entity").addVar("?propval").addVar("?proptimeval")
	    			.addWhere("?entity","a", "j5:Q-Sensor").addWhere(whereB).addOrderBy("?proptimeval");
		return sensorIrrad.buildString();
	    	
	}
	/** creates inputs from power generator variables as well as solar irradiation readings
	 * 
	 * @param model
	 * @param filename
	 * @param irradSensorIRI
	 * @return
	 */
	public List<Double> createInputCSV(OntModel model,String filename,String irradSensorIRI) {
		List<String[]> inputcsv=new ArrayList<String[]>();
		List<Double> xvalue=new ArrayList<Double>();
		String pvquery =  getPVQuery();
		
		String sensorinfo2 = getSolarData();

		String result2 = new QueryBroker().queryFile(irradSensorIRI, sensorinfo2);
		String[] keys2 = JenaResultSetFormatter.getKeys(result2);
		List<String[]> resultListfromqueryirr = JenaResultSetFormatter.convertToListofStringArrays(result2, keys2);
		Double irr=Double.valueOf(resultListfromqueryirr.get(resultListfromqueryirr.size()-1)[1]); //come from the OCR
 			
 			
		ResultSet resultSet = JenaHelper.query(model, pvquery);
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
		String[] keys = JenaResultSetFormatter.getKeys(result);
		List<String[]> resultListPV = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
		int inputPropnum=2; //P and v
		
		for (int d = 0; d < inputPropnum; d++) {
			for (int item = 0; item < resultListPV.size(); item++) {
				if (d == 0) {
					Double P = Double.valueOf(resultListPV.get(item)[1]) * Double.valueOf(resultListPV.get(item)[2])
							* Double.valueOf(resultListPV.get(item)[3]) * Double.valueOf(resultListPV.get(item)[4])
							* irr/1000000; //in MW
					xvalue.add(P);
					String[]content={"property-"+d+"="+P}; //calculated by irr*area*efficiency
					inputcsv.add(content);
					System.out.println("P calculated= "+P);//hardcoded value before is 0.34840962
					
				} else {
					xvalue.add(0.75);
					String[]content={"property-"+d+"="+0.75}; //supposed to be the voltage of the bus attached to the pv gen
					inputcsv.add(content);
				}
			}

		}

		
		String s = MatrixConverter.fromArraytoCsv(inputcsv);
		new QueryBroker().putLocal(filename, s);
		
		
		return xvalue;
	}
	/** Executes simulator producing the phase angle, reactive power, active power and voltage coming from the model solar panel
	 * 
	 * @param model OntModel from readModel greedy, has the subsystems available
	 * @param irradSensorIRI Singapore irradiation sensor IRI, contains irradiation data from the last 48 hours
	 * @return
	 */
	public JSONObject runMODS(OntModel model,String irradSensorIRI) {
			
		
		String absdirinput=QueryBroker.getLocalDataPath()+"/XVALUEPV.CSV";
		List<Double> xData= createInputCSV(model,absdirinput,irradSensorIRI);		
		String PrPWOUTPVCSV = QueryBroker.getLocalDataPath()+"/PrPWOUTPV.CSV"; 


		
		String simDir = Sim4; // pass the directory of the power world surrogate model to simDir
		String modelName = "HDMR_Alg_1";
		try {

			System.load(root + "/MoDS_Java_API_0.1.dll"); // not recommended--Messing with the library path on the
															// command line

		} catch (Error e) {
			logger.error(e.getMessage());

		}
		
		JSONObject ans= new JSONObject();

		//make content of the output
		List<Double> yData = MoDSAPI.evaluateSurrogate(simDir, modelName, xData); // call MoDS API to evaluate the surrogate model basing on the MoDS simulation file "simDir -> modelNam" and the input xData that was collected before
		System.out.println("Success!");														
		System.out.println("yData thetaPV1=" + yData.get(29));   //43 for pv2   //57 for pv3
		System.out.println("yData VoltagePV1=" + yData.get(30));
		System.out.println("yData PLoadPV1=" + yData.get(31));//usually=0
		System.out.println("yData QLoadPV1=" + yData.get(32));//usually=0
		System.out.println("yData PGenPV1=" + yData.get(33));
		System.out.println("yData QGenPV1=" + yData.get(34)); //48for pv2 //62 for pv3
		
		String[]testarray= {""+yData.get(29),""+yData.get(30),""+yData.get(33),""+yData.get(34)};
		int count=0;
		for(int t=0;t<testarray.length;t++) {
			if(!testarray[t].contains("NaN")) {
				count++;
			}
		}
		if(count==4) {
		ans.put("theta", yData.get(29));
		ans.put("voltage", yData.get(30));
		ans.put("PGen", yData.get(33));
		ans.put("QGen", yData.get(34));
		}
		else {
			ans.put("theta", 0.0);
			ans.put("voltage", 1.0);
			ans.put("PGen", 0.0);
			ans.put("QGen", 0.0);
		}
		
		List<String[]> stringsoutput = new ArrayList<String[]>();
		// make header of the output: CURRENTLY STILL FAILS
		//List<String> yNames = com.cmclinnovations.mods.api.MoDSAPI.getYVarNamesFromAPI(simDir, modelName);	
		int t=0;
		String[] e= new String[yData.size()];
		for (Double d : yData) {			
			e[t]=(d.toString());
			t++;
		}
		stringsoutput.add(e);
		String s = MatrixConverter.fromArraytoCsv(stringsoutput);
		new QueryBroker().putLocal(PrPWOUTPVCSV, s);
		
		
		return ans;
		

	}
	/** feeds a query and gets a result
	 * 
	 * @param name IRI
	 * @param query
	 * @return
	 */
	public static List<String[]> queryResult(String name, String query) {
		String result = new QueryBroker().queryFile(name, query);
		String[] keys = JenaResultSetFormatter.getKeys(result);
		List<String[]> resultListfromquery = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
		return resultListfromquery;
	}
	/** Updates OWL values in PV, and Bus 6
	 * a. queries OWL files for pre-existing values
	 * 
	 * @param ans
	 * @param prefix
	 * @param genfilename
	 * @param busfilename
	 * @return
	 */
	public JSONObject updateOWLValue(JSONObject ans,String prefix,String genfilename, String busfilename ) {
		
		JSONObject ans2= new JSONObject();
		SelectBuilder sb = TimeSeriesConverter.createQueryForPowerGeneratorPV();
		WhereBuilder wb = new WhereBuilder().addPrefix("j6", "http://www.w3.org/2006/time#")
				.addWhere("?vpg", "j6:hasTime", "?proptime").addWhere("?proptime", "j6:inXSDDateTime", "?proptimeval")
				.addWhere("?vqg", "j6:hasTime", "?proptime").addWhere("?proptime", "j6:inXSDDateTime", "?proptimeval");
		String genInfo = sb.addVar("?proptimeval").addWhere(wb).addOrderBy("?proptime").buildString();
		List<String[]> resultListfromquerygen = queryResult(prefix+genfilename, genInfo);
		
		SelectBuilder sb2 = TimeSeriesConverter.createQueryForBus() ;
		WhereBuilder wb2 = new WhereBuilder().addPrefix("j6", "http://www.w3.org/2006/time#")
				.addPrefix("j2", "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#")
				.addPrefix("j3", "http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#")
				.addPrefix("j5", "http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#")
				.addWhere("?vVM", "j6:hasTime", "?proptime").addWhere("?proptime", "j6:inXSDDateTime", "?proptimeval")
				.addWhere("?vVA", "j6:hasTime", "?proptime").addWhere("?proptime", "j6:inXSDDateTime", "?proptimeval")
				.addWhere("?model", "j5:hasModelVariable" ,"?BKV")
				.addWhere("?BKV", "a" ,"j3:baseKV")
				.addWhere("?BKV", "j2:hasValue" ,"?vBKV")
				.addWhere("?vBKV", "j2:numericalValue" ,"?BaseKVvalue");

		String busInfo = sb2.addVar("?proptimeval").addVar("?BaseKVvalue").addWhere(wb2).addOrderBy("?proptime").buildString();
		List<String[]>  resultListfromquerybus  = queryResult(prefix+busfilename, busInfo);
		//postprocessing the merged query result //gen and bus must have 2 same time set element amount
		List<String[]> readingFromCSV = new ArrayList<String[]>();
		for (int d=0;d<resultListfromquerygen.size();d++) {
			String timewholecsv=resultListfromquerygen.get(d)[2];
			String monthdatecsv=timewholecsv.split("-")[1]+"-"+timewholecsv.split("-")[2].split("T")[0];			
			String timecsv=timewholecsv.split("-")[2].split("T")[1].split("\\+")[0];
			String[]e= {timewholecsv.split("-")[0],monthdatecsv,timecsv,resultListfromquerygen.get(d)[0],resultListfromquerygen.get(d)[1],resultListfromquerybus.get(d)[0],resultListfromquerybus.get(d)[1]};
			readingFromCSV.add(e);
		}
		
		//convert volt to pu
		Double puvalue= ans.getDouble("voltage")/Double.valueOf(resultListfromquerybus.get(0)[3]);
		   DateTimeFormatter dtf = DateTimeFormatter.ofPattern("yyyy/MM/dd HH:mm:ss");
		   LocalDateTime now = LocalDateTime.now();
		   String com=dtf.format(now);
		   String date=com.split("/")[2].split(" ")[0];
		readingFromCSV.remove(0);
		String[]newline= {com.split("/")[0],com.split("/")[1]+"-"+date,com.split("/")[2].split(" ")[1],""+ans.get("PGen"),""+ans.get("QGen"),""+puvalue,""+ans.get("theta")}; //time in singapore time
		readingFromCSV.add(newline);
		String[]header= {"year","monthdate","time","PGen","QGen","VmPu","Va"};
		readingFromCSV.add(0,header);
		try {
			new TimeSeriesConverter().startConversion(readingFromCSV,"gen",prefix,genfilename);
			new TimeSeriesConverter().startConversion(readingFromCSV,"bus",prefix,busfilename);
		} catch (Exception e) {
			// TODO Auto-generated catch block
			logger.error(e.getMessage());
		}
		ans2.put("gen",prefix+genfilename);
		ans2.put("bus",prefix+busfilename);
		
		return ans2;
	}
}
