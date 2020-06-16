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
	
	public static OntModel readModelGreedy(String iriofnetwork) {
		String electricalnodeInfo = "PREFIX j1:<http://www.jparksimulator.com/ontology/ontoland/OntoLand.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "SELECT ?component "
				+ "WHERE {?entity  a  j2:CompositeSystem  ." + "?entity   j2:hasSubsystem ?component ." + "}";

		QueryBroker broker = new QueryBroker();
		return broker.readModelGreedy(iriofnetwork, electricalnodeInfo);
	}
	
	
	public List<Double> createInputCSV(OntModel model,String filename,String irradSensorIRI) {
		List<String[]> inputcsv=new ArrayList<String[]>();
		List<Double> xvalue=new ArrayList<Double>();
		String pvquery = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#> "
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
				+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
				+ "PREFIX j9:<http://www.theworldavatar.com/ontology/meta_model/mereology/mereology.owl#> "
				+ "SELECT ?entity ?widthvalue ?lengthvalue ?effvalue ?panelnovalue " 
				+ "WHERE {?obj  a  j1:PowerGenerator  ."
				+ "?obj j9:isComposedOf ?entity ."
				
				+ "?entity   j1:hasPanelWidth ?width ." 
				+ "?width     j2:hasValue ?vwidth ."
				+ "?vwidth  j2:numericalValue ?widthvalue ."

				+ "?entity   j1:hasPanelLength ?length ." 
				+ "?length     j2:hasValue ?vlength ."
				+ "?vlength  j2:numericalValue ?lengthvalue ."
				
				+ "?obj   j6:hasEfficiency ?eff ." 
				+ "?eff     j2:hasValue ?veff ."
				+ "?veff  j2:numericalValue ?effvalue ."
				
				+ "?obj   j1:hasNumberOfPanels ?panelno ." 
				+ "?panelno     j2:hasValue ?vpanelno ."
				+ "?vpanelno  j2:numericalValue ?panelnovalue ."

				+ "?obj   j1:hasRatedVoltage ?voltage ." 
				+ "?voltage     j2:hasValue ?vvoltage ."
				+ "?vvoltage  j2:numericalValue ?voltagevalue ."
				
				+ "}";
		
			String sensorinfo2 = "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
 					+ "PREFIX j4:<http://www.theworldavatar.com/ontology/ontosensor/OntoSensor.owl#> "
 					+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/process_control_equipment/measuring_instrument.owl#> "
 					+ "PREFIX j6:<http://www.w3.org/2006/time#> " 
 					+ "SELECT ?entity ?propval ?proptimeval "
 					+ "WHERE { ?entity a j5:Q-Sensor ." 
 					+ "  ?entity j4:observes ?prop ." 
 					+ " ?prop   j2:hasValue ?vprop ."
 					+ " ?vprop   j2:numericalValue ?propval ." 
 					+ " ?vprop   j6:hasTime ?proptime ."
 					+ " ?proptime   j6:inXSDDateTime ?proptimeval ." 
 					+ "}" 
 					+ "ORDER BY ASC(?proptimeval)";

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
	
	public JSONObject runMODS(OntModel model,String irradSensorIRI) {
			
		
		String absdirinput=QueryBroker.getLocalDataPath()+"/XVALUEPV.CSV";
		List<Double> xData= createInputCSV(model,absdirinput,irradSensorIRI);		
		String PrPWOUTPVCSV = QueryBroker.getLocalDataPath()+"/PrPWOUTPV.CSV"; 


		
		String simDir = Sim4; // pass the directory of the power world sorrogate model to simDir
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
	
	public JSONObject updateOWLValue(JSONObject ans,String prefix,String genfilename, String busfilename ) {
		
		JSONObject ans2= new JSONObject();

		
		String genInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
				+ "PREFIX j6:<http://www.w3.org/2006/time#> "
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
				+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
				+ "SELECT ?activepowervalue ?reactivepowervalue ?proptimeval "

				+ "WHERE {?entity  a  j1:PowerGenerator  ."
				+ "?entity   j2:isModeledBy ?model ."

				+ "?model   j5:hasModelVariable ?Pg ." 
				+ "?Pg  a  j3:Pg  ." 
				+ "?Pg  j2:hasValue ?vpg ."
				+ "?vpg   j2:numericalValue ?activepowervalue ." // pg
				+ " ?vpg   j6:hasTime ?proptime ."
				+ " ?proptime   j6:inXSDDateTime ?proptimeval ." 
				

				+ "?model   j5:hasModelVariable ?Qg ." 
				+ "?Qg  a  j3:Qg  ." 
				+ "?Qg  j2:hasValue ?vqg ."
				+ "?vqg   j2:numericalValue ?reactivepowervalue ." // qg
				+ " ?vqg   j6:hasTime ?proptime ."
				+ " ?proptime   j6:inXSDDateTime ?proptimeval ."
				+ "}" 
				+ "ORDER BY ASC(?proptime)"; 
		
		String result3 = new QueryBroker().queryFile(prefix+genfilename, genInfo);
		String[] keys3 = JenaResultSetFormatter.getKeys(result3);
		List<String[]> resultListfromquerygen = JenaResultSetFormatter.convertToListofStringArrays(result3, keys3);
		
		String busInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
				+ "PREFIX j6:<http://www.w3.org/2006/time#> "
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
				+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
				+ "SELECT ?VoltMagvalue ?VoltAnglevalue ?proptimeval ?BaseKVvalue "

				+ "WHERE {?entity  a  j1:BusNode  ." 
				+ "?entity   j2:isModeledBy ?model ."

				+ "?model   j5:hasModelVariable ?VM ." 
				+ "?VM  a  j3:Vm  ." 
				+ "?VM  j2:hasValue ?vVM ."
				+ "?vVM   j2:numericalValue ?VoltMagvalue ." // Vm
				+ " ?vVM   j6:hasTime ?proptime ."
				+ " ?proptime   j6:inXSDDateTime ?proptimeval ." 

				+ "?model   j5:hasModelVariable ?VA ." 
				+ "?VA  a  j3:Va  ." 
				+ "?VA  j2:hasValue ?vVA ."
				+ "?vVA   j2:numericalValue ?VoltAnglevalue ." // Va
				+ " ?vVA   j6:hasTime ?proptime ."
				+ " ?proptime   j6:inXSDDateTime ?proptimeval ." 

				+ "?model   j5:hasModelVariable ?BKV ." 
				+ "?BKV  a  j3:baseKV  ." 
				+ "?BKV  j2:hasValue ?vBKV ."
				+ "?vBKV   j2:numericalValue ?BaseKVvalue ." // Base KV
				+ "}" 
				+ "ORDER BY ASC(?proptime)"; 
		
		String result1 = new QueryBroker().queryFile(prefix+busfilename, busInfo);
		String[] keys1 = JenaResultSetFormatter.getKeys(result1);
		List<String[]> resultListfromquerybus = JenaResultSetFormatter.convertToListofStringArrays(result1, keys1);
		
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
