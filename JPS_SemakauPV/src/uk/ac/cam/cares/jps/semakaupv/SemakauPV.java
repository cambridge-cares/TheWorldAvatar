package uk.ac.cam.cares.jps.semakaupv;

import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.json.JSONObject;

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
	public static String tomcatolddir="C:/apache-tomcat-8.0.24/webapps/ROOT";

	public static String root=AgentLocator.getProperty("absdir.root");
	String Sim4 = root+"/Sim_PV1"; // THIS SIMULATION NEED TO BE EXIST 
	
	public static String XVALUE4 = new String(tomcatolddir+"/XVALUEPV.CSV");//not exist yet 
	//public static String tomcatdir="C:/apache-tomcat-8.0.24/webapps/ROOT";
	
	protected void doGetJPS(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		
		JSONObject joforess = AgentCaller.readJsonParameter(request);
		String ENIRI=joforess.getString("electricalnetwork");
		String irradSensorIRI=joforess.getString("irradiationsensor");
		OntModel model = readModelGreedy(ENIRI);
		JSONObject res=runMODS(model,irradSensorIRI);
		
		AgentCaller.printToResponse(res, response);
			
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
 					+ "PREFIX j6:<http://www.w3.org/2006/time#> " + "SELECT ?entity ?propval ?proptimeval "
 					+ "WHERE { ?entity a j5:Q-Sensor ." + "  ?entity j4:observes ?prop ." + " ?prop   j2:hasValue ?vprop ."
 					+ " ?vprop   j2:numericalValue ?propval ." + " ?vprop   j6:hasTime ?proptime ."
 					+ " ?proptime   j6:inXSDDateTimeStamp ?proptimeval ." + "}" + "ORDER BY ASC(?proptimeval)";

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
					System.out.println("P calculated= "+P);
					for(int r=1;r<=4;r++) {
						System.out.println("property gotten= "+resultListPV.get(item)[r]);
					}
					System.out.println("irr= "+irr);
					
				} else {
					xvalue.add(0.75);
					String[]content={"property-"+d+"="+0.75}; //supposed to be the voltage of the bus attached to the pv gen
					inputcsv.add(content);
				}
			}

		}

		
				
				
//		for(int n=0;n<6;n++) {
//			if(n<3) { //PV power 1,2,3
//				String[]content={"X"+n+"="+"0.34840962"}; //calculated by irr*area*efficiency
//				inputcsv.add(content);
//				xvalue.add(0.34840962);
//			}
//			else { //PV Voltage 1,2,3
//				
//				String[]content={"X"+n+"="+"0.75"}; //queried
//				inputcsv.add(content);
//				xvalue.add(0.75);
//			}
//			
//		}
		
		String s = MatrixConverter.fromArraytoCsv(inputcsv);
		new QueryBroker().putLocal(filename, s);
		
		
		return xvalue;
	}
	
	public JSONObject runMODS(OntModel model,String irradSensorIRI) {
			
		
		String absdirinput=QueryBroker.getLocalDataPath()+"/XVALUEPV.CSV";
		List<Double> xData= createInputCSV(model,absdirinput,irradSensorIRI);		
		String PrPWOUTPVCSV = QueryBroker.getLocalDataPath()+"/PrPWOUTPV.CSV"; 


		
		String simDir = Sim4;                                                                          // pass the directory of the power world sorrogate model to simDir
		String modelName = "HDMR_Alg_1";
		FileWriter fileWriter = null;
		try {			
					
			System.load(root+"/MoDS_Java_API_0.1.dll");                     // not recommended--Messing with the library path on the command line
			// THIS LIBRARY NEED TO BE EXIST 
			
			//make header of the output: CURRENTLY STILL FAILS
			
//			List<String> yNames = com.cmclinnovations.mods.api.MoDSAPI.getYVarNamesFromAPI(simDir, modelName);	
//				for (int j = 0; j < yNames.size(); j++) {
//					fileWriter.append(yNames.get(j));                                                   // write the yNames to the output CSV file
//					fileWriter.append(",");
//					System.out.println(yNames.get(j));
//				}
				
			} catch (Error e) {
		e.printStackTrace();
				
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
		
		ans.put("theta", yData.get(29));
		ans.put("voltage", yData.get(30));
		ans.put("PGen", yData.get(33));
		ans.put("QGen", yData.get(34));
		
		List<String[]> stringsoutput = new ArrayList<String[]>();
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

}
