package uk.ac.cam.cares.jps.powsys.carbontax;

import java.io.File;
import java.io.IOException;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.json.JSONArray;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.CommandHelper;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;
import uk.ac.cam.cares.jps.powsys.nuclear.IriMapper;

@WebServlet(urlPatterns = { "/optimizeforcarbontax" })
public class CarbonTaxAgent extends JPSHttpServlet {

	private static final long serialVersionUID = -2354646810093235777L;
	private Logger logger = LoggerFactory.getLogger(CarbonTaxAgent.class);

	@Override
	protected void doGetJPS(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
	
		JSONObject jo = AgentCaller.readJsonParameter(request);
		BigDecimal carbontax = jo.getBigDecimal("carbontax");
		logger.info("start optimization for carbon tax = " + carbontax);
		
		JSONObject result = new JSONObject();
		JSONArray ja = new JSONArray();
		ja.put("http://www.theworldavatar.com/kb/powerplants/Keppel_Merlimau_Cogen_Power_Plant_Singapore.owl#Keppel_Merlimau_Cogen_Power_Plant_Singapore");
		ja.put("http://www.theworldavatar.com/kb/powerplants/SembCorp_Pulau_Sakra_CCGT_Cogen_Power_Station_Singapore.owl#SembCorp_Pulau_Sakra_CCGT_Cogen_Power_Station_Singapore");
		ja.put("http://www.theworldavatar.com/kb/powerplants/Jurong_Island_-_PLP_CCGT_Power_Plant_Singapore.owl#Jurong_Island_-_PLP_CCGT_Power_Plant_Singapore");
		ja.put("http://www.theworldavatar.com/kb/powerplants/PowerSeraya_OCGT_Power_Plant_Singapore.owl#PowerSeraya_OCGT_Power_Plant_Singapore");
		ja.put("http://www.theworldavatar.com/kb/powerplants/PowerSeraya_Pulau_Seraya_Oil_Power_Station_Singapore.owl#PowerSeraya_Pulau_Seraya_Oil_Power_Station_Singapore");
		ja.put("http://www.theworldavatar.com/kb/powerplants/PowerSeraya_Pulau_Seraya_CCGT_Cogen_Power_Plant_Singapore.owl#PowerSeraya_Pulau_Seraya_CCGT_Cogen_Power_Plant_Singapore");
		result.put("substitutionalpowerplants", ja);
		
		logger.info("optimization result = " + result);
		
		AgentCaller.printToResponse(result, response);
	}
	
	public void runGAMS() throws IOException, InterruptedException {
        System.out.println("Start");
        System.out.println("separator= "+File.separator);
        String executablelocation ="C:/GAMS/win64/26.1/gams.exe";
        //String folderlocation ="D:/Users/KADIT01/Documents/gamsdir/projdir/";
        String folderlocation ="C:/JPS_DATA/workingdir/JPS_POWSYS/";
        String[] cmdArray = new String[5];
        
        cmdArray[0] = executablelocation;
        cmdArray[1] = folderlocation + "final.gms";
        cmdArray[2] = "WDIR="+folderlocation;
        cmdArray[3] = "SCRDIR="+folderlocation;
        cmdArray[4] = "LO=2";
//      cmdArray[2] = "WDIR="+folderlocation + "TMP";
//      cmdArray[3] = "SCRDIR="+folderlocation + "TMP";
        
        String cmdArrayinstring=cmdArray[0]+" "+cmdArray[1]+","+cmdArray[2]+","+cmdArray[3]+" "+cmdArray[4];
        
		//System.out.println(cmdArrayinstring);
        //Process p = Runtime.getRuntime().exec(cmdArray);
		   //p.waitFor();
		String startbatCommand ="C:/JPS_DATA/workingdir/JPS_POWSYS/gamsexecute.bat";
		
		ArrayList<String> groupcommand= new ArrayList<String>();
		groupcommand.add("start");
		groupcommand.add("C:/JPS_DATA/workingdir/JPS_POWSYS/gamsexecute.bat");
		
		CommandHelper.executeSingleCommand(folderlocation,startbatCommand);
//		CommandHelper.executeCommands(folderlocation, groupcommand);   
        System.out.println("Done");
	}
	
	public void prepareCSVGeneratorParameter(String ENiri, String baseUrl) {		

		String genInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
				+ "PREFIX j4:<http://www.theworldavatar.com/ontology/meta_model/topology/topology.owl#> "
				+ "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
				+ "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
				+ "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
				+ "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
				+ "SELECT ?entity ?plant ?Pmaxvalue "

				+ "WHERE {?entity  a  j1:PowerGenerator  ."
				+ "?entity   j2:isSubsystemOf ?plant ." //plant
				+ "?entity   j2:isModeledBy ?model ."
				+ "?model   j5:hasModelVariable ?pmax ." 
				+ "?pmax  a  j3:PMax  ." 
				+ "?pmax  j2:hasValue ?vpmax ."
				+ "?vpmax   j2:numericalValue ?Pmaxvalue ." // pmax

				+ "}";
   
		QueryBroker broker = new QueryBroker();
		
    	String result = broker.queryFile(ENiri, genInfo);
    	String[] keys = {"entity", "Pmaxvalue", "plant"};
    	List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
    	
		//logger.info("number of queried lot entities = " + resultList.size());
    	
    	IriMapper mapper= new IriMapper();
		for (int i=0; i<resultList.size(); i++) {
			String[] current = resultList.get(i);
			String id = "s"+(i+1);
			mapper.add(current[0], id, "lot");
			current[0]=id;
		}
    
		String csv = mapper.serialize();
	    broker.put(baseUrl + "/mappingforlot.csv", csv);
	    
	    String[] header = {"id","ys","xs","as","dcs"};
	    resultList.add(0, header);
	    String s = MatrixConverter.fromArraytoCsv(resultList);
	    broker.put(baseUrl + "/inputlandlots.csv", s);
	    
	    logger.info("landlots input ok"); 
	}
	
}
