package uk.ac.cam.cares.jps.des;

import java.io.File;
import java.io.IOException;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.json.JSONObject;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.config.AgentLocator;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;
import uk.ac.cam.cares.jps.base.util.PythonHelper;


@WebServlet(urlPatterns = { "/DESAgent" })
public class DistributedEnergySystem extends JPSHttpServlet {
	
    @Override
    protected void doHttpJPS(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
        logger = LoggerFactory.getLogger(DistributedEnergySystem.class);
        super.doHttpJPS(request, response);
    }

    @Override
    protected void doHttpJPS(HttpServletRequest request, HttpServletResponse response, JSONObject reqBody) throws IOException, ServletException {
        logger = LoggerFactory.getLogger(DistributedEnergySystem.class);
        super.doHttpJPS(request, response, reqBody);
    }

    @Override
    protected JSONObject processRequestParameters(JSONObject requestParams) {
		QueryBroker broker = new QueryBroker();
		String baseUrl = QueryBroker.getLocalDataPath();
		String iriofnetwork = requestParams.getString("electricalnetwork");
		OntModel model = readModelGreedy(iriofnetwork);
		List<String[]> producer = provideGenlist(model); // instance iri
		List<String[]> consumer = provideLoadlist(model); // instance iri and its class

		String producercsv = MatrixConverter.fromArraytoCsv(producer);
		broker.putLocal(baseUrl + "/inputprod.csv", producercsv);

		String consumercsv = MatrixConverter.fromArraytoCsv(consumer);
		broker.putLocal(baseUrl + "/inputcons.csv", consumercsv);
		
		
		copyTemplate(baseUrl, "schedule.csv");
		copyTemplate(baseUrl, "weather.csv");
		copyTemplate(baseUrl, "constraint.csv");
		try {
			runOptimization(baseUrl);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		String directory = baseUrl + "";
		JSONObject newresult = new JSONObject();
		newresult.put("folder", directory);

		return newresult;
    	
    }
    
    public void runOptimization(String baseUrl) throws IOException {
    	String plantIRI="parameter";
    	String DES = PythonHelper.callPython("Receding Horizon Optimization.py", plantIRI, this);
    	
    	
    }
	public void copyTemplate(String newdir, String filename) {
		File file = new File(AgentLocator.getCurrentJpsAppDirectory(this) + "/workingdir/"+filename);
		
		String destinationUrl = newdir + "/"+filename;
		new QueryBroker().putLocal(destinationUrl, file);
	}
    
	public static OntModel readModelGreedy(String iriofnetwork) {
		String electricalnodeInfo = "PREFIX j1:<http://www.jparksimulator.com/ontology/ontoland/OntoLand.owl#> "
				+ "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
				+ "SELECT ?component "
				+ "WHERE {?entity  a  j2:CompositeSystem  ." + "?entity   j2:hasSubsystem ?component ." + "}";

		QueryBroker broker = new QueryBroker();
		return broker.readModelGreedy(iriofnetwork, electricalnodeInfo);
	}
    
    public static List<String[]> provideGenlist(OntModel model) {
        String gennodeInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
                + "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
                + "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
                + "PREFIX j4:<http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#> "
                + "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
                + "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
                + "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
                + "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
                + "PREFIX j9:<http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#> "
                + "SELECT ?entity "
                + "WHERE {?entity  a  j1:PowerGenerator  ."
                //+ "FILTER EXISTS {?entity j2:isSubsystemOf ?plant } " //filtering gen 001 as it is slackbus
                + "}";


        
        ResultSet resultSet = JenaHelper.query(model, gennodeInfo);
        String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
        String[] keys = JenaResultSetFormatter.getKeys(result);
        List<String[]> resultListfromquery = JenaResultSetFormatter.convertToListofStringArrays(result, keys);

        return resultListfromquery;
    }
    
    public static List<String[]> provideLoadlist(OntModel model) {
        String gennodeInfo = "PREFIX j1:<http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#> "
                + "PREFIX j2:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> "
                + "PREFIX j3:<http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#> "
                + "PREFIX j4:<http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#> "
                + "PREFIX j5:<http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#> "
                + "PREFIX j6:<http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_behavior/behavior.owl#> "
                + "PREFIX j7:<http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#> "
                + "PREFIX j8:<http://www.theworldavatar.com/ontology/ontocape/material/phase_system/phase_system.owl#> "
                + "PREFIX j9:<http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#> "
                + "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> "
                + "SELECT ?entity ?class "
                + "WHERE {?entity  a  ?class  ."
        		+ " {?class rdfs:subClassOf j1:PowerLoad ."
        		+ "} "
        		+ "UNION { ?class a j1:PowerLoad . } ."
        		//group plant (electrolizer) load
        		+ "OPTIONAL {?entity j5:hasEmission ?emission "
        		+ "?entity   j7:hasGISCoordinateSystem ?coorsys ."
        		+ "?entity   j7:hasGISCoordinateSystem ?coorsys ."
        		+ "?entity   j7:hasGISCoordinateSystem ?coorsys ."
        		+ "?entity   j7:hasGISCoordinateSystem ?coorsys ."
        		+ "}"
        		//group (commercial builiding) load       		
        		+ "OPTIONAL {?entity j5:hasEmission ?emission "
        		+ "?entity   j7:hasGISCoordinateSystem ?coorsys ."
        		+ "?entity   j7:hasGISCoordinateSystem ?coorsys ."
        		+ "?entity   j7:hasGISCoordinateSystem ?coorsys ."
        		+ "?entity   j7:hasGISCoordinateSystem ?coorsys ."
        		+ "}"
        		//group (Residential Household) load       		
        		+ "OPTIONAL {?entity j5:hasEmission ?emission "
        		+ "?entity   j7:hasGISCoordinateSystem ?coorsys ."
        		+ "?entity   j7:hasGISCoordinateSystem ?coorsys ."
        		+ "?entity   j7:hasGISCoordinateSystem ?coorsys ."
        		+ "?entity   j7:hasGISCoordinateSystem ?coorsys ."
        		+ "}"
        		//group Fuel Cell load       		
        		+ "OPTIONAL {?entity j5:hasEmission ?emission "
        		+ "?entity   j7:hasGISCoordinateSystem ?coorsys ."
        		+ "?entity   j7:hasGISCoordinateSystem ?coorsys ."
        		+ "?entity   j7:hasGISCoordinateSystem ?coorsys ."
        		+ "?entity   j7:hasGISCoordinateSystem ?coorsys ."
        		+ "}"
        		

                + "}";

        ResultSet resultSet = JenaHelper.query(model, gennodeInfo);
        String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
        String[] keys = JenaResultSetFormatter.getKeys(result);
        List<String[]> resultListfromquery = JenaResultSetFormatter.convertToListofStringArrays(result, keys);

        return resultListfromquery;
    }

}
