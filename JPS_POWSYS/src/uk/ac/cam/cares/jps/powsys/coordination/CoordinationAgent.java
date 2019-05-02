package uk.ac.cam.cares.jps.powsys.coordination;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URI;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.ResultSet;
import org.apache.jena.rdf.model.RDFNode;
import org.json.JSONArray;
import org.json.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import uk.ac.cam.cares.jps.base.discovery.AgentCaller;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.JenaHelper;
import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.query.sparql.JenaModelWrapper;
import uk.ac.cam.cares.jps.base.query.sparql.Paths;
import uk.ac.cam.cares.jps.base.query.sparql.Prefixes;
import uk.ac.cam.cares.jps.base.query.sparql.QueryBuilder;
import uk.ac.cam.cares.jps.base.scenario.JPSHttpServlet;
import uk.ac.cam.cares.jps.base.scenario.ScenarioClient;
import uk.ac.cam.cares.jps.base.util.FileUtil;
import uk.ac.cam.cares.jps.powsys.electricalnetwork.ENAgent;
import uk.ac.cam.cares.jps.powsys.util.Util;

public class CoordinationAgent extends JPSHttpServlet implements Prefixes, Paths {

	private static final long serialVersionUID = 6859324316966357379L;
	private Logger logger = LoggerFactory.getLogger(CoordinationAgent.class);

	@Override
	protected void doGetJPS(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
	
		JSONObject jo = AgentCaller.readJsonParameter(request);
		String electricalNetwork = jo.getString("electricalnetwork");
		List<Object> powerplants = jo.getJSONArray("powerplants").toList();		
	}
	
	public String coordinate(String scenarioUrlOfMockedAgent, JSONObject input) {
		
		String electricalNetwork = input.getString("electricalnetwork");
		
		OntModel model = deletePowerGeneratorsFromElectricalNetwork(electricalNetwork);
		
		List<String> plants = getNuclearPowerPlantsFromMockedScenarioAgent(scenarioUrlOfMockedAgent);
		List<String> newGenerators = completeNuclearPowerGenerators(scenarioUrlOfMockedAgent, plants);
		addNuclearPowerGeneratorsToElectricalNetwork(electricalNetwork, newGenerators);
		
		// model still contains triples from the deleted power generators
		// however, we don't have to reload the model corresponding to the modified electrical network top node
		// since we are only querying buses from model in the following method
		connectNuclearPowerGeneratorsToOptimalBus(model, newGenerators);
		
		// TODO-AE SC URGENT 20190430 PF or OPF and doGETJPS
		// TODO-AE SC URGENT 20190430 remove direct call by agent caller
		//return AgentCaller.executeGetWithJsonParameter("JPS_POWSYS/ENAgent/startsimulationPF", input.toString());
		String baseUrl = QueryBroker.getLocalDataPath() + "/JPS_POWSYS_EN";
		String modeltype = "PF";
		try {
			new ENAgent().startSimulation(electricalNetwork, baseUrl, modeltype);
			return "";
		} catch (IOException e) {
			throw new JPSRuntimeException(e.getMessage(), e);
		}
	}
	
	public List<String> completeNuclearPowerGenerators(String scenarioUrlOfMockedAgent, List<String> nuclearPowerPlants) {
		
		List<String> result = new ArrayList<String>();
		
		String sparqlQuery = getQueryForGeneratorsInNuclearPlant();
		
		for (String current : nuclearPowerPlants) {
			String queryResult = new ScenarioClient().query(scenarioUrlOfMockedAgent, current, sparqlQuery);		
			List<String[]> list = JenaResultSetFormatter.convertToListofStringArrays(queryResult, "generator");
			
			for (String[] currentRow : list) {
				String generator = currentRow[0];
				result.add(generator);
				
				URI uri = new ScenarioClient().getReadUrl(scenarioUrlOfMockedAgent, generator);
				OntModel model = JenaHelper.createModel();
				try {
					URL url = uri.toURL();
					JenaHelper.readFromUrl(url, model);
				} catch (MalformedURLException e) {
					new JPSRuntimeException(e.getMessage(), e);
				}
				
				completePowerGenerator(model, generator);
			}
		}		
		
		return result;
	}
	
	public String getQueryForGeneratorsInNuclearPlant() {
		QueryBuilder builder = new QueryBuilder();
		builder.select("?generator");
		builder.a("?entity", OPSREAL, "NuclearPlant");
		builder.prop("?entity", "?generator", OCPSYST, "hasSubsystem");
		return builder.build().toString();
	}
	
	public List<String> getNuclearPowerPlantsFromMockedScenarioAgent(String scenarioUrlOfMockedAgent) {
		
		List<String> plantList = new ArrayList<String>();
		JSONObject jo = new JSONObject();
		String result = new ScenarioClient().mockedOperation(scenarioUrlOfMockedAgent, "/processresult", jo.toString());
		logger.info("result from mocked scenario agent = " + result);
		JSONArray jaResult = new JSONObject(result).getJSONArray("plantirilist");
		for (int i=0; i<jaResult.length(); i++) {
			plantList.add(jaResult.getString(i));
		}
		
		return plantList;
	}
	
	public void completePowerGenerator(OntModel model, String powerGenerator) {
		
		logger.info("adding additional attributes to nuclear power generator = " + powerGenerator);


		JenaModelWrapper w = new JenaModelWrapper(model, null);
		
		// read some values from the original OWL file
		RDFNode o = w.getPropertyValue(powerGenerator, PGISCOORDX);
		double x = o.asLiteral().getDouble();
		o = w.getPropertyValue(powerGenerator, PGISCOORDY);
		double y = o.asLiteral().getDouble();
		String[] pathActivePowerGenerated = new String[] {OPSBEHA, "hasActivePowerGenerated", OCPSYST, "hasValue", OCPSYST, "numericalValue"};
		o = w.getPropertyValue(powerGenerator, pathActivePowerGenerated);
		double activePowerGenerated = o.asLiteral().getDouble();
		
		// replace the template IRI by the IRI of the power generator
		String path = Util.getResourceDir(this) + "/EGen-001_template.owl";
		String content = FileUtil.readFileLocally(path);
		String templateIRI = "http://www.theworldavatar.com/EGen-001_template.owl";
		content = content.replace(templateIRI + "#EGen-001", powerGenerator);
		String powerGeneratorWithoutFragment = powerGenerator;
		int i = powerGenerator.indexOf("#");
		if (i > 0) {
			powerGeneratorWithoutFragment = powerGenerator.substring(0, i);
		}
		content = content.replace(templateIRI, powerGeneratorWithoutFragment);
		
		// read the template into a model and update its values with those from the original OWL file
		model = JenaHelper.createModel();
		JenaHelper.readFromString(content, model);
		w = new JenaModelWrapper(model, null);
				
		w.setPropertyValue(powerGenerator, x, PGISCOORDX);
		w.setPropertyValue(powerGenerator, y, PGISCOORDY);
		w.setPropertyValue(powerGenerator, activePowerGenerated, pathActivePowerGenerated);
			
		// overwrite the original OWL file
		content = JenaHelper.writeToString(model);
		new QueryBroker().put(powerGenerator, content);
	}
	
	public OntModel deletePowerGeneratorsFromElectricalNetwork(String electricalNetwork) {
		
		// the hasSubsystem triples of the electrical network top node itself are not part of the model
		OntModel model = ENAgent.readModelGreedy(electricalNetwork);
		String query = getQueryForPowerGenerators();
		ResultSet resultSet = JenaHelper.query(model, query);
		String result = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
		String[] keys = JenaResultSetFormatter.getKeys(result);
		List<String[]> resultList = JenaResultSetFormatter.convertToListofStringArrays(result, keys);
		
		StringBuffer delete = new StringBuffer("PREFIX OCPSYST:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> \r\n");
		delete.append("DELETE DATA { \r\n");
		for (String[] current : resultList) {
			delete.append("<" + electricalNetwork + "> OCPSYST:hasSubsystem <" + current[0] + "> . \r\n");
		}
		delete.append("} \r\n");		
		logger.info("deleting current power generators from electrical network top node\n" + delete.toString());

		new QueryBroker().updateFile(electricalNetwork, delete.toString());
		
		return model;
	}
	
	public String getQueryForPowerGenerators() {
		QueryBuilder builder = new QueryBuilder();
		builder.select("?generator");
		builder.a("?generator", OPSREAL, "PowerGenerator");
		return builder.build().toString();
	}
	
	public void addNuclearPowerGeneratorsToElectricalNetwork(String electricalNetwork, List<String> generators) {
		
		StringBuffer insert = new StringBuffer("PREFIX OCPSYST:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#> \r\n");
		insert.append("INSERT DATA { \r\n");
		for (String current : generators) {
			insert.append("<" + electricalNetwork + "> OCPSYST:hasSubsystem <" + current + "> . \r\n");
		}
		insert.append("} \r\n");		
		logger.info("adding nuclear power generators to electrical network top node\n" + insert.toString());

		new QueryBroker().updateFile(electricalNetwork, insert.toString());
	}
	
	public void connectNuclearPowerGeneratorsToOptimalBus(OntModel model, List<String> generators) {
		
		String queryBus = getQueryForBuses();
		ResultSet resultSet = JenaHelper.query(model, queryBus);
		String json = JenaResultSetFormatter.convertToJSONW3CStandard(resultSet);
		List<String[]> buses = JenaResultSetFormatter.convertToListofStringArrays(json, "entity", "x", "y", "busnumber", "busnumbervalue");
		logger.info("finding the optimal bus, number of buses = " + buses.size() + ", number of generators = " + generators.size());
		
		String queryGenerator = getQueryForGenerator();
		//System.out.println(queryGenerator);
		QueryBroker broker = new QueryBroker();
		for (String current : generators) {
		
			String resultGen = broker.queryFile(current, queryGenerator);
			List<String[]> resultGenAsList = JenaResultSetFormatter.convertToListofStringArrays(resultGen, "entity", "x", "y", "busnumber");
			double xGen = Double.valueOf(resultGenAsList.get(0)[1]);
			double yGen = Double.valueOf(resultGenAsList.get(0)[2]);
			String busNumber = resultGenAsList.get(0)[3];
			logger.info("searching optimal bus for generator = " + current + ", x = " + xGen + ", y = " + yGen + ", current bus number instance = " + busNumber);
		
			double distanceClosestBus = -1;
			int indexClosestBus = -1;
			for (int i=0; i<buses.size(); i++) {
				
				String[] currentBus = buses.get(i);

				// TODO-AE SC URGENT 20190429 x and y swapped !!!! 
				// TODO-AE SC URGENT 20190429 distance according to WSG84 degree instead of meters?
				// TODO-AE SC URGENT 20190429 consider bus constraint wrt voltage
				//double xBus = Double.valueOf(currentBus[1]);
				//double yBus = Double.valueOf(currentBus[2]);
				double yBus = Double.valueOf(currentBus[1]);
				double xBus = Double.valueOf(currentBus[2]);
				double dist = Math.sqrt(Math.pow(xGen - xBus, 2) + Math.pow(yGen - yBus, 2));
				if ((dist < distanceClosestBus) || (indexClosestBus == -1)) {
					distanceClosestBus = dist;
					indexClosestBus = i;
				}
			} 
			
			String[] closestBus = buses.get(indexClosestBus);
			int busNumberValue = Integer.valueOf(closestBus[4]);
			logger.info("updating generator for optimal bus = " + closestBus[0] + ", bus number value = " + busNumberValue 
					+ ", x = " + closestBus[1] + ", y = " + closestBus[2] + ", distance = " + distanceClosestBus); 
			
			OntModel modelGen = JenaHelper.createModel(current);
			JenaModelWrapper w = new JenaModelWrapper(modelGen, null);
			w.setPropertyValue(busNumber, busNumberValue, PVALNUMVAL);
				
			// overwrite the original OWL file
			String content = JenaHelper.writeToString(modelGen);
			new QueryBroker().put(current, content);
		}
	}
	
	private String getQueryForGenerator() {
		QueryBuilder builder = new QueryBuilder();
		builder.select("?entity", "?x", "?y" , "?busnumber");
		builder.prop("?entity", "?x", PGISCOORDX);
		builder.prop("?entity", "?y", PGISCOORDY);
		builder.a("?busnumber", OPSMODE, "BusNumber");
		
		return builder.build().toString();
	}
	
	private String getQueryForBuses() {
		
		QueryBuilder builder = new QueryBuilder();
		builder.select("?entity", "?x", "?y", "?busnumber", "?busnumbervalue");
		builder.a("?entity", OPSREAL, "BusNode");
		builder.prop("?entity", "?x", PGISCOORDX);
		builder.prop("?entity", "?y", PGISCOORDY);
		builder.a("?busnumber", OPSMODE, "BusNumber");
		builder.prop("?entity", "?busnumber", OCPSYST, "isModeledBy", OCPMATH, "hasModelVariable");
		builder.prop("?busnumber", "?busnumbervalue", PVALNUMVAL);
		
		return builder.build().toString();
	}
}
