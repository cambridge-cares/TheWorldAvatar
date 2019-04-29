package uk.ac.cam.cares.jps.powsys.coordination;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.jena.ontology.OntModel;
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
	
	public void coordinate(String scenarioUrlOfMockedAgent) {
		
		JSONObject jo = new JSONObject();
		
		List<String> plants = getNuclearPowerPlantsFromMockedScenarioAgent(scenarioUrlOfMockedAgent, jo);
		for (String current : plants) {
			String sparqlQuery = getQueryForGenerators();
			String queryResult = new ScenarioClient().query(scenarioUrlOfMockedAgent, current, sparqlQuery);		
			List<String[]> list = JenaResultSetFormatter.convertToListofStringArrays(queryResult, "generator");
			
			for (String[] currentRow : list) {
				String generator = currentRow[0];
				
				URI uri = new ScenarioClient().getReadUrl(scenarioUrlOfMockedAgent, generator);
				OntModel model = JenaHelper.createModel();
				try {
					JenaHelper.readFromUrl(uri.toURL(), model);
				} catch (MalformedURLException e) {
					new JPSRuntimeException(e.getMessage(), e);
				}
				
				completePowerGenerator(model, generator);
			}
		}		
	}
	
	public String getQueryForGenerators() {
		QueryBuilder builder = new QueryBuilder();
		builder.select("?generator");
		builder.a("?entity", OPSREAL, "NuclearPlant");
		builder.prop("?entity", "?generator", OCPSYST, "hasSubsystem");
		return builder.build().toString();
	}
	
	public List<String> getNuclearPowerPlantsFromMockedScenarioAgent(String scenarioUrlOfMockedAgent, JSONObject jo) {
		
		List<String> plantList = new ArrayList<String>();

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
}
