package uk.ac.cam.cares.derivation.asynexample;

import java.util.Arrays;

import javax.servlet.annotation.WebServlet;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONObject;

import uk.ac.cam.cares.derivation.asynexample.Config;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

/**
 * This InitialiseInstances agent initialises the knowledge graph and creates the chain of derivations.
 * @author Jiaru Bai (jb2197@cam.ac.uk)
 *
 */
@WebServlet(urlPatterns = {InitialiseInstances.API_PATTERN})
public class InitialiseInstances extends JPSAgent {
	
	private static final long serialVersionUID = 1L;
	
	private static final Logger LOGGER = LogManager.getLogger(InitialiseInstances.class);
	
	static final String API_PATTERN = "/InitialiseInstances";
	
	@Override
	public JSONObject processRequestParameters(JSONObject requestParams) {
		Config.initProperties();
		RemoteStoreClient storeClient = new RemoteStoreClient(Config.sparqlEndpointQuery, Config.sparqlEndpointUpdate);
		SparqlClient sparqlClient = new SparqlClient(storeClient);
		DerivationClient devClient = new DerivationClient(storeClient);
		
		// clear KG when initialising
		LOGGER.info("Initialising new instances, all existing instances will get deleted");
    	sparqlClient.clearKG();
		
    	// get the IRIs
    	String ul_rdf_type = SparqlClient.UpperLimit.getQueryString().replaceAll(SparqlClient.prefix+":", SparqlClient.namespace);
    	String ll_rdf_type = SparqlClient.LowerLimit.getQueryString().replaceAll(SparqlClient.prefix+":", SparqlClient.namespace);
    	String np_rdf_type = SparqlClient.NumberOfPoints.getQueryString().replaceAll(SparqlClient.prefix+":", SparqlClient.namespace);
    	String lp_rdf_type = SparqlClient.ListOfRandomPoints.getQueryString().replaceAll(SparqlClient.prefix+":", SparqlClient.namespace);
    	String maxv_rdf_type = SparqlClient.MaxValue.getQueryString().replaceAll(SparqlClient.prefix+":", SparqlClient.namespace);
    	String minv_rdf_type = SparqlClient.MinValue.getQueryString().replaceAll(SparqlClient.prefix+":", SparqlClient.namespace);
    	String diff_rdf_type = SparqlClient.Difference.getQueryString().replaceAll(SparqlClient.prefix+":", SparqlClient.namespace);
		
    	// create ontoagent instances    	
		sparqlClient.createOntoAgentInstance(Config.agentIriRNG, Config.agentHttpUrlRNG, Arrays.asList(ul_rdf_type,ll_rdf_type,np_rdf_type), Arrays.asList(lp_rdf_type));
		sparqlClient.createOntoAgentInstance(Config.agentIriMaxValue, Config.agentHttpUrlMaxValue, Arrays.asList(lp_rdf_type), Arrays.asList(maxv_rdf_type));
		sparqlClient.createOntoAgentInstance(Config.agentIriMinValue, Config.agentHttpUrlMinValue, Arrays.asList(lp_rdf_type), Arrays.asList(minv_rdf_type));
		sparqlClient.createOntoAgentInstance(Config.agentIriDifference, Config.agentHttpUrlDifference, Arrays.asList(maxv_rdf_type,minv_rdf_type), Arrays.asList(diff_rdf_type));
		
		// create upperlimit, lowerlimit, numberofpoints
		String upperLimit = sparqlClient.createUpperLimit();
		String ul_value = sparqlClient.addValueInstance(upperLimit, 20);
		devClient.addTimeInstance(upperLimit);
		devClient.updateTimestamp(upperLimit);
		LOGGER.info("Created UpperLimit instance <" + upperLimit + ">");
		InstanceDatabase.UpperLimit = upperLimit;
		
		String lowerLimit = sparqlClient.createLowerLimit();
		String ll_value = sparqlClient.addValueInstance(lowerLimit, 3);
		devClient.addTimeInstance(lowerLimit);
		devClient.updateTimestamp(lowerLimit);
		LOGGER.info("Created LowerLimit instance <" + lowerLimit + ">");
		InstanceDatabase.LowerLimit = lowerLimit;
		
		String numOfPoints = sparqlClient.createNumberOfPoints();
		String np_value = sparqlClient.addValueInstance(numOfPoints, 6);
		devClient.addTimeInstance(numOfPoints);
		devClient.updateTimestamp(numOfPoints);
		LOGGER.info("Created NumberOfPoints instance <" + numOfPoints + ">");
		InstanceDatabase.NumberOfPoints = numOfPoints;
		
		// create listofrandompoints, points
		String listOfRandomPoints = sparqlClient.createListOfRandomPoints(null);
		LOGGER.info("Created ListOfRandomPoints instance <" + listOfRandomPoints + ">");
		
		// create maxvalue, minvalue
		String maxValue = sparqlClient.createMaxValue();
		String max = sparqlClient.addValueInstance(maxValue, 0);
		LOGGER.info("Created MaxValue instance <" + maxValue + ">");
		
		String minValue = sparqlClient.createMinValue();
		String min = sparqlClient.addValueInstance(minValue, 0);
		LOGGER.info("Created MinValue instance <" + minValue + ">");
		
		// create difference
		String difference = sparqlClient.createDifference();
		String diff = sparqlClient.addValueInstance(difference, 0);
		LOGGER.info("Created Difference instance <" + difference + ">");
		
		// create chain of derivation
		String rng_dev = devClient.createAsynDerivation(Arrays.asList(listOfRandomPoints), Config.agentIriRNG, Arrays.asList(upperLimit,lowerLimit,numOfPoints));
		String max_dev = devClient.createAsynDerivation(Arrays.asList(maxValue), Config.agentIriMaxValue, Arrays.asList(listOfRandomPoints));
		String min_dev = devClient.createAsynDerivation(Arrays.asList(minValue), Config.agentIriMinValue, Arrays.asList(listOfRandomPoints));
		String diff_dev = devClient.createAsynDerivation(Arrays.asList(difference), Config.agentIriDifference, Arrays.asList(maxValue,minValue));
		InstanceDatabase.DerivedDifference = diff_dev;
		
		// check all connections between the derived quantities
		// as the validate method traverse down, checking difference derivation checks all other derivations in the chain
		LOGGER.info("Validating " + diff_dev);
		try {
			devClient.validateDerivation(diff_dev);
			LOGGER.info("Validated Difference Derivation successfully");
		} catch (Exception e) {
			LOGGER.error("Validation failure for Difference Derivation" + e.getMessage());
			throw new JPSRuntimeException(e);
		}
		
		JSONObject response = new JSONObject();
		response.put("UpperLimit instance", upperLimit);
		response.put("LowerLimit instance", lowerLimit);
		response.put("NumberOfPoints instance", numOfPoints);
		response.put("RandomNumberGeneration Derivation", rng_dev);
		response.put("ListOfRandomPoints instance", listOfRandomPoints);
		response.put("MaxValue Derivation", max_dev);
		response.put("MaxValue instance", maxValue);
		response.put("MinValue Derivation", min_dev);
		response.put("MinValue instance", minValue);
		response.put("Difference Derivation", diff_dev);
		response.put("Difference instance", difference);
		
		return response;
	}
}
