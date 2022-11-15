package uk.ac.cam.cares.derivation.example;

import javax.ws.rs.BadRequestException;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.eclipse.rdf4j.model.vocabulary.OWL;
import org.eclipse.rdf4j.model.vocabulary.RDF;

import uk.ac.cam.cares.jps.base.agent.DerivationAgent;
import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.derivation.DerivationInputs;
import uk.ac.cam.cares.jps.base.derivation.DerivationOutputs;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import java.util.List;
import java.util.UUID;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;

/**
 * This agent takes two inputs (MinValue and MaxValue), calculate the difference
 * between them, and write the value in the KG
 * 
 * @author Kok Foong Lee
 * @author Jiaru Bai
 */
@WebServlet(urlPatterns = {DifferenceAgent.URL_Difference})
public class DifferenceAgent extends DerivationAgent {
	private static final long serialVersionUID = 1L;

	// ============================ Static variables ===========================
	private static final Logger LOGGER = LogManager.getLogger(DifferenceAgent.class);
	public static final String URL_Difference = "/DifferenceAgent";

	StoreClientInterface storeClient;
	SparqlClient sparqlClient;

	public DifferenceAgent() {
		LOGGER.info("DifferenceAgent is initialised.");
	}

	// ================================ Methods ================================
	/**
	 * Processes HTTP requests.
	 *
	 * @param requestParams Request parameters in a JSONObject
	 * @param request HTTP Servlet Request
	 * @return
	 */
	@Override
	public void processRequestParameters(DerivationInputs derivationInputs, DerivationOutputs derivationOutputs) {

		if (validateInput(derivationInputs,sparqlClient)) {
			LOGGER.info("Calculating difference");

			// validate input should already ensure that both max and min value exist
			String maxIri = derivationInputs.getIris(SparqlClient.getRdfTypeString(SparqlClient.MaxValue)).get(0);
			String minIri = derivationInputs.getIris(SparqlClient.getRdfTypeString(SparqlClient.MinValue)).get(0);
			Integer minvalue_input = null; Integer maxvalue_input = null;
			maxvalue_input = sparqlClient.getValue(maxIri);
			minvalue_input = sparqlClient.getValue(minIri);

			// calculate a new value
			int difference = maxvalue_input - minvalue_input;

			// write the output triples to derivationOutputs
			String difference_iri = SparqlClient.namespace + UUID.randomUUID().toString();
			derivationOutputs.createNewEntity(difference_iri, SparqlClient.getRdfTypeString(SparqlClient.Difference));
			derivationOutputs.addTriple(difference_iri, RDF.TYPE.toString(), OWL.NAMEDINDIVIDUAL.toString());
			String value_iri = SparqlClient.namespace + UUID.randomUUID().toString();
			derivationOutputs.createNewEntity(value_iri, SparqlClient.getRdfTypeString(SparqlClient.ScalarValue));
			derivationOutputs.addTriple(sparqlClient.addValueInstance(difference_iri, value_iri, difference));
			LOGGER.info("Created a new calculated difference instance <" + difference_iri
					+ ">, and its value instance <" + value_iri + ">");
		} else {
			LOGGER.error("Input validation failed.");
			throw new BadRequestException("Input validation failed.");
		}
	}

	private boolean validateInput(DerivationInputs derivationInputs, SparqlClient sparqlClient) throws BadRequestException {
		boolean valid = false;
		LOGGER.info("Checking inputs for DifferenceAgent");

		// check if the two inputs are complete
		List<String> max = derivationInputs.getIris(SparqlClient.getRdfTypeString(SparqlClient.MaxValue));
		List<String> min = derivationInputs.getIris(SparqlClient.getRdfTypeString(SparqlClient.MinValue));
		if (max.size() == 1 && min.size() == 1) {
			String maxIri = max.get(0);
			String minIri = min.get(0);
			if (sparqlClient.isMaxValue(maxIri)) {
				if (sparqlClient.isMinValue(minIri)) {
					valid = true;
				} else {
					LOGGER.error("MinValue IRI passed in doesn't match MinValue rdf:type.");
					throw new BadRequestException("MinValue IRI passed in doesn't match MinValue rdf:type.");
				}
			} else {
				LOGGER.error("MaxValue IRI passed in doesn't match MaxValue rdf:type.");
				throw new BadRequestException("MaxValue IRI passed in doesn't match MaxValue rdf:type.");
			}
		} else {
			LOGGER.error("Incorrect number of inputs");
			throw new BadRequestException("Incorrect number of inputs");
		}

		return valid;
	}

	@Override
	public void init() throws ServletException {
		// initialise all clients
		Config.initProperties();
		this.storeClient = new RemoteStoreClient(Config.kgurl, Config.kgurl, Config.kguser, Config.kgpassword);
		this.sparqlClient = new SparqlClient(this.storeClient);
		super.devClient = new DerivationClient(this.storeClient, InitialiseInstances.derivationInstanceBaseURL);
	}
}
