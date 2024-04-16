package uk.ac.cam.cares.derivation.example;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.eclipse.rdf4j.model.vocabulary.OWL;
import org.eclipse.rdf4j.model.vocabulary.RDF;
import uk.ac.cam.cares.jps.base.agent.DerivationAgent;
import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.derivation.DerivationInputs;
import uk.ac.cam.cares.jps.base.derivation.DerivationOutputs;
import uk.ac.cam.cares.jps.base.interfaces.TripleStoreClientInterface;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import java.time.Instant;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;

@WebServlet(urlPatterns = {TruckCountingAgent.URL_TruckValue})
public class TruckCountingAgent extends DerivationAgent {

    private static final long serialVersionUID = 1L;
    public static final String URL_TruckValue = "/TruckCountingAgent";
    private static final Logger LOGGER = LogManager.getLogger(TruckCountingAgent.class);


    TripleStoreClientInterface storeClient;
    SparqlClient sparqlClient;

    public TruckCountingAgent() {
        LOGGER.info("TruckCountingAgent is initialised.");
    }

    @Override
    public void init() throws ServletException {
        // initialise all clients
        Config.initProperties();
        this.storeClient = new RemoteStoreClient(Config.kgurl, Config.kgurl, Config.kguser, Config.kgpassword);
        this.sparqlClient = new SparqlClient(this.storeClient);
        super.devClient = new DerivationClient(this.storeClient, InitialiseInstances.derivationInstanceBaseURL);
    }

    @Override
    public void processRequestParameters (DerivationInputs derivationInputs, DerivationOutputs derivationOutputs) {

            LOGGER.info("Received request: " + derivationInputs.toString());
            String sumvalue_iri = derivationInputs.getIris(SparqlClient.getRdfTypeString(SparqlClient.SumValue)).get(0);

            Integer sumvalue= null ;
            sumvalue= sparqlClient.getValue(sumvalue_iri);

            Double truckvaluediv = null;
            truckvaluediv=Math.ceil(Double.valueOf(sumvalue)/500.0);

            String truckvalue =truckvaluediv + "trucks are needed to carry the weight.";

            String truckvalue_iri = SparqlClient.namespace + UUID.randomUUID().toString();
            derivationOutputs.createNewEntity(truckvalue_iri, SparqlClient.getRdfTypeString(SparqlClient.TruckValue));
            derivationOutputs.addTriple(truckvalue_iri, RDF.TYPE.toString(), OWL.NAMEDINDIVIDUAL.toString());
            String value_iri = SparqlClient.namespace + UUID.randomUUID().toString();
            derivationOutputs.createNewEntity(value_iri, SparqlClient.getRdfTypeString(SparqlClient.ScalarValue));
            derivationOutputs.addTriple(sparqlClient.addStringInstance(truckvalue_iri, value_iri, truckvalue));
            LOGGER.info("Created a new truck value instance <" + truckvalue_iri + ">, and its value instance <" + truckvalue + ">");

    }
}
