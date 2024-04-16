package uk.ac.cam.cares.derivation.example;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.ws.rs.BadRequestException;

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

@WebServlet(urlPatterns = {SumValueAgent.URL_SUMVALUE})
public class SumValueAgent extends DerivationAgent {

    private static final long serialVersionUID = 1L;
    public static final String URL_SUMVALUE = "/SumValueAgent";
    private static final Logger LOGGER = LogManager.getLogger(SumValueAgent.class);


    TripleStoreClientInterface storeClient;
    SparqlClient sparqlClient;

    public SumValueAgent() {
        LOGGER.info("SumValueAgent is initialised.");
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
            TimeSeriesClient<Instant> tsClient = new TimeSeriesClient<Instant>(storeClient, Instant.class, Config.dburl, Config.dbuser, Config.dbpassword);

            List inputdata_iri = Arrays.asList(InstancesDatabase.Input);
            TimeSeries timeSeries = tsClient.getTimeSeries(inputdata_iri);
            List<Integer> values = timeSeries.getValues(inputdata_iri.get(0).toString());

            Integer sumvalue = 0;

            for (int i = 0; i < values.size(); i++)
                sumvalue += values.get(i);

            // write the output triples to derivationOutputs
            String sum_iri = SparqlClient.namespace + UUID.randomUUID().toString();
            derivationOutputs.createNewEntity(sum_iri, SparqlClient.getRdfTypeString(SparqlClient.SumValue));
            derivationOutputs.addTriple(sum_iri, RDF.TYPE.toString(), OWL.NAMEDINDIVIDUAL.toString());
            String value_iri = SparqlClient.namespace + UUID.randomUUID().toString();
            derivationOutputs.createNewEntity(value_iri, SparqlClient.getRdfTypeString(SparqlClient.ScalarValue));
            derivationOutputs.addTriple(sparqlClient.addValueInstance(sum_iri, value_iri, sumvalue));
            LOGGER.info("Created a new min value instance <" + sum_iri + ">, and its value instance <" + value_iri + ">");

    }
}
