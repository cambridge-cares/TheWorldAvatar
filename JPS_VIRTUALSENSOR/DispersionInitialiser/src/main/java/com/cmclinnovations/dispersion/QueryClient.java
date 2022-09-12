package com.cmclinnovations.dispersion;

import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.query.ModifyQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;

import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import java.util.Arrays;
import java.util.UUID;

/**
 * sends sparql queries
 */
public class QueryClient {
    private StoreClientInterface storeClient;
    private DerivationClient derivationClient;

    static final String PREFIX = "http://www.theworldavatar.com/dispersion/";
    static final Prefix P_DISP = SparqlBuilder.prefix("disp",iri(PREFIX));
    private static final Prefix P_OM = SparqlBuilder.prefix("om",iri("http://www.ontology-of-units-of-measure.org/resource/om-2/"));

    // classes
    private static final Iri SHIP = P_DISP.iri("Ship");
    private static final Iri SPEED = P_DISP.iri("Speed");
    private static final Iri COURSE_OVER_GROUND = P_DISP.iri("CourseOverGround");
    private static final Iri MMSI = P_DISP.iri("MMSI");
    private static final Iri LOCATION = P_DISP.iri("Location");
    private static final Iri MEASURE = P_OM.iri("Measure");
    private static final Iri SCOPE = P_DISP.iri("Scope");
    private static final Iri SIMULATION_TIME = P_DISP.iri("SimulationTime");

    // properties
    private static final Iri HAS_MMSI = P_DISP.iri("hasMMSI");
    private static final Iri HAS_SPEED = P_DISP.iri("hasSpeed");
    private static final Iri HAS_COURSE = P_DISP.iri("hasCourse");
    private static final Iri HAS_LOCATION = P_DISP.iri("hasLocation");
    private static final Iri HAS_VALUE = P_OM.iri("hasValue");
    private static final Iri HAS_NUMERICALVALUE = P_OM.iri("hasNumericalValue");

    public QueryClient(StoreClientInterface storeClient) {
        this.storeClient = storeClient;
        this.derivationClient = new DerivationClient(storeClient, PREFIX);
    }

    void initialiseAgent() {
        Iri service = iri("http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#Service");
        Iri operation = iri("http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#Operation");
        Iri hasOperation = iri("http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#hasOperation");
        Iri hasHttpUrl = iri("http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#hasHttpUrl");
        Iri hasInput = iri("http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#hasInput");
        Iri hasMandatoryPart = iri("http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#hasMandatoryPart");
        Iri hasType = iri("http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#hasType");
        
        Iri operationIri = iri(PREFIX + UUID.randomUUID());
        Iri inputIri = iri(PREFIX + UUID.randomUUID());
        Iri partIri = iri(PREFIX + UUID.randomUUID());

        ModifyQuery modify = Queries.MODIFY();

        modify.insert(iri(EnvConfig.EPISODE_AGENT_IRI).isA(service).andHas(hasOperation, operationIri));
		modify.insert(operationIri.isA(operation).andHas(hasHttpUrl, iri(EnvConfig.EPISODE_AGENT_URL)).andHas(hasInput, inputIri));
        modify.insert(inputIri.has(hasMandatoryPart, partIri));
        modify.insert(partIri.has(hasType, SIMULATION_TIME)).prefix(P_DISP);

        storeClient.executeUpdate(modify.getQueryString());
    }

    void initialiseScopeDerivation(String scopeIri) {
        ModifyQuery modify = Queries.MODIFY();
        modify.insert(iri(scopeIri).isA(SCOPE));

        String simTime = PREFIX + UUID.randomUUID();
        String simTimeMeasure = PREFIX + UUID.randomUUID();
        modify.insert(iri(simTime).isA(SIMULATION_TIME).andHas(HAS_VALUE,simTimeMeasure));
        modify.insert(iri(simTimeMeasure).isA(MEASURE).andHas(HAS_NUMERICALVALUE, 0));

        modify.prefix(P_DISP,P_OM);
        storeClient.executeUpdate(modify.getQueryString());

        derivationClient.createDerivation(Arrays.asList(scopeIri), EnvConfig.EPISODE_AGENT_IRI, Arrays.asList(simTime));
    }
}
