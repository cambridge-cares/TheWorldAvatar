//package uk.ac.cam.cares.goal.framework;
//
//
//
//import java.time.Instant;
//import java.util.List;
//import java.util.Map;
//import java.util.Objects;
//import java.util.concurrent.TimeUnit;
//import java.util.stream.Collectors;
//
//import javax.ws.rs.BadRequestException;
//
//import org.apache.logging.log4j.LogManager;
//import org.apache.logging.log4j.Logger;
//import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
//import org.json.JSONObject;
//
//import uk.ac.cam.cares.jps.base.agent.JPSAgent;
//import uk.ac.cam.cares.jps.base.derivation.Derivation;
//import uk.ac.cam.cares.jps.base.derivation.DerivationClient;
//import uk.ac.cam.cares.jps.base.derivation.DerivationInputs;
//import uk.ac.cam.cares.jps.base.derivation.DerivationOutputs;
//import uk.ac.cam.cares.jps.base.derivation.DerivationSparql;
//import uk.ac.cam.cares.jps.base.derivation.StatusType;
//import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
//import uk.ac.cam.cares.jps.base.interfaces.DerivationAgentInterface;
//import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;
//
////GoalFramework import statements
//import uk.ac.cam.cares.goal.framework.*;
//
//
//
//public class GoalAgent extends JPSAgent implements GoalAgentInterface {
//
//    /**
//     * Logger for stdout and stderr.
//     */
//    private static final Logger LOGGER = LogManager.getLogger(GoalAgent.class);
//
//    /**
//     *
//     */
//    private static final long serialVersionUID = 1L;
//
//    /**
//     * This constructor initialises DerivationClient which will be used to handle both sync and async derivations.
//     * @param storeClient
//     * @param derivationInstanceBaseURL
//     */
//    public GoalAgent(StoreClientInterface storeClient, String derivationInstanceBaseURL) {
//        this.storeClient = storeClient;
//        this.goalClient = new GoalClient(storeClient, derivationInstanceBaseURL);
//    }
//
//    @Override
//    public JSONObject processRequestParameters(JSONObject requestParams) {
//        // check if this.devClient is initialised properly, this will throw meaningful exception
//        JSONObject res = new JSONObject();
//        return res;
//    }
//
//    @Override
//    public void processRequestParameters(DerivationInputs derivationInputs, DerivationOutputs derivationOutputs) {
//        // TODO developer needs to overwrite this function
//    }
//
//}
