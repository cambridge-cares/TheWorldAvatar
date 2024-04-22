package uk.ac.cam.cares.goal.framework;

import uk.ac.cam.cares.jps.base.derivation.ValuesPattern;
import uk.ac.cam.cares.jps.base.interfaces.StoreClientInterface;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.PropertyPaths;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.ModifyQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPattern;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatterns;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.SubSelect;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf;


public class GoalSparql {

    private StoreClientInterface storeClient;
    private String goalInstanceBaseURL; // an example of this can be
    // "https://www.example.com/triplestore/repository/"

    public static String derivednamespace = "https://www.theworldavatar.com/kg/ontoderivation/";


    // placeholder string used by method getAllDerivations()
    private static final String PLACEHOLDER = "http://This_is_a_placeholder_string";
    // placeholder Iri used by method getDerivation(String rootDerivationIRI)
    private static final Iri PLACEHOLDER_IRI = iri(PLACEHOLDER);

    // status concepts
    private static String REQUESTED = "Requested";
    private static String INPROGRESS = "InProgress";
    private static String FINISHED = "Finished";
    private static String ERROR = "Error";

    // derivation types
    public static final String DERIVATION = "Derivation";
    public static final String GOAL = "Goal";
    public static final String DERIVATIONWITHTIMESERIES = "DerivationWithTimeSeries";
    public static final String DERIVATIONASYN = "DerivationAsyn";
    public static final String ONTODERIVATION_DERIVATION = derivednamespace + DERIVATION;
    public static final String ONTODERIVATION_DERIVATIONASYN = derivednamespace + DERIVATIONASYN;
    public static final String ONTODERIVATION_DERIVATIONWITHTIMESERIES = derivednamespace + DERIVATIONWITHTIMESERIES;

    // prefix/namespace
    private static Prefix prefixAgent = SparqlBuilder.prefix("agent",
            iri("http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#"));
    private static Prefix prefixDerived = SparqlBuilder.prefix("derived", iri(derivednamespace));
    private static Prefix prefixTime = SparqlBuilder.prefix("time", iri("http://www.w3.org/2006/time#"));
    private static Prefix prefixGoal = SparqlBuilder.prefix("goal",iri("http://www.theworldavatar.com/ontology/ontoagent/Goal.owl#"));

    // classes
    private static Iri Service = prefixAgent.iri("Service");
    private static Iri Operation = prefixAgent.iri("Operation");
    private static Iri MessageContent = prefixAgent.iri("MessageContent");
    private static Iri MessagePart = prefixAgent.iri("MessagePart");
    private static Iri TimePosition = prefixTime.iri("TimePosition");
    private static Iri Derivation = prefixDerived.iri(DERIVATION);
    private static Iri DerivationWithTimeSeries = prefixDerived.iri(DERIVATIONWITHTIMESERIES);
    private static Iri DerivationAsyn = prefixDerived.iri(DERIVATIONASYN);
    private static Iri Status = prefixDerived.iri("Status");
    private static Iri Requested = prefixDerived.iri(REQUESTED);
    private static Iri InProgress = prefixDerived.iri(INPROGRESS);
    private static Iri Finished = prefixDerived.iri(FINISHED);
    private static Iri Error = prefixDerived.iri(ERROR);
    private static Iri InstantClass = prefixTime.iri("Instant");
    private static Iri UnixTime = iri("http://dbpedia.org/resource/Unix_time");

    private static Iri Goal = prefixGoal.iri(GOAL);

    // object properties
    private static Iri hasHttpUrl = prefixAgent.iri("hasHttpUrl");
    private static Iri hasOperation = prefixAgent.iri("hasOperation");
    private static Iri hasInput = prefixAgent.iri("hasInput");
    private static Iri hasOutput = prefixAgent.iri("hasOutput");
    private static Iri hasMandatoryPart = prefixAgent.iri("hasMandatoryPart");
    private static Iri hasType = prefixAgent.iri("hasType");
    private static Iri hasName = prefixAgent.iri("hasName");
    private static Iri isDerivedFrom = prefixDerived.iri("isDerivedFrom");
    private static Iri isDerivedUsing = prefixDerived.iri("isDerivedUsing");
    private static Iri belongsTo = prefixDerived.iri("belongsTo");
    private static Iri hasStatus = prefixDerived.iri("hasStatus");
    private static Iri hasNewDerivedIRI = prefixDerived.iri("hasNewDerivedIRI");
    private static Iri hasTime = prefixTime.iri("hasTime");
    private static Iri numericPosition = prefixTime.iri("numericPosition");
    private static Iri hasTRS = prefixTime.iri("hasTRS");
    private static Iri inTimePosition = prefixTime.iri("inTimePosition");
    private static Iri hasGoalRange = prefixGoal.iri("hasGoalRange");
    private static Iri hasRealState = prefixGoal.iri("hasRealState");
    private static Iri isAchievedUsing = prefixGoal.iri("isAchievedUsing");


    // data properties
    private static Iri retrievedInputsAt = prefixDerived.iri("retrievedInputsAt");
    private static Iri uuidLock = prefixDerived.iri("uuidLock");


    /**
     * This constructor should be used to enable customised derivation instance base
     * URL.
     *
     * @param storeClient
     * @param derivationInstanceBaseURL
     */
    public GoalSparql(StoreClientInterface storeClient, String derivationInstanceBaseURL) {
        this.storeClient = storeClient;
        this.goalInstanceBaseURL = derivationInstanceBaseURL;
    }

    private void addTimeInstance(Map<String, Long> entitiesTimestamp) {
		// example complete SPARQL update string for two entities
		// PREFIX derived:
		// <https://www.theworldavatar.com/kg/ontoderivation/>
		// PREFIX time: <http://www.w3.org/2006/time#>
		// INSERT { ?instance time:hasTime ?timeInstant .
		// ?timeInstant a time:Instant ;
		// 	time:inTimePosition ?timeUnix .
		// ?timeUnix a time:TimePosition ;
		// 	time:numericPosition ?timestamp ;
		// 	time:hasTRS <http://dbpedia.org/resource/Unix_time> . }
		// WHERE { { SELECT ?instance ?timeInstant ?timeUnix ?timestamp
		// WHERE {  VALUES ( ?instance ?timeInstant ?timeUnix ?timestamp )
		//	{ (<http://entity1> <http://time_uuid1> <http://time_uuid2> 0)
		//	(<http://entity2> <http://time_uuid3> <http://time_uuid4> 0) }
		// FILTER NOT EXISTS { ?instance derived:belongsTo ?anyDerivation . } }
		// FILTER NOT EXISTS { ?instance time:hasTime/time:inTimePosition/time:numericPosition ?existingTime . } }
		// } }
		ModifyQuery modify = Queries.MODIFY();
		SubSelect sub = GraphPatterns.select();
		Variable instance = SparqlBuilder.var("instance");
		Variable timeInstant = SparqlBuilder.var("timeInstant");
		Variable timeUnix = SparqlBuilder.var("timeUnix");
		Variable timestamp = SparqlBuilder.var("timestamp");

		modify.insert(instance.has(hasTime, timeInstant));
		modify.insert(timeInstant.isA(InstantClass).andHas(inTimePosition, timeUnix));
		modify.insert(timeUnix.isA(TimePosition).andHas(numericPosition, timestamp).andHas(hasTRS, UnixTime));
		ValuesPattern vp = new ValuesPattern(instance, timeInstant, timeUnix, timestamp);
		entitiesTimestamp.forEach((en, ts) -> {
			// create timestamp value pairs for the given entity
			vp.addValuePairForMultipleVariables(iri(en), iri(createTimeIRI()), iri(createTimeIRI()), Rdf.literalOf(ts));
		});
		GraphPattern belongsToAnyDerivationGP = instance.has(belongsTo, SparqlBuilder.var("anyDerivation"));
		GraphPattern existTimestampGP = instance.has(
				PropertyPaths.path(hasTime, inTimePosition, numericPosition),
				SparqlBuilder.var("existingTime"));
		sub.select(instance, timeInstant, timeUnix, timestamp)
				.where(GraphPatterns.and(vp,
						GraphPatterns.filterNotExists(belongsToAnyDerivationGP),
						GraphPatterns.filterNotExists(existTimestampGP)));
		modify.prefix(prefixDerived, prefixTime).where(sub);

		storeClient.executeUpdate(modify.getQueryString());
	}

    /**
     * add a time stamp instance to the input or goal if it does not exist the goal uses the unix timestamp
     *
     * @param kbClient
     * @param input
     */
    void addTimeInstance(String entity) {
        addTimeInstance(Arrays.asList(entity));
    }

    /**
     * This method adds timestamp to the given entities in bulk. It skips entities
     * who already have a timestamp or is a derived data.
     *
     * @param entities
     */
    void addTimeInstance(List<String> entities) {
        Map<String, Long> entitiesTimestamp = new HashMap<>();
        entities.stream().forEach(en -> {
            if (!entitiesTimestamp.containsKey(en)) {
                entitiesTimestamp.put(en, (long) 0);
            }
        });
        addTimeInstance(entitiesTimestamp);
    }


    /**
     * This method creates a new IRI for timestamp related instances.
     * @return
     */
    String createTimeIRI() {
        return goalInstanceBaseURL + "time_" + UUID.randomUUID().toString();
    }


    /**
     * This method creates a new IRI of derivation instance.
     *
     * @return
     */
    String createGoalIRI() {
        // create a unique IRI for this new derived quantity
            return goalInstanceBaseURL + "_" + UUID.randomUUID().toString();
    }

    /**
     * This method creates a new IRI of derivation instance.
     *
     * @return
     */
    String createRangeIRI() {
        // create a unique IRI for this new derived quantity
        return goalInstanceBaseURL + "_" +"goalRange_"+ UUID.randomUUID().toString();
    }

    private void createNewGoal(String goalIRI, String agent_iri, String range_iri, String realstate_iri){

        ModifyQuery modify = Queries.MODIFY();
        SubSelect sub = GraphPatterns.select();
        Variable goal = SparqlBuilder.var("goal");
        Variable agent = SparqlBuilder.var("agent");
        Variable range = SparqlBuilder.var("range");
        Variable realstate = SparqlBuilder.var("realstate");

        modify.insert(goal.has(hasGoalRange,range));
        modify.insert(goal.has(isAchievedUsing,agent));
        modify.insert(goal.has(hasRealState,realstate));

        modify.prefix(prefixGoal, prefixTime).where(sub);

        storeClient.executeUpdate(modify.getQueryString());
    }













}
