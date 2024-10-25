package uk.ac.cam.cares.jps.base.timeseries;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;
import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.literalOf;

import java.time.Duration;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.UUID;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.eclipse.rdf4j.sparqlbuilder.constraint.Expressions;
import org.eclipse.rdf4j.sparqlbuilder.core.Assignment;
import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.TriplesTemplate;
import org.eclipse.rdf4j.sparqlbuilder.core.PrefixDeclarations;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.DeleteDataQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.InsertDataQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.ModifyQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPattern;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatternNotTriples;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatterns;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.SubSelect;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.eclipse.rdf4j.sparqlbuilder.rdf.RdfObject;
import org.json.JSONArray;

import uk.ac.cam.cares.jps.base.derivation.ValuesPattern;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.TripleStoreClientInterface;

/**
 * This class contains a collection of methods to interact with a triple
 * store/knowledge base (kb).
 * This class should not be used directly. It should only be accessed internally
 * via {@link TimeSeriesClient TimeSeriesClient}
 * 
 * @author Kok Foong Lee
 */

public class TimeSeriesSparql {
    // kbClient with the endpoint (triplestore/owl file) specified
    private TripleStoreClientInterface kbClient;

    // Namespaces for ontology and knowledge base
    public static final String TIMESERIES_NAMESPACE = "https://www.theworldavatar.com/kg/ontotimeseries/";
    public static final String NS_TIME = "http://www.w3.org/2006/time#";

    // Prefixes
    static final Prefix PREFIX_ONTOLOGY = SparqlBuilder.prefix("ts", iri(TIMESERIES_NAMESPACE));
    static final Prefix PREFIX_KB = SparqlBuilder.prefix("kb", iri(TIMESERIES_NAMESPACE));
    static final Prefix PREFIX_TIME = SparqlBuilder.prefix("time", iri(NS_TIME));

    // Relationships
    static final Iri hasTimeSeries = PREFIX_ONTOLOGY.iri("hasTimeSeries");
    static final Iri hasRDB = PREFIX_ONTOLOGY.iri("hasRDB");
    static final Iri hasTimeUnit = PREFIX_ONTOLOGY.iri("hasTimeUnit");
    static final Iri hasAveragingPeriod = PREFIX_ONTOLOGY.iri("hasAveragingPeriod");
    static final Iri numericDuration = PREFIX_TIME.iri("numericDuration");
    static final Iri unitType = PREFIX_TIME.iri("unitType");
    static final Iri HAS_RDB_CLIENT_CLASS = PREFIX_ONTOLOGY.iri("hasRDBClientClass");
    static final Iri HAS_TIME_CLASS = PREFIX_ONTOLOGY.iri("hasTimeClass");
    static final Iri HAS_SCHEMA = PREFIX_ONTOLOGY.iri("hasSchema");

    // Fields for class specific exceptions
    private final String exceptionPrefix = this.getClass().getSimpleName() + ": ";

    // Time Series Type
    public static final String AVERAGE_TYPE_STRING = TIMESERIES_NAMESPACE + "AverageTimeSeries";
    public static final String STEPWISE_CUMULATIVE_TYPE_STRING = TIMESERIES_NAMESPACE + "StepwiseCumulativeTimeSeries";
    public static final String CUMULATIVE_TOTAL_TYPE_STRING = TIMESERIES_NAMESPACE + "CumulativeTotalTimeSeries";
    public static final String INSTANTANEOUS_TYPE_STRING = TIMESERIES_NAMESPACE + "InstantaneousTimeSeries";
    public static final String TIMESERIES_TYPE_STRING = TIMESERIES_NAMESPACE + "TimeSeries";

    // RDF type
    public static final Iri TIMESERIES = iri(TIMESERIES_TYPE_STRING);
    public static final Iri STEPWISE_CUMULATIVE_TIMESERIES = iri(STEPWISE_CUMULATIVE_TYPE_STRING);
    public static final Iri CUMULATIVE_TOTAL_TIMESERIES = iri(CUMULATIVE_TOTAL_TYPE_STRING);
    public static final Iri AVERAGE_TIMESERIES = iri(AVERAGE_TYPE_STRING);
    public static final Iri INSTANTANEOUS_TIMESERIES = iri(INSTANTANEOUS_TYPE_STRING);
    private static final Iri Duration = PREFIX_TIME.iri("Duration");

    // Values Pattern for TimeSeries types
    private static final List<RdfObject> types = Arrays.asList(TIMESERIES, CUMULATIVE_TOTAL_TIMESERIES,
            STEPWISE_CUMULATIVE_TIMESERIES, AVERAGE_TIMESERIES, INSTANTANEOUS_TIMESERIES);
    // NOTE: variable tsType will be created as ?x0.
    private static final Variable tsType = Queries.SELECT().var();
    private static final ValuesPattern vp = new ValuesPattern(tsType, types);

    private final ArrayList<String> temporalUnitType = new ArrayList<>(
            Arrays.asList(NS_TIME + "unitSecond", NS_TIME + "unitMinute", NS_TIME + "unitHour", NS_TIME + "unitDay",
                    NS_TIME + "unitWeek", NS_TIME + "unitMonth", NS_TIME + "unitYear"));

    // EnumMap of allowed temporalUnit types
    private static EnumMap<ChronoUnit, String> temporalUnitMap = new EnumMap<>(ChronoUnit.class);
    static {
        temporalUnitMap.put(ChronoUnit.SECONDS, NS_TIME + "unitSecond");
        temporalUnitMap.put(ChronoUnit.MINUTES, NS_TIME + "unitMinute");
        temporalUnitMap.put(ChronoUnit.HOURS, NS_TIME + "unitHour");
        temporalUnitMap.put(ChronoUnit.DAYS, NS_TIME + "unitDay");
        temporalUnitMap.put(ChronoUnit.WEEKS, NS_TIME + "unitWeek");
        temporalUnitMap.put(ChronoUnit.MONTHS, NS_TIME + "unitMonth");
        temporalUnitMap.put(ChronoUnit.YEARS, NS_TIME + "unitYear");
    }

    /**
     * Logger for error output.
     */
    private static final Logger LOGGER = LogManager.getLogger(TimeSeriesSparql.class);

    /**
     * Custom Class to store numeric duration and the corresponding temporal unit
     * pair.
     * Used to map the pair to the corresponding averaging period iri
     */
    protected class CustomDuration {

        private final Double value;
        private final String unit;
        private int hashCode;

        public CustomDuration(Double value, String unit) {
            this.value = value;
            this.unit = unit;
            this.hashCode = Objects.hash(value, unit);
        }

        public double getValue() {
            return value;
        }

        public String getUnit() {
            return unit;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o)
                return true;
            if (o == null || getClass() != o.getClass())
                return false;
            CustomDuration that = (CustomDuration) o;
            return value.equals(that.value) && unit.equals(that.unit);
        }

        @Override
        public int hashCode() {
            return this.hashCode;
        }
    }

    /**
     * Standard constructor
     * 
     * @param kbClient knowledge base client used to query and update the knowledge
     *                 base containing timeseries information
     */
    public TimeSeriesSparql(TripleStoreClientInterface kbClient) {
        this.kbClient = kbClient;
    }

    /**
     * Setter for the knowledge base client
     * 
     * @param kbClient knowledge base client used to query and update the knowledge
     *                 base containing timeseries information
     */
    public void setKBClient(TripleStoreClientInterface kbClient) {
        this.kbClient = kbClient;
    }
    /////////////////////////////////
    // AVERAGE TIME SERIES UTILITIES
    ////////////////////////////////

    /**
     * Returns the custom duration object containing the numerical duration value
     * and temporal unit
     * associated with the avergae time series iri.
     * Returns null if the average time series does not exist or if the given time
     * series is not of average time series type.
     * 
     * @param tsIRI Average time series
     * @return custom duration with numerical duration and temporal unit
     */
    protected CustomDuration getCustomDuration(String tsIRI) {

        CustomDuration duration = null;
        SelectQuery query = Queries.SELECT();
        Variable value = SparqlBuilder.var("value");
        Variable unit = SparqlBuilder.var("unit");
        Variable dur = SparqlBuilder.var("dur");
        TriplePattern queryPattern1 = iri(tsIRI).has(hasAveragingPeriod, dur);
        TriplePattern queryPattern2 = dur.has(numericDuration, value).andHas(unitType, unit);

        query.select(value, unit).where(queryPattern1, queryPattern2).prefix(PREFIX_TIME).prefix(PREFIX_ONTOLOGY);

        JSONArray result = kbClient.executeQuery(query.getQueryString());
        if (!result.isEmpty()) {
            duration = new CustomDuration(Double.valueOf(result.getJSONObject(0).getString("value")),
                    result.getJSONObject(0).getString("unit"));
        }

        return duration;
    }

    /**
     * Check if an averaging period with given temporalUnit and numericDuration
     * already exists in the knowledge graph.
     * Returns null if such an averaging period doesn't exist.
     * 
     * @param temporalUnit unit type of the averaging period
     * @param numericValue numerical duration of the averaging period
     * @return Averaging period IRI attached to the given temporalUnit and
     *         numericDuration in the knowledge graph
     */
    private String getDurationIRI(String temporalUnit, Double numericValue) {

        String durationIRI = null;
        String queryString = "periodIRI";

        SelectQuery query = Queries.SELECT();
        Variable periodIRI = SparqlBuilder.var(queryString);
        TriplePattern queryPattern = periodIRI.has(numericDuration, numericValue).andHas(unitType, iri(temporalUnit));

        query.select(periodIRI).where(queryPattern).prefix(PREFIX_TIME);

        JSONArray result = kbClient.executeQuery(query.getQueryString());
        if (!result.isEmpty()) {
            durationIRI = result.getJSONObject(0).getString(queryString);
        }

        return durationIRI;
    }

    /**
     * Creates a hashMap containing the (numericDuration, temporalUnit) mapping to
     * averaging period IRI
     * for all the averaging period Iris in the kb
     * 
     * @return hashMap containing (numericDuration, temporalUnit) mapping to
     *         averaging period IRI
     */
    private Map<CustomDuration, String> createDurationIRIMapping() {

        HashMap<CustomDuration, String> durationMap = new HashMap<>();

        SelectQuery query = Queries.SELECT();
        Variable durIRI = SparqlBuilder.var("durIRI");
        Variable value = SparqlBuilder.var("value");
        Variable unit = SparqlBuilder.var("unit");

        TriplePattern queryPattern = durIRI.has(numericDuration, value).andHas(unitType, unit);
        query.select(durIRI, value, unit).where(queryPattern).prefix(PREFIX_TIME);
        JSONArray result = kbClient.executeQuery(query.getQueryString());

        for (int i = 0; i < result.length(); i++) {
            CustomDuration duration = new CustomDuration(Double.valueOf(result.getJSONObject(i).getString("value")),
                    result.getJSONObject(i).getString("unit"));
            durationMap.put(duration, result.getJSONObject(i).getString("durIRI"));
        }
        return durationMap;

    }

    /**
     * * Removes average time series and all associated connections from kb (i.e.
     * remove all triples with tsIRI as subject or object)
     * 
     * @param tsIRI
     */
    private void removeAverageTimeSeries(String tsIRI) {
        // sub query to search for all triples with tsIRI as the subject/object
        SubSelect sub = GraphPatterns.select();
        Variable predicate1 = SparqlBuilder.var("a");
        Variable predicate2 = SparqlBuilder.var("b");
        Variable subject = SparqlBuilder.var("c");
        Variable object1 = SparqlBuilder.var("d");
        Variable predicate3 = SparqlBuilder.var("e");
        Variable object2 = SparqlBuilder.var("f");
        Variable durIRI = SparqlBuilder.var("durIRI");

        TriplePattern deleteTp1 = iri(tsIRI).has(predicate1, object1);
        TriplePattern deleteTp2 = subject.has(predicate2, iri(tsIRI));
        TriplePattern deleteTp3 = iri(tsIRI).has(hasAveragingPeriod, durIRI);
        TriplePattern deleteTp4 = durIRI.has(predicate3, object2);
        sub.select(predicate1, predicate2, predicate3, subject, object1, object2, durIRI).where(deleteTp1, deleteTp2,
                deleteTp3, deleteTp4);

        // insert subquery into main sparql update
        ModifyQuery modify = Queries.MODIFY();
        modify.delete(deleteTp1, deleteTp2, deleteTp3, deleteTp4).where(sub).prefix(PREFIX_ONTOLOGY);

        kbClient.setQuery(modify.getQueryString());
        kbClient.executeUpdate();
    }

    protected String getAveragingPeriod(String tsIRI) {

        String averagingPeriodIRI = null;

        SelectQuery query = Queries.SELECT();
        String queryString = "durIRI";
        Variable durIRI = SparqlBuilder.var(queryString);
        TriplePattern queryPattern = iri(tsIRI).has(hasAveragingPeriod, durIRI);

        query.select(durIRI).where(queryPattern).prefix(PREFIX_ONTOLOGY);
        kbClient.setQuery(query.getQueryString());
        JSONArray result = kbClient.executeQuery();
        if (!result.isEmpty()) {
            averagingPeriodIRI = kbClient.executeQuery().getJSONObject(0).getString(queryString);
        }
        return averagingPeriodIRI;

    }

    /**
     * Check whether a particular time series (i.e. tsIRI) exists
     * 
     * @param timeSeriesIRI timeseries IRI provided as string
     * @return True if a time series instance with the tsIRI exists, false otherwise
     */
    public boolean checkTimeSeriesExists(String timeSeriesIRI) {

        SelectQuery query = Queries.SELECT();
        TriplePattern queryPattern = iri(timeSeriesIRI).isA(tsType);

        query.select(tsType).where(queryPattern, vp).prefix(PREFIX_ONTOLOGY);
        kbClient.setQuery(query.getQueryString());

        return (!kbClient.executeQuery(query.getQueryString()).isEmpty());
    }

    /**
     * Check whether given data IRI is attached to a time series
     * 
     * @param dataIRI data IRI provided as string
     * @return True if dataIRI exists and is attached to a time series, false
     *         otherwise
     */
    public boolean checkDataHasTimeSeries(String dataIRI) {
        String query = String.format("ask {<%s> <%s> ?a}", dataIRI, (TIMESERIES_NAMESPACE + "hasTimeSeries"));
        kbClient.setQuery(query);
        return kbClient.executeQuery().getJSONObject(0).getBoolean("ASK");
    }

    /**
     * Check whether time series IRI has time unit
     * 
     * @param tsIRI timeseries IRI provided as string
     * @return True if the timeseries instance exists and has a defined time unit,
     *         false otherwise
     */
    public boolean checkTimeUnitExists(String tsIRI) {
        String query = String.format("ask {<%s> <%s> ?a}", tsIRI, (TIMESERIES_NAMESPACE + "hasTimeUnit"));
        kbClient.setQuery(query);
        return kbClient.executeQuery().getJSONObject(0).getBoolean("ASK");
    }

    /**
     * Instantiate the time series instance in the knowledge base
     * 
     * @param timeSeriesKgMetadata object containing metadata for a time series
     * @param dbURL                jdbc url of database where time series data is
     *                             stored
     * @param schema               schema in database, defaults to public
     * @param timeClass            java class of the time values
     * @param rdbClientClass       java class of the rdb client (e.g.
     *                             TimeSeriesRDBClient)
     */
    protected void initTS(TimeSeriesKgMetadata timeSeriesKgMetadata, String dbURL, String schema, Class<?> timeClass,
            Class<?> rdbClientClass) {
        // Construct time series IRI
        Iri tsIRI = iri(timeSeriesKgMetadata.getTimeSeriesIri());

        ModifyQuery modify = Queries.MODIFY();
        // set prefix declarations
        modify.prefix(PREFIX_ONTOLOGY, PREFIX_KB, PREFIX_TIME);

        // define type
        modify.insert(tsIRI.isA(timeSeriesKgMetadata.getTimeSeriesType()));

        if (timeSeriesKgMetadata.getTimeSeriesType().equals(AVERAGE_TIMESERIES)) {
            Duration duration = timeSeriesKgMetadata.getDuration();
            if (duration.getNano() != 0) {
                LOGGER.warn("Nano is ignored");
            }

            // numeric Duration
            if (duration.isNegative() || duration.isZero()) {
                throw new JPSRuntimeException(exceptionPrefix + "Numeric Duration must be a positive value");
            }

            // Check if the given temporal unit is one of the allowed values.
            // Where, allowed values are of type ChronoUnit.SECONDS, ChronoUnit.MINUTES,
            // ChronoUnit.HOURS, ChronoUit.DAYS, ChronoUnit.WEEKS, ChronoUnit.MONTHS,
            // ChronoUnit.YEARS
            ChronoUnit unit = timeSeriesKgMetadata.getDurationUnit();
            if (!temporalUnitMap.containsKey(unit)) {
                throw new JPSRuntimeException(
                        exceptionPrefix + "Temporal Unit: " + unit.toString() + " of invalid type");
            }

            Double numericValue = Double.valueOf(duration.getSeconds() / unit.getDuration().getSeconds());
            String temporalUnit = temporalUnitMap.get(unit);

            // Check if a duration iri with given temporalUnit and numericDuration exists in
            // the knowledge graph.
            // If true, attach the Average TimeSeries to the existing duration IRI.
            // Otherwise, create a new duration IRI.
            String durationIRI = getDurationIRI(temporalUnit, numericValue);

            if (durationIRI != null) {
                modify.insert(tsIRI.has(hasAveragingPeriod, iri(durationIRI)));
            } else {
                // Duration IRI
                durationIRI = TIMESERIES_NAMESPACE + "AveragingPeriod_" + UUID.randomUUID();
                modify.insert(tsIRI.has(hasAveragingPeriod, iri(durationIRI)));
                modify.insert(iri(durationIRI).isA(Duration));
                modify.insert(iri(durationIRI).has(unitType, iri(temporalUnit)));
                modify.insert(iri(durationIRI).has(numericDuration, numericValue));
            }
        }

        // Check that the data IRIs are not attached to a different time series IRI
        // already
        List<String> dataIRI = timeSeriesKgMetadata.getDataIriList();
        if (hasAnyExistingTimeSeries(dataIRI)) {
            throw new JPSRuntimeException(
                    exceptionPrefix + "One or more of the provided data IRI has an existing time series");
        }

        // relational database URL and classes used by TimeSeriesClientFactory
        modify.insert(
                tsIRI.has(hasRDB, literalOf(dbURL)).andHas(HAS_RDB_CLIENT_CLASS, rdbClientClass.getCanonicalName())
                        .andHas(HAS_TIME_CLASS, timeClass.getCanonicalName()).andHas(HAS_SCHEMA, schema));

        // link each data to time series
        for (String data : dataIRI) {
            TriplePattern ts_tp = iri(data).has(hasTimeSeries, tsIRI);
            modify.insert(ts_tp);
        }

        // optional: define time unit
        String timeUnit = timeSeriesKgMetadata.getTimeUnit();
        if (timeUnit != null) {
            modify.insert(tsIRI.has(hasTimeUnit, literalOf(timeUnit)));
        }

        kbClient.executeUpdate(modify.getQueryString());
    }

    /**
     * similar to initTS, but uploads triples in one update
     * 
     * @param timeSeriesKgMetadata object containing metadata for a time series
     * @param dbURL                jdbc url of database where time series data is
     *                             stored
     * @param schema               schema in database, defaults to public
     * @param timeClass            java class of the time values
     * @param rdbClientClass       java class of the rdb client (e.g.
     *                             TimeSeriesRDBClient)
     */
    protected void bulkInitTS(List<TimeSeriesKgMetadata> timeSeriesKgMetadataList, String rdbURL, String schema,
            Class<?> timeClass, Class<?> rdbClientClass) {

        TriplesTemplate modify = SparqlBuilder.triplesTemplate();
        // set prefix declarations
        PrefixDeclarations pd = SparqlBuilder.prefixes(PREFIX_ONTOLOGY, PREFIX_KB, PREFIX_TIME);

        Map<CustomDuration, String> durationMap = createDurationIRIMapping();

        timeSeriesKgMetadataList.stream().forEach(timeSeriesKgMetadata -> {
            Iri tsIRI = iri(timeSeriesKgMetadata.getTimeSeriesIri());

            modify.and(tsIRI.isA(timeSeriesKgMetadata.getTimeSeriesType()));

            if (timeSeriesKgMetadata.getTimeSeriesType().equals(AVERAGE_TIMESERIES)) {
                Duration duration = timeSeriesKgMetadata.getDuration();
                if (duration.getNano() != 0) {
                    LOGGER.warn("Nano is ignored");
                }

                // numeric Duration
                if (duration.getSeconds() <= 0.0) {
                    throw new JPSRuntimeException(exceptionPrefix + "Numeric Duration must be a positive value");
                }

                ChronoUnit durationUnit = timeSeriesKgMetadata.getDurationUnit();
                if (!temporalUnitMap.containsKey(durationUnit)) {
                    throw new JPSRuntimeException(
                            exceptionPrefix + "Temporal Unit: " + durationUnit.toString() + " of invalid type");
                }

                Double numericValue = Double
                        .valueOf(duration.getSeconds() / durationUnit.getDuration().getSeconds());
                String temporalUnit = temporalUnitMap.get(durationUnit);

                CustomDuration key = new CustomDuration(numericValue, temporalUnit);

                // Check if a duration iri with given temporalUnit and numericDuration exists in
                // the knowledge graph.
                // If true, attach the Average TimeSeries to the existing duration IRI.
                // Otherwise, create a new duration IRI.
                String durationIRI = null;

                if (durationMap.containsKey(key)) {
                    durationIRI = durationMap.get(key);
                }

                if (durationIRI != null) {
                    modify.and(tsIRI.has(hasAveragingPeriod, iri(durationIRI)));
                } else {
                    // Duration IRI
                    durationIRI = TIMESERIES_NAMESPACE + "AveragingPeriod_" + UUID.randomUUID();
                    modify.and(tsIRI.has(hasAveragingPeriod, iri(durationIRI)));
                    modify.and(iri(durationIRI).isA(Duration));
                    modify.and(iri(durationIRI).has(unitType, iri(temporalUnit)));
                    modify.and(iri(durationIRI).has(numericDuration, numericValue));
                }
            }

            // relational database URL and classes used by TimeSeriesClientFactory
            modify.and(
                    tsIRI.has(hasRDB, literalOf(rdbURL)).andHas(HAS_RDB_CLIENT_CLASS, rdbClientClass.getCanonicalName())
                            .andHas(HAS_TIME_CLASS, timeClass.getCanonicalName()).andHas(HAS_SCHEMA, schema));

            // link each data to time series
            for (String data : timeSeriesKgMetadata.getDataIriList()) {
                TriplePattern tsTp = iri(data).has(hasTimeSeries, tsIRI);
                modify.and(tsTp);
            }

            // optional: define time unit
            String timeUnit = timeSeriesKgMetadata.getTimeUnit();
            if (timeUnit != null) {
                modify.and(tsIRI.has(hasTimeUnit, literalOf(timeUnit)));
            }
        });
        String queryString = pd.getQueryString() + modify.getQueryString().replace("{", "").replace("}", "");
        kbClient.uploadTriples(queryString);
    }

    /**
     * Count number of time series IRIs in kb
     * 
     * @return Total number of time series instances in the knowledge base as int
     */
    public int countTS() {
        SelectQuery query = Queries.SELECT();
        String queryKey = "numtimeseries";

        Variable ts = SparqlBuilder.var("ts");
        Variable numtimeseries = SparqlBuilder.var(queryKey);

        GraphPattern querypattern = ts.isA(tsType);
        Assignment count = Expressions.count(ts).as(numtimeseries);

        query.select(count).where(querypattern, vp).prefix(PREFIX_ONTOLOGY);
        kbClient.setQuery(query.getQueryString());

        return kbClient.executeQuery().getJSONObject(0).getInt(queryKey);
    }

    /**
     * Attach data IRI to existing time series IRI in kb (i.e. insert
     * "hasTimeSeries" relationship)
     * 
     * @param dataIRI data IRI provided as string
     * @param tsIRI   time series IRI provided as string
     */
    protected void insertTimeSeriesAssociation(String dataIRI, String tsIRI) {

        // Check that the data IRI is not attached to a different time series IRI
        // already
        String ts = getTimeSeries(dataIRI);
        if (ts != null) {
            throw new JPSRuntimeException(
                    exceptionPrefix + "The data IRI " + dataIRI + " is already attached to time series " + ts);
        }

        // Check whether time series IRI exists
        if (!checkTimeSeriesExists(tsIRI)) {
            throw new JPSRuntimeException(
                    exceptionPrefix + "Time series " + tsIRI + " does not exists in the Knowledge Graph");
        }

        // Add triple with "hasTimeSeries" relationship between dataIRI and tsIRI
        InsertDataQuery insert = Queries.INSERT_DATA(iri(dataIRI).has(hasTimeSeries, iri(tsIRI)));
        insert.prefix(PREFIX_ONTOLOGY);
        kbClient.executeUpdate(insert.getQueryString());

    }

    /**
     * Remove relationship between dataIRI and associated time series from kb
     * 
     * @param dataIRI data IRI provided as string
     */
    protected void removeTimeSeriesAssociation(String dataIRI) {

        // Check whether associated time series has further data associated with it
        String tsIRI = getTimeSeries(dataIRI);

        if (tsIRI != null) {
            List<String> data = getAssociatedData(tsIRI);

            if (data.size() == 1) {
                // Remove entire time series if no further data is associated with it
                removeTimeSeries(tsIRI);
            } else {
                // Remove only relationship between dataIRI and tsIRI
                DeleteDataQuery delete = Queries.DELETE_DATA(iri(dataIRI).has(hasTimeSeries, iri(tsIRI)));
                delete.prefix(PREFIX_ONTOLOGY);
                kbClient.executeUpdate(delete.getQueryString());
            }

        }
    }

    /**
     * Remove time series and all associated connections from kb (i.e. remove all
     * triples with tsIRI as subject or object)
     * 
     * @param tsIRI timeseries IRI provided as string
     */
    protected void removeTimeSeries(String tsIRI) {

        if (checkTimeSeriesExists(tsIRI)) {

            SelectQuery query = Queries.SELECT();
            String queryKey = "numTs";

            Variable ts = SparqlBuilder.var("ts");
            Variable durIRI = SparqlBuilder.var("durIRI");
            Variable numTs = SparqlBuilder.var(queryKey);

            GraphPattern queryPattern1 = iri(tsIRI).has(hasAveragingPeriod, durIRI);
            GraphPattern queryPattern2 = ts.has(hasAveragingPeriod, durIRI);

            Assignment count = Expressions.count(ts).as(numTs);

            query.select(count).where(queryPattern1, queryPattern2).prefix(PREFIX_ONTOLOGY);
            kbClient.setQuery(query.getQueryString());

            Integer avgTsIris = kbClient.executeQuery().getJSONObject(0).getInt(queryKey);

            if (avgTsIris == 1) {
                removeAverageTimeSeries(tsIRI);
            } else {
                // sub query to search for all triples with tsIRI as the subject/object
                SubSelect sub = GraphPatterns.select();
                Variable predicate1 = SparqlBuilder.var("a");
                Variable predicate2 = SparqlBuilder.var("b");
                Variable subject = SparqlBuilder.var("c");
                Variable object = SparqlBuilder.var("d");

                TriplePattern deleteTp1 = iri(tsIRI).has(predicate1, object);
                TriplePattern deleteTp2 = subject.has(predicate2, iri(tsIRI));
                sub.select(predicate1, predicate2, subject, object).where(deleteTp1, deleteTp2);

                // insert subquery into main sparql update
                ModifyQuery modify = Queries.MODIFY();
                modify.delete(deleteTp1, deleteTp2).where(sub);

                kbClient.setQuery(modify.getQueryString());
                kbClient.executeUpdate();
            }
        }
    }

    /**
     * Returns the time series type IRI as a string for a given time series IRI
     * 
     * @param tsIRI timeSeries IRI
     * @return
     */
    protected String getTimeSeriesType(String tsIRI) {
        String result = null;
        SelectQuery query = Queries.SELECT();
        TriplePattern queryPattern = iri(tsIRI).isA(tsType);

        query.select(tsType).where(queryPattern, vp).prefix(PREFIX_ONTOLOGY);
        kbClient.setQuery(query.getQueryString());

        JSONArray queryResult = kbClient.executeQuery();
        if (!queryResult.isEmpty())
            result = queryResult.getJSONObject(0).getString(tsType.getQueryString().substring(1));
        return result;
    }

    /**
     * Remove all time series from kb
     */
    protected void removeAllTimeSeries() {

        Variable predicate1 = SparqlBuilder.var("p1");
        Variable predicate2 = SparqlBuilder.var("p2");
        Variable predicate3 = SparqlBuilder.var("p3");
        Variable subject1 = SparqlBuilder.var("s1");
        Variable object1 = SparqlBuilder.var("o1");
        Variable object2 = SparqlBuilder.var("o2");
        Variable timeseries = SparqlBuilder.var("ts");
        Variable dur = SparqlBuilder.var("dur");

        TriplePattern deleteTp1 = timeseries.has(predicate1, object1);
        TriplePattern deleteTp2 = subject1.has(predicate2, timeseries);
        TriplePattern deleteTp3 = timeseries.has(hasAveragingPeriod, dur);
        TriplePattern deleteTp4 = dur.has(predicate3, object2);

        GraphPatternNotTriples optional = GraphPatterns.optional(deleteTp3, deleteTp4);

        // insert subquery into main sparql update
        ModifyQuery modify = Queries.MODIFY();
        modify.delete(deleteTp1, deleteTp2, deleteTp3, deleteTp4)
                .where(timeseries.isA(tsType), deleteTp1, deleteTp2, optional, vp).prefix(PREFIX_ONTOLOGY);

        kbClient.executeUpdate(modify.getQueryString());
    }

    /**
     * Get time series IRI associated with given data IRI
     * <p>
     * Returns null if dataIRI does not exist or no time series is attached to
     * dataIRI
     * 
     * @param dataIRI data IRI provided as string
     * @return The corresponding timeseries IRI as string
     */
    public String getTimeSeries(String dataIRI) {

        String result = null;
        String queryString = "tsIRI";

        SelectQuery query = Queries.SELECT();
        Variable tsIRI = SparqlBuilder.var(queryString);
        TriplePattern queryPattern = iri(dataIRI).has(hasTimeSeries, tsIRI);

        query.select(tsIRI).where(queryPattern).prefix(PREFIX_ONTOLOGY);

        kbClient.setQuery(query.getQueryString());
        JSONArray queryResult = kbClient.executeQuery();
        if (!queryResult.isEmpty()) {
            result = kbClient.executeQuery().getJSONObject(0).getString(queryString);
        }
        return result;
    }

    /**
     * Get database URL associated with time series IRI
     * <p>
     * Returns null if time series does not exist or does not have associated
     * database URL
     * 
     * @param tsIRI timeseries IRI provided as string
     * @return The URL to the database where data from that timeseries is stored as
     *         string
     */
    public String getDbUrl(String tsIRI) {

        String result = null;
        String queryString = "dbURL";

        SelectQuery query = Queries.SELECT();
        Variable dbURL = SparqlBuilder.var(queryString);
        TriplePattern queryPattern = iri(tsIRI).has(hasRDB, dbURL);

        query.select(dbURL).where(queryPattern).prefix(PREFIX_ONTOLOGY);

        kbClient.setQuery(query.getQueryString());
        JSONArray queryResult = kbClient.executeQuery();
        if (!queryResult.isEmpty()) {
            result = kbClient.executeQuery().getJSONObject(0).getString(queryString);
        }
        return result;
    }

    /**
     * Get time unit associated with time series IRI
     * <p>
     * Returns null if time series does not exist or does not have associated time
     * unit
     * 
     * @param tsIRI timeseries IRI provided as string
     * @return The time unit of timeseries as string
     */
    public String getTimeUnit(String tsIRI) {

        String result = null;
        String queryString = "timeUnit";

        SelectQuery query = Queries.SELECT();
        Variable timeUnit = SparqlBuilder.var(queryString);
        TriplePattern queryPattern = iri(tsIRI).has(hasTimeUnit, timeUnit);

        query.select(timeUnit).where(queryPattern).prefix(PREFIX_ONTOLOGY);

        kbClient.setQuery(query.getQueryString());
        JSONArray queryResult = kbClient.executeQuery();
        if (!queryResult.isEmpty()) {
            result = kbClient.executeQuery().getJSONObject(0).getString(queryString);
        }
        return result;
    }

    /**
     * Get data IRIs associated with a given time series IRI
     * <p>
     * Returns empty List if time series does not exist or does not have associated
     * data
     * 
     * @param tsIRI timeseries IRI provided as string
     * @return List of data IRIs attached to the time series as string
     */
    public List<String> getAssociatedData(String tsIRI) {
        String queryString = "dataIRI";
        SelectQuery query = Queries.SELECT();

        Variable data = SparqlBuilder.var(queryString);
        TriplePattern queryPattern = data.has(hasTimeSeries, iri(tsIRI));

        query.select(data).where(queryPattern).prefix(PREFIX_ONTOLOGY);

        return getInstances(query, queryString);
    }

    /**
     * Extract all time series IRIs from kb
     * <p>
     * Returns empty List if no time series exist in kb
     * 
     * @return List of all time series IRI in the knowledge base provided as string
     */
    public List<String> getAllTimeSeries() {
        String queryString = "ts";
        SelectQuery query = Queries.SELECT();

        Variable ts = SparqlBuilder.var(queryString);
        TriplePattern queryPattern = ts.isA(tsType);

        query.select(ts).where(queryPattern, vp).prefix(PREFIX_ONTOLOGY);

        return getInstances(query, queryString);
    }

    /**
     * Execute a query that selects all individuals in the kb that fulfill a certain
     * pattern
     * 
     * @param instanceSelectQuery the query to execute. Should be a select query
     *                            with a single triple pattern where
     *                            the subject represents the instance to retrieve.
     * @param placeholder         the placeholder used in the select query for the
     *                            instance to retrieve provided as string without ?
     * @return List of all instances' IRIs in the knowledge base provided as string
     */
    private List<String> getInstances(SelectQuery instanceSelectQuery, String placeholder) {
        kbClient.setQuery(instanceSelectQuery.getQueryString());
        JSONArray queryResult = kbClient.executeQuery();

        List<String> instanceIRIs = new ArrayList<>();
        for (int i = 0; i < queryResult.length(); i++) {
            instanceIRIs.add(queryResult.getJSONObject(i).getString(placeholder));
        }

        return instanceIRIs;
    }

    /**
     * used to reinitialise time series instance when deletion in RDB fails
     * 
     * @param timeSeriesKgMetadata
     * @param dbURL
     * @param schema
     * @param timeClass
     * @param rdbClientClass
     */
    void reInitTS(TimeSeriesKgMetadata timeSeriesKgMetadata, String dbURL, String schema, Class<?> timeClass,
            Class<?> rdbClientClass) {
        // Construct time series IRI
        Iri tsIRI = iri(timeSeriesKgMetadata.getTimeSeriesIri());

        ModifyQuery modify = Queries.MODIFY();
        // set prefix declarations
        modify.prefix(PREFIX_ONTOLOGY, PREFIX_KB, PREFIX_TIME);

        Iri timeSeriesType = timeSeriesKgMetadata.getTimeSeriesType();
        modify.insert(tsIRI.isA(timeSeriesType));

        if (timeSeriesType.equals(AVERAGE_TIMESERIES)) {
            String durationIRI = timeSeriesKgMetadata.getDurationIri();
            double durationValue = timeSeriesKgMetadata.getDurationValue();
            String durationUnit = timeSeriesKgMetadata.getDurationUnitIri();

            // numeric Duration
            if (durationValue <= 0.0) {
                throw new JPSRuntimeException(exceptionPrefix + "Numeric Duration must be a positive value");
            }

            // Check if the given temporal unit is one of the allowed values.
            // Where, allowed values are of type ChronoUnit.SECONDS, ChronoUnit.MINUTES,
            // ChronoUnit.HOURS, ChronoUit.DAYS, ChronoUnit.WEEKS, ChronoUnit.MONTHS,
            // ChronoUnit.YEARS
            if (!temporalUnitType.contains(durationUnit)) {
                throw new JPSRuntimeException(exceptionPrefix + "Temporal Unit: " + durationUnit + " of invalid type");
            }

            modify.insert(tsIRI.has(hasAveragingPeriod, iri(durationIRI)));
            modify.insert(iri(durationIRI).isA(Duration));
            modify.insert(iri(durationIRI).has(unitType, iri(durationUnit)));
            modify.insert(iri(durationIRI).has(numericDuration, durationValue));

        }

        // Check that the data IRIs are not attached to a different time series IRI
        // already
        if (hasAnyExistingTimeSeries(timeSeriesKgMetadata.getDataIriList())) {
            throw new JPSRuntimeException(
                    exceptionPrefix + "One or more of the provided data IRI has an existing time series");
        }

        // relational database URL and classes used by TimeSeriesClientFactory
        modify.insert(
                tsIRI.has(hasRDB, literalOf(dbURL)).andHas(HAS_RDB_CLIENT_CLASS, rdbClientClass.getCanonicalName())
                        .andHas(HAS_TIME_CLASS, timeClass.getCanonicalName()).andHas(HAS_SCHEMA, schema));

        // link each data to time series
        for (String data : timeSeriesKgMetadata.getDataIriList()) {
            TriplePattern tsTp = iri(data).has(hasTimeSeries, tsIRI);
            modify.insert(tsTp);
        }

        // optional: define time unit
        if (timeSeriesKgMetadata.getTimeUnit() != null) {
            modify.insert(tsIRI.has(hasTimeUnit, literalOf(timeSeriesKgMetadata.getTimeUnit())));
        }

        kbClient.executeUpdate(modify.getQueryString());
    }

    /**
     * returns true if any of the provided IRI contains a time series instance
     * 
     * @param dataIriList
     * @return
     */
    boolean hasAnyExistingTimeSeries(List<String> dataIriList) {
        String allDataIRI = String.join(" ",
                dataIriList.stream().map(str -> "<" + str + ">").collect(Collectors.toList()));
        String query = String.format("ASK {?data <%s> ?ts . VALUES ?data { %s } }",
                (TIMESERIES_NAMESPACE + "hasTimeSeries"), allDataIRI);
        kbClient.setQuery(query);
        return kbClient.executeQuery().getJSONObject(0).getBoolean("ASK");
    }
}
