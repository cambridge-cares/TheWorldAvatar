package uk.ac.cam.cares.jps.base.timeseries;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf;
import org.json.JSONArray;
import org.json.JSONObject;

import uk.ac.cam.cares.jps.base.derivation.ValuesPattern;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.TripleStoreClientInterface;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

public class TimeSeriesClientFactory {
    private static final Variable dataVar = SparqlBuilder.var("data");
    private static final Variable timeClassVar = SparqlBuilder.var("timeClass");
    private static final Variable rdbClientClassVar = SparqlBuilder.var("rdbClientClass");
    private static final Variable rdbUrlVar = SparqlBuilder.var("rdbUrl");
    private static final Variable schemaVar = SparqlBuilder.var("schema");

    /**
     * Factory method to get a TimeSeriesClient with the appropriate time class and
     * the RDB client class. This is only meant to be used for time series that are
     * already instantiated.
     * Queries the time and RDB client classes for the given data IRIs and
     * instantiates a TimeSeriesClient object.
     * 
     * @param storeClient
     * @param dataIriList
     * @return
     * @throws ClassNotFoundException
     * @throws NoSuchMethodException
     * @throws SecurityException
     * @throws InstantiationException
     * @throws IllegalAccessException
     * @throws IllegalArgumentException
     * @throws InvocationTargetException
     */
    public static TimeSeriesClient<?> getInstance(TripleStoreClientInterface storeClient, List<String> dataIriList)
            throws ClassNotFoundException, NoSuchMethodException, SecurityException, InstantiationException,
            IllegalAccessException, IllegalArgumentException, InvocationTargetException {
        return getInstance(Collections.emptyList(), storeClient, dataIriList);
    }

    /**
     * Similar as above, except that the data are obtained from multiple endpoints
     * 
     * @param endpoints
     * @param dataIriList
     * @return
     * @throws ClassNotFoundException
     */
    public static TimeSeriesClient<?> getInstance(List<String> endpoints, List<String> dataIriList)
            throws ClassNotFoundException, NoSuchMethodException, SecurityException, InstantiationException,
            IllegalAccessException, IllegalArgumentException, InvocationTargetException {
        return getInstance(endpoints, new RemoteStoreClient(), dataIriList);
    }

    private static TimeSeriesClient<?> getInstance(List<String> endpoints, TripleStoreClientInterface storeClient,
            List<String> dataIriList)
            throws ClassNotFoundException, NoSuchMethodException, SecurityException, InstantiationException,
            IllegalAccessException, IllegalArgumentException, InvocationTargetException {

        JSONArray queryResult;
        if (endpoints.isEmpty()) {
            queryResult = storeClient.executeQuery(getQuery(dataIriList).getQueryString());
        } else {
            queryResult = ((RemoteStoreClient) storeClient).executeFederatedQuery(endpoints,
                    getQuery(dataIriList).getQueryString());
        }

        if (queryResult.length() != 1) {
            throw new JPSRuntimeException(
                    "Given IRIs do not have exactly one time class/rdb client class/rdb url/schema");
        }

        JSONObject firstResult = queryResult.getJSONObject(0);

        Class<?> rdbClientClass = Class.forName(firstResult.getString(rdbClientClassVar.getVarName()));
        Constructor<?> constructor = rdbClientClass.getConstructor(Class.class);

        Class<?> timeClass = Class.forName(firstResult.getString(timeClassVar.getVarName()));
        TimeSeriesRDBClientInterface<?> rdbClient = (TimeSeriesRDBClientInterface<?>) constructor
                .newInstance(timeClass);

        rdbClient.setRdbURL(firstResult.getString(rdbUrlVar.getVarName()));

        rdbClient.setSchema(firstResult.getString(schemaVar.getVarName()));

        return new TimeSeriesClient<>(storeClient, rdbClient);
    }

    private TimeSeriesClientFactory() {
        throw new IllegalStateException(TimeSeriesClientFactory.class.getName());
    }

    private static SelectQuery getQuery(List<String> dataIriList) {

        final Variable timeSeriesVar = SparqlBuilder.var("timeSeries");

        return Queries.SELECT(timeClassVar, rdbClientClassVar, rdbUrlVar, schemaVar)
                .distinct()
                .prefix(TimeSeriesSparql.PREFIX_ONTOLOGY)
                .where(dataVar.has(TimeSeriesSparql.hasTimeSeries, timeSeriesVar),
                        timeSeriesVar.has(TimeSeriesSparql.HAS_TIME_CLASS, timeClassVar),
                        timeSeriesVar.has(TimeSeriesSparql.HAS_RDB_CLIENT_CLASS, rdbClientClassVar),
                        timeSeriesVar.has(TimeSeriesSparql.hasRDB, rdbUrlVar),
                        timeSeriesVar.has(TimeSeriesSparql.HAS_SCHEMA, schemaVar),
                        new ValuesPattern(dataVar, dataIriList.stream().map(Rdf::iri).collect(Collectors.toList())));
    }
}
