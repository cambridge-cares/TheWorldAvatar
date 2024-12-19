package com.cmclinnovations.featureinfo.core.trajectory;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;
import org.postgis.LineString;
import org.postgis.Point;
import org.apache.logging.log4j.LogManager;

import org.apache.jena.query.Query;
import org.apache.jena.query.QueryFactory;
import org.apache.jena.sparql.core.Var;
import org.apache.jena.sparql.engine.binding.Binding;
import org.apache.jena.sparql.engine.binding.BindingFactory;
import org.apache.jena.sparql.syntax.Element;
import org.apache.jena.sparql.syntax.ElementData;
import org.apache.jena.sparql.syntax.ElementGroup;
import org.apache.jena.sparql.syntax.ElementPathBlock;
import org.apache.jena.sparql.syntax.ElementTriplesBlock;
import org.apache.jena.sparql.syntax.ElementVisitorBase;
import org.apache.jena.sparql.syntax.ElementWalker;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.graph.Node;

import com.cmclinnovations.featureinfo.config.ConfigEntry;
import com.cmclinnovations.featureinfo.config.ConfigStore;
import com.cmclinnovations.featureinfo.config.StackEndpoint;
import com.cmclinnovations.featureinfo.config.StackEndpointType;
import com.cmclinnovations.featureinfo.config.StackInteractor;
import com.cmclinnovations.featureinfo.core.meta.MetaParser;
import com.cmclinnovations.featureinfo.utils.Utils;

import net.sf.jsqlparser.JSQLParserException;
import net.sf.jsqlparser.parser.CCJSqlParserUtil;
import net.sf.jsqlparser.statement.select.PlainSelect;
import net.sf.jsqlparser.statement.select.SelectItem;
import net.sf.jsqlparser.statement.select.Select;

import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClientFactory;

public class TrajectoryHandler {
    private static final Logger LOGGER = LogManager.getLogger(TrajectoryHandler.class);
    private final Optional<String> enforcedEndpoint;
    private final String iri;
    private final ConfigStore configStore;
    private RemoteStoreClient remoteStoreClient;
    private Long lowerbound;
    private Long upperbound;

    private static final String FEATURE_VARIABLE_NAME = "Feature";

    public TrajectoryHandler(String iri, Optional<String> enforcedEndpoint, ConfigStore configStore, Long lowerbound,
            Long upperbound) {
        this.iri = iri;
        this.enforcedEndpoint = enforcedEndpoint;
        this.configStore = configStore;
        this.lowerbound = lowerbound;
        this.upperbound = upperbound;
    }

    public void setClients(RemoteStoreClient remoteStoreClient) {
        this.remoteStoreClient = remoteStoreClient;
    }

    public JSONObject getData(List<ConfigEntry> classMatches) {
        List<JSONArray> rawResults = new ArrayList<>();
        classMatches.stream().filter(c -> c.getFeatureIriQuery() != null).forEach(classMatch -> {
            // Construct line using points queried from point time series
            List<String> pointIriList = getPointIriList(classMatch.getPointIriQuery());

            LineString trajectoryLine = makeLine(pointIriList);

            String featureIriQuery = classMatch.getFeatureIriQuery().replace("[LINE_WKT]", trajectoryLine.toString());

            List<String> featureIriList = getFeatures(featureIriQuery, classMatch.getTrajectoryDatabase());

            String trajectoryMetaQueryTemplate = classMatch.getTrajectoryMetaQuery();
            rawResults.add(getMetadata(trajectoryMetaQueryTemplate, featureIriList));
        });

        return MetaParser.formatData(rawResults);
    }

    List<String> getPointIriList(String pointIriQueryTemplate) {
        String queryString = Utils.queryInject(pointIriQueryTemplate, iri,
                configStore.getStackEndpoints(StackEndpointType.ONTOP),
                Utils.getBlazegraphEndpoints(configStore, enforcedEndpoint));

        String queryParameter = getSparqlQuerySelectParameter(queryString);

        List<String> endpoints = Utils.getBlazegraphURLs(configStore, enforcedEndpoint);

        JSONArray queryResult;
        if (endpoints.size() == 1) {
            LOGGER.debug("Running non-federated meta data query.");
            remoteStoreClient.setQueryEndpoint(endpoints.get(0));
            queryResult = remoteStoreClient.executeQuery(queryString);
        } else {
            LOGGER.debug("Running federated meta data query.");
            queryResult = remoteStoreClient.executeFederatedQuery(endpoints, queryString);
        }

        List<String> pointIriList = new ArrayList<>();
        for (int i = 0; i < queryResult.length(); i++) {
            pointIriList.add(queryResult.getJSONObject(i).getString(queryParameter));
        }

        return pointIriList;
    }

    private LineString makeLine(List<String> pointIriList) {
        StackEndpoint rdbEndpoint = this.configStore.getStackEndpoints(StackEndpointType.POSTGRES).get(0);

        List<Long> timeList = new ArrayList<>();
        List<Point> pointList = new ArrayList<>();

        // In case of multiple point instances, combine into a single trajectory (e.g.
        // multiple devices linked to a single user)
        pointIriList.forEach(pointIri -> {
            TimeSeriesClient<Long> tsClient = getTimeSeriesClientViaFactory(List.of(pointIri));
            tsClient.setRDBClient(tsClient.getRdbUrl(), rdbEndpoint.username(), rdbEndpoint.password());

            TimeSeries<Long> timeseries = tsClient.getTimeSeriesWithinBounds(List.of(pointIri), lowerbound, upperbound);
            timeList.addAll(timeseries.getTimes());
            pointList.addAll(timeseries.getValuesAsPoint(pointIri));
        });

        // sort points according to time
        sortTwoLists(timeList, pointList);

        return new LineString(pointList.toArray(new Point[pointList.size()]));
    }

    private List<String> getFeatures(String queryString, String trajectoryDatabase) {
        List<String> featureIriList = new ArrayList<>();
        if (isSparql(queryString)) {
            queryString = Utils.queryInject(queryString, iri,
                    configStore.getStackEndpoints(StackEndpointType.ONTOP),
                    Utils.getBlazegraphEndpoints(configStore, enforcedEndpoint));

            String queryParameter = getSparqlQuerySelectParameter(queryString);
            List<String> endpoints = Utils.getBlazegraphURLs(configStore, enforcedEndpoint);
            JSONArray queryResult;
            if (endpoints.size() == 1) {
                LOGGER.debug("Running non-federated meta data query.");
                remoteStoreClient.setQueryEndpoint(endpoints.get(0));
                queryResult = remoteStoreClient.executeQuery(queryString);
            } else {
                LOGGER.debug("Running federated meta data query.");
                queryResult = remoteStoreClient.executeFederatedQuery(endpoints, queryString);
            }

            for (int i = 0; i < queryResult.length(); i++) {
                featureIriList.add(queryResult.getJSONObject(i).getString(queryParameter));
            }
        } else {
            String queryParameter = getSqlSelectParameter(queryString);
            try (Connection conn = connectToDatabase(trajectoryDatabase);
                    Statement statement = conn.createStatement()) {
                ResultSet result = statement.executeQuery(queryString);

                while (result.next()) {
                    featureIriList.add(result.getString(queryParameter));
                }
            } catch (SQLException e) {
                String errmsg = "Error executing SQL query to obtain feature IRIs";
                LOGGER.error(errmsg);
                LOGGER.error(e.getMessage());
                throw new RuntimeException(errmsg, e);
            }
        }
        return featureIriList;
    }

    private JSONArray getMetadata(String queryTemplate, List<String> featureIriList) {
        String queryString = Utils.queryInject(queryTemplate, iri,
                configStore.getStackEndpoints(StackEndpointType.ONTOP),
                Utils.getBlazegraphEndpoints(configStore, enforcedEndpoint));

        Query query = QueryFactory.create(queryString);

        if (checkIfVariableExists(query, FEATURE_VARIABLE_NAME)) {
            // add VALUES ?Feature {<feature1> <feature2> <feature3>} to query
            addValues(query, featureIriList);

            // execute query
            List<String> endpoints = Utils.getBlazegraphURLs(configStore, enforcedEndpoint);

            JSONArray queryResult;
            if (endpoints.size() == 1) {
                LOGGER.debug("Running non-federated meta data query for trajectory.");
                remoteStoreClient.setQueryEndpoint(endpoints.get(0));
                queryResult = remoteStoreClient.executeQuery(query.toString());
            } else {
                LOGGER.debug("Running federated meta data query for trajectory.");
                queryResult = remoteStoreClient.executeFederatedQuery(endpoints, query.toString());
            }

            return queryResult;
        } else {
            String errmsg = "Trajectory metadata query must contain a variable named " + FEATURE_VARIABLE_NAME;
            LOGGER.error(errmsg);
            throw new RuntimeException(errmsg);
        }
    }

    private boolean checkIfVariableExists(Query query, String variableName) {
        // Set to collect variables
        Set<String> variables = new HashSet<>();

        // Walk through the query and collect variables
        ElementWalker.walk(query.getQueryPattern(), new ElementVisitorBase() {
            @Override
            public void visit(ElementTriplesBlock el) {
                el.patternElts().forEachRemaining(triple -> collectTripleVariables(triple.getSubject(),
                        triple.getPredicate(), triple.getObject()));
            }

            @Override
            public void visit(ElementPathBlock el) {
                el.patternElts().forEachRemaining(triplePath -> collectTripleVariables(triplePath.getSubject(),
                        triplePath.getPredicate(), triplePath.getObject()));
            }

            private void collectTripleVariables(Node subject, Node predicate, Node object) {
                if (subject.isVariable())
                    variables.add(subject.getName());
                if (predicate.isVariable())
                    variables.add(predicate.getName());
                if (object.isVariable())
                    variables.add(object.getName());
            }
        });

        // Check if the desired variable exists in the set
        return variables.contains(variableName);
    }

    private void addValues(Query query, List<String> featureIriList) {
        // Inject featureIriList into the given query template
        // create a VALUES clause from featureIriList
        ElementData valuesClause = new ElementData();
        Var featureVar = Var.alloc(FEATURE_VARIABLE_NAME);
        valuesClause.add(featureVar);

        List<Node> featureValues = featureIriList.stream().map(NodeFactory::createURI).collect(Collectors.toList());

        featureValues.forEach(featureValue -> {
            Binding binding = BindingFactory.binding(featureVar, featureValue);
            valuesClause.add(binding);
        });

        // Get the existing WHERE clause (query pattern)
        Element existingPattern = query.getQueryPattern();

        // Create a new ElementGroup to combine the VALUES clause with the existing
        // patterns
        ElementGroup newGroup = new ElementGroup();

        // Add the VALUES clause to the top
        newGroup.addElement(valuesClause);

        // Add the existing pattern to the group (if there is any existing pattern)
        if (existingPattern != null) {
            newGroup.addElement(existingPattern);
        }

        // Set the modified group as the new query pattern (updated WHERE clause)
        query.setQueryPattern(newGroup);
    }

    private TimeSeriesClient<Long> getTimeSeriesClientViaFactory(List<String> pointIriList) {
        // Run query
        List<String> endpoints = Utils.getBlazegraphURLs(configStore, enforcedEndpoint);

        try {
            if (endpoints.size() == 1) {
                LOGGER.debug("Generating time series client via non-federated query.");

                remoteStoreClient.setQueryEndpoint(endpoints.get(0));

                return (TimeSeriesClient<Long>) TimeSeriesClientFactory.getInstance(remoteStoreClient, pointIriList);
            } else {
                return (TimeSeriesClient<Long>) TimeSeriesClientFactory.getInstance(endpoints, pointIriList);
            }
        } catch (Exception e) {
            String errmsg = "Failed to construct TimeSeriesClient using TimeSeriesClientFactory, be sure to use JPS base lib 1.44.0 or later";
            LOGGER.error(errmsg);
            LOGGER.error(e.getMessage());
            throw new RuntimeException(errmsg, e);
        }
    }

    private <T extends Comparable<T>, U> void sortTwoLists(List<T> listToSortBy, List<U> listToSort) {
        // Create a list of indices
        List<Integer> indices = new ArrayList<>();
        for (int i = 0; i < listToSortBy.size(); i++) {
            indices.add(i);
        }

        Collections.sort(indices, Comparator.comparing(listToSortBy::get));

        // Create a sorted copy of listToSortBy and listToSort
        List<T> sortedListToSortBy = new ArrayList<>(listToSortBy.size());
        List<U> sortedListToSort = new ArrayList<>(listToSort.size());
        for (int i : indices) {
            sortedListToSortBy.add(listToSortBy.get(i));
            sortedListToSort.add(listToSort.get(i));
        }

        // Replace the original lists with the sorted ones
        listToSortBy.clear();
        listToSortBy.addAll(sortedListToSortBy);

        listToSort.clear();
        listToSort.addAll(sortedListToSort);
    }

    // Check if the query is SPARQL by trying to parse it using Apache Jena
    private boolean isSparql(String query) {
        try {
            QueryFactory.create(query);
            return true;
        } catch (Exception e) {
            return false;
        }
    }

    private String getSparqlQuerySelectParameter(String queryString) {
        Query query = QueryFactory.create(queryString);

        String queryParameter;
        if (query.isSelectType()) {
            if (query.isQueryResultStar()) {
                String errmsg = "SELECT * is not allowed for trajectory query, please provide exactly one SELECT parameter";
                LOGGER.error(errmsg);
                throw new RuntimeException(errmsg);
            } else if (query.getResultVars().size() != 1) {
                String errmsg = "Trajectory query needs to have exactly one SELECT parameter";
                LOGGER.error(errmsg);
                throw new RuntimeException(errmsg);
            }
            queryParameter = query.getResultVars().get(0);
        } else {
            String errmsg = "Trajectory query is not a SELECT query";
            LOGGER.error(errmsg);
            throw new RuntimeException(errmsg);
        }

        return queryParameter;
    }

    private String getSqlSelectParameter(String queryString) {
        // Parse the SQL query
        net.sf.jsqlparser.statement.Statement statement = null;
        try {
            statement = CCJSqlParserUtil.parse(queryString);
        } catch (JSQLParserException e) {
            String errmsg = "Error parsing SQL feature query";
            LOGGER.error(errmsg);
            LOGGER.error(e.getMessage());
            throw new RuntimeException(errmsg, e);
        }

        Select selectStatement = (Select) statement;

        // Extract the PlainSelect part of the statement
        PlainSelect plainSelect = (PlainSelect) selectStatement.getSelectBody();

        // Get the SELECT items (parameters)
        List<SelectItem> selectItems = plainSelect.getSelectItems();

        if (selectItems.size() != 1) {
            String errmsg = "Feature query needs to have exactly one SELECT parameter";
            LOGGER.error(errmsg);
            throw new RuntimeException(errmsg);
        }

        // Convert SelectItems to a list of strings
        return selectItems.get(0).toString();
    }

    /**
     * Re-initialises the RDB client with a connection to the input database.
     * 
     * @param rdbEndpoint endpoint for postgres.
     * @param database    database name.
     * 
     * @throws SQLException if database cannot be connected to.
     */
    protected Connection connectToDatabase(String database) throws SQLException {
        StackEndpoint rdbEndpoint = this.configStore.getStackEndpoints(StackEndpointType.POSTGRES).get(0);
        LOGGER.info("Making new connection to database: {}", database);
        String postgresURL = StackInteractor.generatePostgresURL(database);
        // Create new connection
        RemoteRDBStoreClient dbClient = new RemoteRDBStoreClient(postgresURL, rdbEndpoint.username(),
                rdbEndpoint.password());
        return dbClient.getConnection();
    }
}
