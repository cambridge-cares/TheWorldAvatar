package uk.ac.cam.cares.jps.agent.historicalhouse45utilitiesagent.ontobim;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.sparql.lang.sparql_11.ParseException;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

class SelectQueryBuilder extends QueryBuilderNode {
    protected static final String SUBJECT_VAR = "?subject";
    protected static final String ZONE_VAR = "?zone";
    protected static final String NAME_VAR = "?name";


    /**
     * Generate the required SELECT query for retrieving the instances of utility consumption time series generated.
     *
     * @return The query in String format.
     */
    protected static String genUtilityTSSelectQuery() {
        SelectBuilder builder = new SelectBuilder();
        QueryHandler.genPrefixMapping(builder);
        builder.addVar(SUBJECT_VAR)
                .addWhere(SUBJECT_VAR, TIMESERIES_HAS_TS, "?timeseries");
        return builder.buildString();
    }

    /**
     * Generate the required SELECT query for retrieving the building and storey instances.
     *
     * @return The query in String format.
     */
    protected static String genZoneSelectQuery() {
        SelectBuilder builder = new SelectBuilder();
        QueryHandler.genPrefixMapping(builder);
        SelectBuilder subquery = builder.clone();
        builder.addVar(ZONE_VAR).addVar(NAME_VAR)
                .addWhere(ZONE_VAR, RDFTYPE, BOT_BUILDING);
        // Add union query
        try {
            subquery.addWhere(ZONE_VAR, RDFTYPE, BOT_STOREY)
                    .addWhere(ZONE_VAR, RDFS_LABEL, NAME_VAR)
                    .addFilter(NAME_VAR + " IN ('Attic','Ground','Level 1')");
        } catch (ParseException e) {
            throw new JPSRuntimeException("FILTER clause cannot be parsed properly! " + e);
        }
        builder.addUnion(subquery);
        return builder.buildString();
    }
}
