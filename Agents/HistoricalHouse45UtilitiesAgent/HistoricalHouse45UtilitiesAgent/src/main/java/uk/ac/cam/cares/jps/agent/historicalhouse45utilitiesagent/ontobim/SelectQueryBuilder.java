package uk.ac.cam.cares.jps.agent.historicalhouse45utilitiesagent.ontobim;

import org.apache.jena.arq.querybuilder.SelectBuilder;

class SelectQueryBuilder {
    protected static final String SUBJECT_VAR = "?subject";
    protected static final String TIMESERIES_PREFIX = "ontotimeseries";
    private static final String TIMESERIES_HAS_TS = TIMESERIES_PREFIX + ":hasTimeSeries";

    /**
     * Generate the required SELECT query for retrieving their instances.
     *
     * @return The query in String format.
     */
    protected static String genSelectQuery() {
        SelectBuilder builder = new SelectBuilder();
        QueryHandler.genPrefixMapping(builder);
        builder.addVar(SUBJECT_VAR)
                .addWhere(SUBJECT_VAR, TIMESERIES_HAS_TS, "?timeseries");
        return builder.buildString();
    }
}
