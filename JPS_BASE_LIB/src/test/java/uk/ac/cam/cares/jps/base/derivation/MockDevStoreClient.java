package uk.ac.cam.cares.jps.base.derivation;

import org.apache.jena.ontology.OntModel;
import org.apache.jena.query.Query;
import org.apache.jena.query.QueryExecution;
import org.apache.jena.query.QueryExecutionFactory;
import org.apache.jena.query.QueryFactory;
import org.apache.jena.query.ResultSet;
import org.apache.jena.update.UpdateAction;
import org.apache.jena.update.UpdateRequest;
import org.json.JSONArray;

import uk.ac.cam.cares.jps.base.query.JenaResultSetFormatter;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

public class MockDevStoreClient extends RemoteStoreClient {
    private final OntModel kb;
    private String query;

    MockDevStoreClient(OntModel kb) {
        this.kb = kb;
    }

    public OntModel getKnowledgeBase() {
        return kb;
    }

    public void closeKnowledgeBase() {
        if (!(kb == null)) {
            kb.close();
        }
    }

    @Override
    public JSONArray executeQuery(String query) {
        return new JSONArray(execute(query));
    }

    @Override
    public JSONArray executeQuery() {
        return executeQuery(query);
    }

    @Override
    public String execute() {
        return execute(query);
    }

    @Override
    public String execute(String sparql) {
        Query query = QueryFactory.create(sparql);
        QueryExecution queryExec = QueryExecutionFactory.create(query, kb);
        ResultSet rs = queryExec.execSelect();
        return JenaResultSetFormatter.convertToSimplifiedList(rs).getJSONArray("results").toString();
    }

    @Override
    public int executeUpdate() {
        return executeUpdate(query);
    }

    @Override
    public int executeUpdate(String update) {
        UpdateAction.parseExecute(update, kb);
        return 0;
    }

    @Override
    public int executeUpdate(UpdateRequest update) {
        return executeUpdate(update.toString());
    }

    @Override
    public String setQuery(String query) {
        this.query = query;
        return this.query;
    }
}
