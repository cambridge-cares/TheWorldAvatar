package uk.ac.cam.cares.jps.base.interfaces;

import org.apache.jena.query.Query;
import org.apache.jena.rdf.model.Model;
import org.json.JSONArray;
import uk.ac.cam.cares.jps.base.tools.cloning.CloningTool;

/**
 * This interface is to be implemented by store clients
 * that establish a connection and perform SPARQL queries and updates
 * on remote triple stores, owl files etc.
 *
 * @author Mehal Agarwal (ma988@cam.ac.uk)
 */

public interface TripleStoreClientInterface extends StoreClientInterface{

    /**
     * Get rdf content from store.
     * @param graphName (if any)
     * @param accept
     * @return String
     */
    String get(String graphName, String accept);

    /**
     * Insert rdf content into store.
     * @param graphName (if any)
     * @param content
     * @param contentType
     */
    void insert(String graphName, String content, String contentType);



    // SPARQL Construct query methods

    /**
     * Execute sparql construct query.
     * @param sparql
     * @return
     */
    Model executeConstruct(Query sparql);

    /**
     * Execute sparql construct query.
     * @param sparql
     * @return
     */
    Model executeConstruct(String sparql);

    /**
     * Can return the URL of the query EndPoint if available.
     */
    String getQueryEndpoint();

    /**
     * Sets the URL of the query EndPoint.
     * @param queryEndpoint
     */
    String setQueryEndpoint(String queryEndpoint);

    /**
     * Returns the URL of the update EndPoint if available.
     */
    String getUpdateEndpoint();

    /**
     * Set the URL of the update EndPoint.
     * @param updateEndpoint
     */
    String setUpdateEndpoint(String updateEndpoint);

    /**
     * Counts the total number of triples in the repository.
     * NOTE: this can be slow (of order of minutes) for large repositories.
     * @return
     */
    default Integer getTotalNumberOfTriples() {
        String query = "SELECT (COUNT(*) AS ?triples) WHERE { ?s ?p ?o . }";
        JSONArray results = executeQuery(query);
        int triples = Integer.parseInt(results.getJSONObject(0).get("triples").toString());
        return triples;
    }

    /**
     * Clone contents of <i>this</i> store to the target store.<br>
     * <b>Note:</b> cloning a large store is a slow process
     * and the target store must be empty. <br>Before initiating a clone see
     * {@link uk.ac.cam.cares.jps.base.tools.CloningTool CloningTool}
     * for more details.
     * @param targetStoreClient
     */
    default void cloneTo(TripleStoreClientInterface targetStoreClient) {
        CloningTool cloningTool = new CloningTool();
        cloningTool.clone(this, targetStoreClient);
    }

    /**
     * Clone contents of the source store to <i>this</i> store.<br>
     * <b>Note:</b> cloning a large store is a slow process
     * and <i>this</i> store must be empty. <br>Before initiating a clone see
     * {@link uk.ac.cam.cares.jps.base.tools.CloningTool CloningTool}
     * for more details.
     * @param sourceStoreClient
     */
    default void cloneFrom(TripleStoreClientInterface sourceStoreClient) {
        CloningTool cloningTool = new CloningTool();
        cloningTool.clone(sourceStoreClient, this);
    }
}
