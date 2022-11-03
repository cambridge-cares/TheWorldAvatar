package uk.ac.cam.cares.jps.base.query;

import org.apache.jena.arq.querybuilder.ExprFactory;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.sparql.expr.Expr;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.cache.LRUCache;
import uk.ac.cam.cares.jps.base.config.IKeys;
import uk.ac.cam.cares.jps.base.config.KeyValueMap;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.interfaces.TripleStoreClientInterface;
import uk.ac.cam.cares.jps.base.router.AbstractCachedRouter;
import uk.ac.cam.cares.jps.base.util.InputValidator;
import uk.ac.cam.cares.jps.base.util.MiscUtil;

public class RDBStoreRouter extends AbstractCachedRouter<String, String> {

    private static Logger LOGGER = LogManager.getLogger(RDBStoreRouter.class);
    public static final String RDFS_PREFIX = "rdfs";
    public static final String RDFS = "http://www.w3.org/2000/01/rdf-schema#";
    public static final String RDF_PREFIX = "rdf";
    public static final String RDF = "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
    public static final String RDF_TYPE = "type";
    public static final String ONTORDBROUTER_PREFIX = "ontordbrouter";
    public static final String ONTORDBROUTER = "http://www.theworldavatar.com/kg/ontordbrouter/";
    public static final String RESOURCE = "resource";
    public static final String LABEL = "label";
    public static final String COLON = ":";
    public static final String QUESTION_MARK = "?";
    public static final String SLASH="/";
    public static final String TARGET_RDB_RESOURCE = "TargetRDBResource";
    public static final String URL = "url";
    public static final String HAS_URL = "hasUrl";
    public static String RDBStoreRouterEndpoint;
    public static final String RDB_STOREROUTER_ENDPOINT_NAME = "RDB_STOREROUTER_ENDPOINT";

    static{
        RDBStoreRouterEndpoint = System.getenv(RDB_STOREROUTER_ENDPOINT_NAME);
        if(RDBStoreRouterEndpoint == null) {
            // if endpoint is not set in the system environment
            // then get the default value from jps.properties
            LOGGER.info("RDB_STOREROUTER_ENDPOINT not found in environment variables..."
                    + " Using jps.properties.");
            RDBStoreRouterEndpoint = KeyValueMap.getInstance().get(IKeys.URL_RDB_STOREROUTER_ENDPOINT);
        }
        LOGGER.info("RDB STOREROUTER_ENDPOINT set to "+RDBStoreRouterEndpoint);
    }

    /*
     * LRU Cache configuration:
     * key=label, value=rdbUrl
     */
    private static final int CACHE_SIZE = Integer.parseInt(KeyValueMap.getInstance().get(IKeys.RDB_STOREROUTER_CACHE_SIZE));

    static RDBStoreRouter rdbStoreRouter = null;

    /**
     * RDBStoreRouter singleton
     */
    private RDBStoreRouter() {
        super(new LRUCache<String, String>(CACHE_SIZE));
    }

    public static synchronized RDBStoreRouter getInstance() {
        if (rdbStoreRouter == null) {
            rdbStoreRouter = new RDBStoreRouter();
        }
        return rdbStoreRouter;
    }


    public static String getRDBUrl(String targetRDBResourceID){

        String rdbUrl = null;
        if (targetRDBResourceID != null && !targetRDBResourceID.isEmpty()) {
            //instantiate singleton if not already done so
            getInstance();

           if(isRemoteTargetResourceID(targetRDBResourceID)){

                String targetResourceLabel = getLabelFromTargetResourceID(targetRDBResourceID);
                LOGGER.info("Remote store. targetResourceLabel="+targetResourceLabel);

                rdbUrl = rdbStoreRouter.get(targetResourceLabel);

                if(rdbUrl==null){
                    LOGGER.error("Url could not be retrieved for the following resource IRI:"+targetRDBResourceID+", label:"+targetResourceLabel);
                }

            }else {
                throw new JPSRuntimeException("Invalid targetResourceID: "+targetRDBResourceID);
            }
        }else {
            LOGGER.error("targetResourceID is null.");
        }
        return rdbUrl;
    }

    @Override
    public String getFromStore(String targetResourceLabel, TripleStoreClientInterface storeClient){

        ExprFactory exprFactory = new ExprFactory();
        Expr exprRegex = exprFactory.regex(exprFactory.str( QUESTION_MARK.concat(LABEL)), targetResourceLabel, "");

        SelectBuilder builder = new SelectBuilder()
                .addPrefix( RDFS_PREFIX,  RDFS )
                .addPrefix( RDF_PREFIX,  RDF )
                .addPrefix( ONTORDBROUTER_PREFIX,  ONTORDBROUTER )
                .addVar( QUESTION_MARK.concat(URL) )
                .addWhere( QUESTION_MARK.concat(RESOURCE), RDF_PREFIX.concat(COLON).concat(RDF_TYPE), ONTORDBROUTER_PREFIX.concat(COLON).concat(TARGET_RDB_RESOURCE) )
                .addOptional( QUESTION_MARK.concat(RESOURCE), ONTORDBROUTER_PREFIX.concat(COLON).concat(HAS_URL), QUESTION_MARK.concat(URL) )
                .addWhere( QUESTION_MARK.concat(RESOURCE), RDFS_PREFIX.concat(COLON).concat(LABEL), QUESTION_MARK.concat(LABEL))
                .addFilter(exprRegex);
        JSONArray results = storeClient.executeQuery(builder.toString());

        if(!results.isEmpty()) {
            JSONObject obj = results.getJSONObject(0);
            String url = MiscUtil.optNullKey(obj, URL);
            return url;

        }else {
            LOGGER.error("URL not found for resource="+targetResourceLabel);
            return null;
        }

    }

    public static String getLabelFromTargetResourceID(String targetResourceID) {
        return targetResourceID.substring(targetResourceID.lastIndexOf(SLASH)+1).trim();
    }

    public static boolean isRemoteTargetResourceID(String targetRDBResourceID) {
        if(InputValidator.checkIfValidIRI(targetRDBResourceID)){
            return true;
        }else {
            if(targetRDBResourceID.matches("[A-Za-z0-9\\-\\_]+")) {
                return true;
            }else {
                LOGGER.error("Invalid namespace label:"+targetRDBResourceID+". Not alphanumeric (special characters - and _ are allowed).");
                return false;
            }
        }
    }

    /**
     * Get store client for ontordbrouter
     */
    @Override
    public TripleStoreClientInterface getRouterStoreClient() {
        return new RemoteStoreClient(RDBStoreRouterEndpoint);
    }

}
