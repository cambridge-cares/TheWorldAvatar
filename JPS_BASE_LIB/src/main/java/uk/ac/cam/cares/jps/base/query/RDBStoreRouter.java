package uk.ac.cam.cares.jps.base.query;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.config.IKeys;
import uk.ac.cam.cares.jps.base.config.KeyValueMap;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.util.InputValidator;

public class RDBStoreRouter {

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

    static RDBStoreRouter rdbStoreRouter = null;

    public static String getRDBUrl(String targetRDBResourceID){

        String rdbUrl = null;
        if (targetRDBResourceID != null && !targetRDBResourceID.isEmpty()) {
            if (rdbStoreRouter == null) {
                rdbStoreRouter = new RDBStoreRouter();
            }
           if(isRemoteTargetResourceID(targetRDBResourceID)){

                String targetResourceLabel = getLabelFromTargetResourceID(targetRDBResourceID);
                LOGGER.info("Remote store. targetResourceLabel="+targetResourceLabel);

                rdbUrl = queryRDBUrl(targetResourceLabel);

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

    public static String queryRDBUrl(String targetResourceName){
        SelectBuilder builder = new SelectBuilder()
                .addPrefix( RDFS_PREFIX,  RDFS )
                .addPrefix( RDF_PREFIX,  RDF )
                .addPrefix( ONTORDBROUTER_PREFIX,  ONTORDBROUTER )
                .addVar( QUESTION_MARK.concat(RESOURCE))
                .addVar( QUESTION_MARK.concat(LABEL) )
                .addVar( QUESTION_MARK.concat(URL) )
                .addWhere(getRDBRouterWhereBuilder())
                .addWhere( QUESTION_MARK.concat(RESOURCE), ONTORDBROUTER_PREFIX.concat(COLON).concat(HAS_URL), QUESTION_MARK.concat(URL) );
        RemoteStoreClient rKBClient = new RemoteStoreClient(RDBStoreRouterEndpoint);
        System.out.println(builder.toString());
        String json = rKBClient.execute(builder.toString());
        JSONArray jsonArray = new JSONArray(json);
        for (int i = 0; i<jsonArray.length(); i++){
            JSONObject obj = jsonArray.getJSONObject(i);
            if(obj.getString(LABEL).equals(targetResourceName)){
                System.out.println(obj.get(URL));
                return obj.getString(URL);
            }
        }
        return null;
    }
    private static WhereBuilder getRDBRouterWhereBuilder(){
        return new WhereBuilder()
                .addPrefix( RDFS_PREFIX,  RDFS )
                .addPrefix( RDF_PREFIX,  RDF )
                .addPrefix( ONTORDBROUTER_PREFIX,  ONTORDBROUTER )
                .addWhere( QUESTION_MARK.concat(RESOURCE), RDF_PREFIX.concat(COLON).concat(RDF_TYPE), ONTORDBROUTER_PREFIX.concat(COLON).concat(TARGET_RDB_RESOURCE) )
                .addWhere( QUESTION_MARK.concat(RESOURCE), RDFS_PREFIX.concat(COLON).concat(LABEL), QUESTION_MARK.concat(LABEL) );
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

}
