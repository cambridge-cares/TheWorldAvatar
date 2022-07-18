package uk.ac.cam.cares.jps;

import org.apache.jena.arq.querybuilder.*;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.graph.Triple;
import org.apache.jena.sparql.lang.sparql_11.ParseException;
import org.apache.jena.update.UpdateRequest;
import org.json.JSONArray;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.config.JPSConstants;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;
import javax.ws.rs.HttpMethod;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.cam.cares.jps.base.query.AccessAgentCaller;

import java.util.*;

@WebServlet(urlPatterns = {BuildingMatchingAgent.URI_LISTEN})
public class BuildingMatchingAgent extends JPSAgent {

    //Agent endpoint and parameter keys
    public static final String URI_LISTEN = "/match";
    public static final String KEY_OCGML = "ocgml";
    public static final String KEY_OBE = "obe";
    public static final String KEY_NAMESPACE = "namespace";

    private static final Logger LOGGER = LogManager.getLogger(BuildingMatchingAgent.class);
    private static final String REQUEST_RECEIVED_MSG = "Request received.";

    //Prefix Iris
    private static final String ocgmlUri = "http://www.theworldavatar.com/ontology/ontocitygml/citieskg/OntoCityGML.owl#";;
    private static final String osidUri = "http://www.theworldavatar.com/ontology/ontocitygml/citieskg/OntoOSID.owl#";
    private static final String obeUri = "https://www.theworldavatar.com/kg/ontobuiltenv/";
    private static final String dabgeoUri = "http://www.purl.org/oema/infrastructure/";
    private static final String kbUri = "https://www.theworldavatar.com/kg/ontobuiltenv/";

    //Query labels
    private static final String BLDG = "bldg";
    private static final String uprn = "uprn";
    private static final String CITYOBJ = "cityobj";
    private static final String ATTR = "attr";
    private static final String QM ="?";
    private static final String FLAT = "flat";

    private static String targetResourceId_ocgml = null;
    private static String targetResourceId_obe = null;
    private static String bldgGraph = null;

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams, HttpServletRequest request) {
        return processRequestParameters(requestParams);
    }

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams){

        LOGGER.info(REQUEST_RECEIVED_MSG);
        validateInput(requestParams);
        requestParams.put("acceptHeaders", "application/json");

        MatchingAgentTask();
        return requestParams;
    }

    @Override
    public boolean validateInput(JSONObject requestParams) throws BadRequestException{
        if(!requestParams.isEmpty() && requestParams.get(JPSConstants.METHOD).equals(HttpMethod.PUT)){
            if(!requestParams.has(KEY_OCGML) || !requestParams.has(KEY_OBE) || !requestParams.has(KEY_NAMESPACE)){
                throw new BadRequestException("Missing request parameters");
            }
            else {
                targetResourceId_ocgml = requestParams.getString(KEY_OCGML);
                targetResourceId_obe = requestParams.getString(KEY_OBE);
                bldgGraph = requestParams.getString(KEY_NAMESPACE).endsWith("/")?  requestParams.getString(KEY_NAMESPACE)+"building/" :  requestParams.getString(KEY_NAMESPACE)+"/building/";
            }
            return true;
        }
        throw new BadRequestException("Request parameters not defined correctly");
    }

    public static void MatchingAgentTask() {

        SelectBuilder selectBuilder_ocgml = null;
        SelectBuilder selectBuilder_obe = null;
        try {
            selectBuilder_ocgml = ocgmlQueryBuilder();
            selectBuilder_obe = obeQueryBuilder();
        } catch (ParseException e) {
            e.printStackTrace();
        }

//        String ocgml_response = AccessAgentCaller.query(targetResourceId_ocgml, selectBuilder_ocgml.buildString());
//        JSONArray ocgml_result = new JSONArray(new JSONObject(ocgml_response).getString("result"));
        JSONArray ocgml_result = AccessAgentCaller.queryStore(targetResourceId_ocgml, selectBuilder_ocgml.buildString());
        HashMap<String, String> ontocitygml = createOcgmlMapping(ocgml_result);

//        String obe_response = AccessAgentCaller.query(targetResourceId_ocgml, selectBuilder_obe.buildString());
//        JSONArray obe_result = new JSONArray(new JSONObject(obe_response).getString("result"));
        JSONArray obe_result = AccessAgentCaller.queryStore(targetResourceId_obe, selectBuilder_obe.buildString());
        HashMap<String, ArrayList> ontobuilenv = createObeMapping(obe_result);

        UpdateBuilder insertion = new UpdateBuilder();

        for (String env_bldg : ontobuilenv.keySet()) {
            ArrayList<String> UPRNS = ontobuilenv.get(env_bldg);
            HashMap<String, Integer> results = new HashMap<>();
            String ocgml_bldg = "";
            if (UPRNS.size() > 1) {
                for (String UPRN : UPRNS) {
                    if (ontocitygml.keySet().contains(UPRN)) {
                        if (results.containsKey(ontocitygml.get(UPRN))) {
                            results.put(ontocitygml.get(UPRN), results.get(ontocitygml.get(UPRN)) + 1);
                        } else {
                            results.put(ontocitygml.get(UPRN), 1);
                        }
                    }
                }
                if (!results.isEmpty())
                    ocgml_bldg = Collections.max(results.entrySet(), Map.Entry.comparingByValue()).getKey();
            } else {
                if (ontocitygml.keySet().contains(UPRNS.get(0)))
                    ocgml_bldg = ontocitygml.get(UPRNS.get(0));
            }
            if (!ocgml_bldg.isEmpty()) {
                Triple triple = new Triple(NodeFactory.createURI(env_bldg), NodeFactory.createURI(kbUri + "hasOntoCityGMLRepresentation"), NodeFactory.createURI(ocgml_bldg));
                insertion.addInsert(triple);
            }
        }

        AccessAgentCaller.updateStore(targetResourceId_obe, new UpdateRequest().add(insertion.build()).toString());
    }

    private static SelectBuilder ocgmlQueryBuilder() throws ParseException {
        WhereBuilder where = new WhereBuilder().addPrefix("ocgml", ocgmlUri).addWhere("?bldg", "ocgml:objectClassId",  "26");

        SelectBuilder selectBuilder = new SelectBuilder().setDistinct(true)
                .addPrefix("ocgml", ocgmlUri).addPrefix("osid", osidUri)
                .addVar(QM+BLDG).addVar(QM+uprn)
                .addGraph(NodeFactory.createURI(bldgGraph), where)
                .addBind("IRI(REPLACE(str("+QM+BLDG+"), \"building\", \"cityobject\"))", QM+CITYOBJ)
                .addWhere(QM+ATTR, "osid:intersectsFeature" , QM+CITYOBJ)
                .addWhere(QM+ATTR, "osid:hasValue", QM+uprn).addOrderBy(QM+BLDG);
        return selectBuilder;
    }

    private static SelectBuilder obeQueryBuilder() throws ParseException {
        WhereBuilder inner_where = new WhereBuilder()
                .addPrefix("dabgeo", dabgeoUri).addPrefix("obe", obeUri)
                .addWhere(QM+BLDG, "a", "dabgeo:Building")
                .addFilter("EXISTS { "+QM+BLDG+"^obe:isIn "+QM+FLAT+" }")
                .addWhere(QM+FLAT, "obe:hasIdentifier", QM+uprn);

        WhereBuilder where = new WhereBuilder()
                .addPrefix("dabgeo", dabgeoUri).addPrefix("obe", obeUri)
                .addWhere(QM+BLDG, "a", "dabgeo:Building")
                .addFilter("NOT EXISTS {"+QM+BLDG+" ^obe:isIn "+QM+FLAT+" }")
                .addWhere(QM+BLDG, "obe:hasIdentifier", QM+uprn)
                .addUnion(inner_where);

        SelectBuilder selectBuilder = new SelectBuilder()
                .addPrefix("obe", obeUri).addPrefix("dabgeo", dabgeoUri).addPrefix("kb", kbUri)
                .addVar(QM+BLDG).addVar(QM+uprn)
                .addWhere(where);

        return selectBuilder;
    }

    private static HashMap<String, String> createOcgmlMapping(JSONArray ocgml_result){
        HashMap<String, String> ontocitygml = new HashMap<>();
        for (int i = 0; i < ocgml_result.length(); i++) {
            String bldg = ocgml_result.getJSONObject(i).getString(BLDG);
            String UPRN = ocgml_result.getJSONObject(i).getString(uprn);
            ontocitygml.put(UPRN, bldg);
        }
        return ontocitygml;
    }

    private static  HashMap<String, ArrayList> createObeMapping(JSONArray obe_result){

        HashMap<String, ArrayList> ontobuilenv = new HashMap<>();
        ArrayList<String> UPRNS_2 = new ArrayList<>();

        String subject_bldg = obe_result.getJSONObject(0).getString(BLDG);
        UPRNS_2.add(obe_result.getJSONObject(0).getString(uprn));
        for (int i = 1; i < obe_result.length(); i++) {
            String next_subject_bldg = obe_result.getJSONObject(i).getString(BLDG);
            if (next_subject_bldg.equals(subject_bldg)) {
                UPRNS_2.add(obe_result.getJSONObject(i).getString(uprn));
            }
            else {
                ontobuilenv.put(subject_bldg, UPRNS_2);
                UPRNS_2 = new ArrayList<>();
                subject_bldg = next_subject_bldg;
                UPRNS_2.add(obe_result.getJSONObject(i).getString(uprn));
            }
        }
        ontobuilenv.put(subject_bldg, UPRNS_2);
        return ontobuilenv;
    }
}
