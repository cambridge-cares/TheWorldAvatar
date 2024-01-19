package uk.ac.cam.cares.jps.bio;

import org.json.JSONArray;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.query.AccessAgentCaller;
import uk.ac.cam.cares.jps.bio.json.parser.JSonRequestParser;

import java.sql.SQLException;

public class CrossDomainQuery extends JSONObject{

    // Variables to be used in the queries
    static String bioIRIs                              = " ?IRIs";
    static String cost                                 = " ?cost";


    /**
    * This part is the main method that takes input from CrossDomainQueryAgent and passes them in
    * the below mentioned queries. The geospatial query returns the list of IRIs of all the items
    * present within a bounding box; the chemical engineering part of the query returns the costs
    * and the IRIs after a filter based on the cost of the items. Then we compare these two lists
    * to return a final list that contains IRIs and costs of equipments within a specified region
    * and have a cost above the prescribed limit.
    */

    public static JSONObject performCrossDomainQuery(JSONObject jsonObject) throws SQLException {

        JSONArray result = new JSONArray();
        JSONObject result1 = new JSONObject();

        String upperBounds                          = JSonRequestParser.getUPPER_LIMITS(jsonObject.toString());
        String lowerBounds                          = JSonRequestParser.getLOWER_LIMITS(jsonObject.toString());
        String equipmentCost                        = JSonRequestParser.getEQUIP_COST(jsonObject.toString());

        String georesult              = AccessAgentCaller.query("singaporeEPSG24500", buildGeoSpatialQuery(lowerBounds, upperBounds));
        JSONObject georesultjson = new JSONObject(georesult);
        String geoSpatialResultString = georesultjson.getString("result");
        JSONArray geoSpatialQueryResult = new JSONArray(geoSpatialResultString);

        for (int i = 0; i < geoSpatialQueryResult.length(); i++) {
            JSONObject geoS = geoSpatialQueryResult.getJSONObject(i);
            String geoIRIs = geoS.getString("IRIs");

            String chemresult              = AccessAgentCaller.query("sgbiodieselplants", buildChemEngQuery(equipmentCost, bioIRIs));
            JSONObject chemresultjson = new JSONObject(chemresult);
            String chemEngQueryResultString = chemresultjson.getString("result");
            JSONArray chemEngQueryResult = new JSONArray(chemEngQueryResultString);


            for (int j = 0; j< chemEngQueryResult.length(); j++) {
                JSONObject chemS = chemEngQueryResult.getJSONObject(j);
                String chemIRIs = chemS.getString("IRIs");
                if (chemIRIs.equals(geoIRIs)) {
                    System.out.println(chemIRIs);
                    String cost = chemS.getString("cost");
                    System.out.println(cost);
                    JSONObject row = new JSONObject();
                    row.put("iri", chemIRIs);
                    row.put("cost", cost);
                    result.put(row);
                }
            }
        }
        result1.put("result", result);

        return result1;
    }


 /*   JSON Object example of the input to the agent
       // {"job":{"lower_bounds":"10428#26648#0#10428#26648#0#10428#26648#0#10428#26648#0#10428#26648#0","upper_bounds":"10880#26955#10#10880#26955#10#10880#26955#10#10880#26955#10#10880#26955#10","equip_cost":"30000"}};
    }*/


    // Geospatial part of the query
    private static String buildGeoSpatialQuery (String lowerBounds, String upperBounds){

        StringBuffer geosSpatialQuery = new StringBuffer("PREFIX ocgml: <http://www.theworldavatar.com/ontology/ontocitygml/citieskg/OntoCityGML.owl#>\n");
        geosSpatialQuery.append("PREFIX bio: <http://www.jparksimulator.com/kb/sgp/jurongisland/biodieselplant3/>\n");
        geosSpatialQuery.append("PREFIX geo:<http://www.bigdata.com/rdf/geospatial#>\n");
        geosSpatialQuery.append("PREFIX twa:<http://www.theworldavatar.com:83/citieskg/namespaces/singaporeEPSG24500/sparql/cityobject/>\n");
        geosSpatialQuery.append("SELECT ?gmlid").append(bioIRIs).append("?ObjectId\n");
        geosSpatialQuery.append("WHERE {\n");
        geosSpatialQuery.append("GRAPH <http://www.theworldavatar.com:83/citieskg/namespace/singaporeEPSG24500/sparql/cityobjectgenericattrib/>\n");
        geosSpatialQuery.append("{?s ocgml:attrName 'BioDiesel_Power_Plant_IRI';\n");
        geosSpatialQuery.append("ocgml:uriVal ?IRIs;\n");
        geosSpatialQuery.append("ocgml:cityObjectId ?ObjectId.}\n");
        geosSpatialQuery.append("BIND(STRAFTER(SUBSTR(STR(?ObjectId),STRLEN('http://www.theworldavatar.com:83/citieskg/namespace/singaporeEPSG24500/sparql/cityobject/')),\"/\") AS ?gmlId )\n");
        geosSpatialQuery.append("BIND(STRBEFORE(?gmlId , \"/\") AS ?gmlid)\n");
        geosSpatialQuery.append("GRAPH <http://www.theworldavatar.com:83/citieskg/namespace/singaporeEPSG24500/sparql/cityobject/>\n");
        geosSpatialQuery.append("{?cityObject ocgml:gmlId ?gmlid;\n");
        geosSpatialQuery.append("ocgml:EnvelopeType ?envelopes.\n");
        geosSpatialQuery.append("SERVICE geo:search {\n");
        geosSpatialQuery.append("?cityObject geo:predicate ocgml:EnvelopeType.\n");
        geosSpatialQuery.append("?cityObject geo:searchDatatype <http://localhost/blazegraph/literals/POLYGON-3-15>.\n");
        geosSpatialQuery.append("?cityObject geo:customFields \"X0#Y0#Z0#X1#Y1#Z1#X2#Y2#Z2#X3#Y3#Z3#X4#Y4#Z4\".\n");
        geosSpatialQuery.append("?cityObject geo:customFieldsLowerBounds \"").append(lowerBounds).append("\".\n");
        geosSpatialQuery.append("?cityObject geo:customFieldsUpperBounds \"").append(upperBounds).append("\".}\n");
        geosSpatialQuery.append("}}");

        return geosSpatialQuery.toString();
    }

    //Chemical Engineering part of the query
    private static String buildChemEngQuery (String equipmentCost, String bioIRIs){

        StringBuffer chemEngQuery = new StringBuffer("PREFIX cost:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>\n");
        chemEngQuery.append("PREFIX iri:<http://www.theworldavatar.com/ontology/ontoeip/ecoindustrialpark/EcoIndustrialPark.owl#>\n");
        chemEngQuery.append("SELECT").append(bioIRIs).append(cost).append("\n");
        chemEngQuery.append("WHERE{\n");
        chemEngQuery.append("{?s iri:hasIRI ?iri;\n");
        chemEngQuery.append("FILTER(CONTAINS(?iri, 'http://www.jparksimulator.com/kb/sgp')).\n");
        chemEngQuery.append("BIND(STRAFTER(SUBSTR(STR(?iri),STRLEN('http://www.jparksimulator.com/kb/sgp/jurongisland/biodieselplant3/')),\"/\") AS ?iri1).\n");
        chemEngQuery.append("BIND(STRBEFORE(?iri1, '.owl') AS ?iri2).\n");
        chemEngQuery.append("BIND(CONCAT(?iri, '#') AS ?iri3).\n");
        chemEngQuery.append("BIND(IRI(CONCAT(?iri3, ?iri2)) AS").append(bioIRIs).append(").\n");
        chemEngQuery.append("BIND(CONCAT('ValueOf_EquipmentCost_', ?iri2) AS ?EquipCost1).\n");
        chemEngQuery.append("BIND(IRI(CONCAT(?iri3, ?EquipCost1)) AS ?EquipCost).}");
        chemEngQuery.append("{?EquipCost cost:numericalValue").append(cost).append(".\n");
        chemEngQuery.append("FILTER(").append(cost).append(">").append(equipmentCost).append(").}}");

        return chemEngQuery.toString();
    }



    //Variables that can be used later in the agent
    /*
    These lines are commented for now and will be used later when the queries are later replaced by Query Builder

    private static final String OCGML                   = "http://www.theworldavatar.com/ontology/ontocitygml/citieskg/OntoCityGML.owl#";
    private static final String BIO                     = "http://www.jparksimulator.com/kb/sgp/jurongisland/biodieselplant3/";
    private static final String GEO                     = "http://www.bigdata.com/rdf/geospatial#";
    private static final String TWA                     = "http://www.theworldavatar.com:83/citieskg/namespaces/singaporeEPSG24500/sparql/cityobject/";
    private static final String GEO_GRAPH               = "http://www.theworldavatar.com:83/citieskg/namespace/singaporeEPSG24500/sparql/cityobjectgenericattrib/";
    */


        /**
         *
         * Replacing the queries using Query Builder
        String upperBounds              = JSonRequestParser.getUPPER_LIMITS(jsonObject.toString());
        String lowerBounds              = JSonRequestParser.getLOWER_LIMITS(jsonObject.toString());
        String equipmentCost            = JSonRequestParser.getEQUIP_COST(jsonObject.toString());

        SelectBuilder Select_Geospatial = new SelectBuilder();
                Select_Geospatial.addPrefix("ocgml", OCGML)
                                 .addPrefix("bio", BIO)
                                 .addPrefix("twa", TWA)
                                 .addPrefix("geo", GEO)
                                 .addVar("?gmlID")
                                 .addVar("?IRIs")
                                 .addVar("?ObjectId");

        ExprFactory exprFactory         = new ExprFactory();

        WhereBuilder Where_Geospatial   = new WhereBuilder();
        Where_Geospatial.addGraph(GEO_GRAPH)

        */

}
