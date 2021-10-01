package uk.ac.cam.cares.jps.bio;

import com.sun.jna.StringArray;
import org.json.JSONArray;
import org.json.JSONObject;
import org.locationtech.jts.awt.PointShapeFactory;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.bio.json.parser.JSonRequestParser;

import java.sql.SQLException;

public class CrossDomainQuery extends JSONObject{
    //Variables that can be used later in the agent
    private static final String OCGML           = "http://www.theworldavatar.com/ontology/ontocitygml/citieskg/OntoCityGML.owl#";
    private static final String BIO             = "http://www.jparksimulator.com/kb/sgp/jurongisland/biodieselplant3/";
    private static final String GEO             = "http://www.bigdata.com/rdf/geospatial#";
    private static final String TWA             = "http://www.theworldavatar.com:83/citieskg/namespaces/singaporeEPSG24500/sparql/cityobject/";
    private static final String GEO_GRAPH       = "http://www.theworldavatar.com:83/citieskg/namespace/singaporeEPSG24500/sparql/cityobjectgenericattrib/";

    String queryEndpointGeo                     = "http://www.theworldavatar.com:83/citieskg/namespaces/singaporeEPSG24500/sparql";
    String queryEndpointChem                    = "http://www.theworldavatar.com/blazegraph/sgbiodieselplants/sparql";

    String bioIRIs                              = " ?IRIs";
    String cost                                 = " ?cost";




    public JSONObject performCrossDomainQuery(JSONObject jsonObject) throws SQLException {

        String upperBounds                          = JSonRequestParser.getUPPER_LIMITS(jsonObject.toString());
        String lowerBounds                          = JSonRequestParser.getLOWER_LIMITS(jsonObject.toString());
        String equipmentCost                        = JSonRequestParser.getEQUIP_COST(jsonObject.toString());

        JSONArray geoSpatialQueryResult             = runQuery(buildGeoSpatialQuery(lowerBounds,upperBounds), queryEndpointGeo);

        return null;
    }

    public static  void main(String[] args) throws SQLException {
        CrossDomainQuery crossDomainQuery = new CrossDomainQuery();
        JSONObject jsonObject = new JSONObject();
        JSONObject bounds = new JSONObject();
        bounds.put("lower_bounds","10428#26648#0#10428#26648#0#10428#26648#0#10428#26648#0#10428#26648#0");
        bounds.put("upper_bounds","10880#26955#10#10880#26955#10#10880#26955#10#10880#26955#10#10880#26955#10");
        bounds.put("equip_cost","30000");
        jsonObject.put("job",bounds);


       // {"job":{"lower_bounds":"10428#26648#0#10428#26648#0#10428#26648#0#10428#26648#0#10428#26648#0","upper_bounds":"10880#26955#10#10880#26955#10#10880#26955#10#10880#26955#10#10880#26955#10","equip_cost":"30000"}};
        crossDomainQuery.performCrossDomainQuery(jsonObject);
    }

    private String buildGeoSpatialQuery (String lowerBounds, String upperBounds){
        // Geospatial part of the query


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
    private String buildChemEngQuery (String equipmentCost, String bioIRIs){


        StringBuffer chemEngQuery = new StringBuffer("PREFIX cost:<http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#>\n");
        chemEngQuery.append("SELECT").append(bioIRIs).append(cost).append("\n");
        chemEngQuery.append("WHERE{\n");
        chemEngQuery.append("{BIND(STRBEFORE(").append(bioIRIs).append(", '#') AS ?iri4).\n");
        chemEngQuery.append("BIND(CONCAT(?iri4, '#') AS ?iri5).\n");
        chemEngQuery.append("BIND(STRAFTER(?IRIs, '#') AS ?iri6).\n");
        chemEngQuery.append("BIND(CONCAT('ValueOf_EquipmentCost_', ?iri6) AS ?EquipCost1).\n");
        chemEngQuery.append("BIND(IRI(CONCAT(?iri5, ?EquipCost1)) AS ?EquipCost).}\n");
        chemEngQuery.append("{?EquipCost cost:numericalValue ?Cost.\n");
        chemEngQuery.append("FILTER(").append(cost).append(">").append(equipmentCost).append(").}}");

        return chemEngQuery.toString();
    }

    public JSONArray runQuery(String query,String queryEndpoint) throws SQLException{
        RemoteStoreClient kbClient = new RemoteStoreClient(queryEndpoint);
        JSONArray queryResult         = kbClient.executeQuery(query);

        return queryResult;
    }




        /**
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
