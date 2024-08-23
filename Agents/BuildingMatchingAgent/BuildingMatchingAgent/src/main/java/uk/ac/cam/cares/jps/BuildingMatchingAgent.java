package uk.ac.cam.cares.jps;

import org.apache.jena.arq.querybuilder.*;
import org.apache.jena.datatypes.BaseDatatype;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.graph.Triple;
import org.apache.jena.sparql.lang.sparql_11.ParseException;
import org.apache.jena.update.UpdateRequest;
import org.cts.CRSFactory;
import org.cts.IllegalCoordinateException;
import org.cts.crs.CRSException;
import org.cts.crs.CoordinateReferenceSystem;
import org.cts.crs.GeodeticCRS;
import org.cts.op.CoordinateOperation;
import org.cts.op.CoordinateOperationException;
import org.cts.op.CoordinateOperationFactory;
import org.cts.registry.EPSGRegistry;
import org.cts.registry.RegistryManager;
import org.json.JSONArray;
import org.json.JSONObject;
import org.locationtech.jts.geom.*;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.config.JPSConstants;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.ws.rs.BadRequestException;
import javax.ws.rs.HttpMethod;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.AccessAgentCaller;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import java.util.*;

@WebServlet(urlPatterns = {BuildingMatchingAgent.URI_LISTEN})
public class BuildingMatchingAgent extends JPSAgent {

    //Agent endpoint and parameter keys
    public static final String URI_LISTEN = "/match";
    public static final String KEY_OCGML = "ocgml";
    public static final String KEY_OBE = "obe";
    public static final String KEY_OSID = "osid";
    public static final String KEY_PREFIXIRI = "prefixIRI";

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
    private static final String QM ="?";
    private static final String FLAT = "flat";
    private static final String SURF_GEOM = "surf_geom";
    private static final String SURFACE_GEOMETRY = "surfaceGeometry";
    private static final String GEOMETRY_TYPE = "geometryType";
    private static final String DATATYPE = "datatype";

    private static final String VALUE_DELIMITER = "#";
    private static final String STRUCTURE_PREFIX = "POLYGON-";
    private static final String STRUCTURE_DELIMITER = "-";

    protected static final GeometryFactory factory = new GeometryFactory();

    private static String targetResourceId_ocgml = null;
    private static String targetResourceId_obe = null;
    private static String bldgGraph = null;
    private static String surfGeomGraph = null;
    private static String identifiersGraph = null;

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
            if(!requestParams.has(KEY_OCGML) || !requestParams.has(KEY_OBE) || !requestParams.has(KEY_PREFIXIRI)){
                throw new BadRequestException("Missing request parameters");
            }
            else {
                targetResourceId_ocgml = requestParams.getString(KEY_OCGML);
                targetResourceId_obe = requestParams.getString(KEY_OBE);
                bldgGraph = requestParams.getString(KEY_PREFIXIRI).endsWith("/")?  requestParams.getString(KEY_PREFIXIRI)+"building/" :  requestParams.getString(KEY_PREFIXIRI)+"/building/";
                surfGeomGraph = requestParams.getString(KEY_PREFIXIRI).endsWith("/")?  requestParams.getString(KEY_PREFIXIRI)+"surfacegeometry/" :  requestParams.getString(KEY_PREFIXIRI)+"/surfacegeometry/";
                identifiersGraph = requestParams.getString(KEY_PREFIXIRI).endsWith("/")?  requestParams.getString(KEY_PREFIXIRI)+"identifiers" :  requestParams.getString(KEY_PREFIXIRI)+"/identifiers";
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

        RemoteStoreClient ocgml_client = new RemoteStoreClient(targetResourceId_ocgml);
//        JSONArray ocgml_result = AccessAgentCaller.queryStore(targetResourceId_ocgml, selectBuilder_ocgml.buildString());
        JSONArray ocgml_result = ocgml_client.executeQuery(selectBuilder_ocgml.buildString());
        HashMap<String, String> ontocitygml = createOcgmlMapping(ocgml_result);

        RemoteStoreClient obe_client = new RemoteStoreClient(targetResourceId_obe, targetResourceId_obe);
//        JSONArray obe_result = AccessAgentCaller.queryStore(targetResourceId_obe, selectBuilder_obe.buildString());
        JSONArray obe_result = obe_client.executeQuery(selectBuilder_obe.buildString());
        HashMap<String, ArrayList> ontobuilenv = createObeMapping(obe_result);

        UpdateBuilder insertion = new UpdateBuilder();
        UpdateBuilder insertion_centroid = new UpdateBuilder();
        Coordinate lat_lon;
        Coordinate centroid;

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
                try {
                    SelectBuilder geom_query = getGeomQuery(ocgml_bldg);
//                    JSONArray geom_result = AccessAgentCaller.queryStore(targetResourceId_ocgml, geom_query.buildString());
                    JSONArray geom_result = ocgml_client.executeQuery(geom_query.buildString());
                    if (geom_result.length()>1){
                        Double xsum = 0.0;
                        Double ysum = 0.0;
                        Double zsum = 0.0;
                        Double asum=0.0;
                        for (int i = 0; i < geom_result.length(); i++) {
                            String data = geom_result.getJSONObject(i).getString(GEOMETRY_TYPE);
                            String datatypeIri = geom_result.getJSONObject(i).getString(DATATYPE);

                            Polygon polygon = getPolygon(data, datatypeIri);
                            centroid = computeCentroid(polygon.getExteriorRing().getCoordinates(), true);
                            Double area = polygon.getArea();
                            xsum += centroid.x * area;
                            ysum += centroid.y *area;
                            zsum += centroid.z *area;
                            asum += area;
                        }
//                        lat_lon = new Coordinate(xsum/asum, ysum/asum, zsum/asum);
                        lat_lon = transformEPSG27700ToWGS84(new Coordinate(xsum/asum, ysum/asum, zsum/asum));
                    }
                    else {
                        String data = geom_result.getJSONObject(0).getString(GEOMETRY_TYPE);
                        String datatypeIri = geom_result.getJSONObject(0).getString(DATATYPE);
                        Polygon polygon = getPolygon(data, datatypeIri);
                        centroid = computeCentroid(polygon.getExteriorRing().getCoordinates(), true);
//                        lat_lon = centroid;
                        lat_lon = transformEPSG27700ToWGS84(centroid);
                    }
                    StringBuilder value = new StringBuilder();
                    for (int i = 1; i >= 0; i--)
                        value.append("#").append(lat_lon.getOrdinate(i));
                    value.deleteCharAt(0);
                    triple = new Triple(NodeFactory.createURI(env_bldg), NodeFactory.createURI(kbUri + "hasWGS84LatitudeLongitude"), NodeFactory.createLiteral(value.toString(), new BaseDatatype("http://www.bigdata.com/rdf/geospatial/literals/v1#lat-lon")));
                    insertion_centroid.addInsert(triple);
                } catch (ParseException | CRSException | CoordinateOperationException | IllegalCoordinateException e) {
                    e.printStackTrace();
                }
            }
        }

        obe_client.executeUpdate(new UpdateRequest().add(insertion.build()).toString());
        obe_client.executeUpdate(new UpdateRequest().add(insertion_centroid.build()).toString());
    }

    private static SelectBuilder ocgmlQueryBuilder() throws ParseException {
        WhereBuilder where = new WhereBuilder().addPrefix(KEY_OSID, osidUri).addWhere(QM+CITYOBJ, "^"+KEY_OSID+":intersectsFeature/"+KEY_OSID+":hasValue",  QM+uprn);

        return   new SelectBuilder().setDistinct(true)
                .addPrefix(KEY_OCGML, ocgmlUri)
                .addVar(QM+BLDG).addVar(QM+uprn)
                .addGraph(NodeFactory.createURI(identifiersGraph), where)
                .addBind("IRI(REPLACE(str("+QM+CITYOBJ+"), \"cityobject\", \"building\"))", QM+BLDG)
                .addGraph(NodeFactory.createURI(bldgGraph), QM+BLDG, KEY_OCGML+":objectClassId", "26")
                .addOrderBy(QM+BLDG);

    }

    private static SelectBuilder obeQueryBuilder() throws ParseException {
        WhereBuilder inner_where = new WhereBuilder()
                .addPrefix("dabgeo", dabgeoUri).addPrefix(KEY_OBE, obeUri)
                .addWhere(QM+BLDG, "a", "dabgeo:Building")
                .addWhere(QM+BLDG, "^obe:isIn ", QM+FLAT)
                .addWhere(QM+FLAT, "obe:hasIdentifier", QM+uprn);

        WhereBuilder where = new WhereBuilder()
                .addPrefix("dabgeo", dabgeoUri).addPrefix(KEY_OBE, obeUri)
                .addWhere(QM+BLDG, "a", "dabgeo:Building")
                .addFilter("NOT EXISTS {"+QM+BLDG+" ^obe:isIn "+QM+FLAT+" }")
                .addWhere(QM+BLDG, "obe:hasIdentifier", QM+uprn)
                .addUnion(inner_where);

        return new SelectBuilder()
                .addPrefix("obe", obeUri).addPrefix("dabgeo", dabgeoUri).addPrefix("kb", kbUri)
                .addVar(QM+BLDG).addVar(QM+uprn)
                .addWhere(where);

    }

    private static SelectBuilder getGeomQuery(String bldg) throws ParseException {
        WhereBuilder innerwhere = new WhereBuilder().addPrefix(KEY_OCGML, ocgmlUri).addWhere(QM+SURF_GEOM, KEY_OCGML+":parentId", QM+SURFACE_GEOMETRY).addWhere(QM+SURF_GEOM, KEY_OCGML+":GeometryType", QM+GEOMETRY_TYPE);
        WhereBuilder where1 = new WhereBuilder().addPrefix(KEY_OCGML, ocgmlUri).addGraph(NodeFactory.createURI(bldgGraph), "<" + bldg + ">", KEY_OCGML+":lod0FootprintId", QM+SURFACE_GEOMETRY)
                .addGraph(NodeFactory.createURI(surfGeomGraph), innerwhere);

        return new SelectBuilder()
                .addPrefix(KEY_OCGML, ocgmlUri)
                .addVar(QM + SURF_GEOM).addVar(QM + GEOMETRY_TYPE).addVar(QM + DATATYPE)
                .addWhere(where1)
                .addBind("(DATATYPE("+QM+GEOMETRY_TYPE+"))", QM + DATATYPE);

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

    private static Polygon getPolygon(String data, String datatype) {
        // decode coordinates
        String[] valueStrings = data.split(VALUE_DELIMITER);
        double[] values = new double[valueStrings.length];
        for (int i = 0; i < valueStrings.length; i++) values[i] = Double.parseDouble(valueStrings[i]);
        // Deserialize coordinates into rings based on structure string, which should be e.g. [...]POLYGON-3-24-15-15.
        String[] splitDatatypeString = datatype.split(STRUCTURE_PREFIX); // ["...", "3-24-15-15"]
        String[] splitStructureString = splitDatatypeString[splitDatatypeString.length-1].split(STRUCTURE_DELIMITER); // ["3", "24", "15", "15"]
        String[] ringSizes = Arrays.copyOfRange(splitStructureString, 1, splitStructureString.length); // // ["24", "15", "15"]
        LinearRing exterior = null;
        LinearRing[] holes = new LinearRing[ringSizes.length - 1];
        int k = 0;
        try {
            for (int i = 0; i < ringSizes.length; i++) {
                int ringSize = Integer.parseInt(ringSizes[i]) / 3;
                Coordinate[] coords = new Coordinate[ringSize];
                for (int j = 0; j < ringSize; j++)
                    coords[j] = new Coordinate(values[k++], values[k++], values[k++]);
                if (i == 0) exterior = factory.createLinearRing(coords);
                else holes[i - 1] = factory.createLinearRing(coords);
            }
        } catch (IndexOutOfBoundsException e) {
            throw new JPSRuntimeException(e);
        }
        Polygon polygon = factory.createPolygon(exterior, holes);
        return polygon;
    }

    public static Coordinate computeCentroid(Coordinate[] coordinates, boolean skipLast) {
        double x = 0;
        double y = 0;
        double z = 0;
        int length = coordinates.length - (skipLast ? 1 : 0);
        for (int i = 0; i < length; i++) {
            x += coordinates[i].getX();
            y += coordinates[i].getY();
            z += coordinates[i].getZ();
        }
        return new Coordinate(x / length, y / length, z / length);
    }

    private static Coordinate transformEPSG27700ToWGS84(Coordinate centroid) throws CRSException, CoordinateOperationException, IllegalCoordinateException {
        CRSFactory crsFactory = new CRSFactory();
        RegistryManager registryManager = crsFactory.getRegistryManager();
        registryManager.addRegistry(new EPSGRegistry());
        CoordinateReferenceSystem crs1 = crsFactory.getCRS("EPSG:27700");
        CoordinateReferenceSystem crs2 = crsFactory.getCRS("EPSG:4326");

        Set<CoordinateOperation> operations = CoordinateOperationFactory
                .createCoordinateOperations((GeodeticCRS) crs1, (GeodeticCRS) crs2);
        double[] transform = new double[2];
        if (operations.size() != 0) {
            // Test each transformation method (generally, only one method is available)
            for (CoordinateOperation op : operations) {
                // Transform coord using the op CoordinateOperation from crs1 to crs2
                 transform  = op.transform(new double[] {centroid.x, centroid.y, centroid.z});
            }
        }
        return new Coordinate(transform[0], transform[1]);

    }
}
