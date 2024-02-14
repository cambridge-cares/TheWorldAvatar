package uk.ac.cam.cares.jps.agent.cea.utils.geometry;

import uk.ac.cam.cares.jps.agent.cea.data.CEAGeometryData;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.agent.cea.utils.uri.OntologyURIHelper;

import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.Polygon;
import org.apache.jena.query.Query;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.sparql.core.Var;
import org.apache.jena.sparql.lang.sparql_11.ParseException;

import org.json.JSONArray;
import java.util.ArrayList;
import java.util.List;

public class GeometryQueryHelper {
    private String ontopUrl = "http://stackNAME-ontop:8080/sparql";

    /**
     * Queries for building geometry related information
     * @param uriString city object id
     * @param endpoint SPARQL endpoint
     * @return building geometry related information
     */
    public static CEAGeometryData getBuildingGeometry(String uriString, String endpoint, Boolean flag) {
        String height = getBuildingHeight(uriString, endpoint);

        CEAGeometryData ceaGeometryData = getLod0Footprint(uriString, endpoint, height, flag);

        return ceaGeometryData;
    }

    /**
     * Retrieves building height for building IRI uriString
     * @param uriString building IRI
     * @return returns building height as a string
     */
    public static String getBuildingHeight(String uriString, String endpoint) {
        try {
            WhereBuilder wb = new WhereBuilder()
                    .addPrefix("bldg", OntologyURIHelper.getOntologyUri(OntologyURIHelper.bldg))
                    .addWhere("?s", "bldg:measuredHeight", "?height")
                    .addFilter("!isBlank(?height)");
            SelectBuilder sb = new SelectBuilder()
                    .addVar("?height")
                    .addWhere(wb);
            sb.setVar( Var.alloc( "s" ), NodeFactory.createURI(uriString));

            RemoteStoreClient storeClient = new RemoteStoreClient(endpoint);

            JSONArray queryResultArray = storeClient.executeQuery(sb.build().toString());

            String height = "10.0";

            if (!queryResultArray.isEmpty()) {
                 height = queryResultArray.getJSONObject(0).getString("height");
            }

            return height;
        }
        catch (ParseException e) {
            e.printStackTrace();
            return null;
        }
    }

    /**
     * Retrieves building footprint for building IRI uriString
     * @param uriString building IRI
     * @param endpoint endpoint to retrieve uriString
     * @param height building height associated with uriString
     * @param flag flag to indicate whether to call GeometryHandler.extractFootprint
     * @return building footprint as CEAGeometryData
     */
    public static CEAGeometryData getLod0Footprint(String uriString, String endpoint, String height, Boolean flag) {
        try {
            RemoteStoreClient storeClient = new RemoteStoreClient(endpoint);

            WhereBuilder wb = new WhereBuilder()
                    .addPrefix("geo", OntologyURIHelper.getOntologyUri(OntologyURIHelper.geo))
                    .addPrefix("bldg", OntologyURIHelper.getOntologyUri(OntologyURIHelper.bldg))
                    .addPrefix("grp", OntologyURIHelper.getOntologyUri(OntologyURIHelper.grp));

            wb.addWhere("?building", "bldg:lod0FootPrint", "?Lod0FootPrint")
                    .addWhere("?geometry", "grp:parent", "?Lod0FootPrint")
                    .addWhere("?geometry", "geo:asWKT", "?wkt");

            SelectBuilder sb = new SelectBuilder()
                    .addPrefix("geof", OntologyURIHelper.getOntologyUri(OntologyURIHelper.geof))
                    .addWhere(wb)
                    .addVar("?wkt")
                    .addVar("geof:getSRID(?wkt)", "?crs");
            sb.setVar(Var.alloc("building"), NodeFactory.createURI(uriString));

            Query query = sb.build();

            JSONArray queryResultArray = storeClient.executeQuery(query.toString());

            String crs = queryResultArray.getJSONObject(0).getString("crs");

            if (crs.contains("EPSG")) {
                crs = crs.split(OntologyURIHelper.getOntologyUri(OntologyURIHelper.epsg))[1];
                crs = crs.split(">")[0];
            }
            else if (crs.contains("CRS84")){
                crs = "4326";
            }

            List<Geometry> geometry = new ArrayList<>();

            if (flag) {
                geometry = GeometryHandler.extractFootprint(queryResultArray, crs, Double.parseDouble(height));
            }
            else{
                for (int i = 0; i < queryResultArray.length(); i++) {
                    String wkt = queryResultArray.getJSONObject(i).getString("wkt");

                    Polygon temp = (Polygon) GeometryHandler.toGeometry(wkt);

                    temp = (Polygon) GeometryHandler.transformGeometry(temp, "EPSG:"+crs, GeometryHandler.EPSG_4326);
                    geometry.add(GeometryHandler.extractExterior(temp));
                }
            }
            return new CEAGeometryData(geometry, "4326", height);
        }
        catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException("No geometry data retrievable for building " + uriString);
        }
    }

    /**
     * Check whether the CRS of the geometries in endpoint is CRS84
     * @param endpoint endpoint storing building geometries
     * @return true if CRS in endpoint is CRS84, false otherwise
     */
    public static boolean checkCRS84(String endpoint) {
        try {
            RemoteStoreClient storeClient = new RemoteStoreClient(endpoint);
            WhereBuilder wb = new WhereBuilder()
                    .addPrefix("geo", OntologyURIHelper.getOntologyUri(OntologyURIHelper.geo))
                    .addPrefix("bldg", OntologyURIHelper.getOntologyUri(OntologyURIHelper.bldg))
                    .addPrefix("grp", OntologyURIHelper.getOntologyUri(OntologyURIHelper.grp));

            wb.addWhere("?building", "bldg:lod0FootPrint", "?Lod0FootPrint")
                    .addWhere("?geometry", "grp:parent", "?Lod0FootPrint")
                    .addWhere("?geometry", "geo:asWKT", "?wkt");

            SelectBuilder sb = new SelectBuilder()
                    .addPrefix("geof", OntologyURIHelper.getOntologyUri(OntologyURIHelper.geof))
                    .addWhere(wb)
                    .addVar("geof:getSRID(?wkt)", "?crs")
                    .setDistinct(true);

            JSONArray queryResultArray = storeClient.executeQuery(sb.build().toString());

            if (!queryResultArray.isEmpty()) {
                return queryResultArray.length() == 1 && queryResultArray.getJSONObject(0).getString("crs").contains("CRS84");
            }

            return false;
        } catch (ParseException e) {
            throw new RuntimeException(e);
        }
    }
}