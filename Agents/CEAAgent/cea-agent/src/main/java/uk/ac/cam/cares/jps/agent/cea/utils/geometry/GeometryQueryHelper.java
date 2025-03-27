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

import org.json.JSONArray;
import org.json.JSONObject;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class GeometryQueryHelper {

    /**
     * Queries for building geometry related information
     * 
     * @param uriString city object id
     * @param endpoint  SPARQL endpoint
     * @param flag      flag to indicate whether to call
     *                  GeometryHandler.extractFootprint
     * @return building geometry related information
     */
    public static CEAGeometryData getBuildingGeometry(String uriString, String endpoint, Boolean flag) {

        try {
            RemoteStoreClient storeClient = new RemoteStoreClient(endpoint);

            WhereBuilder wb = new WhereBuilder()
                    .addPrefix("geo", OntologyURIHelper.getOntologyUri(OntologyURIHelper.geo))
                    .addPrefix("bldg", OntologyURIHelper.getOntologyUri(OntologyURIHelper.bldg))
                    .addPrefix("grp", OntologyURIHelper.getOntologyUri(OntologyURIHelper.grp));

            // query for WKT of building footprint

            wb.addWhere("?building", "bldg:lod0FootPrint", "?Lod0FootPrint")
                    .addWhere("?geometry", "grp:parent", "?Lod0FootPrint")
                    .addWhere("?geometry", "geo:asWKT", "?wkt");

            // query for building height

            wb.addWhere("?building", "bldg:measuredHeight", "?height")
                    .addFilter("!isBlank(?height)");

            SelectBuilder sb = new SelectBuilder()
                    .addPrefix("geof", OntologyURIHelper.getOntologyUri(OntologyURIHelper.geof))
                    .addWhere(wb)
                    .addVar("?height")
                    .addVar("?wkt")
                    .addVar("geof:getSRID(?wkt)", "?crs");
            sb.setVar(Var.alloc("building"), NodeFactory.createURI(uriString));

            Query query = sb.build();

            JSONArray queryResultArray = storeClient.executeQuery(query.toString());

            return processBuildingData(queryResultArray, flag);

        } catch (Exception e) {
            e.printStackTrace();
            throw new JPSRuntimeException("No geometry data retrievable for building " + uriString);
        }
    }

    public static List<CEAGeometryData> bulkGetBuildingGeometry(List<String> uriStringList, String endpoint,
            Boolean flag) {

        RemoteStoreClient storeClient = new RemoteStoreClient(endpoint);

        WhereBuilder wb = new WhereBuilder()
                .addPrefix("geo", OntologyURIHelper.getOntologyUri(OntologyURIHelper.geo))
                .addPrefix("bldg", OntologyURIHelper.getOntologyUri(OntologyURIHelper.bldg))
                .addPrefix("grp", OntologyURIHelper.getOntologyUri(OntologyURIHelper.grp));

        // query for WKT of building footprint

        wb.addWhere("?building", "bldg:lod0FootPrint", "?Lod0FootPrint")
                .addWhere("?geometry", "grp:parent", "?Lod0FootPrint")
                .addWhere("?geometry", "geo:asWKT", "?wkt");

        // query for building height

        wb.addWhere("?building", "bldg:measuredHeight", "?height")
                .addFilter("!isBlank(?height)");

        SelectBuilder sb = new SelectBuilder()
                .addPrefix("geof", OntologyURIHelper.getOntologyUri(OntologyURIHelper.geof))
                .addWhere(wb)
                .addVar("?building")
                .addVar("?height")
                .addVar("?wkt")
                .addVar("geof:getSRID(?wkt)", "?crs");

        // Add VALUES clause for building IRIs
        for (String uri : uriStringList) {
            sb.addValueVar("?building", NodeFactory.createURI(uri));
        }

        Query query = sb.build();

        JSONArray queryResultArray = storeClient.executeQuery(query.toString());

        // create HashMap to group results per building IRI

        Map<String, JSONArray> queryMap = new HashMap<>();

        // Traverse JSONArray and group by building IRI
        for (int i = 0; i < queryResultArray.length(); i++) {
            JSONObject row = queryResultArray.getJSONObject(i);
            String buildingIRI = row.getString("building");
            queryMap.computeIfAbsent(buildingIRI, k -> new JSONArray()).put(row);
        }

        // Process building data to CEA geometry data

        List<CEAGeometryData> listCeaGeometryData = new ArrayList<>();

        for (String uri : uriStringList) {
            JSONArray resultJSONArray = queryMap.getOrDefault(uri, new JSONArray());
            listCeaGeometryData.add(processBuildingData(resultJSONArray, flag));
        }

        return listCeaGeometryData;

    }

    public static CEAGeometryData processBuildingData(JSONArray queryResultArray, Boolean flag) {

        // assume CRS and Height are the same across for the same building

        String crs = queryResultArray.getJSONObject(0).getString("crs");

        String height = "10.0";

        if (!queryResultArray.isEmpty()) {
            height = queryResultArray.getJSONObject(0).getString("height");
        }

        if (crs.contains("EPSG")) {
            crs = crs.split(OntologyURIHelper.getOntologyUri(OntologyURIHelper.epsg))[1];
            crs = crs.split(">")[0];
        } else if (crs.contains("CRS84")) {
            crs = "4326";
        }

        List<Geometry> geometry = new ArrayList<>();

        if (flag) {
            geometry = GeometryHandler.extractFootprint(queryResultArray, crs, Double.parseDouble(height));
        } else {
            for (int i = 0; i < queryResultArray.length(); i++) {
                String wkt = queryResultArray.getJSONObject(i).getString("wkt");

                Polygon temp = (Polygon) GeometryHandler.toGeometry(wkt);

                try {
                    temp = (Polygon) GeometryHandler.transformGeometry(temp, "EPSG:" + crs, GeometryHandler.EPSG_4326);
                } catch (Exception e) {
                    e.printStackTrace();
                    throw new JPSRuntimeException("Cannot transform geometry: " + wkt);
                }
                geometry.add(GeometryHandler.extractExterior(temp));
            }
        }
        return new CEAGeometryData(geometry, "4326", height);

    }

    /**
     * Check whether the CRS of the geometries in endpoint is CRS84
     * 
     * @param endpoint endpoint storing building geometries
     * @return true if CRS in endpoint is CRS84, false otherwise
     */
    public static boolean checkCRS84(String endpoint) {
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
            return queryResultArray.length() == 1
                    && queryResultArray.getJSONObject(0).getString("crs").contains("CRS84");
        }

        return false;
    }
}