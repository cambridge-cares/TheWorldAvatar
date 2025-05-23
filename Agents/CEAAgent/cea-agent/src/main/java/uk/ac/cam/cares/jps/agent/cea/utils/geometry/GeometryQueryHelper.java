package uk.ac.cam.cares.jps.agent.cea.utils.geometry;

import uk.ac.cam.cares.jps.agent.cea.data.CEAGeometryData;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.agent.cea.utils.FileReader;
import uk.ac.cam.cares.jps.agent.cea.utils.uri.OntologyURIHelper;

import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.Polygon;

import org.apache.jena.query.Query;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.graph.NodeFactory;

import org.json.JSONArray;
import org.json.JSONObject;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;

public class GeometryQueryHelper {

    private static final String PROPERTIES_PATH = "/resources/CEAAgentConfig.properties";
    private static final String minFootprintArea;

    static {
        try (InputStream input = FileReader.getStream(PROPERTIES_PATH)) {
            Properties config = new Properties();
            config.load(input);
            minFootprintArea = config.getProperty("geometry.query.minfootprintarea", "1.0");
        }
        catch (FileNotFoundException e) {
            e.printStackTrace();
            throw new JPSRuntimeException("config.properties file not found");
        }
        catch (IOException e) {
            e.printStackTrace();
            throw new JPSRuntimeException(e);
        }
    }

    public static List<CEAGeometryData> bulkGetBuildingGeometry(List<String> uriStringList, String endpoint) {

        RemoteStoreClient storeClient = new RemoteStoreClient(endpoint);

        WhereBuilder wb = new WhereBuilder()
                .addPrefix("geo", OntologyURIHelper.getOntologyUri(OntologyURIHelper.geo))
                .addPrefix("bldg", OntologyURIHelper.getOntologyUri(OntologyURIHelper.bldg))
                .addPrefix("grp", OntologyURIHelper.getOntologyUri(OntologyURIHelper.grp));

        // query for WKT of building footprint

        wb.addWhere("?building", "bldg:lod0FootPrint", "?Lod0FootPrint")
                .addWhere("?geometry", "grp:parent", "?Lod0FootPrint")
                .addWhere("?geometry", "geo:asWKT", "?wkt")
                .addWhere("?geometry", "geo:hasMetricArea", "?area")
                .addFilter("?area >= "+ minFootprintArea);

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
            if (resultJSONArray.length() > 0) {
                listCeaGeometryData.add(processBuildingData(resultJSONArray));
            } else {
                System.out.println("Warning: Empty JSONArray encountered for URI: " + uri);
            }
        }

        return listCeaGeometryData;

    }

    public static CEAGeometryData processBuildingData(JSONArray queryResultArray) {

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

    
    public static String getCountry(Coordinate coordinate) {
        String urlString = String.format(
                "https://nominatim.openstreetmap.org/reverse?lat=%f&lon=%f&format=json",
                coordinate.getY(), coordinate.getX());

        try {
            URL url = new URL(urlString);
            HttpURLConnection conn = (HttpURLConnection) url.openConnection();
            conn.setRequestMethod("GET");

            conn.setRequestProperty("User-Agent", "CEA agent");

            // Read the response
            BufferedReader reader = new BufferedReader(new InputStreamReader(conn.getInputStream()));
            StringBuilder response = new StringBuilder();
            String line;

            while ((line = reader.readLine()) != null) {
                response.append(line);
            }
            reader.close();

            JSONObject j = new JSONObject(response.toString());

            if (j.has("address")) {
                if (j.getJSONObject("address").has("country_code")) {
                    return j.getJSONObject("address").getString("country_code");
                }
            }

            return "CH";
        } catch (Exception e) {
            e.printStackTrace();
            return "CH";
        }
    }
}