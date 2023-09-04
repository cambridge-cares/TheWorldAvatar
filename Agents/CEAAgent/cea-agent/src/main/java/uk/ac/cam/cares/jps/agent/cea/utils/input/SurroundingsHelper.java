package uk.ac.cam.cares.jps.agent.cea.utils.input;

import uk.ac.cam.cares.jps.agent.cea.utils.geometry.GeometryHandler;
import uk.ac.cam.cares.jps.agent.cea.utils.geometry.GeometryQueryHelper;
import uk.ac.cam.cares.jps.agent.cea.utils.uri.OntologyURIHelper;
import uk.ac.cam.cares.jps.agent.cea.data.CEAInputData;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.query.Query;
import org.apache.jena.sparql.lang.sparql_11.ParseException;
import org.apache.jena.sparql.syntax.ElementGroup;
import org.apache.jena.sparql.syntax.ElementService;
import org.json.JSONArray;

import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.Polygon;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class SurroundingsHelper {
    private OntologyURIHelper ontologyUriHelper;

    public SurroundingsHelper(OntologyURIHelper uriHelper) {
        this.ontologyUriHelper = uriHelper;
    }

    /**
     * Retrieves the surrounding buildings
     * @param buildingFootprint footprint of building
     * @param endpoint SPARQL endpoint
     * @param unique array list of unique surrounding buildings
     * @param surroundingCoordinates list of coordinates that form bounding box for surrounding query, used for terrain calculation
     * @return the surrounding buildings as an ArrayList of CEAInputData
     */
    public ArrayList<CEAInputData> getSurroundings(String buildingFootprint, String endpoint, List<String> unique, List<Coordinate> surroundingCoordinates, String crs) {
        try {
            CEAInputData temp;
            String uri;
            ArrayList<CEAInputData> surroundings = new ArrayList<>();
            GeometryQueryHelper geometryQueryHelper = new GeometryQueryHelper(ontologyUriHelper);

            Double buffer = 100.00;

            Polygon polygon = (Polygon) GeometryHandler.toGeometry(buildingFootprint);

            Geometry envelope = polygon.getEnvelope();

            Geometry boundingBoxGeometry = ((Polygon) GeometryHandler.bufferPolygon(envelope, "EPSG:" + crs, buffer)).getExteriorRing();

            String boundingBox = boundingBoxGeometry.toText();

            Query query = getBuildingsWithinBoundsQuery(boundingBox);

            RemoteStoreClient storeClient = new RemoteStoreClient(endpoint);

            // Execute SPARQL query
            JSONArray queryResultArray = storeClient.executeQuery(query.toString());

            for (int i = 0; i < queryResultArray.length(); i++) {
                uri = queryResultArray.getJSONObject(i).get("building").toString();

                if (!unique.contains(uri)) {
                    String footprint = queryResultArray.getJSONObject(i).get("geometry").toString();
                    String height = geometryQueryHelper.getBuildingGeometry(uri, endpoint, "height");
                    temp = new CEAInputData(footprint, height, null, null, null, null, null);
                    unique.add(uri);
                    surroundings.add(temp);
                }
            }
            surroundingCoordinates.addAll(Arrays.asList(boundingBoxGeometry.getCoordinate()));
            return surroundings;
        }
        catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }

    /**
     * Builds a SPARQL geospatial query for city object id of buildings whose envelope are within lowerBounds and upperBounds
     * @return returns a query string
     */
    private Query getBuildingsWithinBoundsQuery(String boundingBox) throws ParseException {
        WhereBuilder wb = new WhereBuilder();

        wb.addBind(boundingBox, "box");
        wb.addWhere("?building", "rdf:type", "bot:building");
        wb.addWhere("?building", "haslod0Footprint", "?geometry");
        wb.addFilter("geo:sfCovereBby(?geometry, ?box)");

        SelectBuilder sb = new SelectBuilder();
        sb.addVar("?building");
        sb.addVar("?geometry");

        Query query = sb.build();

        // add geospatial service
        ElementGroup body = new ElementGroup();
        body.addElement(new ElementService(ontologyUriHelper.getOntologyUri(OntologyURIHelper.geo) + "search", wb.build().getQueryPattern()));
        query.setQueryPattern(body);

        return query;
    }
}
