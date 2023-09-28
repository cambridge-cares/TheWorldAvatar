package uk.ac.cam.cares.jps.agent.cea.utils.input;

import org.locationtech.jts.geom.*;
import uk.ac.cam.cares.jps.agent.cea.data.CEAGeometryData;
import uk.ac.cam.cares.jps.agent.cea.utils.geometry.GeometryHandler;
import uk.ac.cam.cares.jps.agent.cea.utils.geometry.GeometryQueryHelper;
import uk.ac.cam.cares.jps.agent.cea.utils.uri.OntologyURIHelper;
import uk.ac.cam.cares.jps.agent.cea.data.CEABuildingData;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.query.Query;
import org.apache.jena.sparql.lang.sparql_11.ParseException;
import org.json.JSONArray;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import java.util.ArrayList;
import java.util.List;

public class SurroundingsHelper {
    private OntologyURIHelper ontologyUriHelper;

    public SurroundingsHelper(OntologyURIHelper uriHelper) {
        this.ontologyUriHelper = uriHelper;
    }

    /**
     * Retrieves the surrounding buildings
     * @param endpoint SPARQL endpoint
     * @return the surrounding buildings as an ArrayList of CEAInputData
     */
    public List<CEAGeometryData> getSurroundings(ArrayList<CEABuildingData> ceaBuildingDataList, String endpoint) {
        try {
            String uri;
            List<CEAGeometryData> surroundings = new ArrayList<>();
            GeometryQueryHelper geometryQueryHelper = new GeometryQueryHelper(ontologyUriHelper);

            Double buffer = 100.00;

            List<Geometry> geometries = new ArrayList<>();

            for (CEABuildingData ceaBuildingData : ceaBuildingDataList) {
                for (Geometry geometry : ceaBuildingData.getGeometry().getFootprint()) {
                    geometries.add(geometry);
                }
            }

            String crs = ceaBuildingDataList.get(0).getGeometry().getCrs();

            GeometryFactory geometryFactory = new GeometryFactory();

            GeometryCollection geoCol = geometryFactory.createGeometryCollection(geometries.toArray(new Polygon[0]));

            Geometry envelope = geoCol.getEnvelope();

            Geometry boundingBoxGeometry = GeometryHandler.bufferPolygon(envelope, "EPSG:" + crs, buffer);

            String boundingBox = boundingBoxGeometry.toText();

            Query query = getBuildingsWithinBoundsQuery(boundingBox, crs);

            RemoteStoreClient storeClient = new RemoteStoreClient(endpoint);

            // Execute SPARQL query
            JSONArray queryResultArray = storeClient.executeQuery(query.toString());

            for (int i = 0; i < queryResultArray.length(); i++) {
                uri = queryResultArray.getJSONObject(i).get("building").toString();

                CEAGeometryData temp = geometryQueryHelper.getBuildingGeometry(uri, endpoint);
                surroundings.add(temp);
            }

            return surroundings;
        }
        catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }

    /**
     * Builds a SPARQL geospatial query for city object id of buildings whose envelope are within lowerBounds and upperBounds
     * @param boundingBox WKT string that define the boundary of surroundings query
     * @param crs CRS of boundingBox
     * @return returns a query string
     */
    private Query getBuildingsWithinBoundsQuery(String boundingBox, String crs) throws ParseException {
        boundingBox = "<" + ontologyUriHelper.getOntologyUri(OntologyURIHelper.epsg) + crs + "> " + boundingBox + "^^geo:wktLiteral";

        WhereBuilder wb = new WhereBuilder()
                .addPrefix("rdf", ontologyUriHelper.getOntologyUri(OntologyURIHelper.rdf))
                .addPrefix("bot", ontologyUriHelper.getOntologyUri(OntologyURIHelper.bot))
                .addPrefix("ocgml", ontologyUriHelper.getOntologyUri(OntologyURIHelper.ocgml))
                .addPrefix("geof", ontologyUriHelper.getOntologyUri(OntologyURIHelper.geof));

        wb.addWhere("?building", "rdf:type", "bot:building")
                .addWhere("?building", "ocgml:lodFootprint", "?Lod0Footprint")
                .addFilter("geof:sfIntersects(?geometry, ?box)");

        SelectBuilder sb = new SelectBuilder()
                .addPrefix("geo", ontologyUriHelper.getOntologyUri(OntologyURIHelper.geo))
                .addBind(boundingBox, "box")
                .addWhere(wb)
                .addVar("?building")
                .addVar("?geometry");

        Query query = sb.build();

        return query;
    }
}
