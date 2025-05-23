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
import org.json.JSONArray;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class SurroundingsHelper {
    /**
     * Retrieves the surrounding buildings
     * 
     * @param endpoint SPARQL endpoint
     * @return the surrounding buildings as a list of CEAGeometryData
     */
    public static List<CEAGeometryData> getSurroundings(List<CEABuildingData> ceaBuildingDataList,
            List<String> buildingIRIs, String endpoint) {
        try {

            Double buffer = 50.0; // this should be in metre

            // collect footprints of buildings

            List<Geometry> geometries = new ArrayList<>();

            for (CEABuildingData ceaBuildingData : ceaBuildingDataList) {
                for (Geometry geometry : ceaBuildingData.getGeometry().getFootprint()) {
                    geometries.add(geometry);
                }
            }

            String crs = ceaBuildingDataList.get(0).getGeometry().getCrs();

            String boundingBox = GeometryHandler.getBufferEnvelope(geometries, crs, buffer);

            Query query = getBuildingsWithinBoundsQuery(boundingBox);

            RemoteStoreClient storeClient = new RemoteStoreClient(endpoint);

            // Execute SPARQL query
            JSONArray queryResultArray = storeClient.executeQuery(query.toString());

            List<String> newUriList = IntStream.range(0, queryResultArray.length())
                    .mapToObj(i -> queryResultArray.getJSONObject(i).get("building").toString())
                    .filter(uri -> !buildingIRIs.contains(uri)).collect(Collectors.toList());

            return GeometryQueryHelper.bulkGetBuildingGeometry(newUriList, endpoint);
        } catch (Exception e) {
            System.out.println(
                    "No surroundings retrieved, agent will run CEA with CEA's default surroundings retrieved from OpenStreetMap.");
            return Collections.emptyList();
        }
    }

    /**
     * Builds a SPARQL geospatial query for city object id of buildings whose
     * envelope are within lowerBounds and upperBounds
     * 
     * @param boundingBox WKT string that define the boundary of surroundings query
     * @return returns a query object for the geospatial query
     */
    private static Query getBuildingsWithinBoundsQuery(String boundingBox) {
        boundingBox = "\"" + boundingBox + "\"^^geo:wktLiteral";

        WhereBuilder wb = new WhereBuilder()
                .addPrefix("geof", OntologyURIHelper.getOntologyUri(OntologyURIHelper.geof))
                .addPrefix("bldg", OntologyURIHelper.getOntologyUri(OntologyURIHelper.bldg))
                .addPrefix("grp", OntologyURIHelper.getOntologyUri(OntologyURIHelper.grp))
                .addPrefix("geo", OntologyURIHelper.getOntologyUri(OntologyURIHelper.geo));

        wb.addWhere("?building", "bldg:lod0FootPrint", "?Lod0FootPrint")
                .addWhere("?geometry", "grp:parent", "?Lod0FootPrint")
                .addWhere("?geometry", "geo:asWKT", "?wkt")
                .addFilter("geof:sfIntersects(?wkt, ?box)");

        SelectBuilder sb = new SelectBuilder()
                .addPrefix("geo", OntologyURIHelper.getOntologyUri(OntologyURIHelper.geo))
                .addBind(boundingBox, "box")
                .addWhere(wb)
                .addVar("?building");


        return sb.build();
    }
}
