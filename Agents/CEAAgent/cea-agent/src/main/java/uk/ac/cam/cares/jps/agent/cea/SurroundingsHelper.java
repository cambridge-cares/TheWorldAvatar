package uk.ac.cam.cares.jps.agent.cea;

import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.arq.querybuilder.handlers.WhereHandler;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.query.Query;
import org.apache.jena.sparql.lang.sparql_11.ParseException;
import org.apache.jena.sparql.syntax.ElementGroup;
import org.apache.jena.sparql.syntax.ElementService;
import org.json.JSONArray;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.Polygon;
import uk.ac.cam.cares.jps.agent.ceatasks.CEAInputData;
import uk.ac.cam.cares.jps.base.query.AccessAgentCaller;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class SurroundingsHelper {
    public String customDataType = "<http://localhost/blazegraph/literals/POLYGON-3-15>";
    public String customField = "X0#Y0#Z0#X1#Y1#Z1#X2#Y2#Z2#X3#Y3#Z3#X4#Y4#Z4";
    public static final String CITY_OBJECT = "cityobject";

    private OntologyURIHelper ontologyUriHelper;
    public SurroundingsHelper(OntologyURIHelper uriHelper) {
        this.ontologyUriHelper = uriHelper;
    }

    /**
     * Builds a SPARQL geospatial query for city object id of buildings whose envelope are within lowerBounds and upperBounds
     * @param uriString city object id of the target building
     * @param lowerBounds coordinates of customFieldsLowerBounds as a string
     * @param upperBounds coordinates of customFieldsUpperBounds as a string
     * @return returns a query string
     */
    private Query getBuildingsWithinBoundsQuery(String uriString, String lowerBounds, String upperBounds) throws ParseException {
        // where clause for geospatial search
        WhereBuilder wb = new WhereBuilder()
                .addPrefix("ocgml", ontologyUriHelper.getOntologyUri(OntologyURIHelper.ocgml))
                .addPrefix("geo", ontologyUriHelper.getOntologyUri(OntologyURIHelper.geo))
                .addWhere("?cityObject", "geo:predicate", "ocgml:EnvelopeType")
                .addWhere("?cityObject", "geo:searchDatatype", customDataType)
                .addWhere("?cityObject", "geo:customFields", customField)
                // PLACEHOLDER because lowerBounds and upperBounds would be otherwise added as doubles, not strings
                .addWhere("?cityObject", "geo:customFieldsLowerBounds", "PLACEHOLDER" + lowerBounds)
                .addWhere("?cityObject", "geo:customFieldsUpperBounds", "PLACEHOLDER" + upperBounds);

        // where clause to check that the city object is a building
        WhereBuilder wb2 = new WhereBuilder()
                .addPrefix("ocgml", ontologyUriHelper.getOntologyUri(OntologyURIHelper.ocgml))
                .addWhere("?cityObject", "ocgml:objectClassId", "?id")
                .addFilter("?id=26");

        SelectBuilder sb = new SelectBuilder()
                .addVar("?cityObject");

        Query query = sb.build();
        // add geospatial service
        ElementGroup body = new ElementGroup();
        body.addElement(new ElementService(ontologyUriHelper.getOntologyUri(OntologyURIHelper.geo) + "search", wb.build().getQueryPattern()));
        body.addElement(wb2.build().getQueryPattern());
        query.setQueryPattern(body);

        WhereHandler wh = new WhereHandler(query.cloneQuery());

        // add city object graph
        WhereHandler wh2 = new WhereHandler(sb.build());
        wh2.addGraph(NodeFactory.createURI(BuildingHelper.getGraph(uriString, CITY_OBJECT)), wh);

        return wh2.getQuery();
    }

    /**
     * Retrieves the surrounding buildings
     * @param uriString city object id
     * @param route route to pass to access agent
     * @param unique array list of unique surrounding buildings
     * @param surroundingCoordinates list of coordinates that form bounding box for surrounding query, used for terrain calculation
     * @return the surrounding buildings as an ArrayList of CEAInputData
     */
    public ArrayList<CEAInputData> getSurroundings(String uriString, String route, List<String> unique, List<Coordinate> surroundingCoordinates) {
        try {
            CEAInputData temp;
            String uri;
            ArrayList<CEAInputData> surroundings = new ArrayList<>();
            GeometryQueryHelper geometryQueryHelper = new GeometryQueryHelper(ontologyUriHelper);
            String envelopeCoordinates = geometryQueryHelper.getValue(uriString, "envelope", route);

            Double buffer = 100.00;

            Polygon envelopePolygon = (Polygon) GeometryHelper.toPolygon(envelopeCoordinates);

            Geometry boundingBoxGeometry = ((Polygon) GeometryHelper.inflatePolygon(envelopePolygon, buffer)).getExteriorRing();

            Coordinate[] boundingBoxCoordinates = boundingBoxGeometry.getCoordinates();

            String boundingBox = GeometryHelper.coordinatesToString(boundingBoxCoordinates);

            String[] points = boundingBox.split("#");

            String lowerPoints= points[0] + "#" + points[1] + "#" + 0 + "#";

            String lowerBounds = lowerPoints + lowerPoints + lowerPoints + lowerPoints + lowerPoints;
            lowerBounds = lowerBounds.substring(0, lowerBounds.length() - 1 );

            Double maxZ;

            if (points[8].equals("NaN")) {
                // highest elevation on Earth is 8848
                maxZ = 8850.0;
            }
            else{
                maxZ = Double.parseDouble(points[8])+200;
            }

            String upperPoints = points[6] + "#" + points[7] + "#" + maxZ + "#";

            String upperBounds = upperPoints + upperPoints + upperPoints + upperPoints + upperPoints;
            upperBounds = upperBounds.substring(0, upperBounds.length() - 1);

            Query query = getBuildingsWithinBoundsQuery(uriString, lowerBounds, upperBounds);

            String queryString = query.toString().replace("PLACEHOLDER", "");

            JSONArray queryResultArray = AccessAgentCaller.queryStore(route, queryString);

            for (int i = 0; i < queryResultArray.length(); i++) {
                uri = queryResultArray.getJSONObject(i).get("cityObject").toString();

                if (!unique.contains(uri)) {
                    String height = geometryQueryHelper.getBuildingGeometry(uri, route, "height");
                    String footprint = geometryQueryHelper.getBuildingGeometry(uri, route, "footprint");

                    temp = new CEAInputData(footprint, height, null, null, null, null, null);
                    unique.add(uri);
                    surroundings.add(temp);
                }
            }
            surroundingCoordinates.addAll(Arrays.asList(boundingBoxCoordinates));
            return surroundings;
        }
        catch (ParseException e) {
            e.printStackTrace();
            return null;
        }
    }
}
