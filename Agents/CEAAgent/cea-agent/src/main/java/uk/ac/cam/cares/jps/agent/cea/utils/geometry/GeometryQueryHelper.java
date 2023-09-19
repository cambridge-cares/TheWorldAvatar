package uk.ac.cam.cares.jps.agent.cea.utils.geometry;

import com.cmclinnovations.stack.clients.core.StackClient;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.agent.cea.utils.uri.OntologyURIHelper;

import org.apache.jena.query.Query;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.sparql.core.Var;
import org.apache.jena.sparql.lang.sparql_11.ParseException;
import org.json.JSONArray;
import org.locationtech.jts.geom.Geometry;
import java.util.ArrayList;
import java.util.List;


public class GeometryQueryHelper {
    private String ontopUrl = "http://stackNAME-ontop:8080/sparql";
    private OntologyURIHelper ontologyUriHelper;

    /**
     * Constructs a GeometryQueryHelper object with the given OntologyURIHelper
     * @param uriHelper OntologyURIHelper
     */
    public GeometryQueryHelper(OntologyURIHelper uriHelper) {
        this.ontopUrl = ontopUrl.replace("stackNAME", StackClient.getStackName());
        this.ontologyUriHelper = uriHelper;
    }

    /**
     * Queries for building geometry related information
     * @param uriString city object id
     * @param endpoint SPARQL endpoint
     * @param type type of building geometry related information to be queried
     * @return building geometry related information
     */
    public String getBuildingGeometry(String uriString, String endpoint, String type) {
        String result;

        switch(type) {
            case "height":
                // Set default value of 10m if height can not be obtained from knowledge graph
                result = getValue(uriString, "HeightMeasuredHeight", endpoint);
                result = result.length() == 0 ? "10.0" : result;
                break;

            case "footprint":
                // Get footprint from ground thematic surface or find from surface geometries depending on data
                result = getValue(uriString, "Lod0Footprint", endpoint);
                result = result.length() == 0 ? getValue(uriString, "FootprintThematicSurface", endpoint) : result;
                break;

            case "crs":
                result = getValue(uriString, "CRS", endpoint);
                break;

            default:
                result = "";
                break;
        }

        return result;
    }

    /**
     * Executes query on SPARQL endpoint and retrieves requested value of building
     * @param uriString city object id
     * @param value building value requested
     * @param endpoint SPARQL endpoint
     * @return geometry as string
     */
    public String getValue(String uriString, String value, String endpoint)  {

        String result = "";

        Query q = getQuery(uriString, value);

        RemoteStoreClient storeClient = new RemoteStoreClient(endpoint);

        // Execute SPARQL query
        JSONArray queryResultArray = storeClient.executeQuery(q.toString());

        if(!queryResultArray.isEmpty()){
            if (value.equals("CRS")) {
                result = queryResultArray.getJSONObject(0).get(value).toString().split(ontologyUriHelper.getOntologyUri(OntologyURIHelper.epsg))[1];
            }
            else{
                result = queryResultArray.getJSONObject(0).get(value).toString();
            }
        }

        return result;
    }

    /**
     * Calls a SPARQL query for a specific URI for height or geometry.
     * @param uriString city object id
     * @param value building value requested
     * @return returns a query string
     */
    private Query getQuery(String uriString, String value) {
        switch(value) {
            case "FootprintThematicSurface":
                return getGeometryQueryThematicSurface(uriString);
            case "HeightMeasuredHeight":
                return getHeightQueryMeasuredHeight(uriString);
            case "CRS":
                return getCrsQuery(uriString);
        }
        return null;
    }


    /**
     * Builds a SPARQL query for a specific URI to retrieve the building height for data with ocgml:measuredHeight attribute
     * @param uriString city object id
     * @return returns a query string
     */
    private Query getHeightQueryMeasuredHeight(String uriString) {
        try {
            WhereBuilder wb = new WhereBuilder()
                    .addPrefix("ocgml", ontologyUriHelper.getOntologyUri(OntologyURIHelper.ocgml))
                    .addWhere("?s", "ocgml:measuredHeight", "?HeightMeasuredHeight")
                    .addFilter("!isBlank(?HeightMeasuredHeight)");
            SelectBuilder sb = new SelectBuilder()
                    .addVar("?HeightMeasuredHeight")
                    .addWhere(wb);
            sb.setVar( Var.alloc( "s" ), NodeFactory.createURI(uriString));
            return sb.build();
        } catch (ParseException e) {
            e.printStackTrace();
            return null;
        }
    }

    /**
     * Builds a SPARQL query for a specific URI to retrieve ground surface geometries for building linked to thematic surfaces with ocgml:objectClassId 35
     * @param uriString city object id
     * @return returns a query string
     */
    private Query getGeometryQueryThematicSurface(String uriString) {
        try {
            WhereBuilder wb = new WhereBuilder()
                    .addPrefix("ocgml", ontologyUriHelper.getOntologyUri(OntologyURIHelper.ocgml))
                    .addWhere("?surf", "ocgml:cityObjectId", "?s")
                    .addWhere("?surf", "ocgml:GeometryType", "?geometry")
                    .addWhere("?surf", "ocgml:objectClassId", "?groundSurfId")
                    .addFilter("?groundSurfId = 35") //Thematic Surface Ids are 33:roof, 34:wall and 35:ground
                    .addFilter("!isBlank(?geometry)");
            SelectBuilder sb = new SelectBuilder()
                    .addVar("?geometry")
                    .addWhere(wb);
            sb.setVar( Var.alloc( "building" ), NodeFactory.createURI(uriString));
            return sb.build();

        } catch (ParseException e) {
            e.printStackTrace();
            return null;
        }
    }

    /**
     * Builds a SPARQL query for a CRS not in the DatabaseSRS graph using namespace from uri
     * @param uriString city object id
     * @return returns a query string
     */
    private Query getCrsQuery(String uriString) {
        try {
        WhereBuilder wb = new WhereBuilder()
                .addPrefix("ocgml", ontologyUriHelper.getOntologyUri(OntologyURIHelper.ocgml))
                .addPrefix("geo", ontologyUriHelper.getOntologyUri(OntologyURIHelper.geo))
                .addPrefix("geof", ontologyUriHelper.getOntologyUri(OntologyURIHelper.geof))
                .addWhere("?building", "ocgml:lod0Footprint", "?Lod0Footprint")
                .addBind("geof:getSRID(?Lod0Footprint)", "CRS");

        SelectBuilder sb = new SelectBuilder()

                .addVar("CRS")
                .addWhere(wb);

        sb.setVar( Var.alloc( "building" ), NodeFactory.createURI(uriString));
        return sb.build();
        } catch (ParseException e) {
            throw new RuntimeException(e);
        }
    }

    public List<Geometry> getLod0Footprint(String uriString, String endpoint) {
        RemoteStoreClient storeClient = new RemoteStoreClient(endpoint);

        WhereBuilder wb = new WhereBuilder()
                .addPrefix("ocgml", ontologyUriHelper.getOntologyUri(OntologyURIHelper.ocgml))
                .addPrefix("geo", ontologyUriHelper.getOntologyUri(OntologyURIHelper.geo))
                .addWhere("?building", "ocgml:lod0Footprint", "?Lod0Footprint");
        SelectBuilder sb = new SelectBuilder()
                .addWhere(wb)
                .addVar("?Lod0Footprint");
        sb.setVar(Var.alloc("building"), NodeFactory.createURI(uriString));

        Query query = sb.build();

        JSONArray queryResultArray = storeClient.executeQuery(query.toString());

        List<Geometry> result = new ArrayList<>();


        for (int i = 0; i < queryResultArray.length(); i++) {
            String wkt = queryResultArray.getJSONObject(i).getString("Lod0Footprint").split("> ")[1];
            result.add(GeometryHandler.toGeometry(wkt));
        }

        return result;
    }
}