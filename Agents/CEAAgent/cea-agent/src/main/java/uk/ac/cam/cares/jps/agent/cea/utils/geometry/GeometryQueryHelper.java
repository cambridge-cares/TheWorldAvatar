package uk.ac.cam.cares.jps.agent.cea.utils.geometry;

import com.cmclinnovations.stack.clients.core.StackClient;
import org.locationtech.jts.geom.Geometry;
import uk.ac.cam.cares.jps.agent.cea.data.CEAGeometryData;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import uk.ac.cam.cares.jps.agent.cea.utils.uri.OntologyURIHelper;

import org.apache.jena.query.Query;
import org.apache.jena.arq.querybuilder.SelectBuilder;
import org.apache.jena.arq.querybuilder.WhereBuilder;
import org.apache.jena.graph.NodeFactory;
import org.apache.jena.sparql.core.Var;
import org.apache.jena.sparql.lang.sparql_11.ParseException;
import org.json.JSONArray;

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
     * @return building geometry related information
     */
    public CEAGeometryData getBuildingGeometry(String uriString, String endpoint) {
        String height = getBuildingHeight(uriString, endpoint);

        CEAGeometryData ceaGeometryData = getLod0Footprint(uriString, endpoint, height);

        return ceaGeometryData;
    }

    /**
     * Builds a SPARQL query for a specific URI to retrieve the building height for data with ocgml:measuredHeight attribute
     * @param uriString city object id
     * @return returns a query string
     */
    public String getBuildingHeight(String uriString, String endpoint) {
        try {
            WhereBuilder wb = new WhereBuilder()
                    .addPrefix("bldg", ontologyUriHelper.getOntologyUri(OntologyURIHelper.bldg))
                    .addWhere("?s", "bldg:measuredHeight", "?height")
                    .addFilter("!isBlank(?height)");
            SelectBuilder sb = new SelectBuilder()
                    .addVar("?height")
                    .addWhere(wb);
            sb.setVar( Var.alloc( "s" ), NodeFactory.createURI(uriString));

            RemoteStoreClient storeClient = new RemoteStoreClient(endpoint);

            JSONArray queryResultArray = storeClient.executeQuery(sb.build().toString());

            String height = queryResultArray.getJSONObject(0).getString("height");

            return height;
        }
        catch (ParseException e) {
            e.printStackTrace();
            return null;
        }
    }

    public CEAGeometryData getLod0Footprint(String uriString, String endpoint, String height) {
        try {
            RemoteStoreClient storeClient = new RemoteStoreClient(endpoint);

            WhereBuilder wb = new WhereBuilder()
                    .addPrefix("geo", ontologyUriHelper.getOntologyUri(OntologyURIHelper.geo))
                    .addPrefix("bldg", ontologyUriHelper.getOntologyUri(OntologyURIHelper.bldg))
                    .addPrefix("grp", ontologyUriHelper.getOntologyUri(OntologyURIHelper.grp));

            wb.addWhere("?building", "bldg:lod0FootPrint", "?Lod0FootPrint")
                    .addWhere("?geometry", "grp:parent", "?Lod0FootPrint")
                    .addWhere("?geometry", "geo:asWKT", "?wkt");

            SelectBuilder sb = new SelectBuilder()
                    .addPrefix("geof", ontologyUriHelper.getOntologyUri(OntologyURIHelper.geof))
                    .addWhere(wb)
                    .addVar("?wkt")
                    .addVar("geof:getSRID(?wkt)", "?crs");
            sb.setVar(Var.alloc("building"), NodeFactory.createURI(uriString));

            Query query = sb.build();

            JSONArray queryResultArray = storeClient.executeQuery(query.toString());

            String crs = queryResultArray.getJSONObject(0).getString("crs").split(ontologyUriHelper.getOntologyUri(OntologyURIHelper.epsg))[1];

            crs = crs.split(">")[0];

            List<Geometry> geometry = GeometryHandler.extractFootprint(queryResultArray, crs, Double.parseDouble(height));

            return new CEAGeometryData(geometry, "4326", height);
        }
        catch (ParseException e) {
            e.printStackTrace();
            return null;
        }
    }
}