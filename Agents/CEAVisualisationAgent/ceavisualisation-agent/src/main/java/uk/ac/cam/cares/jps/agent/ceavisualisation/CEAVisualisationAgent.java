package uk.ac.cam.cares.jps.agent.ceavisualisation;

import com.cmclinnovations.stack.clients.geoserver.GeoServerClient;
import com.cmclinnovations.stack.clients.geoserver.GeoServerVectorSettings;
import com.cmclinnovations.stack.clients.geoserver.UpdatedGSVirtualTableEncoder;
import org.json.JSONArray;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.agent.JPSAgent;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

import javax.servlet.annotation.WebServlet;
import javax.ws.rs.BadRequestException;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@WebServlet(urlPatterns = {
        CEAVisualisationAgent.URI_RUN })
public class CEAVisualisationAgent extends JPSAgent {
    public static final String URI_RUN = "/run";

    public static final String KEY_DB = "DB_NAME";
    public final String DB_NAME = System.getenv(KEY_DB);
    public final String DB_USER;
    public final String DB_PASSWORD;
    public RemoteRDBStoreClient rdbStoreClient;

    public static final String KEY_DATA = "data";
    public static final String SCHEMA = "ceavis";
    public static final String TABLE = "cea";
    public static final String IRI = "building_iri";
    public static final String epsg4326 = "4326";
    public static final String geoName = "geo";

    public static final String ceaStore = "cea";
    public static final String ceaLayer = "cea";
    public static final String ceaScaledLayer = "scaled_cea";
    public static final String notCEALayer = "not_cea";
    public static final String geoWorkSpace = "twa_cea";

    private boolean geoserverLayerInitialised = false;

    public CEAVisualisationAgent() {
        EndpointConfig endpointConfig = new EndpointConfig();
        DB_USER = endpointConfig.getDbUser();
        DB_PASSWORD = endpointConfig.getDbPassword();
        rdbStoreClient = new RemoteRDBStoreClient(endpointConfig.getDbUrl(DB_NAME), DB_USER, DB_PASSWORD);
        initialiseTable();
    }

    @Override
    public JSONObject processRequestParameters(JSONObject requestParams) {
        if (validateInput(requestParams)) {
            JSONArray data = requestParams.getJSONArray(KEY_DATA);

            List<VisValues> visValues = new ArrayList<>();

            for (int i = 0; i < data.length(); i++) {
                Map<String, Double> areas = new HashMap<>();
                for (Area area : Area.values()) {

                    areas.put(area.getValue(), Double.valueOf(data.getJSONObject(i).getString(area.getValue())));
                }

                Map<String, Double> annuals = new HashMap<>();
                for (Annual annual : Annual.values()) {

                    annuals.put(annual.getAnnual(),
                            Double.valueOf(data.getJSONObject(i).getString(annual.getAnnual())));
                }

                Map<String, Double> ceaValues = visValues(areas, annuals);

                visValues.add(new VisValues(data.getJSONObject(i).getString(IRI), ceaValues));
            }

            updateTable(visValues);
        }

        if (!geoserverLayerInitialised) {
            createGeoServerLayers();
            geoserverLayerInitialised = true;
        }

        return requestParams;
    }

    @Override
    public boolean validateInput(JSONObject requestParams) {
        boolean validation = true;
        if (requestParams.has(KEY_DATA)) {
            JSONArray data = requestParams.getJSONArray("data");
            validation = !data.isEmpty();
            if (validation) {
                for (int i = 0; i < data.length(); i++) {
                    for (Annual annual : Annual.values()) {
                        validation = validation && data.getJSONObject(i).has(annual.getAnnual());
                    }
                    for (Area area : Area.values()) {
                        validation = validation && data.getJSONObject(i).has(area.getValue());
                    }
                    validation = validation && data.getJSONObject(i).has(IRI);
                }
            }
        } else {
            throw new BadRequestException();
        }

        if (!validation) {
            throw new BadRequestException();
        }

        return true;
    }

    /***
     * Initialise table to be used for visualisation in TWA-VF
     * 
     * @return returns the columns in the table as a list
     */
    public void initialiseTable() {
        // create schema
        String createSchema = "CREATE SCHEMA IF NOT EXISTS " + SCHEMA;

        rdbStoreClient.executeUpdate(createSchema);

        // create table
        String createTable = "CREATE TABLE IF NOT EXISTS " + SCHEMA + "." + TABLE + "("
                + IRI + " VARCHAR(4000) UNIQUE,\n";

        for (String column : Column.getColumns()) {
            createTable += column + " DOUBLE PRECISION,";
        }

        createTable = createTable.substring(0, createTable.length() - 1) + ")";

        rdbStoreClient.executeUpdate(createTable);
    }

    /**
     * Update table with building IRI and CEA values used for visualisation
     * 
     * @param visValues list storing building IRI and CEA values
     */
    public void updateTable(List<VisValues> visValues) {
        String insert = "INSERT INTO " + SCHEMA + "." + TABLE + " (" + IRI + ",";
        String values = "VALUES\n";
        String conflict = "ON CONFLICT (" + IRI + ")";
        String update = "DO UPDATE SET";
        String set = "";

        for (String column : Column.getColumns()) {
            // column names
            insert += column + ",";
            set += column + "=EXCLUDED." + column + ",";
        }

        insert = insert.substring(0, insert.length() - 1) + ")";
        set = set.substring(0, set.length() - 1);

        // column values
        for (VisValues vis : visValues) {
            String valueRow = "(";
            valueRow += "'" + vis.getIri() + "'" + ",";
            Map<String, Double> ceaValues = vis.getValues();
            for (String column : Column.getColumns()) {
                valueRow += ceaValues.get(column) + ",";
            }
            valueRow = valueRow.substring(0, valueRow.length() - 1) + "),";
            values += valueRow;
        }

        values = values.substring(0, values.length() - 1);

        String sql = insert + "\n" + values + "\n" + conflict + "\n" + update + "\n" + set;

        rdbStoreClient.executeUpdate(sql);
    }

    /**
     * Calculates the annual per area values, and return a map of all the CEA
     * values, i.e. area values, annual values, and annual per area values
     * 
     * @param areas   map of area values
     * @param annuals map of annual values
     * @return map of CEA values for updating the table
     */
    public Map<String, Double> visValues(Map<String, Double> areas, Map<String, Double> annuals) {
        Map<String, Double> ceaValues = new HashMap<>();

        for (Map.Entry<String, Double> area : areas.entrySet()) {
            Double areaValue = area.getValue();
            ceaValues.put(area.getKey(), areaValue);
            for (Annual annual : Column.getAnnuals(area.getKey())) {
                Double annualValue = annuals.get(annual.getAnnual());
                ceaValues.put(annual.getAnnual(), annualValue);
                if (areaValue != 0.0) {
                    ceaValues.put(annual.getAnnualPerArea(), annualValue / areaValue);
                } else {
                    ceaValues.put(annual.getAnnualPerArea(), 0.0);
                }
            }
        }

        return ceaValues;
    }

    /**
     * Creates two GeoServer layers, one for buildings with CEA outputs, and one for
     * buildings without CEA outputs
     */
    public void createGeoServerLayers() {
        GeoServerClient geoServerClient = GeoServerClient.getInstance();

        geoServerClient.createWorkspace(geoWorkSpace);

        // creating Geoserver layer for buildings with raw CEA outputs

        String raw = "v.%s AS %s,";
        String building = "v." + IRI
                + " AS iri, b.measured_height AS height, public.ST_Transform(sg.geometry, 4326) AS " + geoName + " ";
        String from = "FROM " + SCHEMA + "." + TABLE + " v ";
        String join = "INNER JOIN citydb.cityobject_genericattrib cga ON v." + IRI + " = cga.urival\n" +
                "INNER JOIN citydb.building b ON cga.cityobject_id = b.id\n" +
                "INNER JOIN citydb.surface_geometry sg ON b.lod0_footprint_id = sg.parent_id";

        String raws = "";

        for (Annual annual : Annual.values()) {
            raws += String.format(raw, annual.getAnnual(), annual.getAnnual());
            raws += String.format(raw, annual.getAnnualPerArea(), annual.getAnnualPerArea());
        }

        String ceaSql = "SELECT " + raws + building + from + join;

        createGeoServerLayer(geoServerClient, ceaLayer, ceaSql);

        // creating Geoserver layer for buildings with scaled CEA outputs

        String scale = "v.%s / GREATEST((SELECT MAX(v.%s) FROM " + SCHEMA + "." + TABLE + " v), 1e-9) AS scaled_%s,";

        String scales = "";

        for (Annual annual : Annual.values()) {
            scales += String.format(scale, annual.getAnnual(), annual.getAnnual(), annual.getAnnual());
            scales += String.format(scale, annual.getAnnualPerArea(), annual.getAnnualPerArea(),
                    annual.getAnnualPerArea());
        }

        String ceaScaledSql = "SELECT " + scales + building + from + join;

        createGeoServerLayer(geoServerClient, ceaScaledLayer, ceaScaledSql);

        // creating GeoServer layer for buildings without CEA outputs
        String notCeaSql = "SELECT b.measured_height AS height, public.ST_Transform(sg.geometry, " + epsg4326 + ") AS "
                + geoName + "\nFROM citydb.cityobject_genericattrib cga\n" +
                "INNER JOIN citydb.building b ON b.id = cga.cityobject_id\n" +
                "INNER JOIN citydb.surface_geometry sg ON b.lod0_footprint_id = sg.parent_id\n" +
                "WHERE cga.urival NOT IN (SELECT " + IRI + " FROM " + SCHEMA + "." + TABLE + ")";

        createGeoServerLayer(geoServerClient, notCEALayer, notCeaSql);
    }

    private void createGeoServerLayer(GeoServerClient geoServerClient, String layerName, String sql) {

        UpdatedGSVirtualTableEncoder table = new UpdatedGSVirtualTableEncoder();

        GeoServerVectorSettings settings = new GeoServerVectorSettings();

        table.setSql(sql);
        table.setEscapeSql(true);
        table.setName(layerName);
        table.addVirtualTableGeometry(geoName, "Geometry", epsg4326);

        settings.setVirtualTable(table);
        geoServerClient.createPostGISLayer(geoWorkSpace, DB_NAME, SCHEMA, layerName, settings);

    }
}
