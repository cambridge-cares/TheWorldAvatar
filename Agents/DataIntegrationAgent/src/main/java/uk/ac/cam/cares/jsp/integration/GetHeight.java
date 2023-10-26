package uk.ac.cam.cares.jsp.integration;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;

import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class GetHeight {
    private static final Logger LOGGER = LogManager.getLogger(GetHeight.class);
    private String[] config;
    private static final String INVALID_CONNECTION_MESSAGE = "Connection is invalid...";
    private SqlConnectionPool pool;
    List<GeoObject3D> allObject3D = new ArrayList<>();
    
    protected void preCalculation(String[] config) throws SQLException {

        GeoObject3D object3D = new GeoObject3D();
        this.config = config;
        object3D.setConfig(config);
        object3D.setSqlConnectionPool();
        // this.allObject3D = object3D.getObject3D();   
        calculateHeight();
    }

    /**
     * Get Max Z and Min Z of building to calculate height and store in building table as measeured height
     */
    private void calculateHeight(){
        this.pool = new SqlConnectionPool(config);
        try (Connection srcConn = this.pool.get3DConnection()) {
            if (!srcConn.isValid(60)) {
                LOGGER.fatal(INVALID_CONNECTION_MESSAGE);
                throw new JPSRuntimeException(INVALID_CONNECTION_MESSAGE);
            }else{
                try (Statement stmt = srcConn.createStatement()) {
                    String upSql = "UPDATE building SET measured_height = h FROM (" +
                                    "SELECT COALESCE(building.id,thematic_surface.building_id) AS bid, " + 
                                    "MAX(public.ST_ZMax(geometry)) - MIN(public.ST_ZMin(geometry)) AS h " +
                                    "FROM surface_geometry FULL JOIN building ON surface_geometry.root_id = COALESCE(" +
                                    "building.lod4_multi_surface_id, building.lod3_multi_surface_id," +
                                    "building.lod2_multi_surface_id, building.lod1_multi_surface_id, " +
                                    "building.lod4_solid_id, building.lod3_solid_id, building.lod2_solid_id, building.lod1_solid_id) FULL JOIN " +
                                    "thematic_surface ON surface_geometry.root_id = COALESCE(thematic_surface.lod4_multi_surface_id, " +
                                    "thematic_surface.lod3_multi_surface_id, thematic_surface.lod2_multi_surface_id) WHERE geometry IS NOT NULL " +
                                    "GROUP BY building.id, thematic_surface.building_id) AS x " +
                                    "WHERE building.measured_height IS NULL AND x.bid = building.id;";
                    stmt.executeUpdate(upSql);                                                    
                }
                
            }
        }catch (SQLException e) {
            LOGGER.fatal("Error connecting to source database: " + e);
            throw new JPSRuntimeException("Error connecting to source database: " + e);
        }
        
    }
}
