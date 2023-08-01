package uk.ac.cam.cares.jsp.integration;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;

import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.postgis.PGgeometry;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

public class GetHeight {
    private static final Logger LOGGER = LogManager.getLogger(GeoObject3D.class);
    private String[] config;
    private static final String INVALID_CONNECTION_MESSAGE = "Connection is invalid...";
    private SqlConnectionPool pool;
    List<GeoObject3D> allObject3D = new ArrayList<>();
    
    protected void preCalculation(String[] config) throws SQLException {

        GeoObject3D object3D = new GeoObject3D();
        this.config = config;
        this.allObject3D = object3D.getObject3D(config);   
        calculateHeight(this.allObject3D);
    }

    private void calculateHeight(List<GeoObject3D> allObject3D){
        this.pool = new SqlConnectionPool(config);
        try (Connection srcConn = this.pool.get3DConnection()) {
            if (!srcConn.isValid(60)) {
                LOGGER.fatal(INVALID_CONNECTION_MESSAGE);
                throw new JPSRuntimeException(INVALID_CONNECTION_MESSAGE);
            }else{
                try (Statement stmt = srcConn.createStatement()) {
                    for(int i = 0; i < allObject3D.size(); i++){
                        GeoObject3D object3D = allObject3D.get(i);
                        int objectid = object3D.getId();

                        String sql = "SELECT public.ST_3DMaxDistance(( " 
                            + "SELECT geometry FROM surface_geometry WHERE id IN ( "
                            + "SELECT lod0_roofprint_id FROM building WHERE id = " + objectid + " AND lod0_roofprint_id is not null)), "
                            + "(SELECT geometry FROM surface_geometry WHERE id IN "
                            + "(SELECT lod0_footprint_id FROM building WHERE id = " + objectid + " AND lod0_footprint_id is not null))) As height";
                        ResultSet result = stmt.executeQuery(sql);
                        if (result.next()) {
                            double height = result.getDouble("height");
                            if(height > 0){
                                String upSql = "UPDATE building SET measured_height = " + height +  " WHERE id = " + objectid + ";";
                                stmt.executeUpdate(upSql);
                            }                        
                        }
                    }                                     
                }
                
            }
        }catch (SQLException e) {
            LOGGER.fatal("Error connecting to source database: " + e);
            throw new JPSRuntimeException("Error connecting to source database: " + e);
        }
        
    }
}
