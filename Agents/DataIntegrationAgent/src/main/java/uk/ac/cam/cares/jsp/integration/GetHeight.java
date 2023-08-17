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
    
    protected void preCalculation(String[] config, String thematicParams) throws SQLException {

        GeoObject3D object3D = new GeoObject3D();
        this.config = config;
        object3D.setConfig(config);
        object3D.setSqlConnectionPool();
        this.allObject3D = object3D.getObject3D();   
        calculateHeight(this.allObject3D, thematicParams);
    }

    private void calculateHeight(List<GeoObject3D> allObject3D, String thematicParams){
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
                        double minZ = 0.0;
                        double maxZ = 0.0;
                        String maxZsql = null;
                        String minZsql = "SELECT public.ST_Zmin(geometry) FROM surface_geometry WHERE id IN (SELECT lod0_footprint_id FROM building WHERE id = " + objectid + " AND lod0_footprint_id is not null)";
                        if(thematicParams.equals("true")){
                            maxZsql = "SELECT MAX(public.ST_Zmax(sg.geometry)) as maxZ FROM surface_geometry sg " + 
                                            "JOIN thematic_surface ts ON ts.lod2_multi_surface_id = sg.parent_id " +
                                            "JOIN building b ON b.id = ts.building_id WHERE b.id =" + objectid;
                        }else{
                            maxZsql = "SELECT MAX(public.ST_Zmax(sg.geometry)) as maxZ FROM surface_geometry sg " + 
                                            "JOIN building b ON b.lod3_multi_surface_id = sg.root_id AND b.id =" + objectid;
                        }
                        
                        ResultSet resultMin = stmt.executeQuery(minZsql);
                        if (resultMin.next()){
                            minZ = resultMin.getDouble("st_zmin");
                        }
                        ResultSet resultMax = stmt.executeQuery(maxZsql);
                        if (resultMax.next()) {
                             maxZ = resultMax.getDouble("maxZ");                                                  
                        }
                        double height = maxZ - minZ;
                        if(height > 0){
                                String upSql = "UPDATE building SET measured_height = " + height +  " WHERE id = " + objectid + ";";
                                stmt.executeUpdate(upSql);
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
