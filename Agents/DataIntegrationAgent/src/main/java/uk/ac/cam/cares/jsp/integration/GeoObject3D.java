package uk.ac.cam.cares.jsp.integration;

import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.postgis.PGgeometry;


import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
//cityobject table in postgresql
public class GeoObject3D {
    private String geometry;
    private String name;
    private int objectClassid;
    private String gmlid;
    private int cityobjectid;
    private ObjectAddress address = new ObjectAddress();

    private static final Logger LOGGER = LogManager.getLogger(GeoObject3D.class);
    private String[] config;
    private static final String INVALID_CONNECTION_MESSAGE = "Connection is invalid...";
    private SqlConnectionPool pool;
    private Connection srcConn;

    public GeoObject3D () {}
    public GeoObject3D(String name, int cityobjectid, int objectClassid, String geometry){
        this.name = name;
        this.cityobjectid = cityobjectid;
        this.objectClassid = objectClassid;
        this.geometry = geometry;
    }

    public String getName(){
        return this.name;
    }

    public int getId() {return this.cityobjectid;}

    public String getGmlId() {return this.gmlid;}

    public String getGeometry3D(){
        return this.geometry;
    }

    public void setId(int cityobjectid) {
        this.cityobjectid = cityobjectid;
    }
    public void setName(String name){
        this.name = name;
    }

    public void setGmlid (String gmlid){
        this.gmlid = gmlid;
    }

    public void setObjectClassid (int objectClassid) {
        this.objectClassid = objectClassid;
    }

    public void setGeometry (String geometry) {
        this.geometry = geometry;
    }
    public void setAddress(ObjectAddress address){
        this.address = address;
    }

    public int getSrid(String geom){
        String[] srid = geom.split("[= ;]");
        return Integer.parseInt(srid[1]);
    }

    public ObjectAddress getAddress(){ return this.address;}

    public List<GeoObject3D> getObject3D () throws SQLException {

        List<GeoObject3D> allObject3D = new ArrayList<>();
        // this.config = config;
        if(this.pool == null){
            this.pool = new SqlConnectionPool(this.config);
        }
        if (this.srcConn==null) {
            this.srcConn = this.pool.get3DConnection();
        }

        String sql = "SELECT id, gmlid, objectclass_id, name, public.ST_AsEWKT(envelope) AS geom FROM cityobject WHERE objectclass_id = 26";
        try (Statement stmt = srcConn.createStatement()) {
            ResultSet result = stmt.executeQuery(sql);
            while (result.next()) {
                GeoObject3D object3D = new GeoObject3D();
                object3D.setId(result.getInt("id"));
                object3D.setGmlid(result.getString("gmlid"));
                object3D.setObjectClassid(result.getInt("objectclass_id"));
                object3D.setName(result.getString("name"));
                object3D.setGeometry(result.getString("geom"));
                object3D.setAddress(this.address.queryAddress(result.getInt("id"), srcConn));
                allObject3D.add(object3D);
            }
            return allObject3D;
            
        } catch (SQLException e) {
            LOGGER.fatal("Error connecting to source database: " + e);
            throw new JPSRuntimeException("Error connecting to source database: " + e);
        }
    }

    void setConfig(String[] config) {
        this.config = config;
    }
    void setSqlConnectionPool() {
        this.pool = new SqlConnectionPool(this.config);
    }

    public void updateName(GeoObject3D object3D, String[] config) throws SQLException{
        String upSql = "UPDATE cityobject SET ";
        if(object3D.name != null){
            upSql = upSql + "name = '" + object3D.name + "' WHERE gmlid = '" + object3D.gmlid + "';";           
            
            try {
                if(this.srcConn == null){
                    this.pool = new SqlConnectionPool(config);
                    LOGGER.info("Pinging source database for availability...");
                    this.srcConn = this.pool.get3DConnection();
                }
                Statement stmt = srcConn.createStatement();
                stmt.executeUpdate(upSql);       
            } catch (SQLException e) {
                LOGGER.fatal("Error connecting to source database: " + e);
                throw new JPSRuntimeException("Error connecting to source database: " + e);
            }    
        }else{
            LOGGER.info("No Update");
        }

    }

    public void updateAddress(ObjectAddress address, String[] config) throws SQLException {
        this.address.updateAddress(address,config);
    }

    public Map<Integer, PGgeometry> queryBuildingSurfaces(int cityobjectid, boolean thematic){
        Map<Integer, PGgeometry> gourndSurface = new java.util.HashMap<>();
        try (Connection srcConn = this.pool.get3DConnection()) {
            if (!srcConn.isValid(60)) {
                LOGGER.fatal(INVALID_CONNECTION_MESSAGE);
                throw new JPSRuntimeException(INVALID_CONNECTION_MESSAGE);
            }else{
                String sql = null;
                if(thematic){
                    sql = "SELECT public.ST_ExteriorRing(public.ST_Dump(public.ST_3DUnion(geometry))) as footprint, id "
                    + "FROM surface_geometry WHERE root_id IN (SELECT lod2_multi_surface_id FROM thematic_surface WHERE building_id = "
                    + cityobjectid+ " AND objectclass_id = 35) AND geometry is not null";
                }else{
                    sql = "SELECT geometry, id FROM surface_geometry WHERE root_id = " + cityobjectid + "AND geometry is not null";
                }

                try (Statement stmt = srcConn.createStatement()) {
                    ResultSet result = stmt.executeQuery(sql);
                    while (result.next()) {
                        SpatialLink sp = new SpatialLink();
                        PGgeometry geom = (PGgeometry)result.getObject("footprint");
                        if(geom != null){
                            gourndSurface.put(Integer.valueOf(result.getInt("id")) ,geom);
                        }                        
                    }                  
                }
                return gourndSurface;
            }
        } catch (SQLException e) {
            LOGGER.fatal("Error connecting to source database: " + e);
            throw new JPSRuntimeException("Error connecting to source database: " + e);
        }

    }

     /**
     * Extract footprint based on ground surface
     * Extract roofprint based on roof surface
     * Store resultes in postgresql
     */
    public void extractPrint_thematic (List<GeoObject3D> allObject3D, String surfaceType){
        PGgeometry footprint = new PGgeometry();
        if(surfaceType.equals("footprint")){
            this.objectClassid = 35;
        }else if (surfaceType.equals("roofprint")){
            this.objectClassid = 33;
        }else{
            this.objectClassid = 0;
        }
        
        try {
            if (this.pool == null){
                this.pool = new SqlConnectionPool(this.config);
            }
            if (this.srcConn == null) {               
                this.srcConn = this.pool.get3DConnection();
            }
            for(int i = 0; i < allObject3D.size(); i++){
                cityobjectid = allObject3D.get(i).getId();
                //footprint extraction method is not perfect now, got error if parameter < 1 in ST_ConcaveHull (need to be imporved)
                String sql = "SELECT public.ST_ConcaveHull(public.ST_Collect(geom),1) FROM ( " +
                "SELECT public.ST_MakeValid(public.ST_Union(geometry),'method=structure keepcollapsed=false') as geom " +
                "FROM surface_geometry WHERE parent_id  IN (SELECT lod2_multi_surface_id FROM thematic_surface WHERE building_id = " 
                + cityobjectid + " AND objectclass_id = " + this.objectClassid + ") AND geometry is not null) As footprint;"; 
                Statement stmt = srcConn.createStatement();
                ResultSet result = stmt.executeQuery(sql);
                while (result.next()) {
                    footprint = (PGgeometry)result.getObject("st_concavehull");                      
                }  
                updatePrint(cityobjectid, footprint, surfaceType);
            }
                   
        }catch (SQLException e) {
            LOGGER.fatal("Error connecting to source database: " + e);
            throw new JPSRuntimeException("Error connecting to source database: " + e);
        }
  
    }

    /**
     * To identify surface type (ground, roof and wall (current only work on ground, the others need to be developed))
     * 1. If has not surface_calculation table, it will be created to store orientation and ratio (area/3d_area) of surface for types identification. 
     *    Becasue the database would crash, the calculation will be processed on each building one by one and store in postgresql
     *    a. The orientation: It returns -1 if the polygon is counterclockwise oriented (roof) and 1 if the polygon is clockwise oriented (ground)
     *    b. Ratio: If the polygon is flat, the ratio will be 1. if it is vertical, the ratio will be zero. A threshold value to separate wall from roof and ground 
     * 2. Surface types will be update in postgresql
     */
    public void identifySurface (List<GeoObject3D> allObject3D, String surfaceType){
        Map<Integer, List<Integer[]>> groundMap = new HashMap<>();
        if(surfaceType.equals("footprint")){
            this.objectClassid = 35;
        }else if (surfaceType.equals("roofprint")){
            this.objectClassid = 33;
        }else{
            this.objectClassid = 0;
        }
        try {
            if (this.srcConn == null) {
                srcConn = this.pool.get3DConnection();
            }

            String createTabel = 
                "CREATE TABLE IF NOT EXISTS surface_calculation \r\n" + //
                        "(\r\n" + //
                        "    id serial primary key NOT NULL,\r\n" + //
                        "    surface_id integer NOT NULL,\r\n" + //
                        "    orientation numeric,\r\n" + //
                        "    ratio double precision\r\n" + //
                        ")";
            Statement stmt = srcConn.createStatement();
            stmt.execute(createTabel);

            for(int i = 0; i < allObject3D.size(); i++){
                int buildingid = allObject3D.get(i).getId();
                String insertSql = "INSERT INTO surface_calculation (surface_id, orientation, ratio) " +
                "SELECT * FROM ( " +
                "SELECT sg.id, public.ST_Orientation(sg.geometry), public.ST_Area(sg.geometry)/public.ST_3DArea(public.ST_MakeValid(sg.geometry)) "+
                "FROM surface_geometry sg " +
                "JOIN building b ON b.lod3_multi_surface_id = sg.parent_id " +
                "WHERE sg.geometry is NOT NULL AND public.ST_3DArea(public.ST_MakeValid(sg.geometry)) != 0 AND b.id = " + buildingid +
                " ) AS i(surface_id, orientation, ratio) " +
                "WHERE NOT EXISTS ( SELECT FROM surface_calculation sc WHERE sc.surface_id = i.surface_id);";
                stmt.executeUpdate(insertSql);
            }
            
            if(this.objectClassid == 35){
                // String sql = "SELECT sg.id as surfaceid, sg.root_id as rootid, sg.parent_id as parentid, sg.cityobject_id as cityobjid, b.id as buildingid FROM surface_geometry sg "+
                // "JOIN building b ON b.lod3_multi_surface_id = sg.parent_id "+
                // "WHERE sg.geometry IS NOT NULL AND public.ST_3DArea(public.ST_MakeValid(sg.geometry)) != 0 "+
                // "AND public.ST_Orientation(sg.geometry) = 1 " +
                // "AND public.ST_Area(sg.geometry)/public.ST_3DArea(public.ST_MakeValid(sg.geometry))>0.8 " +
                // "GROUP BY buildingid, rootid, surfaceid"; 
                
                String sql = "SELECT sg.id as surfaceid, sg.root_id as rootid, sg.parent_id as parentid, sg.cityobject_id as cityobjid, b.id as buildingid FROM surface_geometry sg "+
                "JOIN building b ON b.lod3_multi_surface_id = sg.parent_id "+
                "JOIN surface_calculation sc ON sg.id = sc.surface_id "+
                "WHERE sc.orientation = 1 AND sc.ratio > 0.8 "+
                "GROUP BY buildingid, rootid, surfaceid";
                // Statement stmt = srcConn.createStatement();
                ResultSet result = stmt.executeQuery(sql);
                while (result.next()) {
                    Integer[] groundSurface = new Integer[4];
                    List<Integer[]> groundList = new ArrayList<>();
                    int buildingId = result.getInt("buildingid");
                    int surfaceId = result.getInt("surfaceid");
                    int parentid = result.getInt("parentid");
                    int rootid = result.getInt("rootid");
                    int cityobjid = result.getInt("cityobjid");
                    groundSurface[0] = surfaceId;
                    groundSurface[1] = parentid;
                    groundSurface[2] = rootid;
                    groundSurface[3] = cityobjid;

                    if (groundMap.containsKey(buildingId)) {
                        groundList = groundMap.get(buildingId);
                        groundList.add(groundSurface);
                    } else {
                        groundList.add(groundSurface);
                        groundMap.put(buildingId, groundList);
                    }                   
                                    
                }                               
                updateSurface(groundMap);  
                //else for the other surface type need to be developed
            }
        }catch (SQLException e) {
            LOGGER.fatal("Error connecting to source database: " + e);
            throw new JPSRuntimeException("Error connecting to source database: " + e);
        }
    }

    /**
     * 1 building has only 1 ground surface cityobject and 1 ground surface thematic surface
     * 2. new cityobject of ground surface
     * 3. new surface geometry of root ground surface, no geometry
     * 4. new thematic surface link to new parentid
     * 5. update all ground surface with new rootid and cityobjectid
     */
    public void updateSurface(Map<Integer, List<Integer[]>> groundMap) throws SQLException{
        String upSql;
        String insertSql;
        int buildingid = 0;
        int surfaceid = 0;
        int parentid = 0;
        int rootid = 0;
        int cityobjid = 0;
        
        try{
            if (srcConn == null) {
                this.srcConn = this.pool.get3DConnection();
            }
            PreparedStatement pstmt;
            for (var entry : groundMap.entrySet()) {
                buildingid = entry.getKey();
                rootid = entry.getValue().get(0)[2];
                insertSql = "INSERT INTO cityobject (objectclass_id) VALUES (?);";
                pstmt = srcConn.prepareStatement(insertSql, new String[] {"id"});
                pstmt.setInt(1, this.objectClassid);
                pstmt.executeUpdate();
                ResultSet rs =  pstmt.getGeneratedKeys();
                if(rs.next()){
                    cityobjid = rs.getInt(1);
                }  

                insertSql = "INSERT INTO surface_geometry (root_id, cityobject_id) VALUES (?,?);";
                pstmt = srcConn.prepareStatement(insertSql, new String[] {"id"});
                pstmt.setInt(1, rootid);
                pstmt.setInt(2, cityobjid);
                pstmt.executeUpdate();
                rs =  pstmt.getGeneratedKeys();
                if(rs.next()){
                    parentid = rs.getInt(1);
                }

                insertSql = "INSERT INTO thematic_surface (id,objectclass_id, building_id, lod2_multi_surface_id) VALUES (?,?,?,?);";
                pstmt = srcConn.prepareStatement(insertSql);
                pstmt.setInt(1, cityobjid);
                pstmt.setInt(2, this.objectClassid);
                pstmt.setInt(3, buildingid);
                pstmt.setInt(4, parentid);
                pstmt.executeUpdate();

                List<Integer[]> groundList = entry.getValue();
                for (int i = 0; i< groundList.size(); i++) {
                    surfaceid = groundList.get(i)[0];

                    upSql = "UPDATE surface_geometry SET cityobject_id = " + cityobjid + ", parent_id= " + parentid + " WHERE id = " + surfaceid + ";";  
                    pstmt = srcConn.prepareStatement(upSql); 
                    pstmt.executeUpdate();

                }
            }
             
        }catch (SQLException e) {
            LOGGER.fatal("Error connecting to source database: " + e);
            throw new JPSRuntimeException("Error connecting to source database: " + e);
        }
        // this.srcConn.close();
    }

    /**
     * 1. insert footprint as a new surface in surface_geometry
     * 2. update new surface id in building (lod0_XXXXprint_id)
     */
    public void updatePrint(int buildingid, PGgeometry polygon, String surfaceType){
  
        String surface = null;
        if(surfaceType.equals("footprint")){
            surface = "lod0_footprint_id";
        }else if(surfaceType.equals("roofprint")){
            surface = "lod0_roofprint_id";
        }else{
            LOGGER.fatal("Insert data information miss");
            throw new JPSRuntimeException("Insert data information miss");
        }
        UUID uuid1 = UUID.randomUUID();
        UUID uuid2 = UUID.randomUUID();
        String insertSql1 = "INSERT INTO surface_geometry (gmlid) VALUES ('" + uuid1 + "');";
        
        int pSurfaceid = 0;
        int surfaceid = 0;
       
        try {
             if(this.pool == null){
                this.pool = new SqlConnectionPool(this.config);
            }
            if (this.srcConn == null) {
                this.srcConn = this.pool.get3DConnection();
            }

            PreparedStatement stmt = srcConn.prepareStatement(insertSql1, new String[] {"id"});
            stmt.executeUpdate();
            
            ResultSet rsParent =  stmt.getGeneratedKeys();
            if(rsParent.next()){
                pSurfaceid = rsParent.getInt(1);
            }
            if(pSurfaceid != 0){
                String insertSql2 = "INSERT INTO surface_geometry (gmlid, parent_id, root_id, geometry) VALUES (?, ?, ?, ?)"; 
                stmt = srcConn.prepareStatement(insertSql2, new String[] {"id"});
                stmt.setString(1, uuid2.toString());
                stmt.setInt(2, pSurfaceid);
                stmt.setInt(3, pSurfaceid);
                stmt.setObject(4, polygon);
    
                stmt.executeUpdate();
                
                ResultSet rs =  stmt.getGeneratedKeys();
                if(rs.next()){
                    surfaceid = rs.getInt(1);
                }

                String upSql = "UPDATE building SET " + surface + " = " + surfaceid + " WHERE id = " + buildingid + ";";
                Statement upStmt = srcConn.createStatement();
                upStmt.executeUpdate(upSql);
                    
            }
                      
        } catch (SQLException e) {
            LOGGER.fatal("Error connecting to source database: " + e);
            throw new JPSRuntimeException("Error connecting to source database: " + e);
        }    
        
    }

    
    
}
