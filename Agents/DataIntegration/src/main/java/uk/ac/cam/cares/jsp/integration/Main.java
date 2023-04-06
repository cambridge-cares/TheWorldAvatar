package uk.ac.cam.cares.jsp.integration;

import java.sql.SQLException;
import java.util.List;

public class Main {
    public static void main(String[] args) {

        String dburl = "jdbc:postgresql://localhost:5432/sg_2D";
        String dbuser = "postgres";
        String dbpassword = "123456";
        GeoObject3D object3D = new GeoObject3D();
        GeoObject2D object2D = new GeoObject2D();
        PostgresClient conn = new PostgresClient(dburl, dbuser, dbpassword);
        try {
            conn.getConnection();
            System.out.println("Connected to the PostgreSQL server successfully.");
            object2D.setPostGISClient(conn);
            List<GeoObject2D> allObject2D = object2D.getObject2D();
            System.out.println(allObject2D.size());
        } catch (SQLException e) {
            System.out.println(e.getMessage());
        }
    }
}