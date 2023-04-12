package uk.ac.cam.cares.jsp.integration;

import org.locationtech.jts.io.ParseException;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import java.sql.SQLException;
import java.util.List;

public class Main {
    public static void main(String[] args) {

        String dburl2D = "jdbc:postgresql://localhost:5432/sg_2D";
        String dburl3D = "jdbc:postgresql://localhost:5432/sg_ntu";
        String dbuser = "postgres";
        String dbpassword = "123456";
        GeoObject3D object3D = new GeoObject3D();
        GeoObject2D object2D = new GeoObject2D();
        PostgresClient conn2 = new PostgresClient(dburl2D, dbuser, dbpassword);
        PostgresClient conn3 = new PostgresClient(dburl3D, dbuser, dbpassword);
        try {
            conn2.getConnection();
            System.out.println("Connected to the PostgreSQL server successfully.");
            object2D.setPostGISClient(conn2);
            List<GeoObject2D> allObject2D = object2D.getObject2D();
            System.out.println(allObject2D.size());
            conn3.getConnection();
            object3D.setPostGISClient(conn3);
            List<GeoObject3D> allObject3D = object3D.getObject3D();
            System.out.println(allObject3D.size());

            SpatialLink spLink = new SpatialLink();
            spLink.allObject2D = allObject2D;
            spLink.allObject3D = allObject3D;
            spLink.findMatchedObjects();
        } catch (SQLException e) {
            System.out.println(e.getMessage());
        } catch (ParseException | FactoryException | TransformException e) {
            throw new RuntimeException(e);
        }
    }
}