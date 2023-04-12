package uk.ac.cam.cares.jsp.integration;

import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;
import org.geotools.geometry.jts.JTSFactoryFinder;
import org.geotools.referencing.CRS;
import org.geotools.geometry.jts.JTS;
import org.locationtech.jts.geom.Geometry;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.geom.MultiPolygon;
import org.locationtech.jts.geom.Polygon;
import org.locationtech.jts.io.ParseException;
import org.locationtech.jts.io.WKTReader;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.crs.CoordinateReferenceSystem;
import org.opengis.referencing.operation.MathTransform;
import org.opengis.referencing.operation.TransformException;

import java.util.ArrayList;
import java.util.List;

public class SpatialLink {

    private static final Logger LOGGER = LogManager.getLogger(SpatialLink.class);
    private PostgresClient postgresClient;

    List<GeoObject3D> allObject3D = new ArrayList<>();
    List<GeoObject2D> allObject2D = new ArrayList<>();

    public SpatialLink() {}

    public void findMatchedObjects() throws ParseException, FactoryException, TransformException {
//        List<GeoObject2D> allObject2D = object2D.getObject2D();
//        List<GeoObject3D> allObject3D = object3D.getObject3D();
        GeometryFactory geometryFactory = JTSFactoryFinder.getGeometryFactory();
        WKTReader reader = new WKTReader( geometryFactory );

        for (int i = 0; i < this.allObject3D.size(); i++) {
            GeoObject3D object3D = this.allObject3D.get(i);
            int srid3D = object3D.getEnvelope().getGeometry().getSrid();
            String geom3 = object3D.getEnvelope().toString();
            geom3 = geom3.split(";")[1];
            Polygon envelope = (Polygon) reader.read(geom3);
            System.out.println(envelope);
            for (int j = 0; j < this.allObject2D.size(); j++) {
                GeoObject2D object2D = this.allObject2D.get(j);
                int srid2D = object2D.getGeometry2D().getGeometry().getSrid();
                String geom2D = object2D.getGeometry2D().toString();
                geom2D = geom2D.split(";")[1];
                MultiPolygon polygon = (MultiPolygon) reader.read(geom2D);
                System.out.println(polygon);

                if(srid3D != srid2D){
                    CoordinateReferenceSystem CRS2D = CRS.decode("EPSG:" + srid2D);
                    CoordinateReferenceSystem CRS3D = CRS.decode("EPSG:" + srid3D);
                    MathTransform tr = CRS.findMathTransform(CRS3D, CRS2D);
                    Geometry newEnvelope = JTS.transform(envelope, tr);
                    envelope = (Polygon) reader.read(newEnvelope.toString());
                }

                if (!polygon.within(envelope)){
                    Geometry intersect = polygon.intersection(envelope);
                    double areaRatio = 100.0*intersect.getArea() / envelope.getArea();
                    System.out.println("ratio: "+areaRatio + "%");
                    if(areaRatio>70){
                        object3D.setName(object2D.getName());
                    }
                }else{
                    object3D.setName(object2D.getName());
                }

                object3D.updateName(object3D);
            }
        }

    }

}
