package uk.ac.cam.cares.ogm.models.geo.test;

import org.citydb.database.adapter.blazegraph.SchemaManagerAdapter;
import org.junit.jupiter.api.Test;
import uk.ac.cam.cares.ogm.models.FieldAnnotation;
import uk.ac.cam.cares.ogm.models.Model;
import uk.ac.cam.cares.ogm.models.ModelAnnotation;
import uk.ac.cam.cares.ogm.models.ModelContext;
import uk.ac.cam.cares.ogm.models.geo.GeometryType;
import uk.ac.cam.cares.ogm.models.geo.SurfaceGeometry;

import java.lang.reflect.Field;
import java.math.BigInteger;
import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

public class SurfaceGeometryTest {

    @Test
    public void testNewSurfaceGeometryAnnotations() throws NoSuchFieldException {

        SurfaceGeometry surfaceGeometry = new SurfaceGeometry();

        assertEquals(SchemaManagerAdapter.SURFACE_GEOMETRY_GRAPH + "/", surfaceGeometry.getClass().getAnnotation(ModelAnnotation.class).defaultGraphName());

        Field cityObjectId = surfaceGeometry.getClass().getDeclaredField("cityObjectId");
        assertEquals(SchemaManagerAdapter.ONTO_CITY_OBJECT_ID, cityObjectId.getAnnotation(FieldAnnotation.class).value());
        assertFalse(cityObjectId.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", cityObjectId.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, cityObjectId.getAnnotation(FieldAnnotation.class).innerType());

        Field implicitGeometryType = surfaceGeometry.getClass().getDeclaredField("implicitGeometryType");
        assertEquals(SchemaManagerAdapter.ONTO_GEOMETRY_IMPLICIT, implicitGeometryType.getAnnotation(FieldAnnotation.class).value());
        assertFalse(implicitGeometryType.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", implicitGeometryType.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, implicitGeometryType.getAnnotation(FieldAnnotation.class).innerType());

        Field isComposite = surfaceGeometry.getClass().getDeclaredField("isComposite");
        assertEquals(SchemaManagerAdapter.ONTO_IS_COMPOSITE, isComposite.getAnnotation(FieldAnnotation.class).value());
        assertFalse(isComposite.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", isComposite.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, isComposite.getAnnotation(FieldAnnotation.class).innerType());

        Field isReverse = surfaceGeometry.getClass().getDeclaredField("isReverse");
        assertEquals(SchemaManagerAdapter.ONTO_IS_REVERSE, isReverse.getAnnotation(FieldAnnotation.class).value());
        assertFalse(isReverse.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", isReverse.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, isReverse.getAnnotation(FieldAnnotation.class).innerType());

        Field isSolid = surfaceGeometry.getClass().getDeclaredField("isSolid");
        assertEquals(SchemaManagerAdapter.ONTO_IS_SOLID, isSolid.getAnnotation(FieldAnnotation.class).value());
        assertFalse(isSolid.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", isSolid.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, isSolid.getAnnotation(FieldAnnotation.class).innerType());

        Field isTriangulated = surfaceGeometry.getClass().getDeclaredField("isTriangulated");
        assertEquals(SchemaManagerAdapter.ONTO_IS_TRIANGULATED, isTriangulated.getAnnotation(FieldAnnotation.class).value());
        assertFalse(isTriangulated.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", isTriangulated.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, isTriangulated.getAnnotation(FieldAnnotation.class).innerType());

        Field isXlink = surfaceGeometry.getClass().getDeclaredField("isXlink");
        assertEquals(SchemaManagerAdapter.ONTO_IS_XLINK, isXlink.getAnnotation(FieldAnnotation.class).value());
        assertFalse(isXlink.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", isXlink.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, isXlink.getAnnotation(FieldAnnotation.class).innerType());

        Field parentId = surfaceGeometry.getClass().getDeclaredField("parentId");
        assertEquals(SchemaManagerAdapter.ONTO_PARENT_ID, parentId.getAnnotation(FieldAnnotation.class).value());
        assertFalse(parentId.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", parentId.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, parentId.getAnnotation(FieldAnnotation.class).innerType());

        Field rootId = surfaceGeometry.getClass().getDeclaredField("rootId");
        assertEquals(SchemaManagerAdapter.ONTO_ROOT_ID, rootId.getAnnotation(FieldAnnotation.class).value());
        assertFalse(rootId.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", rootId.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, rootId.getAnnotation(FieldAnnotation.class).innerType());

        Field solidType = surfaceGeometry.getClass().getDeclaredField("solidType");
        assertEquals(SchemaManagerAdapter.ONTO_GEOMETRY_SOLID, solidType.getAnnotation(FieldAnnotation.class).value());
        assertFalse(solidType.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", solidType.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, solidType.getAnnotation(FieldAnnotation.class).innerType());

        Field geometryType = surfaceGeometry.getClass().getDeclaredField("geometryType");
        assertEquals(SchemaManagerAdapter.ONTO_GEOMETRY, geometryType.getAnnotation(FieldAnnotation.class).value());
        assertFalse(geometryType.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", geometryType.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, geometryType.getAnnotation(FieldAnnotation.class).innerType());

        Field gmlId = surfaceGeometry.getClass().getDeclaredField("gmlId");
        assertEquals(SchemaManagerAdapter.ONTO_GML_ID, gmlId.getAnnotation(FieldAnnotation.class).value());
        assertFalse(gmlId.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", gmlId.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(Model.class, gmlId.getAnnotation(FieldAnnotation.class).innerType());

        Field childGeometries = surfaceGeometry.getClass().getDeclaredField("childGeometries");
        assertEquals(SchemaManagerAdapter.ONTO_PARENT_ID, childGeometries.getAnnotation(FieldAnnotation.class).value());
        assertTrue(childGeometries.getAnnotation(FieldAnnotation.class).backward());
        assertEquals("", childGeometries.getAnnotation(FieldAnnotation.class).graphName());
        assertEquals(SurfaceGeometry.class, childGeometries.getAnnotation(FieldAnnotation.class).innerType());

    }

    @Test
    public void testNewSurfaceGeometryMethods() throws NoSuchMethodException {

        SurfaceGeometry surfaceGeometry = new SurfaceGeometry();

        assertNotNull(surfaceGeometry.getClass().getDeclaredMethod("getCityObjectId"));
        assertNotNull(surfaceGeometry.getClass().getDeclaredMethod("setCityObjectId", URI.class));

        assertNotNull(surfaceGeometry.getClass().getDeclaredMethod("getImplicitGeometryType"));
        assertNotNull(surfaceGeometry.getClass().getDeclaredMethod("setImplicitGeometryType", String.class));

        assertNotNull(surfaceGeometry.getClass().getDeclaredMethod("getIsComposite"));
        assertNotNull(surfaceGeometry.getClass().getDeclaredMethod("setIsComposite", BigInteger.class));

        assertNotNull(surfaceGeometry.getClass().getDeclaredMethod("getIsReverse"));
        assertNotNull(surfaceGeometry.getClass().getDeclaredMethod("setIsReverse", BigInteger.class));

        assertNotNull(surfaceGeometry.getClass().getDeclaredMethod("getIsSolid"));
        assertNotNull(surfaceGeometry.getClass().getDeclaredMethod("setIsSolid", BigInteger.class));

        assertNotNull(surfaceGeometry.getClass().getDeclaredMethod("getIsTriangulated"));
        assertNotNull(surfaceGeometry.getClass().getDeclaredMethod("setIsTriangulated", BigInteger.class));

        assertNotNull(surfaceGeometry.getClass().getDeclaredMethod("getIsXlink"));
        assertNotNull(surfaceGeometry.getClass().getDeclaredMethod("setIsXlink", BigInteger.class));

        assertNotNull(surfaceGeometry.getClass().getDeclaredMethod("getParentId"));
        assertNotNull(surfaceGeometry.getClass().getDeclaredMethod("setParentId", SurfaceGeometry.class));

        assertNotNull(surfaceGeometry.getClass().getDeclaredMethod("getRootId"));
        assertNotNull(surfaceGeometry.getClass().getDeclaredMethod("setRootId", URI.class));

        assertNotNull(surfaceGeometry.getClass().getDeclaredMethod("getSolidType"));
        assertNotNull(surfaceGeometry.getClass().getDeclaredMethod("setSolidType", String.class));

        assertNotNull(surfaceGeometry.getClass().getDeclaredMethod("getGeometryType"));
        assertNotNull(surfaceGeometry.getClass().getDeclaredMethod("setGeometryType", GeometryType.class));

        assertNotNull(surfaceGeometry.getClass().getDeclaredMethod("getGmlId"));
        assertNotNull(surfaceGeometry.getClass().getDeclaredMethod("setGmlId", String.class));

        assertNotNull(surfaceGeometry.getClass().getDeclaredMethod("getChildGeometries"));
        assertNotNull(surfaceGeometry.getClass().getDeclaredMethod("setChildGeometries", ArrayList.class));
    }

    @Test
    public void testgetFlattenedSubtreeOne(){

       ModelContext context = new ModelContext("", "");


        GeometryType.setSourceCrsName("EPSG:27700");
        /*            root
         *           /    \
         *      walls      roofAndGround
         *      /   \       /     \     \
         *  wall1  wall2  roofs ground1 ground2
         *               /    \
         *           roof1   roof2
         */
        String prefix = "http://localhost:9999/blazegraph/namespace/test/surfacegeometry/";
        SurfaceGeometry root = context.createNewModel(SurfaceGeometry.class, prefix + "root_uuid");
        SurfaceGeometry walls = context.createNewModel(SurfaceGeometry.class, prefix + "walls_uuid");
        SurfaceGeometry wall1 = context.createNewModel(SurfaceGeometry.class, prefix + "wall1_uuid");
        SurfaceGeometry wall2 = context.createNewModel(SurfaceGeometry.class, prefix + "wall2_uuid");
        SurfaceGeometry roofAndGround = context.createNewModel(SurfaceGeometry.class, prefix + "roofandground_uuid");
        SurfaceGeometry roofs = context.createNewModel(SurfaceGeometry.class, prefix + "roofs_uuid");
        SurfaceGeometry roof1 = context.createNewModel(SurfaceGeometry.class, prefix + "roof1_uuid");
        SurfaceGeometry roof2 = context.createNewModel(SurfaceGeometry.class, prefix + "roof2_uuid");
        SurfaceGeometry ground1 = context.createNewModel(SurfaceGeometry.class, prefix + "ground1_uuid");
        SurfaceGeometry ground2 = context.createNewModel(SurfaceGeometry.class, prefix + "ground2_uuid");
        wall1.setGeometryType(new GeometryType("0#0#0#0#0#1#1#0#1#1#0#0#0#0#0", "http://localhost/blazegraph/literals/POLYGON-3-15"));
        wall2.setGeometryType(new GeometryType("0#1#0#1#1#0#1#1#1#0#1#1#0#1#0", "http://localhost/blazegraph/literals/POLYGON-3-15"));
        roof1.setGeometryType(new GeometryType("0#0#1#1#0#1#1#1#1#0#1#1#0#0#1", "http://localhost/blazegraph/literals/POLYGON-3-15"));
        roof2.setGeometryType(new GeometryType("0#0#2#1#0#2#1#1#2#0#1#2#0#0#2", "http://localhost/blazegraph/literals/POLYGON-3-15"));
        ground1.setGeometryType(new GeometryType("0#0#0#0#1#0#1#1#0#1#0#0#0#0#0", "http://localhost/blazegraph/literals/POLYGON-3-15"));
        ground2.setGeometryType(new GeometryType("0#0#-1#0#1#-1#1#1#-1#1#0#-1#0#0#-1", "http://localhost/blazegraph/literals/POLYGON-3-15"));
        root.getChildGeometries().addAll(Arrays.asList(walls, roofAndGround));
        walls.getChildGeometries().addAll(Arrays.asList(wall1, wall2));
        roofAndGround.getChildGeometries().addAll(Arrays.asList(roofs, ground1, ground2));
        roofs.getChildGeometries().addAll(Arrays.asList(roof1, roof2));

        List<SurfaceGeometry> outputList = new ArrayList<>();

        outputList = root.getFlattenedSubtree(true);
        assertTrue(outputList.stream().anyMatch(object -> object.getIri().equals(prefix + "wall1_uuid")));
        assertTrue(outputList.stream().anyMatch(object -> object.getIri().equals(prefix + "wall2_uuid")));
        assertTrue(outputList.stream().anyMatch(object -> object.getIri().equals(prefix + "roof1_uuid")));
        assertTrue(outputList.stream().anyMatch(object -> object.getIri().equals(prefix + "roof2_uuid")));
        assertTrue(outputList.stream().anyMatch(object -> object.getIri().equals(prefix + "ground1_uuid")));
        assertTrue(outputList.stream().anyMatch(object -> object.getIri().equals(prefix + "ground2_uuid")));
        assertTrue(outputList.stream().anyMatch(object -> object.getIri().equals(prefix + "wall1_uuid")));
        assertFalse(outputList.stream().anyMatch(object -> object.getIri().equals(prefix + "walls_uuid")));
        assertFalse(outputList.stream().anyMatch(object -> object.getIri().equals(prefix + "root_uuid")));
        assertFalse(outputList.stream().anyMatch(object -> object.getIri().equals(prefix + "roofandground_uuid")));
        assertFalse(outputList.stream().anyMatch(object -> object.getIri().equals(prefix + "roofs_uuid")));

        outputList.clear();

        outputList = root.getFlattenedSubtree(false);
        assertTrue(outputList.stream().anyMatch(object -> object.getIri().equals(prefix + "wall1_uuid")));
        assertTrue(outputList.stream().anyMatch(object -> object.getIri().equals(prefix + "wall2_uuid")));
        assertTrue(outputList.stream().anyMatch(object -> object.getIri().equals(prefix + "roof1_uuid")));
        assertTrue(outputList.stream().anyMatch(object -> object.getIri().equals(prefix + "roof2_uuid")));
        assertTrue(outputList.stream().anyMatch(object -> object.getIri().equals(prefix + "ground1_uuid")));
        assertTrue(outputList.stream().anyMatch(object -> object.getIri().equals(prefix + "ground2_uuid")));
        assertTrue(outputList.stream().anyMatch(object -> object.getIri().equals(prefix + "wall1_uuid")));
        assertTrue(outputList.stream().anyMatch(object -> object.getIri().equals(prefix + "walls_uuid")));
        assertTrue(outputList.stream().anyMatch(object -> object.getIri().equals(prefix + "root_uuid")));
        assertTrue(outputList.stream().anyMatch(object -> object.getIri().equals(prefix + "roofandground_uuid")));
        assertTrue(outputList.stream().anyMatch(object -> object.getIri().equals(prefix + "roofs_uuid")));
    }

    @Test
    public void testgetFlattenedSubtreeTwo(){

        SurfaceGeometry surfaceGeometry = new SurfaceGeometry();
        ModelContext context = new ModelContext("", "");


        GeometryType.setSourceCrsName("EPSG:27700");
        /*            root
         *           /    \
         *      walls      roofAndGround
         *      /   \       /     \     \
         *  wall1  wall2  roofs ground1 ground2
         *               /    \
         *           roof1   roof2
         */
        String prefix = "http://localhost:9999/blazegraph/namespace/test/surfacegeometry/";
        SurfaceGeometry root = context.createNewModel(SurfaceGeometry.class, prefix + "root_uuid");
        SurfaceGeometry walls = context.createNewModel(SurfaceGeometry.class, prefix + "walls_uuid");
        SurfaceGeometry wall1 = context.createNewModel(SurfaceGeometry.class, prefix + "wall1_uuid");
        SurfaceGeometry wall2 = context.createNewModel(SurfaceGeometry.class, prefix + "wall2_uuid");
        SurfaceGeometry roofAndGround = context.createNewModel(SurfaceGeometry.class, prefix + "roofandground_uuid");
        SurfaceGeometry roofs = context.createNewModel(SurfaceGeometry.class, prefix + "roofs_uuid");
        SurfaceGeometry roof1 = context.createNewModel(SurfaceGeometry.class, prefix + "roof1_uuid");
        SurfaceGeometry roof2 = context.createNewModel(SurfaceGeometry.class, prefix + "roof2_uuid");
        SurfaceGeometry ground1 = context.createNewModel(SurfaceGeometry.class, prefix + "ground1_uuid");
        SurfaceGeometry ground2 = context.createNewModel(SurfaceGeometry.class, prefix + "ground2_uuid");
        wall1.setGeometryType(new GeometryType("0#0#0#0#0#1#1#0#1#1#0#0#0#0#0", "http://localhost/blazegraph/literals/POLYGON-3-15"));
        wall2.setGeometryType(new GeometryType("0#1#0#1#1#0#1#1#1#0#1#1#0#1#0", "http://localhost/blazegraph/literals/POLYGON-3-15"));
        roof1.setGeometryType(new GeometryType("0#0#1#1#0#1#1#1#1#0#1#1#0#0#1", "http://localhost/blazegraph/literals/POLYGON-3-15"));
        roof2.setGeometryType(new GeometryType("0#0#2#1#0#2#1#1#2#0#1#2#0#0#2", "http://localhost/blazegraph/literals/POLYGON-3-15"));
        ground1.setGeometryType(new GeometryType("0#0#0#0#1#0#1#1#0#1#0#0#0#0#0", "http://localhost/blazegraph/literals/POLYGON-3-15"));
        ground2.setGeometryType(new GeometryType("0#0#-1#0#1#-1#1#1#-1#1#0#-1#0#0#-1", "http://localhost/blazegraph/literals/POLYGON-3-15"));
        root.getChildGeometries().addAll(Arrays.asList(walls, roofAndGround));
        walls.getChildGeometries().addAll(Arrays.asList(wall1, wall2));
        roofAndGround.getChildGeometries().addAll(Arrays.asList(roofs, ground1, ground2));
        roofs.getChildGeometries().addAll(Arrays.asList(roof1, roof2));

        List<SurfaceGeometry> outputList = new ArrayList<>();

        root.getFlattenedSubtree(outputList, true);
        assertTrue(outputList.stream().anyMatch(object -> object.getIri().equals(prefix + "wall1_uuid")));
        assertTrue(outputList.stream().anyMatch(object -> object.getIri().equals(prefix + "wall2_uuid")));
        assertTrue(outputList.stream().anyMatch(object -> object.getIri().equals(prefix + "roof1_uuid")));
        assertTrue(outputList.stream().anyMatch(object -> object.getIri().equals(prefix + "roof2_uuid")));
        assertTrue(outputList.stream().anyMatch(object -> object.getIri().equals(prefix + "ground1_uuid")));
        assertTrue(outputList.stream().anyMatch(object -> object.getIri().equals(prefix + "ground2_uuid")));
        assertTrue(outputList.stream().anyMatch(object -> object.getIri().equals(prefix + "wall1_uuid")));
        assertFalse(outputList.stream().anyMatch(object -> object.getIri().equals(prefix + "walls_uuid")));
        assertFalse(outputList.stream().anyMatch(object -> object.getIri().equals(prefix + "root_uuid")));
        assertFalse(outputList.stream().anyMatch(object -> object.getIri().equals(prefix + "roofandground_uuid")));
        assertFalse(outputList.stream().anyMatch(object -> object.getIri().equals(prefix + "roofs_uuid")));

        outputList.clear();

        root.getFlattenedSubtree(outputList, false);
        assertTrue(outputList.stream().anyMatch(object -> object.getIri().equals(prefix + "wall1_uuid")));
        assertTrue(outputList.stream().anyMatch(object -> object.getIri().equals(prefix + "wall2_uuid")));
        assertTrue(outputList.stream().anyMatch(object -> object.getIri().equals(prefix + "roof1_uuid")));
        assertTrue(outputList.stream().anyMatch(object -> object.getIri().equals(prefix + "roof2_uuid")));
        assertTrue(outputList.stream().anyMatch(object -> object.getIri().equals(prefix + "ground1_uuid")));
        assertTrue(outputList.stream().anyMatch(object -> object.getIri().equals(prefix + "ground2_uuid")));
        assertTrue(outputList.stream().anyMatch(object -> object.getIri().equals(prefix + "wall1_uuid")));
        assertTrue(outputList.stream().anyMatch(object -> object.getIri().equals(prefix + "walls_uuid")));
        assertTrue(outputList.stream().anyMatch(object -> object.getIri().equals(prefix + "root_uuid")));
        assertTrue(outputList.stream().anyMatch(object -> object.getIri().equals(prefix + "roofandground_uuid")));
        assertTrue(outputList.stream().anyMatch(object -> object.getIri().equals(prefix + "roofs_uuid")));
    }
}

