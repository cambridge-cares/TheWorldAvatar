package uk.ac.cam.cares.jps.ship.model.test;

import com.fasterxml.jackson.annotation.JsonBackReference;
import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.ship.model.ShipDetailsEntity;
import uk.ac.cam.cares.jps.ship.model.ShipEntity;

import javax.persistence.*;
import java.lang.reflect.Field;

import static org.junit.Assert.assertNotEquals;

public class ShipDetailsEntityTest extends TestCase {
    private static final int TST_INT = 123;
    private static final String TST_STR = "123";
    private static final Integer TST_INTO = 321;
    private static final Double TST_DOUBO = 456d;
    private static final Boolean TST_BOOL = true;

    public void testNewShipDetailsEntity() {
        ShipDetailsEntity sd = null;
        try {
            sd = new ShipDetailsEntity();
        } catch (Exception e) {
            throw new JPSRuntimeException(e);
        } finally {
            assertNotNull(sd);
            assertEquals(sd, new ShipDetailsEntity());
            assertEquals(2, sd.getClass().getAnnotations().length);
            assertTrue(sd.getClass().isAnnotationPresent(Entity.class));
            assertTrue(sd.getClass().isAnnotationPresent(Table.class));
            assertEquals("ship_details", sd.getClass().getAnnotation(Table.class).name());
            assertEquals("public", sd.getClass().getAnnotation(Table.class).schema());
            assertEquals("adms_ships", sd.getClass().getAnnotation(Table.class).catalog());
        }
    }

    public void testNewShipDetailsEntityFields() {
        ShipDetailsEntity sd = new ShipDetailsEntity();
        Field[] fields = sd.getClass().getDeclaredFields();
        assertEquals(17, sd.getClass().getDeclaredFields().length);

        for (Field field: fields) {
            String name = field.getName();
            switch (name) {
                case "id":
                    assertEquals(4, field.getAnnotations().length);
                    assertTrue(field.isAnnotationPresent(Id.class));
                    assertTrue(field.isAnnotationPresent(Column.class));
                    assertTrue(field.isAnnotationPresent(SequenceGenerator.class));
                    assertTrue(field.isAnnotationPresent(GeneratedValue.class));
                    assertEquals("id", field.getAnnotation(Column.class).name());
                    assertFalse(field.getAnnotation(Column.class).nullable());
                    assertEquals("mySeq", field.getAnnotation(SequenceGenerator.class).name());
                    assertEquals("MY_SEQ", field.getAnnotation(SequenceGenerator.class).sequenceName());
                    assertEquals(1, field.getAnnotation(SequenceGenerator.class).allocationSize());
                    assertEquals(1, field.getAnnotation(SequenceGenerator.class).initialValue());
                    assertEquals(GenerationType.IDENTITY, field.getAnnotation(GeneratedValue.class).strategy());
                    assertEquals("mySeq", field.getAnnotation(GeneratedValue.class).generator());
                    break;
                case "dest":
                    assertEquals(2, field.getAnnotations().length);
                    assertTrue(field.isAnnotationPresent(Basic.class));
                    assertTrue(field.isAnnotationPresent(Column.class));
                    assertEquals("dest", field.getAnnotation(Column.class).name());
                    assertEquals(-1, field.getAnnotation(Column.class).length());
                    break;
                case "ss":
                    assertEquals(2, field.getAnnotations().length);
                    assertTrue(field.isAnnotationPresent(Basic.class));
                    assertTrue(field.isAnnotationPresent(Column.class));
                    assertEquals("ss", field.getAnnotation(Column.class).name());
                    break;
                case "cu":
                    assertEquals(2, field.getAnnotations().length);
                    assertTrue(field.isAnnotationPresent(Basic.class));
                    assertTrue(field.isAnnotationPresent(Column.class));
                    assertEquals("cu", field.getAnnotation(Column.class).name());
                    break;
                case "dw":
                    assertEquals(2, field.getAnnotations().length);
                    assertTrue(field.isAnnotationPresent(Basic.class));
                    assertTrue(field.isAnnotationPresent(Column.class));
                    assertEquals("dw", field.getAnnotation(Column.class).name());
                    break;
                case "draught":
                    assertEquals(2, field.getAnnotations().length);
                    assertTrue(field.isAnnotationPresent(Basic.class));
                    assertTrue(field.isAnnotationPresent(Column.class));
                    assertEquals("draught", field.getAnnotation(Column.class).name());
                    break;
                case "lat":
                    assertEquals(2, field.getAnnotations().length);
                    assertTrue(field.isAnnotationPresent(Basic.class));
                    assertTrue(field.isAnnotationPresent(Column.class));
                    assertEquals("lat", field.getAnnotation(Column.class).name());
                    break;
                case "lon":
                    assertEquals(2, field.getAnnotations().length);
                    assertTrue(field.isAnnotationPresent(Basic.class));
                    assertTrue(field.isAnnotationPresent(Column.class));
                    assertEquals("lon", field.getAnnotation(Column.class).name());
                    break;
                case "r":
                    assertEquals(2, field.getAnnotations().length);
                    assertTrue(field.isAnnotationPresent(Basic.class));
                    assertTrue(field.isAnnotationPresent(Column.class));
                    assertEquals("r", field.getAnnotation(Column.class).name());
                    break;
                case "lc":
                    assertEquals(2, field.getAnnotations().length);
                    assertTrue(field.isAnnotationPresent(Basic.class));
                    assertTrue(field.isAnnotationPresent(Column.class));
                    assertEquals("lc", field.getAnnotation(Column.class).name());
                    break;
                case "sl":
                    assertEquals(2, field.getAnnotations().length);
                    assertTrue(field.isAnnotationPresent(Basic.class));
                    assertTrue(field.isAnnotationPresent(Column.class));
                    assertEquals("sl", field.getAnnotation(Column.class).name());
                    break;
                case "sc":
                    assertEquals(2, field.getAnnotations().length);
                    assertTrue(field.isAnnotationPresent(Basic.class));
                    assertTrue(field.isAnnotationPresent(Column.class));
                    assertEquals("sc", field.getAnnotation(Column.class).name());
                    break;
                case "heading":
                    assertEquals(2, field.getAnnotations().length);
                    assertTrue(field.isAnnotationPresent(Basic.class));
                    assertTrue(field.isAnnotationPresent(Column.class));
                    assertEquals("heading", field.getAnnotation(Column.class).name());
                    break;
                case "etats":
                    assertEquals(2, field.getAnnotations().length);
                    assertTrue(field.isAnnotationPresent(Basic.class));
                    assertTrue(field.isAnnotationPresent(Column.class));
                    assertEquals("etats", field.getAnnotation(Column.class).name());
                    break;
                case "ts":
                    assertEquals(2, field.getAnnotations().length);
                    assertTrue(field.isAnnotationPresent(Basic.class));
                    assertTrue(field.isAnnotationPresent(Column.class));
                    assertEquals("ts", field.getAnnotation(Column.class).name());
                    break;
                case "tst":
                    assertEquals(2, field.getAnnotations().length);
                    assertTrue(field.isAnnotationPresent(Basic.class));
                    assertTrue(field.isAnnotationPresent(Column.class));
                    assertEquals("tst", field.getAnnotation(Column.class).name());
                    break;
                case "shipByShipMmsi":
                    assertEquals(3, field.getAnnotations().length);
                    assertTrue(field.isAnnotationPresent(JsonBackReference.class));
                    assertTrue(field.isAnnotationPresent(ManyToOne.class));
                    assertTrue(field.isAnnotationPresent(JoinColumn.class));
                    assertEquals(FetchType.LAZY, field.getAnnotation(ManyToOne.class).fetch());
                    assertEquals("ship_mmsi", field.getAnnotation(JoinColumn.class).name());
                    assertEquals("mmsi", field.getAnnotation(JoinColumn.class).referencedColumnName());
                    break;
                default:
                    throw new JPSRuntimeException("Field name not recognised.");
            }
        }
    }

    public void testNewShipEntityMethods() {
        ShipDetailsEntity sd = new ShipDetailsEntity();
        ShipEntity ship = new ShipEntity();
        try {
            sd.setId(TST_INT);
            sd.setDest(TST_STR);
            sd.setSs(TST_DOUBO);
            sd.setCu(TST_DOUBO);
            sd.setDw(TST_INTO);
            sd.setDraught(TST_DOUBO);
            sd.setLat(TST_DOUBO);
            sd.setLon(TST_DOUBO);
            sd.setR(TST_INTO);
            sd.setLc(TST_INTO);
            sd.setSl(TST_BOOL);
            sd.setSc(TST_INTO);
            sd.setHeading(TST_INTO);
            sd.setEtats(TST_INTO);
            sd.setTs(TST_INTO);
            sd.setTst(TST_INTO);
            sd.setShipByShipMmsi(ship);
        } catch (Exception e) {
            throw new JPSRuntimeException(e);
        } finally {
            assertEquals(TST_INT, sd.getId());
            assertEquals(TST_STR, sd.getDest());
            assertEquals(TST_DOUBO, sd.getSs());
            assertEquals(TST_DOUBO, sd.getCu());
            assertEquals(TST_INTO, sd.getDw());
            assertEquals(TST_DOUBO, sd.getDraught());
            assertEquals(TST_DOUBO, sd.getLat());
            assertEquals(TST_DOUBO  , sd.getLon());
            assertEquals(TST_INTO, sd.getR());
            assertEquals(TST_INTO, sd.getLc());
            assertEquals(TST_BOOL, sd.getSl());
            assertEquals(TST_INTO, sd.getSc());
            assertEquals(TST_INTO, sd.getHeading());
            assertEquals(TST_INTO, sd.getEtats());
            assertEquals(TST_INTO, sd.getTs());
            assertEquals(TST_INTO, sd.getTst());
            assertEquals(sd.getShipByShipMmsi(), ship);
            assertNotEquals(sd, new ShipDetailsEntity());
            assertEquals(2103889337, sd.hashCode());
        }
    }
}
