package uk.ac.cam.cares.jps.ship.model.test;

import com.fasterxml.jackson.annotation.JsonManagedReference;
import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.ship.model.ShipDetailsEntity;
import uk.ac.cam.cares.jps.ship.model.ShipEntity;
import uk.ac.cam.cares.jps.ship.model.ShipPollutionEntity;

import javax.persistence.*;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collection;

import static org.junit.Assert.assertNotEquals;

public class ShipEntityTest extends TestCase {
    private static final int TST_INT = 123;
    private static final String TST_STR = "123";
    private static final Integer TST_INTO = 321;

    public void testNewShipEntity() {
        ShipEntity ship = null;
        try {
            ship = new ShipEntity();
        } catch (Exception e) {
            throw new JPSRuntimeException(e);
        } finally {
            assertNotNull(ship);
            assertEquals(ship, new ShipEntity());
            assertEquals(2, ship.getClass().getAnnotations().length);
            assertTrue(ship.getClass().isAnnotationPresent(Entity.class));
            assertTrue(ship.getClass().isAnnotationPresent(Table.class));
            assertEquals("ship", ship.getClass().getAnnotation(Table.class).name());
            assertEquals("public", ship.getClass().getAnnotation(Table.class).schema());
            assertEquals("adms_ships", ship.getClass().getAnnotation(Table.class).catalog());
        }
    }

    public void testNewShipEntityFields() {
        ShipEntity ship = new ShipEntity();
        Field[] fields = ship.getClass().getDeclaredFields();
        assertEquals(11, ship.getClass().getDeclaredFields().length);

        for (Field field: fields) {
            assertEquals(2, field.getAnnotations().length);
            String name = field.getName();
            switch (name) {
                case "mmsi":
                    assertTrue(field.isAnnotationPresent(Id.class));
                    assertTrue(field.isAnnotationPresent(Column.class));
                    assertEquals("mmsi", field.getAnnotation(Column.class).name());
                    assertFalse(field.getAnnotation(Column.class).nullable());
                    break;
                case "imo":
                    assertTrue(field.isAnnotationPresent(Basic.class));
                    assertTrue(field.isAnnotationPresent(Column.class));
                    assertEquals("imo", field.getAnnotation(Column.class).name());
                    break;
                case "name":
                    assertTrue(field.isAnnotationPresent(Basic.class));
                    assertTrue(field.isAnnotationPresent(Column.class));
                    assertEquals("name", field.getAnnotation(Column.class).name());
                    assertEquals(-1, field.getAnnotation(Column.class).length());
                    break;
                case "type":
                    assertTrue(field.isAnnotationPresent(Basic.class));
                    assertTrue(field.isAnnotationPresent(Column.class));
                    assertEquals("type", field.getAnnotation(Column.class).name());
                    assertEquals(-1, field.getAnnotation(Column.class).length());
                    break;
                case "y":
                    assertTrue(field.isAnnotationPresent(Basic.class));
                    assertTrue(field.isAnnotationPresent(Column.class));
                    assertEquals("y", field.getAnnotation(Column.class).name());
                    break;
                case "country":
                    assertTrue(field.isAnnotationPresent(Basic.class));
                    assertTrue(field.isAnnotationPresent(Column.class));
                    assertEquals("country", field.getAnnotation(Column.class).name());
                    assertEquals(-1, field.getAnnotation(Column.class).length());
                    break;
                case "al":
                    assertTrue(field.isAnnotationPresent(Basic.class));
                    assertTrue(field.isAnnotationPresent(Column.class));
                    assertEquals("al", field.getAnnotation(Column.class).name());
                    break;
                case "gt":
                    assertTrue(field.isAnnotationPresent(Basic.class));
                    assertTrue(field.isAnnotationPresent(Column.class));
                    assertEquals("gt", field.getAnnotation(Column.class).name());
                    break;
                case "aw":
                    assertTrue(field.isAnnotationPresent(Basic.class));
                    assertTrue(field.isAnnotationPresent(Column.class));
                    assertEquals("aw", field.getAnnotation(Column.class).name());
                    break;
                case "shipDetailsByMmsi":
                    assertTrue(field.isAnnotationPresent(JsonManagedReference.class));
                    assertTrue(field.isAnnotationPresent(OneToMany.class));
                    assertEquals(CascadeType.ALL, field.getAnnotation(OneToMany.class).cascade()[0]);
                    assertEquals(FetchType.LAZY, field.getAnnotation(OneToMany.class).fetch());
                    assertEquals("shipByShipMmsi", field.getAnnotation(OneToMany.class).mappedBy());
                    break;
                case "shipPollutionByMmsi":
                    assertTrue(field.isAnnotationPresent(JsonManagedReference.class));
                    assertTrue(field.isAnnotationPresent(OneToOne.class));
                    assertEquals(CascadeType.ALL, field.getAnnotation(OneToOne.class).cascade()[0]);
                    assertEquals(FetchType.LAZY, field.getAnnotation(OneToOne.class).fetch());
                    assertEquals("shipByShipMmsi", field.getAnnotation(OneToOne.class).mappedBy());
                    break;
                default:
                    throw new JPSRuntimeException("Field name not recognised.");
            }
        }
    }

    public void testNewShipEntityMethods() {
        ShipEntity ship = new ShipEntity();
        try {
            ship.setMmsi(TST_INT);
            ship.setImo(TST_INTO);
            ship.setName(TST_STR);
            ship.setType(TST_STR);
            ship.setY(TST_INTO);
            ship.setCountry(TST_STR);
            ship.setAl(TST_INTO);
            ship.setGt(TST_INTO);
            ship.setAw(TST_INTO);
            ShipDetailsEntity sd = new ShipDetailsEntity();
            Collection<ShipDetailsEntity> sdCol = new ArrayList<>();
            sdCol.add(sd);
            ship.setShipDetailsByMmsi(sdCol);
        } catch (Exception e) {
            throw new JPSRuntimeException(e);
        } finally {
            assertEquals(TST_INT, ship.getMmsi());
            assertEquals(TST_INTO, ship.getImo());
            assertEquals(TST_STR, ship.getName());
            assertEquals(TST_STR, ship.getType());
            assertEquals(TST_INTO, ship.getY());
            assertEquals(TST_STR, ship.getCountry());
            assertEquals(TST_INTO, ship.getAl());
            assertEquals(TST_INTO, ship.getGt());
            assertEquals(TST_INTO, ship.getAw());
            assertNotNull(ship.getShipDetailsByMmsi());
            assertEquals(1, ship.getShipDetailsByMmsi().size());
            assertEquals(ship.getShipDetailsByMmsi().toArray()[0], new ShipDetailsEntity());
            assertNotNull(ship.getShipPollutionByMmsi());
            assertEquals(ship.getShipPollutionByMmsi(), new ShipPollutionEntity());
            assertNotEquals(ship, new ShipEntity());
            assertEquals(-1452651190, ship.hashCode());
        }
    }
}
