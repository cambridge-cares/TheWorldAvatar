package uk.ac.cam.cares.jps.ship.model.test;

import com.fasterxml.jackson.annotation.JsonBackReference;
import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.ship.model.ShipEntity;
import uk.ac.cam.cares.jps.ship.model.ShipPollutionEntity;

import javax.persistence.*;
import java.lang.reflect.Field;

import static org.junit.Assert.assertNotEquals;

public class ShipPollutionEntityTest extends TestCase {
    private static final int TST_INT = 123;
    private static final String TST_STR = "123";

    public void testNewShipPollutionEntity() {
        ShipPollutionEntity sp = null;
        try {
            sp = new ShipPollutionEntity();
        } catch (Exception e) {
            throw new JPSRuntimeException(e);
        } finally {
            assertNotNull(sp);
            assertEquals(sp, new ShipPollutionEntity());
            assertEquals(2, sp.getClass().getAnnotations().length);
            assertTrue(sp.getClass().isAnnotationPresent(Entity.class));
            assertTrue(sp.getClass().isAnnotationPresent(Table.class));
            assertEquals("ship_pollution", sp.getClass().getAnnotation(Table.class).name());
            assertEquals("public", sp.getClass().getAnnotation(Table.class).schema());
            assertEquals("adms_ships", sp.getClass().getAnnotation(Table.class).catalog());
        }
    }

    public void testNewShipPollutionEntityFields() {
        ShipPollutionEntity sp = new ShipPollutionEntity();
        Field[] fields = sp.getClass().getDeclaredFields();
        assertEquals(3, sp.getClass().getDeclaredFields().length);

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
                case "chimneyIri":
                    assertEquals(2, field.getAnnotations().length);
                    assertTrue(field.isAnnotationPresent(Basic.class));
                    assertTrue(field.isAnnotationPresent(Column.class));
                    assertEquals("chimney_iri", field.getAnnotation(Column.class).name());
                    assertEquals(-1, field.getAnnotation(Column.class).length());
                    assertTrue(field.getAnnotation(Column.class).nullable());
                    break;
                case "shipByShipMmsi":
                    assertEquals(3, field.getAnnotations().length);
                    assertTrue(field.isAnnotationPresent(JsonBackReference.class));
                    assertTrue(field.isAnnotationPresent(OneToOne.class));
                    assertTrue(field.isAnnotationPresent(JoinColumn.class));
                    assertEquals(FetchType.LAZY, field.getAnnotation(OneToOne.class).fetch());
                    assertEquals("ship_mmsi", field.getAnnotation(JoinColumn.class).name());
                    assertEquals("mmsi", field.getAnnotation(JoinColumn.class).referencedColumnName());
                    assertTrue(field.getAnnotation(JoinColumn.class).unique());
                    assertFalse(field.getAnnotation(JoinColumn.class).nullable());
                    break;
                default:
                    throw new JPSRuntimeException("Field name not recognised.");
            }
        }
    }

    public void testNewShipPollutionEntityMethods() {
        ShipPollutionEntity sp = new ShipPollutionEntity();
        try {
            sp.setId(TST_INT);
            sp.setChimneyIri(TST_STR);
            ShipEntity ship = new ShipEntity();
            sp.setShipByShipMmsi(ship);
        } catch (Exception e) {
            throw new JPSRuntimeException(e);
        } finally {
            assertEquals(TST_INT, sp.getId());
            assertEquals(TST_STR, sp.getChimneyIri());
            assertEquals(sp.getShipByShipMmsi(), new ShipEntity());
            assertNotEquals(sp, new ShipPollutionEntity());
            assertEquals(52503, sp.hashCode());
        }
    }
}
