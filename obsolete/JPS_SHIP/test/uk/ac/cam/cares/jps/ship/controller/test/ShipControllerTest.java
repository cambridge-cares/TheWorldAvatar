package uk.ac.cam.cares.jps.ship.controller.test;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.ship.controller.ShipController;
import javax.ws.rs.Path;
import javax.ws.rs.GET;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

public class ShipControllerTest extends TestCase {


    public void testNewShipController() {
        ShipController sc = null;
        try {
            sc = new ShipController();
        } catch (Exception e) {
            throw new JPSRuntimeException(e);
        } finally {
            assertNotNull(sc);
            assertEquals(sc.getClass(), ShipController.class);
            assertEquals(1, sc.getClass().getAnnotations().length);
            assertTrue(sc.getClass().isAnnotationPresent(Path.class));
            assertEquals("/ships", sc.getClass().getAnnotation(Path.class).value());
        }
    }

    public void testNewShipControllerMethods() {
        ShipController sc = new ShipController();
        try {
            assertEquals(1, sc.getClass().getDeclaredMethods().length);
            assertEquals(3, sc.getClass().getDeclaredMethods()[0].getAnnotations().length);
            assertTrue(sc.getClass().getDeclaredMethods()[0].isAnnotationPresent(GET.class));
            assertTrue(sc.getClass().getDeclaredMethods()[0].isAnnotationPresent(Produces.class));
            assertTrue(sc.getClass().getDeclaredMethods()[0].isAnnotationPresent(Path.class));
            assertEquals(MediaType.APPLICATION_JSON, sc.getClass().getDeclaredMethods()[0].getAnnotation(Produces.class).value()[0]);
            assertEquals("{mmsi}", sc.getClass().getDeclaredMethods()[0].getAnnotation(Path.class).value());
        } catch (Exception e) {
            throw new JPSRuntimeException(e);
        } finally {
            try {
                sc.read(123);
            } catch (IllegalStateException e) {
                assertEquals(e.getMessage(), "Context is not initialized yet.");
            }
        }

    }


}
