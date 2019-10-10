package uk.ac.cam.cares.jps.ship.listener.test;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.ship.listener.LocalOntologyModelManager;

import javax.servlet.annotation.WebListener;

public class LocalOntologyModelManagerTest extends TestCase {

    public void testNewLocalOntologyModelManager() {
        LocalOntologyModelManager lomm = null;
        try {
            lomm = new LocalOntologyModelManager();
        } catch (Exception e) {
            throw new JPSRuntimeException(e);
        } finally {
            assertNotNull(lomm);
            assertEquals(lomm.getClass(), new LocalOntologyModelManager().getClass());
            assertEquals(1, lomm.getClass().getAnnotations().length);
            assertTrue(lomm.getClass().isAnnotationPresent(WebListener.class));
        }
    }

    public void testLocalOntologyModelManagerFields() {
        LocalOntologyModelManager lomm = new LocalOntologyModelManager();
        assertEquals(40, lomm.getClass().getDeclaredFields().length);
    }

    public void testNewLocalEntityManagerFactoryMethods() {
        //@todo: [AC] Implement this test
        assertTrue(true);
    }


}
