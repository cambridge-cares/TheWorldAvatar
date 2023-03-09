package uk.ac.cam.cares.jps.ship.listener.test;

import junit.framework.TestCase;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.ship.ServletTestHelper;
import uk.ac.cam.cares.jps.ship.listener.LocalEntityManagerFactory;

import javax.persistence.EntityManager;
import javax.servlet.*;
import javax.servlet.annotation.WebListener;


public class LocalEntityManagerFactoryTest extends TestCase {

    private final String EX_CTXINIT = "Context is not initialized yet.";

    public void testNewLocalEntityManagerFactory() {
        LocalEntityManagerFactory lmf = null;
        try {
            lmf = new LocalEntityManagerFactory();
        } catch (Exception e) {
            throw new JPSRuntimeException(e);
        } finally {
            assertNotNull(lmf);
            assertEquals(lmf.getClass(), new LocalEntityManagerFactory().getClass());
            assertEquals(1, lmf.getClass().getAnnotations().length);
            assertTrue(lmf.getClass().isAnnotationPresent(WebListener.class));
        }
    }

    public void testNewLocalEntityManagerFactoryFields() {
        LocalEntityManagerFactory lmf = new LocalEntityManagerFactory();
        assertEquals(2, lmf.getClass().getDeclaredFields().length);
    }

    public void testNewLocalEntityManagerFactoryMethods() {
        EntityManager em = null;
        LocalEntityManagerFactory lmf = new LocalEntityManagerFactory();
        ServletConfig ctx = ServletTestHelper.getServletConfig();
        ServletContextEvent event = new ServletContextEvent(ctx.getServletContext());
        try {
            lmf.createEntityManager();
        } catch (IllegalStateException e) {
            assertEquals(e.getMessage(), EX_CTXINIT);
        }
        try {
            lmf.contextInitialized(event);
            lmf.createEntityManager();
        } catch (Exception e) {
            assertNotSame(e.getMessage(), EX_CTXINIT);
        } finally {
            lmf.contextDestroyed(event);
        }
    }


}
