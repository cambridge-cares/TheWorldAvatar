package uk.ac.cam.cares.jps.ship.listener;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.persistence.Persistence;
import javax.servlet.ServletContextEvent;
import javax.servlet.ServletContextListener;
import javax.servlet.annotation.WebListener;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@WebListener
public class LocalEntityManagerFactory implements ServletContextListener {

    private static EntityManagerFactory emf;
    private static Logger logger = LoggerFactory.getLogger(LocalEntityManagerFactory.class);

    @Override
    public void contextInitialized(ServletContextEvent event) {
    	logger.info("initializaing the local entity manager");
        emf = Persistence.createEntityManagerFactory("ShipPU");
        if (emf == null) {
        	logger.error("initializaing the local entity manager failed");
        }
    }

    @Override
    public void contextDestroyed(ServletContextEvent event) {
        emf.close();
    }

    public static EntityManager createEntityManager() {
        if (emf == null) {
            throw new IllegalStateException("Context is not initialized yet.");
        }
        return emf.createEntityManager();
    }
}