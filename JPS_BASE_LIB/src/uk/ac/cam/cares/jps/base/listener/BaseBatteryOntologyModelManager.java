package uk.ac.cam.cares.jps.base.listener;

import javax.servlet.ServletContextEvent;
import javax.servlet.annotation.WebListener;

@WebListener
public class BaseBatteryOntologyModelManager extends BaseOntologyModelManager {

    @Override
    public void contextInitialized(ServletContextEvent sce) {
        super.contextInitialized(sce);
        //@todo LKA - implementation
    }

}
