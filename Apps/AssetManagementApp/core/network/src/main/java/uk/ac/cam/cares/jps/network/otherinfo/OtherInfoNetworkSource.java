package uk.ac.cam.cares.jps.network.otherinfo;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.Logger;

import javax.inject.Inject;

import uk.ac.cam.cares.jps.network.Connection;

public class OtherInfoNetworkSource {
    private static final Logger LOGGER = Logger.getLogger(OtherInfoNetworkSource.class);

    String path = "feature-info-agent/get";


    Connection connection;

    @Inject
    public OtherInfoNetworkSource(Connection connection) {
        BasicConfigurator.configure();
        this.connection = connection;
    }

}
