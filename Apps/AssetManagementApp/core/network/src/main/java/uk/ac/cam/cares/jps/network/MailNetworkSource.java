package uk.ac.cam.cares.jps.network;

import org.apache.log4j.BasicConfigurator;

import javax.inject.Inject;

public class MailNetworkSource {

    // TODO: get mail list & detailed single mail from inAppMailBoxAgent
    String path = "";
    Connection connection;
    @Inject
    public MailNetworkSource(Connection connection) {
        BasicConfigurator.configure();
        this.connection = connection;
    }
}
