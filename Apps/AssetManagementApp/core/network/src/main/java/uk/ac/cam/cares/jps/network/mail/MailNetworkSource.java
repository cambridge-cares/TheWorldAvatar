package uk.ac.cam.cares.jps.network.mail;

import org.apache.log4j.BasicConfigurator;

import javax.inject.Inject;

import uk.ac.cam.cares.jps.network.Connection;

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
