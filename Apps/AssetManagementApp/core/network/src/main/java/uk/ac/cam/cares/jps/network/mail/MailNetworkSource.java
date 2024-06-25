package uk.ac.cam.cares.jps.network.mail;

import android.content.Context;

import org.apache.log4j.BasicConfigurator;

import javax.inject.Inject;

import dagger.hilt.android.qualifiers.ApplicationContext;
import uk.ac.cam.cares.jps.network.Connection;

public class MailNetworkSource {

    // TODO: get mail list & detailed single mail from inAppMailBoxAgent
    String path = "";
    Connection connection;
    Context context;
    @Inject
    public MailNetworkSource(Connection connection, @ApplicationContext Context applicationContext) {
        BasicConfigurator.configure();
        this.connection = connection;
        this.context = applicationContext;
    }
}
