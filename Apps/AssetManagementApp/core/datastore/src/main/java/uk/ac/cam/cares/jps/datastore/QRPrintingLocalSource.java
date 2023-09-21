package uk.ac.cam.cares.jps.datastore;

import android.content.Context;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.Logger;

import dagger.hilt.android.qualifiers.ApplicationContext;

public class QRPrintingLocalSource {
    private static final Logger LOGGER = Logger.getLogger(QRPrintingLocalSource.class);

    public QRPrintingLocalSource(@ApplicationContext Context applicationContext) {
        BasicConfigurator.configure();

    }
}
