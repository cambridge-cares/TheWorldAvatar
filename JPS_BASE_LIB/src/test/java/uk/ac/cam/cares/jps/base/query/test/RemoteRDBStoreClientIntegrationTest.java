package uk.ac.cam.cares.jps.base.query.test;

import org.junit.Test;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;

import java.lang.reflect.Field;
import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;

import static org.junit.Assert.*;
import static org.junit.Assert.assertEquals;

//Requires Docker to run the tests. When on Windows, WSL2 as backend is required to ensure proper execution.
public class RemoteRDBStoreClientIntegrationTest {

    // Define RDB database setup (analogous to a triple-store endpoint)
    // Using special testcontainers URL that will spin up a Postgres Docker container when accessed by a driver
    // (see: https://www.testcontainers.org/modules/databases/jdbc/). Note: requires Docker to be installed!
    private static final String dbURL = "jdbc:tc:postgresql:13.3:///timeseries";
    // For easier local debugging, use the following dbURL instead of the testcontainer dbURL
    // NOTE: Requires local postgreSQL database "timeseries" to be set up beforehand
    //private static final String dbURL = "jdbc:postgresql:timeseries";
    private static final String user = "postgres";
    private static final String password = "postgres";



}
