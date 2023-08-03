package com.cmclinnovations.stack.clients.postgis;

import java.util.Objects;

import com.cmclinnovations.stack.clients.core.PasswordEndpointConfig;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

@JsonInclude(Include.NON_NULL)
public class PostGISEndpointConfig extends PasswordEndpointConfig {

    private final String hostName;
    private final String port;
    private final String username;

    private final String jdbcURL;
    private final String sqlAlchemyURL;
    private final String jdbcDriver;
    private final String jdbcDriverURL;

    protected PostGISEndpointConfig() {
        this(null, null, null, null, null);
    }

    public PostGISEndpointConfig(String name, String hostName, String port, String username, String passwordFile) {
        super(name, passwordFile);
        this.hostName = hostName;
        this.port = port;
        this.username = username;

        // By default assume PostgreSQL, calculate jdbcURL & sqlAlchemyURL when
        // requested
        this.jdbcURL = null;
        this.sqlAlchemyURL = null;
        this.jdbcDriver = "org.postgresql.Driver";
        this.jdbcDriverURL = "https://jdbc.postgresql.org/download/postgresql-42.3.6.jar";
    }

    public String getHostName() {
        return hostName;
    }

    public String getPort() {
        return port;
    }

    public String getUsername() {
        return username;
    }

    public String getJdbcURL(String database) {
        if (null == jdbcURL) {
            Objects.requireNonNull(database,
                    "If a 'jdbcURL' is not explicitly specified then a database name must be in the code.");
            // By default assume PostgreSQL
            return "jdbc:postgresql://" + hostName + ":" + port + "/" + database;
        } else {
            return jdbcURL;
        }
    }

    public String getSQLALchemyURL(String database) { // TODO: Combine with getJdbcURL
        if (null == sqlAlchemyURL) {
            Objects.requireNonNull(database,
                    "If a 'sqlAlchemyURL' is not explicitly specified then a database name must be in the code.");
            // By default assume PostgreSQL
            return "postgresql://" + username + ":" + getPassword() + "@" + hostName + ":" + port + "/" + database;
        } else {
            return sqlAlchemyURL;
        }
    }

    public String getJdbcDriver() {
        return jdbcDriver;
    }

    public String getJdbcDriverURL() {
        return jdbcDriverURL;
    }

}