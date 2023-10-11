package com.cmclinnovations.stack.clients.influxdb;

import com.cmclinnovations.stack.clients.core.PasswordEndpointConfig;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

@JsonInclude(Include.NON_NULL)
public class InfluxDBEndpointConfig extends PasswordEndpointConfig {

    private final String hostName;
    private final String port;
    private final String username;
    private final String org;  // Organization name in InfluxDB
    private final String bucket;  // Data bucket in InfluxDB

    protected InfluxDBEndpointConfig() {
        this(null, null, null, null, null, null, null);
    }

    public InfluxDBEndpointConfig(String name, String hostName, String port, String username, String passwordFile, String org, String bucket) {
        super(name, passwordFile);
        this.hostName = hostName;
        this.port = port;
        this.username = username;
        this.org = org;
        this.bucket = bucket;
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

    public String getOrg() {
        return org;
    }

    public String getBucket() {
        return bucket;
    }

    public String getInfluxDBURL() {
        return "http://" + hostName + ":" + port;
    }

}
