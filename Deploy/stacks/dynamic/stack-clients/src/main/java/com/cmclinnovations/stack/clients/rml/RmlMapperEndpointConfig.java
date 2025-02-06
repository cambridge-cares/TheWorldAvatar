package com.cmclinnovations.stack.clients.rml;

import com.cmclinnovations.stack.clients.core.PasswordEndpointConfig;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

@JsonInclude(Include.NON_NULL)
public class RmlMapperEndpointConfig extends PasswordEndpointConfig {
  private final String hostName;
  private final String port;
  private final String username;

  protected RmlMapperEndpointConfig() {
    this(null, null, null, null, null);
  }

  public RmlMapperEndpointConfig(String name, String hostName, String port, String username, String passwordFile) {
    super(name, passwordFile);
    this.hostName = hostName;
    this.port = port;
    this.username = username;
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

  @JsonIgnore
  public String getServiceUrl() {
    return "http://" + hostName + ":" + port + "/yarrrml-parser";
  }
}