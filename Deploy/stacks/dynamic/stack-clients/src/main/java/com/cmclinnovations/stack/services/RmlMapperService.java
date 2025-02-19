package com.cmclinnovations.stack.services;

import com.cmclinnovations.stack.services.config.ServiceConfig;

public class RmlMapperService extends ContainerService {

  public static final String TYPE = "yarrrml-parser";

  public RmlMapperService(String stackName, ServiceConfig config) {
    super(stackName, config);
  }
}
