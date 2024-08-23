package com.cmclinnovations.stack.services;

import java.util.Map;

import com.cmclinnovations.stack.services.config.Connection;

public interface Service {

    Map<String, Connection> getEndpoints();

}
