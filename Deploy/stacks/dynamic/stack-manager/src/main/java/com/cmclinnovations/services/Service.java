package com.cmclinnovations.services;

import java.util.Map;

import com.cmclinnovations.services.config.Connection;

public interface Service {

    Map<String, Connection> getEndpoints();

}
