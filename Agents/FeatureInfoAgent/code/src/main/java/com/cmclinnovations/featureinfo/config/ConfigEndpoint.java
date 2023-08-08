package com.cmclinnovations.featureinfo.config;

/**
 * Record for a service endpoint as defined in the configuration
 */
public record ConfigEndpoint(
    String name,
    String url,
    String username,
    String password,
    EndpointType type
){}