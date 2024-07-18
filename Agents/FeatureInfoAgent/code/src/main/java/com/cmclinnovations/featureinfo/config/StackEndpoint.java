package com.cmclinnovations.featureinfo.config;

/**
 * Record for a TWA Stack service endpoint as determined by
 * the stack's client library.
 */
public record StackEndpoint(

    /**
     * Internal URL.
     */
    String url,

    /**
     * Optional username.
     */
    String username,

    /**
     * Optional password.
     */
    String password,

    /**
     * Type of endpoint.
     */
    StackEndpointType type
){}