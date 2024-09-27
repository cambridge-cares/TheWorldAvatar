package com.cmclinnovations.featureinfo.objects;

import java.util.Optional;

import com.fasterxml.jackson.annotation.JsonAlias;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonIgnoreProperties(ignoreUnknown = true)
public class Request {
    @JsonProperty("iri")
    @JsonAlias("IRI")
    private String iri;

    @JsonAlias("ENDPOINT")
    @JsonProperty("endpoint")
    private String endpoint;

    // time bounds for trajectory
    @JsonProperty("upperbound")
    private Long upperbound;
    @JsonProperty("lowerbound")
    private Long lowerbound;

    public Request() {
    }

    public Request(String iri, String endpoint) {
        this.iri = iri;
        this.endpoint = endpoint;
    }

    public String getIri() {
        return iri;
    }

    public Optional<String> getEndpoint() {
        return Optional.ofNullable(endpoint);
    }

    public Long getUpperBound() {
        return upperbound;
    }

    public Long getLowerBound() {
        return lowerbound;
    }

}
