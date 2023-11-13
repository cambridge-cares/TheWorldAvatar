package com.cmclinnovations.featureinfo.objects;

import java.util.Optional;

import com.fasterxml.jackson.annotation.JsonAlias;
import com.fasterxml.jackson.annotation.JsonProperty;

public class Request // (@JsonProperty("iri") @JsonAlias("IRI") String iri, @JsonProperty("endpoint")
                     // @JsonAlias("ENDPOINT") Optional<String> endpoint)
{
    @JsonProperty("iri")
    @JsonAlias("IRI")
    private String iri;

    @JsonAlias("ENDPOINT")
    @JsonProperty("endpoint")
    private Optional<String> endpoint;

    public Request(String iri, String endpoint) {
        this.iri = iri;
        this.endpoint = Optional.ofNullable(endpoint);
    }

    public String getIri() {
        return iri;
    }

    public void setIri(String iri) {
        this.iri = iri;
    }

    public Optional<String> getEndpoint() {
        return endpoint;
    }

    public void setEndpoint(Optional<String> endpoint) {
        this.endpoint = endpoint;
    }

}
