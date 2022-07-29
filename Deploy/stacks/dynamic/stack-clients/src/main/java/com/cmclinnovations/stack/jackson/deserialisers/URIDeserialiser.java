package com.cmclinnovations.stack.jackson.deserialisers;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.ObjectCodec;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.deser.std.StdDeserializer;
import com.fasterxml.jackson.databind.exc.MismatchedInputException;

public class URIDeserialiser extends StdDeserializer<URI> {

    protected URIDeserialiser() {
        this(null);
    }

    protected URIDeserialiser(Class<?> vc) {
        super(vc);
    }

    @Override
    public URI deserialize(JsonParser p, DeserializationContext ctxt) throws IOException {
        ObjectCodec objectCodec = p.getCodec();
        JsonNode node = objectCodec.readTree(p);
        String stringURI = node.asText();
        try {
            return new URI(stringURI);
        } catch (URISyntaxException ex) {
            throw MismatchedInputException.from(p, URI.class, "Invalid URI '" + stringURI + "'.");
        }
    }

}
