package com.cmclinnovations.stack.jackson.deserialisers;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;

import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.ObjectCodec;
import com.fasterxml.jackson.databind.DeserializationContext;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.deser.std.StdDeserializer;
import com.fasterxml.jackson.databind.exc.MismatchedInputException;

public class URLDeserialiser extends StdDeserializer<URL> {

    protected URLDeserialiser() {
        this(null);
    }

    protected URLDeserialiser(Class<?> vc) {
        super(vc);
    }

    @Override
    public URL deserialize(JsonParser p, DeserializationContext ctxt) throws IOException {
        ObjectCodec objectCodec = p.getCodec();
        JsonNode node = objectCodec.readTree(p);
        String stringUrl = node.asText();
        try {
            return new URL(stringUrl);
        } catch (MalformedURLException ex) {
            throw MismatchedInputException.from(p, URL.class, "Invalid URL '" + stringUrl + "'.");
        }
    }

}
