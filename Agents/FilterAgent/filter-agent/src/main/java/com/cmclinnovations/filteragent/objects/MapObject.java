package com.cmclinnovations.filteragent.objects;

import java.util.Map;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonValue;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;

public class MapObject {
    private Map<String, String> map;

    @JsonCreator
    public MapObject(String stringMap) throws JsonProcessingException {
        this.map = new ObjectMapper().readValue(stringMap, new TypeReference<Map<String, String>>() {
        });
    }

    @JsonValue
    public Map<String, String> getMap() {
        return map;
    }
}
