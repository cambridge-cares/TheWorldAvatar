package com.cmclinnovations.stack.clients.core.datasets;

import java.io.IOException;
import java.util.List;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.cmclinnovations.stack.clients.utils.JsonHelper;
import com.fasterxml.jackson.databind.ObjectMapper;

class OntopSettingsTest {

    private final ObjectMapper mapper = JsonHelper.getMapper();

    @Test
    void defaultConstructor() {
        OntopSettings ontopSettings = new OntopSettings();
        Assertions.assertEquals(List.of(), ontopSettings.getRules());
    }

    @Test
    void constructFromJson() throws IOException {
        OntopSettings ontopSettings = mapper
                .readValue(OntopSettingsTest.class.getResourceAsStream("ontopSettings.json"), OntopSettings.class);
        Assertions.assertEquals(List.of("rules.toml", "rules2.toml"), ontopSettings.getRules());
    }
}
