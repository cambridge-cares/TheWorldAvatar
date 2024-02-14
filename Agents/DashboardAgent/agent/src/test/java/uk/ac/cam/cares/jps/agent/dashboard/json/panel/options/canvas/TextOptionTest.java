package uk.ac.cam.cares.jps.agent.dashboard.json.panel.options.canvas;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class TextOptionTest {
    private static final String DYNAMIC_FIELD = "sqlfield";
    private static final String STATIC_FIELD = "Always display this";

    @Test
    void testConstruct_DynamicField() {
        TextOption option = new TextOption(DYNAMIC_FIELD, false);
        assertEquals(genExpectedTextOptions(DYNAMIC_FIELD, false), option.construct());
    }

    @Test
    void testConstruct_StaticField() {
        TextOption option = new TextOption(STATIC_FIELD, true);
        assertEquals(genExpectedTextOptions(STATIC_FIELD, true), option.construct());
    }

    public static String genExpectedTextOptions(String valueOrField, boolean isFixed) {
        if (isFixed) {
            return "{\"fixed\":\"" + valueOrField + "\"}";
        } else {
            return "{\"field\":\"" + valueOrField + "\",\"fixed\":\"\",\"mode\":\"field\"}";
        }
    }
}