package uk.ac.cam.cares.jps.agent.dashboard.json.panel.options.canvas;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class ColorOptionTest {
    public static final String TEXT_FIXED_VALUE = "text";
    private static final String COLOR_FIXED_VALUE = "#ffffff";
    private static final String FIELD = "sqlfield";

    @Test
    void testConstruct_FixedColor() {
        ColorOption option = new ColorOption(COLOR_FIXED_VALUE, "");
        assertEquals(genExpectedColorOptions(COLOR_FIXED_VALUE, ""), option.construct());
    }

    @Test
    void testConstruct_FixedText() {
        ColorOption option = new ColorOption(TEXT_FIXED_VALUE, "");
        assertEquals(genExpectedColorOptions(TEXT_FIXED_VALUE, ""), option.construct());
    }

    @Test
    void testConstruct_DynamicField() {
        ColorOption option = new ColorOption(TEXT_FIXED_VALUE, FIELD);
        assertEquals(genExpectedColorOptions(TEXT_FIXED_VALUE, FIELD), option.construct());
    }

    public static String genExpectedColorOptions(String fixedValue, String field) {
        if (fixedValue.equals("text") && !field.isEmpty()) {
            return "{\"field\":\"" + field + "\",\"fixed\":\"text\"}";
        } else {
            return "{\"fixed\":\"" + fixedValue + "\"}";
        }
    }
}