package uk.ac.cam.cares.jps.agent.dashboard.json.panel.options.canvas;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class CanvasFrameElementTest {
    private static final String ELEMENT_NAME = "Sample Element";
    private static final String FONT_SIZE = "50";
    private static final String HEIGHT = "75";
    private static final String WIDTH = "150";
    private static final String TOP_PLACEMENT = "25";
    private static final String LEFT_PLACEMENT = "0";
    private static final String[] PLACEMENTS = new String[4];

    @BeforeAll
    static void setup() {
        PLACEMENTS[0] = HEIGHT;
        PLACEMENTS[1] = WIDTH;
        PLACEMENTS[2] = TOP_PLACEMENT;
        PLACEMENTS[3] = LEFT_PLACEMENT;
    }

    @Test
    void testConstruct_StaticTextAndStaticTextColor() {
        // Set up
        String textStaticField = "field";
        ColorOption colorOption = new ColorOption(ColorOptionTest.TEXT_FIXED_VALUE, ""); // Static color based on text
        TextOption textOption = new TextOption(textStaticField, true); // Static text field
        CanvasFrameElement sample = new CanvasFrameElement(ELEMENT_NAME, textOption, colorOption, FONT_SIZE);
        // Execute method
        String constructedSyntax = sample.construct(HEIGHT, WIDTH, TOP_PLACEMENT, LEFT_PLACEMENT);
        // Verify result
        assertEquals(genExpectedCanvasFrameElementSyntax(ELEMENT_NAME, FONT_SIZE, PLACEMENTS, textOption, colorOption), constructedSyntax);
    }

    @Test
    void testConstruct_StaticColorAndText() {
        // Set up
        String colorFixedValue = "#d9217h";
        String textStaticField = "field";
        ColorOption colorOption = new ColorOption(colorFixedValue, ""); // Static color
        TextOption textOption = new TextOption(textStaticField, true); // Static text field
        CanvasFrameElement sample = new CanvasFrameElement(ELEMENT_NAME, textOption, colorOption, FONT_SIZE);
        // Execute method
        String constructedSyntax = sample.construct(HEIGHT, WIDTH, TOP_PLACEMENT, LEFT_PLACEMENT);
        // Verify result
        assertEquals(genExpectedCanvasFrameElementSyntax(ELEMENT_NAME, FONT_SIZE, PLACEMENTS, textOption, colorOption), constructedSyntax);
    }

    @Test
    void testConstruct_DynamicColorAndText() {
        // Set up
        String dynamicField = "sql field";
        ColorOption colorOption = new ColorOption(ColorOptionTest.TEXT_FIXED_VALUE, dynamicField); // Dynamic color
        TextOption textOption = new TextOption(dynamicField, false); // Dynamic text field
        CanvasFrameElement sample = new CanvasFrameElement(ELEMENT_NAME, textOption, colorOption, FONT_SIZE);
        // Execute method
        String constructedSyntax = sample.construct(HEIGHT, WIDTH, TOP_PLACEMENT, LEFT_PLACEMENT);
        // Verify result
        assertEquals(genExpectedCanvasFrameElementSyntax(ELEMENT_NAME, FONT_SIZE, PLACEMENTS, textOption, colorOption), constructedSyntax);
    }

    public static String genExpectedCanvasFrameElementSyntax(String elementName, String fontSize, String[] placementPositions, TextOption textOption, ColorOption colorOption) {
        return "{" +
                "\"background\":{\"color\": {\"fixed\":\"transparent\"}}," +
                "\"border\":{\"color\": {\"fixed\":\"dark-green\"}}," +
                "\"config\":{" +
                "\"align\":\"center\"," +
                "\"color\":" + colorOption.construct() + "," +
                "\"size\":" + fontSize + "," +
                "\"text\":" + textOption.construct() + "," +
                "\"valign\": \"middle\"" +
                "}," +
                "\"constraint\":{\"horizontal\":\"left\",\"vertical\":\"top\"}," +
                "\"name\":\"" + elementName + "\"," +
                "\"placement\":{\"height\":" + placementPositions[0] + ",\"width\":" + placementPositions[1] + ",\"top\":" + placementPositions[2] + ",\"left\":" + placementPositions[3] + "}," +
                "\"type\":\"metric-value\"" +
                "}";
    }
}