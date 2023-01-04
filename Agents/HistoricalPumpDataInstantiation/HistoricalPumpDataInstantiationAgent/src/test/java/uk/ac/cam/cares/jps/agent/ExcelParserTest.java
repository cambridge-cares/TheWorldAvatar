package uk.ac.cam.cares.jps.agent;

import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.ss.usermodel.*;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.Instant;
import java.time.Year;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

class ExcelParserTest {
    @TempDir
    private static Path tempDir;
    private ExcelParser testParser;
    private Map<String, Map<String, List<?>>> testResults;
    private static Path sampleData;
    // Constants
    private static final String BASE_KEY = "base";
    private static final String YEAR_KEY = "Year";
    private static final String GROUP_KEY = "Facility";
    private static final String VALUE_KEY = "Values";
    private static final Integer[] YEAR_ARRAY = {2000, 2001, 2002};
    private static final Integer[] MEASURES_ARRAY = {91, 10, 51};
    private static final String[] GROUP_ARRAY = {"building", "station"};

    @BeforeAll
    static void initTestExcelData() {
        sampleData = tempDir.resolve("data.xls");
        createTestExcelFile(sampleData);
    }

    @Test
    void testExcelParserConstructor() throws IOException {
        assertTrue(Files.exists(sampleData), "Test file does not exist");
        // For Overloaded constructor
        assertNotNull(new ExcelParser(sampleData.toString(), YEAR_KEY), "Overload constructor failed to be constructed");
        // For Standard constructor
        assertNotNull(new ExcelParser(sampleData.toString(), YEAR_KEY, 0), "Standard constructor failed to be constructed");
    }

    @Test
    void testExcelParserConstructorFail() {
        // Test overload constructor
        FileNotFoundException thrown = assertThrows(FileNotFoundException.class, () ->
                new ExcelParser("filepath", YEAR_KEY));
        assertEquals("No excel file found at specified filepath: %s", thrown.getMessage());
        // Test standard constructor
        thrown = assertThrows(FileNotFoundException.class, () ->
                new ExcelParser("filepath", YEAR_KEY, 1));
        assertEquals("No excel file found at specified filepath: %s", thrown.getMessage());
        IOException message = assertThrows(IOException.class, () ->
                new ExcelParser(sampleData.toString(), YEAR_KEY, -1));
        assertEquals("Sheet Index is invalid, please input a integer starting from 0.", message.getMessage());
    }

    @Test
    void testParseToHashMap() throws IOException {
        // Execute method
        testParser = new ExcelParser(sampleData.toString(), YEAR_KEY);
        testResults = testParser.parseToHashMap(1, -1);
        // Tests the retrieved values are similar to the generated Excel workbook
        assertTrue(testResults.get(BASE_KEY).containsKey(YEAR_KEY));
        assertTrue(testResults.get(BASE_KEY).containsKey(VALUE_KEY));
        // Redundant to test Dates across rows as they have same values. Test may fail if running around 11.59pm/12am
        Instant[] instArr = genYearInstants();
        assertEquals(instArr[0], testResults.get(BASE_KEY).get(YEAR_KEY).get(0));
        assertEquals(instArr[1], testResults.get(BASE_KEY).get(YEAR_KEY).get(1));
        assertEquals(instArr[2], testResults.get(BASE_KEY).get(YEAR_KEY).get(2));
        assertEquals(((Number) MEASURES_ARRAY[0]).doubleValue(), testResults.get(BASE_KEY).get(VALUE_KEY).get(0));
        assertEquals(((Number) MEASURES_ARRAY[1]).doubleValue(), testResults.get(BASE_KEY).get(VALUE_KEY).get(1));
        assertEquals(((Number) MEASURES_ARRAY[2]).doubleValue(), (Double) testResults.get(BASE_KEY).get(VALUE_KEY).get(2));
    }

    @Test
    void testParseToHashMapGroupTimeSeries() throws IOException {
        // Set up
        Instant[] instArr = genYearInstants();
        String[] facilityKey = {"Building", "Station"};
        // Execute method
        testParser = new ExcelParser(sampleData.toString(), YEAR_KEY, 1);
        testResults = testParser.parseToHashMap(1, 2);
        // Test first group keys and values
        assertTrue(testResults.get(facilityKey[0]).containsKey(YEAR_KEY));
        assertTrue(testResults.get(facilityKey[0]).containsKey(VALUE_KEY));
        assertEquals(instArr[0], testResults.get(facilityKey[0]).get(YEAR_KEY).get(0));
        assertEquals(instArr[1], testResults.get(facilityKey[0]).get(YEAR_KEY).get(1));
        assertEquals(instArr[2], testResults.get(facilityKey[0]).get(YEAR_KEY).get(2));
        assertEquals(((Number) MEASURES_ARRAY[0]).doubleValue(), testResults.get(facilityKey[0]).get(VALUE_KEY).get(0));
        assertEquals(((Number) MEASURES_ARRAY[1]).doubleValue(), testResults.get(facilityKey[0]).get(VALUE_KEY).get(1));
        assertEquals(((Number) MEASURES_ARRAY[2]).doubleValue(), (Double) testResults.get(facilityKey[0]).get(VALUE_KEY).get(2));
        // Test second group keys and values
        assertTrue(testResults.get(facilityKey[1]).containsKey(YEAR_KEY));
        assertTrue(testResults.get(facilityKey[1]).containsKey(VALUE_KEY));
        assertEquals(instArr[0], testResults.get(facilityKey[1]).get(YEAR_KEY).get(0));
        assertEquals(instArr[1], testResults.get(facilityKey[1]).get(YEAR_KEY).get(1));
        assertEquals(instArr[2], testResults.get(facilityKey[1]).get(YEAR_KEY).get(2));
        assertEquals(((Number) MEASURES_ARRAY[0]).doubleValue(), testResults.get(facilityKey[1]).get(VALUE_KEY).get(2));
        assertEquals(((Number) MEASURES_ARRAY[1]).doubleValue(), testResults.get(facilityKey[1]).get(VALUE_KEY).get(1));
        assertEquals(((Number) MEASURES_ARRAY[2]).doubleValue(), (Double) testResults.get(facilityKey[1]).get(VALUE_KEY).get(0));
    }

    protected static void createTestExcelFile(Path excel) {
        // Set up
        Workbook wb = new HSSFWorkbook();
        CreationHelper createHelper = wb.getCreationHelper();
        Sheet sheet = wb.createSheet(YEAR_KEY);
        Sheet sheetGroup = wb.createSheet("Group Year");
        // Header rows
        Row row = sheet.createRow(0); // first sheet
        row.createCell(0).setCellValue(createHelper.createRichTextString(YEAR_KEY));
        row.createCell(1).setCellValue(createHelper.createRichTextString("  Values"));
        row = sheetGroup.createRow(0); // second sheet
        row.createCell(0).setCellValue(createHelper.createRichTextString(YEAR_KEY));
        row.createCell(1).setCellValue(createHelper.createRichTextString("  Values"));
        row.createCell(2).setCellValue(createHelper.createRichTextString(GROUP_KEY));
        // Starting row for contents of first and second sheet
        for (int i = 1; i < 7; i++) {

            Row contentRowSecondSheet = sheetGroup.createRow(i);
            // Set cell value based on row
            switch (i) {
                case 1:
                    Row contentRow = sheet.createRow(i);
                    contentRow.createCell(0).setCellValue(YEAR_ARRAY[0]);
                    contentRow.createCell(1).setCellValue(MEASURES_ARRAY[0]);
                    contentRowSecondSheet.createCell(0).setCellValue(YEAR_ARRAY[0]);
                    contentRowSecondSheet.createCell(1).setCellValue(MEASURES_ARRAY[0]);
                    contentRowSecondSheet.createCell(2).setCellValue(GROUP_ARRAY[0]);
                    break;
                case 2:
                    contentRow = sheet.createRow(i);
                    contentRow.createCell(0).setCellValue(YEAR_ARRAY[1]);
                    contentRow.createCell(1).setCellValue(MEASURES_ARRAY[1]);
                    contentRowSecondSheet.createCell(0).setCellValue(YEAR_ARRAY[1]);
                    contentRowSecondSheet.createCell(1).setCellValue(MEASURES_ARRAY[1]);
                    contentRowSecondSheet.createCell(2).setCellValue(GROUP_ARRAY[0]);
                    break;
                case 3:
                    contentRow = sheet.createRow(i);
                    contentRow.createCell(0).setCellValue(YEAR_ARRAY[2]);
                    contentRow.createCell(1).setCellValue(MEASURES_ARRAY[2]);
                    contentRowSecondSheet.createCell(0).setCellValue(YEAR_ARRAY[2]);
                    contentRowSecondSheet.createCell(1).setCellValue(MEASURES_ARRAY[2]);
                    contentRowSecondSheet.createCell(2).setCellValue(GROUP_ARRAY[0]);
                    break;
                case 4:
                    contentRowSecondSheet.createCell(0).setCellValue(YEAR_ARRAY[0]);
                    contentRowSecondSheet.createCell(1).setCellValue(MEASURES_ARRAY[2]);
                    contentRowSecondSheet.createCell(2).setCellValue(GROUP_ARRAY[1]);
                    break;
                case 5:
                    contentRowSecondSheet.createCell(0).setCellValue(YEAR_ARRAY[1]);
                    contentRowSecondSheet.createCell(1).setCellValue(MEASURES_ARRAY[1]);
                    contentRowSecondSheet.createCell(2).setCellValue(GROUP_ARRAY[1]);
                    break;
                case 6:
                    contentRowSecondSheet.createCell(0).setCellValue(YEAR_ARRAY[2]);
                    contentRowSecondSheet.createCell(1).setCellValue(MEASURES_ARRAY[0]);
                    contentRowSecondSheet.createCell(2).setCellValue(GROUP_ARRAY[1]);
                    break;
            }
        }
        // Output the Excel workbook with two sheets to Temp Directory
        try (OutputStream output = Files.newOutputStream(excel)) {
            wb.write(output);
            wb.close();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    private static Instant[] genYearInstants() {
        Instant[] arr = new Instant[YEAR_ARRAY.length];
        for (int i = 0; i < YEAR_ARRAY.length; i++) {
            Year year = Year.of(YEAR_ARRAY[i]);
            String instantStr = year.atDay(year.length()) + "T00:00:00Z";
            arr[i] = Instant.parse(instantStr);
        }
        return arr;
    }
}