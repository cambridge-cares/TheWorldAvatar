package uk.ac.cam.cares.jps.agent.historicalhouse45utilitiesagent;

import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.ss.usermodel.*;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.time.LocalDate;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

public class ExcelParserTest {
    @TempDir
    private static Path tempDir;
    private static Path testData;
    private ExcelParser testParser;
    private Map<String, List<?>> testResults;

    @BeforeAll
    static void initTestExcelData() {
        testData = tempDir.resolve("data.xls");
        createTestExcelFile(testData);
    }

    public static void createTestExcelFile(Path excel) {
        // Set up
        Workbook wb = new HSSFWorkbook();
        CreationHelper createHelper = wb.getCreationHelper();
        Sheet sheet = wb.createSheet("Date");
        Sheet sheetMultiColDate = wb.createSheet("MultiColumn Date");
        Sheet sheetMultiRowHeader = wb.createSheet("MultiRow Header");

        // Header rows
        Row row = sheet.createRow(0); // first sheet
        row.createCell(0).setCellValue(createHelper.createRichTextString("dates"));
        row.createCell(1).setCellValue(createHelper.createRichTextString("  Values"));
        row = sheetMultiColDate.createRow(0); // second sheet
        row.createCell(0).setCellValue(createHelper.createRichTextString("Day"));
        row.createCell(1).setCellValue(createHelper.createRichTextString("Month"));
        row.createCell(2).setCellValue(createHelper.createRichTextString("Year"));
        row.createCell(3).setCellValue(createHelper.createRichTextString("VaLUes  "));

        // Style the "dates" cell in Date format in first sheet
        CellStyle cellStyle = wb.createCellStyle();
        cellStyle.setDataFormat(
                createHelper.createDataFormat().getFormat("m/d/yy"));

        // Starting row for contents of first and second sheet
        for (int i = 1; i < 4; i++) {
            Row contentRow = sheet.createRow(i);
            Row contentRowSecondSheet = sheetMultiColDate.createRow(i);
            // Set style and add current Date value
            Cell cell = contentRow.createCell(0);
            cell.setCellValue(LocalDate.now());
            cell.setCellStyle(cellStyle);
            // Set cell value based on row
            switch (i) {
                case 1:
                    contentRow.createCell(1).setCellValue(8);
                    contentRowSecondSheet.createCell(0).setCellValue(8);
                    contentRowSecondSheet.createCell(1).setCellValue(8);
                    contentRowSecondSheet.createCell(2).setCellValue(2022);
                    contentRowSecondSheet.createCell(3).setCellValue(16);
                    break;
                case 2:
                    contentRow.createCell(1).setCellValue(4);
                    contentRowSecondSheet.createCell(0).setCellValue(10);
                    contentRowSecondSheet.createCell(1).setCellValue(8);
                    contentRowSecondSheet.createCell(2).setCellValue(2022);
                    contentRowSecondSheet.createCell(3).setCellValue(8);
                    break;
                case 3:
                    contentRow.createCell(1).setCellValue(10);
                    contentRowSecondSheet.createCell(0).setCellValue(15);
                    contentRowSecondSheet.createCell(1).setCellValue(9);
                    contentRowSecondSheet.createCell(2).setCellValue(2022);
                    contentRowSecondSheet.createCell(3).setCellValue(20);
                    break;
            }
        }

        // Third sheet
        row = sheetMultiRowHeader.createRow(0);
        row.createCell(0).setCellValue(createHelper.createRichTextString("Day"));
        row.createCell(1).setCellValue(createHelper.createRichTextString("Month"));
        row.createCell(2).setCellValue(createHelper.createRichTextString("Year"));
        row.createCell(4).setCellValue(createHelper.createRichTextString("Test"));
        row.createCell(5).setCellValue(createHelper.createRichTextString("Test"));
        row = sheetMultiRowHeader.createRow(1); // Second row of Third sheet
        row.createCell(3).setCellValue(createHelper.createRichTextString("Test"));
        row.createCell(5).setCellValue(createHelper.createRichTextString("MissinG  lAST  row  "));
        row = sheetMultiRowHeader.createRow(2); // Third row of Third sheet
        row.createCell(3).setCellValue(createHelper.createRichTextString(" mISSing First ROW"));
        row.createCell(4).setCellValue(createHelper.createRichTextString("MissinG SECONd row"));
        row = sheetMultiRowHeader.createRow(3); // Last row of Third sheet
        row.createCell(0).setCellValue(12);
        row.createCell(1).setCellValue(6);
        row.createCell(2).setCellValue(2022);
        row.createCell(3).setCellValue(15);
        row.createCell(4).setCellValue(5);
        row.createCell(5).setCellValue(55);

        // Output the Excel workbook with three sheets to Temp Directory
        try (OutputStream output = Files.newOutputStream(excel)) {
            wb.write(output);
            wb.close();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    @Test
    void testExcelParserConstructor() {
        assertAll(
                () -> assertTrue(Files.exists(testData), "Test file does not exist"),
                // For Standard constructor
                () -> assertNotNull(new ExcelParser(testData.toString(), "dates"), "Standard constructor failed to be constructed"),
                // For constructor with sheetIndex
                () -> assertNotNull(new ExcelParser(testData.toString(), "dates", 1), "Second constructor failed to be constructed")
        );
    }

    @Test
    void testExcelParserConstructorFail() {
        assertAll(
                // For Standard constructor
                () -> assertThrows(FileNotFoundException.class, () ->
                        new ExcelParser("filepath", "dates")),
                // For constructor with sheetIndex
                () -> assertThrows(FileNotFoundException.class, () ->
                        new ExcelParser("filepath", "dates", 1)),
                () -> assertThrows(IOException.class, () ->
                        new ExcelParser(testData.toString(), "dates", -1))
        );
    }

    @Test
    @DisplayName("Test default parseToHashMap method")
    void testParseToHashMap() {
        try {
            testParser = new ExcelParser(testData.toString(), "Dates");
            testResults = testParser.parseToHashMap(1);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
        assertAll(
                // Tests the retrieved values are similar to the generated Excel workbook
                () -> assertTrue(testResults.containsKey("Dates")),
                () -> assertTrue(testResults.containsKey("Values")),
                // Redundant to test Dates across rows as they have same values. Test may fail if running around 11.59pm/12am
                () -> assertEquals(LocalDate.now().atStartOfDay(), testResults.get("Dates").get(0)),
                () -> assertEquals(8.0, testResults.get("Values").get(0)),
                () -> assertEquals(4.0, testResults.get("Values").get(1)),
                () -> assertEquals(10.0, testResults.get("Values").get(2))
        );
    }

    @Test
    @DisplayName("Test parseToHashMap method for an Excel with multi-column date data")
    void testParseToHashMapMultiColDate() {
        int[] dateArray = {0, 1, 2};
        try {
            // Test data is at second sheet
            testParser = new ExcelParser(testData.toString(), "dates", 1);
            testResults = testParser.parseToHashMap(1, dateArray);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
        assertAll(
                // Tests the retrieved values are similar to the generated Excel workbook
                () -> assertTrue(testResults.containsKey("dates")),
                () -> assertTrue(testResults.containsKey("Values")),
                () -> assertEquals(LocalDate.parse("2022-08-08"), testResults.get("dates").get(0)),
                () -> assertEquals(LocalDate.parse("2022-08-10"), testResults.get("dates").get(1)),
                () -> assertEquals(LocalDate.parse("2022-09-15"), testResults.get("dates").get(2)),
                () -> assertEquals(16.0, testResults.get("Values").get(0)),
                () -> assertEquals(8.0, testResults.get("Values").get(1)),
                () -> assertEquals(20.0, testResults.get("Values").get(2))
        );
    }

    @Test
    @DisplayName("Test parseToHashMap method for an Excel with multi-column date data and multi-row headings")
    void testParseToHashMapMultiRowHeader() {
        int[] dateArray = {0, 1, 2};
        try {
            // Test data is at third sheet
            testParser = new ExcelParser(testData.toString(), "dates", 2);
            testResults = testParser.parseToHashMap(3, dateArray);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
        assertAll(
                () -> assertTrue(testResults.containsKey("dates")),
                () -> assertTrue(testResults.containsKey("Test_MissingFirstRow")),
                () -> assertTrue(testResults.containsKey("Test_MissingSecondRow")),
                () -> assertTrue(testResults.containsKey("Test_MissingLastRow")),
                () -> assertEquals(LocalDate.parse("2022-06-12"), testResults.get("dates").get(0)),
                () -> assertEquals(15.0, testResults.get("Test_MissingFirstRow").get(0)),
                () -> assertEquals(5.0, testResults.get("Test_MissingSecondRow").get(0)),
                () -> assertEquals(55.0, testResults.get("Test_MissingLastRow").get(0))
        );
    }

    @Test
    void testGetParserDateArray() throws FileNotFoundException {
        // When object is initialised, date array should be null
        testParser = new ExcelParser(testData.toString(), "dates");
        assertNull(testParser.getParserDateArray());
    }
    @Test
    void testSetParserDateArray() throws FileNotFoundException {
        testParser = new ExcelParser(testData.toString(), "dates");

        int[] setterTest = {0, 1, 2};
        testParser.setParserDateArray(setterTest);

        assertEquals(setterTest, testParser.getParserDateArray());
    }
}
