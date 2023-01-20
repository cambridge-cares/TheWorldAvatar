package uk.ac.cam.cares.jps.agent;

import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.ss.usermodel.*;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;


class ExcelParserHelperTest {
    private static Cell CELL_STR_ONE;
    private static Cell CELL_STR_TWO;
    private static Cell CELL_STR_THREE;
    private static Cell CELL_DOUBLE_ONE;
    private static Cell CELL_BOOLEAN_ONE;

    @BeforeAll
    static void createSampleCells() throws IOException {
        // Set up
        CreationHelper createHelper;
        Sheet sheet;
        try (Workbook wb = new HSSFWorkbook()) {
            createHelper = wb.getCreationHelper();
            sheet = wb.createSheet("sheet");
        }
        Row row = sheet.createRow(0);
        // Create sample cells
        CELL_STR_ONE = row.createCell(0);
        CELL_STR_ONE.setCellValue(createHelper.createRichTextString("mY<FIrst-; cE.!ll "));
        CELL_STR_TWO = row.createCell(1);
        CELL_STR_TWO.setCellValue(createHelper.createRichTextString("   emPTY   SPAces   "));
        CELL_STR_THREE = row.createCell(2);
        CELL_STR_THREE.setCellValue(createHelper.createRichTextString("simple"));
        CELL_DOUBLE_ONE = row.createCell(3);
        CELL_DOUBLE_ONE.setCellValue(555);
        CELL_BOOLEAN_ONE = row.createCell(4);
        CELL_BOOLEAN_ONE.setCellValue(true);
    }

    @Test
    void testFormatHeaderStringCell() {
        String result = ExcelParserHelper.formatHeaderStringCell(CELL_STR_ONE);
        assertEquals("MyfirstCell", result);
        result = ExcelParserHelper.formatHeaderStringCell(CELL_STR_TWO);
        assertEquals("EmptySpaces", result);
    }

    @Test
    void testStoreInParentMap() {
        // Set up
        Map<String, Map<String, List<?>>> groupedExcelValues = new HashMap<>();
        String group = "only";
        String colHeaderOne = "Year";
        List<Integer> yearValues = Arrays.asList(2001, 2006, 2011);
        String colHeaderTwo = "Values";
        List<Integer> valValues = Arrays.asList(5, 9, 2);
        // Check that the parent map is initially empty
        assertTrue(groupedExcelValues.isEmpty());
        // Execute method for an empty map
        ExcelParserHelper.storeInParentMap(groupedExcelValues, group, colHeaderOne, yearValues);
        // Test that it added the following key and values
        assertFalse(groupedExcelValues.isEmpty()); // Not empty
        assertTrue(groupedExcelValues.containsKey(group));
        assertTrue(groupedExcelValues.get(group).containsKey(colHeaderOne));
        assertEquals(yearValues, groupedExcelValues.get(group).get(colHeaderOne));

        // Add another list of values to same group
        ExcelParserHelper.storeInParentMap(groupedExcelValues, group, colHeaderTwo, valValues);
        // Test that it contains the following key and values
        assertTrue(groupedExcelValues.containsKey(group));
        // Check previous key and value pair still exists in the nested map
        assertTrue(groupedExcelValues.get(group).containsKey(colHeaderOne));
        assertEquals(yearValues, groupedExcelValues.get(group).get(colHeaderOne));
        // Test that new key and value pair are added
        assertTrue(groupedExcelValues.get(group).containsKey(colHeaderTwo));
        assertEquals(valValues, groupedExcelValues.get(group).get(colHeaderTwo));
    }

    @Test
    void testReadCell() {
        assertEquals("simple", ExcelParserHelper.readCell(CELL_STR_THREE));
        assertEquals((double) 555, ExcelParserHelper.readCell(CELL_DOUBLE_ONE));
        assertEquals(true, ExcelParserHelper.readCell(CELL_BOOLEAN_ONE));
    }
}