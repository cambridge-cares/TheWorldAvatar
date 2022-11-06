package uk.ac.cam.cares.jps.agent.historicalhouse45utilitiesagent;

import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.ss.usermodel.*;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Properties;

import static org.junit.jupiter.api.Assertions.*;

class FileManagerTest {
    @TempDir
    private static Path tempDir;
    @TempDir
    private static Path emptyTempDir;
    @TempDir
    private static Path multiFileTempDir;
    private static final String endpointKey = "sparql.query.endpoint";
    private static final String endpointValue = "http://www.test.org/test/";
    private static final String clientProperties = "client.properties";

    @BeforeAll
    static void initTestExcelData() {
        // Create one workbook
        Path excelWorkbook = tempDir.resolve("data.xls");
        createTestExcel(excelWorkbook);
        // Create two workbooks in a separate temp directory
        Path excelWorkbook2 = multiFileTempDir.resolve("workbook.xls");
        Path excelWorkbook3 = multiFileTempDir.resolve("workbook_two.xls");
        createTestExcel(excelWorkbook2);
        createTestExcel(excelWorkbook3);
    }

    @Test
    void testRetrieveExcelPath() throws IOException {
        String path = FileManager.retrieveExcelPath(tempDir.toString());
        assertTrue(path.contains(tempDir.toString()));
    }

    @Test
    void testRetrieveExcelPathFailNoWorkbook() {
        JPSRuntimeException thrown = assertThrows(JPSRuntimeException.class, () -> FileManager.retrieveExcelPath(emptyTempDir.toString()), "JPSRuntimeException was expected");
        assertTrue(thrown.getMessage().contains("No Excel workbook detected! Please place your file in the directory:"));
    }

    @Test
    void testRetrieveExcelPathFailWithTwoFiles() {
        JPSRuntimeException thrown = assertThrows(JPSRuntimeException.class, () -> FileManager.retrieveExcelPath(multiFileTempDir.toString()), "JPSRuntimeException was expected");
        assertEquals("Multiple Excel workbooks detected! This agent can only process one workbook at a time.", thrown.getMessage());
    }

    @Test
    void testOverloadRetrieveExcelPath() {
        // Note that the current working directory follows the pom.xml location
        // For testing, we will create a new temp data directory to ensure the method is running properly
        String workingDir = System.getProperty("user.dir") + "/data/";
        try {
            // Create an empty directory for the default directory
            Files.createDirectories(Paths.get(workingDir));
            // Test that the default directory for the overloaded method ends with the /data/
            JPSRuntimeException thrown = assertThrows(JPSRuntimeException.class, FileManager::retrieveExcelPath, "JPSRuntimeException was expected");
            assertTrue(thrown.getMessage().contains("No Excel workbook detected! Please place your file in the directory:"));
            assertTrue(thrown.getMessage().endsWith("/data/"));
        } catch (IOException e) {
            throw new JPSRuntimeException(e);
        } finally {
            // Delete this temp directory regardless of result
            File dataDir = new File(workingDir);
            dataDir.delete();
        }
    }

    @Test
    void testRetrieveEndpoint() throws IOException {
        Path propertiesPath = tempDir.resolve(clientProperties);
        genSampleProperties(propertiesPath);
        assertEquals(endpointValue, FileManager.retrieveEndpoint(propertiesPath.toString(), endpointKey));
    }

    @Test
    void testRetrieveEndpointFailNoFile() {
        Path propertiesPath = emptyTempDir.resolve(clientProperties);
        JPSRuntimeException thrown = assertThrows(JPSRuntimeException.class,
                () -> FileManager.retrieveEndpoint(propertiesPath.toString(), endpointKey), "JPSRuntimeException was expected");
        assertEquals("No client.properties file detected! Please place the file in the config directory.", thrown.getMessage());
    }

    @Test
    void testRetrieveEndpointFailNoKey() throws IOException {
        Path propertiesPath = tempDir.resolve(clientProperties);
        genPropertiesNoEndpoint(propertiesPath);
        assertNull(FileManager.retrieveEndpoint(propertiesPath.toString(), endpointKey));
    }

    private static void genSampleProperties(Path propertiesPath) throws IOException {
        Properties prop = new Properties();
        prop.setProperty(endpointKey, endpointValue);
        try (OutputStream output = Files.newOutputStream(propertiesPath)) {
            prop.store(output, null);
        }
    }

    private static void genPropertiesNoEndpoint(Path propertiesPath) throws IOException {
        Properties prop = new Properties();
        prop.setProperty("fail", endpointValue);
        try (OutputStream output = Files.newOutputStream(propertiesPath)) {
            prop.store(output, null);
        }
    }

    private static void createTestExcel(Path excel) {
        // Set up
        Workbook wb = new HSSFWorkbook();
        CreationHelper createHelper = wb.getCreationHelper();
        Sheet sheet = wb.createSheet("Test");

        // Header rows
        Row row = sheet.createRow(0); // first sheet
        row.createCell(0).setCellValue(createHelper.createRichTextString("dates"));
        row.createCell(1).setCellValue(createHelper.createRichTextString("Values"));
        // Starting row for contents of first and second sheet
        for (int i = 1; i < 2; i++) {
            Row contentRow = sheet.createRow(i);
            // Set style and add current Date value
            Cell cell = contentRow.createCell(0);
            cell.setCellValue(1);
            Cell firstCell = contentRow.createCell(1);
            firstCell.setCellValue(1);
            // Output the Excel workbook with three sheets to Temp Directory
            try (OutputStream output = Files.newOutputStream(excel)) {
                wb.write(output);
                wb.close();
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }
    }
}