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
import java.util.Map;
import java.util.Properties;

import static org.junit.jupiter.api.Assertions.*;

class FileManagerTest {
    @TempDir
    private static Path tempDir;
    @TempDir
    private static Path emptyTempDir;
    @TempDir
    private static Path multiFileTempDir;
    private static final String queryEndpoint = "http://www.test.org/test/sparql";
    private static final String updateEndpoint = "http://www.test.org/test/sparql";
    private static final String dbUrl = "jdbc:postgresql://host.docker.internal:5432/test";
    private static final String dbUser = "user";
    private static final String dbPass = "pass";
    private static final String clientProperties = "client.properties";
    private static final String missingProperties = "missing.properties";


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
    void testRetrieveClientProperties() throws IOException {
        Path propertiesPath = tempDir.resolve(clientProperties);
        genSampleProperties(propertiesPath);
        Map<String, String> testProperties = FileManager.retrieveClientProperties(propertiesPath.toString());
        assertEquals(queryEndpoint, testProperties.get(FileManager.QUERY_ENDPOINT_KEY));
        assertEquals(updateEndpoint, testProperties.get(FileManager.UPDATE_ENDPOINT_KEY));
        assertEquals(dbUrl, testProperties.get(FileManager.RDB_URL_KEY));
        assertEquals(dbUser, testProperties.get(FileManager.RDB_USER_KEY));
        assertEquals(dbPass, testProperties.get(FileManager.RDB_PASS_KEY));
    }

    @Test
    void testRetrieveClientPropertiesFailNoFile() {
        Path propertiesPath = emptyTempDir.resolve(clientProperties);
        JPSRuntimeException thrown = assertThrows(JPSRuntimeException.class,
                () -> FileManager.retrieveClientProperties(propertiesPath.toString()), "JPSRuntimeException was expected");
        assertEquals("No client.properties file detected! Please place the file in the config directory.", thrown.getMessage());
    }

    @Test
    void testRetrieveClientPropertiesFailNoKeys() throws IOException {
        Path propertiesPath = tempDir.resolve(clientProperties);
        genPropertiesNoValues(propertiesPath);
        JPSRuntimeException thrown = assertThrows(JPSRuntimeException.class,
                () -> FileManager.retrieveClientProperties(propertiesPath.toString()), "JPSRuntimeException was expected");
        assertEquals("Missing Properties:\n" +
                "sparql.query.endpoint is missing! Please add the input to client.properties.\n" +
                "sparql.update.endpoint is missing! Please add the input to client.properties.\n" +
                "db.url is missing! Please add the input to client.properties.\n" +
                "db.user is missing! Please add the input to client.properties.\n" +
                "db.password is missing! Please add the input to client.properties.\n", thrown.getMessage());
    }

    @Test
    void testRetrieveClientPropertiesFailMissingKeys() throws IOException {
        Path propertiesPath = tempDir.resolve(missingProperties);
        genPropertiesMissingValues(propertiesPath);
        JPSRuntimeException thrown = assertThrows(JPSRuntimeException.class,
                () -> FileManager.retrieveClientProperties(propertiesPath.toString()), "JPSRuntimeException was expected");
        assertEquals("Missing Properties:\n" +
                "sparql.update.endpoint is missing! Please add the input to client.properties.\n" +
                "db.user is missing! Please add the input to client.properties.\n" +
                "db.password is missing! Please add the input to client.properties.\n", thrown.getMessage());
    }

    private static void genSampleProperties(Path propertiesPath) throws IOException {
        Properties prop = new Properties();
        prop.setProperty(FileManager.QUERY_ENDPOINT_PROPERTY, queryEndpoint);
        prop.setProperty(FileManager.UPDATE_ENDPOINT_PROPERTY, updateEndpoint);
        prop.setProperty(FileManager.RDB_URL_PROPERTY, dbUrl);
        prop.setProperty(FileManager.RDB_USER_PROPERTY, dbUser);
        prop.setProperty(FileManager.RDB_PASS_PROPERTY, dbPass);
        try (OutputStream output = Files.newOutputStream(propertiesPath)) {
            prop.store(output, null);
        }
    }

    private static void genPropertiesMissingValues(Path propertiesPath) throws IOException {
        Properties prop = new Properties();
        prop.setProperty(FileManager.QUERY_ENDPOINT_PROPERTY, queryEndpoint);
        prop.setProperty(FileManager.RDB_URL_PROPERTY, dbUrl);
        try (OutputStream output = Files.newOutputStream(propertiesPath)) {
            prop.store(output, null);
        }
    }

    private static void genPropertiesNoValues(Path propertiesPath) throws IOException {
        Properties prop = new Properties();
        prop.setProperty("fail", queryEndpoint);
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