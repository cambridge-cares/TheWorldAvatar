package com.cmclinnovations.agent.service;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.FileSystemNotFoundException;
import java.util.ArrayDeque;
import java.util.Queue;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.springframework.core.io.DefaultResourceLoader;
import org.springframework.core.io.ResourceLoader;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;

public class FileServiceTest {
  private static ResourceLoader resourceLoader;
  private static ObjectMapper objectMapper;

  private static final String RESOURCE_DIR = "file:";
  private static final String SAMPLE_RESOURCE_DIR_PATH = "path/to/";
  private static final String SAMPLE_RESOURCE_FILE = "resource.sparql";
  private static final String MISSING_RESOURCE_FILE = "does/not/exist.txt";

  public static final String IRI_LOCAL_NAME_TEST_CASE1 = "Concept";
  public static final String IRI_TEST_CASE1 = "http://www.example.com/kg/" + IRI_LOCAL_NAME_TEST_CASE1;
  public static final String IRI_LOCAL_NAME_TEST_CASE2 = "19245127857";
  public static final String IRI_TEST_CASE2 = "https://www.example.com/kg/" + IRI_LOCAL_NAME_TEST_CASE2;
  public static final String IRI_LOCAL_NAME_TEST_CASE3 = "HexConcept";
  public static final String IRI_TEST_CASE3 = "https://www.example.org/kg#" + IRI_LOCAL_NAME_TEST_CASE3;
  public static final String INVALID_TEST_CASE = "invalid";

  @BeforeAll
  static void setup() {
    objectMapper = new ObjectMapper();
    resourceLoader = new DefaultResourceLoader();
  }

  @Test
  void testGetContentsWithReplacement() throws IOException {
    // Set up
    String sampleFilePath = SAMPLE_RESOURCE_DIR_PATH + SAMPLE_RESOURCE_FILE;
    File sampleFile = genSampleFile(sampleFilePath, FileService.REPLACEMENT_TARGET);
    try {
      // Execute
      String result = new FileService(resourceLoader, objectMapper)
          .getContentsWithReplacement(RESOURCE_DIR + sampleFilePath,
              IRI_TEST_CASE1);
      // Assert
      assertFalse(result.contains(FileService.REPLACEMENT_TARGET),
          "Placeholder value for replacement should not exist!");
      assertEquals(IRI_TEST_CASE1, result.trim());
      // Clean up
    } finally {
      sampleFile.delete();
    }
  }

  @Test
  void testGetContentsWithReplacement_MissingFile() {
    // Execute and assert
    FileSystemNotFoundException exception = assertThrows(FileSystemNotFoundException.class, () -> {
      new FileService(resourceLoader, objectMapper)
          .getContentsWithReplacement(MISSING_RESOURCE_FILE, IRI_TEST_CASE1);
    });
    assertEquals("Resource at " + MISSING_RESOURCE_FILE
        + " is not found. Please ensure you have a valid resource in the file path.", exception.getMessage());
  }

  @Test
  void testGetResourceTarget() throws IOException {
    // Set up
    String sampleFilePath = SAMPLE_RESOURCE_DIR_PATH + SAMPLE_RESOURCE_FILE;
    // Set up file contents
    Queue<String> testKeys = new ArrayDeque<>();
    Queue<String> testValues = new ArrayDeque<>();
    testKeys.offer(IRI_LOCAL_NAME_TEST_CASE1.toLowerCase());
    testValues.offer(IRI_TEST_CASE1);
    testKeys.offer(IRI_LOCAL_NAME_TEST_CASE2.toLowerCase());
    testValues.offer(IRI_TEST_CASE2);
    testKeys.offer(IRI_LOCAL_NAME_TEST_CASE3.toLowerCase());
    testValues.offer(IRI_TEST_CASE3);
    String sampleJsonContents = genFormJsonTemplate(testKeys, testValues);
    // Generate sample file
    File sampleFormApplicationFile = genSampleFile(sampleFilePath, sampleJsonContents);

    try {
      FileService service = new FileService(resourceLoader, objectMapper);
      // Execute & Assert
      assertEquals(IRI_TEST_CASE1,
          service.getResourceTarget(IRI_LOCAL_NAME_TEST_CASE1.toLowerCase(), RESOURCE_DIR + sampleFilePath));
      assertEquals(IRI_TEST_CASE2,
          service.getResourceTarget(IRI_LOCAL_NAME_TEST_CASE2.toLowerCase(), RESOURCE_DIR + sampleFilePath));
      assertEquals(IRI_TEST_CASE3,
          service.getResourceTarget(IRI_LOCAL_NAME_TEST_CASE3.toLowerCase(), RESOURCE_DIR + sampleFilePath));
    } finally {
      sampleFormApplicationFile.delete();
    }
  }

  @Test
  void testGetResourceTarget_InvalidType() throws IOException {
    // Set up
    String sampleFilePath = SAMPLE_RESOURCE_DIR_PATH + SAMPLE_RESOURCE_FILE;
    // Set up file contents
    Queue<String> testKeys = new ArrayDeque<>();
    Queue<String> testValues = new ArrayDeque<>();
    testKeys.offer(IRI_LOCAL_NAME_TEST_CASE1.toLowerCase());
    testValues.offer(IRI_TEST_CASE1);
    String sampleJsonContents = genFormJsonTemplate(testKeys, testValues);
    // Generate sample file
    File sampleFormApplicationFile = genSampleFile(sampleFilePath, sampleJsonContents);
    try {
      FileService service = new FileService(resourceLoader, objectMapper);
      // Execute & Assert
      assertEquals("", service.getResourceTarget(INVALID_TEST_CASE, RESOURCE_DIR + sampleFilePath));
    } finally {
      sampleFormApplicationFile.delete();
    }
  }

  @Test
  void testGetResourceTarget_MissingFile() {
    // Execute and assert
    FileSystemNotFoundException exception = assertThrows(FileSystemNotFoundException.class, () -> {
      new FileService(resourceLoader, objectMapper)
          .getResourceTarget(INVALID_TEST_CASE, MISSING_RESOURCE_FILE);
    });
    assertEquals("Resource at " + MISSING_RESOURCE_FILE
        + " is not found. Please ensure you have a valid resource in the file path.", exception.getMessage());
  }

  /**
   * Generate a sample file with the required contents.
   *
   * @param filePath     Path to the target file, including its directories.
   * @param fileContents Contents of file.
   * @return A file object so that it can be deleted after testing.
   * @throws IOException
   */
  public static File genSampleFile(String filePath, String fileContents) throws IOException {
    File file = new File(filePath);
    // Check if the directory exists, create it if it doesn't
    if (!file.getParentFile().exists())
      file.getParentFile().mkdirs();
    // Create a new file
    file.createNewFile();
    // Write the lines required
    PrintWriter writer = new PrintWriter(file);
    writer.println(fileContents);
    writer.close();
    return file;
  }

  /**
   * Generate the JSON template for key value pairs for the form resource.
   *
   * @param keys   Keys for the template at the nested level
   * @param values Values for the template at the nested level
   * @return The JSON template as string
   */
  public static String genFormJsonTemplate(Queue<String> keys, Queue<String> values) throws JsonProcessingException {
    ObjectNode resourceNode = objectMapper.createObjectNode();
    while (!keys.isEmpty()) {
      resourceNode.put(keys.poll(), values.poll());
    }
    return objectMapper.writeValueAsString(resourceNode);
  }
}
