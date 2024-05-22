package com.cmclinnovations.agent;

import java.io.File;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

public class TestUtils {

  private TestUtils() {
  }

  /**
   * A utility function to generate a json file for testing.
   * 
   * @param filePath the target file path.
   * @param node     the json node to write to the file.
   * @param mapper   the object mapper instance.
   */
  public static File genJsonFile(String filePath, JsonNode node, ObjectMapper mapper) {
    try {
      File file = new File(filePath);
      createParentDirectoryIfUnavailable(file);
      mapper.writerWithDefaultPrettyPrinter().writeValue(file, node);
      return file;
    } catch (Exception e) {
      throw new RuntimeException(e.getMessage());
    }
  }

  private static void createParentDirectoryIfUnavailable(File file) {
    // Check if the directory exists, create it if it doesn't
    if (!file.getParentFile().exists()) {
      boolean result = file.getParentFile().mkdirs();
      if (!result) {
        throw new RuntimeException("Directory does not exist. But failed to create directory...");
      }
    }
  }
}
