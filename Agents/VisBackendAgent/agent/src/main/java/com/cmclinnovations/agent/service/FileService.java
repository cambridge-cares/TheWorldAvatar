package com.cmclinnovations.agent.service;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileSystemNotFoundException;
import java.text.MessageFormat;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.core.io.Resource;
import org.springframework.core.io.ResourceLoader;
import org.springframework.stereotype.Service;
import org.springframework.util.StreamUtils;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

@Service
public class FileService {
  private final ObjectMapper objectMapper;
  private final ResourceLoader resourceLoader;
  private static final Logger LOGGER = LogManager.getLogger(FileService.class);

  public static final String SPRING_FILE_PATH_PREFIX = "file:/";
  private static final String RESOURCE_DIR = "usr/local/tomcat/resources/";
  public static final String APPLICATION_FORM_RESOURCE = RESOURCE_DIR + "application-form.json";

  private static final String CLASS_PATH_DIR = "classpath:";
  private static final String QUERY_DIR = CLASS_PATH_DIR + "query/";
  private static final String QUERY_CONSTR_DIR = QUERY_DIR + "construct/";
  private static final String QUERY_GET_DIR = QUERY_DIR + "get/";
  public static final String FORM_QUERY_RESOURCE = QUERY_CONSTR_DIR + "form.sparql";
  public static final String INSTANCE_QUERY_RESOURCE = QUERY_GET_DIR + "instance.sparql";

  public static final String REPLACEMENT_TARGET = "[target]";

  /**
   * Constructs a new service with the following dependencies.
   * 
   * @param resourceLoader ResourceLoader instance for loading file resources.
   * @param objectMapper   The JSON object mapper.
   */
  public FileService(ResourceLoader resourceLoader, ObjectMapper objectMapper) {
    this.resourceLoader = resourceLoader;
    this.objectMapper = objectMapper;
  }

  /**
   * Retrieve the target file contents with replacement for [target].
   * 
   * @param resourceFilePath File path to resource.
   * @param replacement      The value to replace [target] with.
   */
  public String getContentsWithReplacement(String resourceFilePath, String replacement) {
    LOGGER.debug("Retrieving the contents at {}...", resourceFilePath);
    String contents = "";
    try (InputStream inputStream = this.resourceLoader.getResource(resourceFilePath).getInputStream()) {
      contents = StreamUtils.copyToString(inputStream, StandardCharsets.UTF_8);
      contents = contents.replace(REPLACEMENT_TARGET, replacement);
    } catch (FileNotFoundException e) {
      LOGGER.info("Resource at {} is not found. Please ensure you have a valid resource in the file path.",
          resourceFilePath);
      throw new FileSystemNotFoundException(MessageFormat.format(
          "Resource at {0} is not found. Please ensure you have a valid resource in the file path.", resourceFilePath));
    } catch (IOException e) {
      LOGGER.error(e);
      throw new UncheckedIOException(e);
    }
    return contents;
  }

  /**
   * Find the target class within a JSON file associated with the identifier.
   * 
   * @param target           The identifier for the target class.
   * @param resourceFilePath File path to resource.
   */
  public String getTargetClass(String target, String resourceFilePath) {
    LOGGER.debug("Finding the target class for the identifier {}...", target);
    Resource resource = this.resourceLoader.getResource(resourceFilePath);
    try (InputStream inputStream = resource.getInputStream()) {
      JsonNode resourceNode = this.objectMapper.readTree(inputStream).findValue(target);
      if (resourceNode == null) {
        LOGGER.error("No valid identifier found for {}!", target);
        return "";
      }
      return this.objectMapper.treeToValue(resourceNode, String.class);
    } catch (FileNotFoundException e) {
      LOGGER.info("Resource at {} is not found. Please ensure you have a valid resource in the file path.",
          resourceFilePath);
      throw new FileSystemNotFoundException(MessageFormat.format(
          "Resource at {0} is not found. Please ensure you have a valid resource in the file path.", resourceFilePath));
    } catch (IOException e) {
      LOGGER.info(e.getMessage());
      throw new UncheckedIOException(e);
    }
  }
}