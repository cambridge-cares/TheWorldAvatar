package com.cmclinnovations.filteragent.utils;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.stream.Collectors;

import com.cmclinnovations.filteragent.FilterAgentApplication;

public class FileUtils {

    private FileUtils() {
    }

    public static String readStringFromResources(String resourceName) {
        String string;
        try (InputStream resource = FilterAgentApplication.class.getResourceAsStream(resourceName);
                InputStreamReader inputStreamReader = new InputStreamReader(resource);
                BufferedReader bufferedReader = new BufferedReader(inputStreamReader)) {
            string = bufferedReader.lines().collect(Collectors.joining(System.lineSeparator()));
        } catch (IOException ex) {
            throw new RuntimeException("Failed to load resource \"" + resourceName + "\".", ex);
        }
        return string;
    }

}
