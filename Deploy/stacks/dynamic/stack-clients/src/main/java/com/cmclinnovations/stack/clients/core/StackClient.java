package com.cmclinnovations.stack.clients.core;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;
import java.util.stream.Stream;

import com.cmclinnovations.stack.clients.core.datasets.Dataset;
import com.fasterxml.jackson.databind.ObjectMapper;

public final class StackClient {

    public static final String STACK_NAME_KEY = "STACK_NAME";
    public static final String STACK_NAME_LABEL = "com.docker.stack.namespace";
    public static final String SCRATCH_DIR = "/stack_scratch";

    private static final String stackName;

    private static final Map<String, String> stackNameLabelMap;

    private static boolean inStack = true;

    static {
        String envVarStackName = System.getenv(StackClient.STACK_NAME_KEY);
        stackName = (null != envVarStackName) ? envVarStackName : "Test_Stack";

        stackNameLabelMap = Map.of(STACK_NAME_LABEL, stackName);
    }

    private StackClient() {
    }

    public static String getStackName() {
        return stackName;
    }

    public static String prependStackName(String name) {
        return stackName + "_" + name;
    }

    public static Map<String, String> getStackNameLabelMap() {
        return stackNameLabelMap;
    }

    public static boolean isInStack() {
        return inStack;
    }

    public static void setInStack(boolean inStack) {
        StackClient.inStack = inStack;
    }

    public static void uploadInputDatasets() {
        try (Stream<Path> files = Files.list(Path.of("/inputs/config"))) {
            files.filter(Files::isRegularFile)
                    .filter(path -> path.toString().endsWith(".json"))
                    .forEach(StackClient::uploadInputDataset);
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

    public static void uploadInputDataset(Path configFile) {

        ObjectMapper objectMapper = new ObjectMapper();

        try {
            Dataset dataset = objectMapper.readValue(configFile.toFile(), Dataset.class);
            dataset.loadData();
        } catch (IOException ex) {
            throw new RuntimeException("Failed to read in dataset config file '" + configFile + "'.", ex);
        }
    }

}
