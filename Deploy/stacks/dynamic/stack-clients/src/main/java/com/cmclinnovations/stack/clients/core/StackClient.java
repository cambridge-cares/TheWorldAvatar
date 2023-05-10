package com.cmclinnovations.stack.clients.core;

import java.nio.file.Path;
import java.util.Map;

public final class StackClient {

    public static final String STACK_NAME_KEY = "STACK_NAME";
    private static final String STACK_BASE_DIR_KEY = "STACK_BASE_DIR";
    public static final String STACK_NAME_LABEL = "com.docker.stack.namespace";
    public static final String PROJECT_NAME_LABEL = "com.docker.compose.project";
    public static final String SCRATCH_DIR = "/stack_scratch";
    public static final String GEOTIFFS_DIR = "/geotiffs";
    public static final Path STACK_CONFIG_DIR = Path.of("/inputs/config");

    private static final String stackName;

    private static final Map<String, String> stackNameLabelMap;

    private static boolean inStack = true;

    static {
        String envVarStackName = System.getenv(StackClient.STACK_NAME_KEY);
        stackName = (null != envVarStackName) ? envVarStackName : "Test_Stack";

        stackNameLabelMap = Map.of(STACK_NAME_LABEL, stackName, PROJECT_NAME_LABEL, stackName);
    }

    private StackClient() {
    }

    public static String getStackName() {
        return stackName;
    }

    public static String prependStackName(String name) {
        return stackName + "_" + name;
    }

    public static String removeStackName(String name) {
        return name.replaceFirst("^/?" + StackClient.getStackName() + "(?:-|_)", "");
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

    public static String getContainerEngineName() {
        return System.getenv().getOrDefault("EXECUTABLE", "docker");
    }

    private static Path getStackBaseDir() {
        return Path.of(System.getenv(STACK_BASE_DIR_KEY));
    }

}
