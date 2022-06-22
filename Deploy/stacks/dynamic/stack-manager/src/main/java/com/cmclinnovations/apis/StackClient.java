package com.cmclinnovations.apis;

import java.util.Map;

public final class StackClient {

    public static final String STACK_NAME_KEY = "STACK_NAME";

    private static final String stackName;

    public static final Map<String, String> stackNameLabelMap;

    private static boolean inStack = true;

    static {
        stackName = System.getenv(StackClient.STACK_NAME_KEY);

        stackNameLabelMap = Map.of(StackClient.STACK_NAME_KEY, stackName);
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

}
