package com.cmclinnovations.stack.clients.citydb;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import com.fasterxml.jackson.annotation.JsonProperty;

public class ImpExpOptions {

    public enum Subcommand {
        IMPORT("import", "--import-mode=skip"),
        EXPORT("export"),
        VIS_EXPORT("export-vis"),
        DELETE("delete"),
        VALIDATE("validate");

        private final String string;

        private final List<String> defaultArgs;

        private Subcommand(String string, String... defaultArgs) {
            this.string = string;
            this.defaultArgs = List.of(defaultArgs);
        }

        @Override
        public String toString() {
            return string;
        }

        public List<String> getDefaultArgs() {
            return defaultArgs;
        }

    }

    private Subcommand subcommand;

    @JsonProperty
    private final String sridIn;

    @JsonProperty
    private final Map<String, List<String>> options = new HashMap<>();

    public ImpExpOptions() {
        this(Subcommand.IMPORT);
    }

    public ImpExpOptions(Subcommand subcommand) {
        this.subcommand = subcommand;
        sridIn = "";
    }

    public String getSridIn() {
        return sridIn;
    }

    String[] appendArgs(String filepath, String... otherArgs) {
        List<String> allArgs = new ArrayList<>();
        allArgs.add("impexp");
        allArgs.add(subcommand.toString());
        allArgs.addAll(subcommand.getDefaultArgs());

        options.forEach((option, values) -> {
            allArgs.add(option);
            values.stream().collect(Collectors.toCollection(() -> allArgs));
        });

        allArgs.addAll(List.of(otherArgs));

        switch (subcommand) {
            case IMPORT:
            case VALIDATE:
                allArgs.add("--");
                allArgs.add(filepath);
                break;
            case EXPORT:
            case VIS_EXPORT:
                allArgs.add("--output");
                allArgs.add(filepath);
                break;
            default:
        }

        return allArgs.toArray(new String[0]);
    }
}