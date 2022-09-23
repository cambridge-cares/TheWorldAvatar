package com.cmclinnovations.stack.clients.citydb;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import com.fasterxml.jackson.annotation.JsonProperty;

public class ImpExpOptions {

    public enum Subcommand {
        IMPORT("import"),
        EXPORT("export"),
        VIS_EXPORT("export-vis"),
        DELETE("delete"),
        VALIDATE("validate");

        private final String string;

        private Subcommand(String string) {
            this.string = string;
        }

        @Override
        public String toString() {
            return string;
        }
    }

    private Subcommand subcommand;

    @JsonProperty
    private final Map<String, List<String>> options = new HashMap<>();

    public ImpExpOptions(Subcommand subcommand) {
        this.subcommand = subcommand;
    }

    String[] appendArgs(String... filepaths) {
        List<String> allArgs = new ArrayList<>();
        allArgs.add("impexp");
        allArgs.add(subcommand.toString());

        options.forEach((option, values) -> {
            allArgs.add(option);
            values.stream().collect(Collectors.toCollection(() -> allArgs));
        });

        allArgs.add("--");
        allArgs.addAll(List.of(filepaths));
        return allArgs.toArray(new String[0]);
    }
}