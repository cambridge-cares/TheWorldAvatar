package com.cmclinnovations.stack.clients.gdal;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import com.cmclinnovations.stack.clients.core.StackClient;
import com.fasterxml.jackson.annotation.JsonProperty;

class CommonOptions<T extends CommonOptions<T>> {

    private String sridIn = null;
    private String sridOut = null;

    @JsonProperty
    private final Map<String, String> inputDatasetOpenOptions = new HashMap<>();
    @JsonProperty
    private final Map<String, String> envVars = new HashMap<>();
    @JsonProperty
    private final Map<String, List<String>> otherOptions = new HashMap<>();

    protected CommonOptions() {
    }

    public T setSridIn(String sridIn) {
        this.sridIn = sridIn;
        return (T) this;
    }

    public T setSridOut(String sridOut) {
        this.sridOut = sridOut;
        return (T) this;
    }

    public T addInputDatasetOpenOption(String name, String value) {
        inputDatasetOpenOptions.put(name, value);
        return (T) this;
    }

    public T addOtherOption(String option, String... values) {
        otherOptions.put(option, Arrays.asList(values));
        return (T) this;
    }

    public T withEnv(String key, String value) {
        envVars.put(key, value);
        return (T) this;
    }

    public Map<String, String> getEnv() {
        return envVars;
    }

    protected List<String> appendCommonToArgs(String... args) {

        // Setting this option causes GDAL to try to detect the type of the columns in
        // CSV source files, as described here:
        // https://gdal.org/drivers/vector/csv.html#open-options
        inputDatasetOpenOptions.put("AUTODETECT_TYPE", "YES");
        inputDatasetOpenOptions.put("EMPTY_STRING_AS_NULL", "YES");

        List<String> allArgs = new ArrayList<>(args.length);
        Collections.addAll(allArgs, args);

        if (null != sridOut) {
            allArgs.add("-t_srs");
            allArgs.add(sridOut);
            if (null != sridIn) {
                allArgs.add("-s_srs");
                allArgs.add(sridIn);
            }
        } else {
            if (null != sridIn) {
                allArgs.add("-a_srs");
                allArgs.add(sridIn);
            }
        }

        inputDatasetOpenOptions.forEach((name, value) -> addKeyValuePair(allArgs, "-oo", name, value));

        otherOptions.forEach((option, values) -> {
            allArgs.add(option);
            values.stream()
                    .map(value -> value.startsWith("@") ? handleFileArg(option, value) : value)
                    .collect(Collectors.toCollection(() -> allArgs));
        });
        return allArgs;
    }

    protected void addKeyValuePair(List<String> allArgs, String option, String name, String value) {
        allArgs.add(option);
        allArgs.add(name + "=" + value);
    }

    protected String handleFileArg(String option, String value) {
        Path sourcePath = Path.of(value.replaceFirst("^@", ""));
        Path scratchPath = Path.of(StackClient.SCRATCH_DIR, sourcePath.toString());
        try {
            Files.createDirectories(scratchPath.getParent());
            Files.copy(sourcePath, scratchPath, StandardCopyOption.REPLACE_EXISTING);
            return "@" + scratchPath;
        } catch (IOException ex) {
            throw new RuntimeException(
                    "Failed to copy file '" + sourcePath + "'' referenced in GDAL option '" + option + "' to '"
                            + scratchPath
                            + "'.",
                    ex);
        }
    }
}