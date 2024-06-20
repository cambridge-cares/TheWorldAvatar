package com.cmclinnovations.stack.clients.gdal;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import com.cmclinnovations.stack.clients.core.Option;
import com.cmclinnovations.stack.clients.core.StackClient;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonInclude(Include.NON_EMPTY)
class CommonOptions<T extends CommonOptions<T>> {

    @JsonIgnore
    private final String command;

    private String sridIn = null;
    private String sridOut = null;

    @JsonProperty
    private final Map<String, String> inputDatasetOpenOptions = new LinkedHashMap<>();
    @JsonProperty
    private final Map<String, String> envVars = new LinkedHashMap<>();
    @JsonProperty
    private final Map<String, Option> otherOptions = new LinkedHashMap<>();
    @JsonProperty
    private final Map<String, String> configOptions = new LinkedHashMap<>();

    protected CommonOptions(String command) {
        this.command = command;
    }

    public final String getSridIn() {
        return sridIn;
    }

    public final T setSridIn(String sridIn) {
        this.sridIn = sridIn;
        return (T) this;
    }

    public final String getSridOut() {
        return sridOut;
    }

    public final T setSridOut(String sridOut) {
        this.sridOut = sridOut;
        return (T) this;
    }

    public final T addInputDatasetOpenOption(String name, String value) {
        inputDatasetOpenOptions.put(name, value);
        return (T) this;
    }

    public final T addOtherOption(String option, String... values) {
        otherOptions.put(option, new Option(values));
        return (T) this;
    }

    public final T addConfigOption(String option, String value) {
        configOptions.put(option, value);
        return (T) this;
    }

    public final T withEnv(String key, String value) {
        envVars.put(key, value);
        return (T) this;
    }

    public final Map<String, String> getEnv() {
        return envVars;
    }

    protected final String[] generateCommandInternal(List<String> args, String source, String destination,
            String... extraArgs) {
        // Prepend the name of the executable
        args.add(0, command);

        processArgs(args);

        Collections.addAll(args, extraArgs);

        processSourceAndDestination(source, destination, args);

        return args.toArray(extraArgs);
    }

    /**
     * Most tools take the source before the destination, override if not.
     */
    protected void processSourceAndDestination(String source, String destination, List<String> args) {
        args.add(source);
        args.add(destination);
    }

    protected void processArgs(final List<String> args) {

        // Setting this option causes GDAL to try to detect the type of the columns in
        // CSV source files, as described here:
        // https://gdal.org/drivers/vector/csv.html#open-options
        processInputDatasetOpenOption(args, "AUTODETECT_TYPE", "YES");
        processInputDatasetOpenOption(args, "EMPTY_STRING_AS_NULL", "YES");

        processSRIDs(args);

        inputDatasetOpenOptions.forEach((name, value) -> processInputDatasetOpenOption(args, name, value));

        otherOptions.forEach((option, values) -> processOtherOption(args, option, values));

        configOptions.forEach((name, value) -> processConfigOption(args, name, value));
    }

    protected void processOtherOption(final List<String> args, String option, Option values) {
        args.add(option);
        values.getOptionList().stream()
                .map(value -> value.startsWith("@") ? handleFileArg(option, value) : value)
                .collect(Collectors.toCollection(() -> args));
    }

    protected void processOtherOption(final List<String> args, String option, String value) {
        args.add(option);
        args.add(value.startsWith("@") ? handleFileArg(option, value) : value);
    }

    protected void processInputDatasetOpenOption(final List<String> args, String name, String value) {
        processKeyValuePair(args, "-oo", name, value);
    }

    protected void processConfigOption(final List<String> args, String name, String value) {
        args.add("--config");
        args.add(name);
        args.add(value);
    }

    protected void processSRIDs(List<String> args) {
        if (null != sridOut) {
            if (null != sridIn) {
                args.add("-s_srs");
                args.add(sridIn);
            }
            args.add("-t_srs");
            args.add(sridOut);
        } else {
            if (null != sridIn) {
                args.add("-a_srs");
                args.add(sridIn);
            }
        }
    }

    protected void processKeyValuePair(List<String> allArgs, String option, String name, String value) {
        allArgs.add(option);
        allArgs.add(name + "=" + value);
    }

    protected String handleFileArg(String option, String value) {
        Path sourcePath = Path.of(value.replaceFirst("^@", ""));
        Path scratchPath = Path.of(StackClient.getScratchDir(), sourcePath.toString());
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