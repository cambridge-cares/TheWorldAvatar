package com.cmclinnovations.mods.modssimpleagent.datamodels;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVPrinter;

import com.cmclinnovations.mods.modssimpleagent.FileGenerator.FileGenerationException;
import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

public class ModelInput {

    @JsonProperty
    private final Data data;

    @JsonProperty
    private final String path;

    @JsonCreator
    public ModelInput(@JsonProperty("data") Data data, @JsonProperty("path") String path) {
        this.data = data;
        this.path = path;
    }

    public void marshal(Path dirPath) throws FileGenerationException {
        Path filePath = dirPath.resolve(path);
        try (CSVPrinter printer = new CSVPrinter(Files.newBufferedWriter(filePath), CSVFormat.RFC4180)) {
            printer.printRecord(data.getHeaders());
            printer.printRecords(data.getAllRows());
        } catch (Exception ex) {
            throw new FileGenerationException("Failed to write out a Data object to the CSV file '" + filePath + "'.",
                    ex);
        }
    }

}
