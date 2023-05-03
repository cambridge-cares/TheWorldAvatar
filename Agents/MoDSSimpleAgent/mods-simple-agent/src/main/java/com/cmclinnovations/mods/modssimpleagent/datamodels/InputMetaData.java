package com.cmclinnovations.mods.modssimpleagent.datamodels;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.springframework.http.HttpStatus;
import org.springframework.web.server.ResponseStatusException;

import com.cmclinnovations.mods.modssimpleagent.FileGenerator.FileGenerationException;
import com.cmclinnovations.mods.modssimpleagent.MoDSBackend;
import com.cmclinnovations.mods.modssimpleagent.simulations.Simulation;
import com.cmclinnovations.mods.modssimpleagent.utils.ListUtils;

public class InputMetaData {
    private List<InputMetaDataRow> rows;

    private static final String[] columnNames = { "variable_name", "minimum", "maximum", "mean", "scaling" };

    public static final String DEFAULT_INPUT_INFO_FILE_NAME = "inputMetaData.csv";

    public static InputMetaData createInputMetaData(Request request, MoDSBackend modsBackend) throws IOException {
        Data inputs = request.inputs();

        List<String> varNames = new ArrayList<>();
        List<Double> minima = new ArrayList<>();
        List<Double> maxima = new ArrayList<>();
        List<Double> means = new ArrayList<>();
        List<String> scaling = new ArrayList<>();

        if (request.getSurrogateToLoad() != null) {
            Path path = Simulation.getSurrogateDirectory(modsBackend).resolve(DEFAULT_INPUT_INFO_FILE_NAME);

            if (!Files.exists(path)) {
                throw new IOException("Input Info '" + path + "' file not found in algorithm directory.");
            }

            try (BufferedReader br = new BufferedReader(new FileReader(path.toString()))) {
                String line = br.readLine();
                while ((line = br.readLine()) != null) {
                    String[] values = line.split(",");
                    varNames.add(values[0]);
                    minima.add(Double.parseDouble(values[1]));
                    maxima.add(Double.parseDouble(values[2]));
                    means.add(Double.parseDouble(values[3]));
                    scaling.add(values[4]);
                }
            } catch (IOException ex) {
                throw new ResponseStatusException(HttpStatus.NO_CONTENT, "Failed to read data info load file.", ex);
            }
            return new InputMetaData(varNames, minima, maxima, means, scaling);
        } else if (inputs != null) {
            varNames = inputs.getHeaders().stream().collect(Collectors.toList());

            minima = ListUtils.filterAndSort(inputs.getMinimums().getColumns(), varNames,
                    DataColumn::getName, column -> column.getValues().get(0)).stream().collect(Collectors.toList());

            maxima = ListUtils.filterAndSort(inputs.getMaximums().getColumns(), varNames,
                    DataColumn::getName, column -> column.getValues().get(0)).stream().collect(Collectors.toList());

            means = ListUtils.filterAndSort(inputs.getAverages().getColumns(), varNames,
                    DataColumn::getName, column -> column.getValues().get(0)).stream().collect(Collectors.toList());

            scaling = Collections.nCopies(varNames.size(), "linear");

            return new InputMetaData(varNames, minima, maxima, means, scaling);
        } else {
            throw new IOException("No loaded surrogate or data provided.");
        }
    }

    public InputMetaData(List<String> varNames, List<Double> minima, List<Double> maxima, List<Double> means,
            List<String> scaling) {
        List<InputMetaDataRow> dataInfoRows = new ArrayList<>();
        for (int i = 0; i < varNames.size(); i++) {
            dataInfoRows.add(
                    new InputMetaDataRow(varNames.get(i), minima.get(i), maxima.get(i), means.get(i), scaling.get(i)));
        }
        this.rows = dataInfoRows;
    }

    public void writeToCSV(Path path) throws FileGenerationException {

        try {
            if (!Files.exists(path)) {
                Files.createFile(path);
            }
            writeDataLinesToCSV(path, toWritableStrings());
        } catch (IOException ex) {
            throw new FileGenerationException("Failed to generate data info file.", ex);
        }
    }

    private List<String[]> toWritableStrings() {
        List<String[]> writableStrings = new ArrayList<>();
        writableStrings.add(columnNames);

        rows.stream().forEach(x -> writableStrings.add(x.toWritableStrings()));

        return writableStrings;
    }

    private void writeDataLinesToCSV(Path directory, List<String[]> dataLines) throws IOException {
        try (PrintWriter pw = new PrintWriter(directory.toFile())) {
            dataLines.stream()
                    .map(this::convertToCSV)
                    .forEach(pw::println);
        } catch (IOException ex) {
            throw new IOException("Failed to write data info file.", ex);
        }
    }

    private String convertToCSV(String[] data) {
        return Stream.of(data)
                .collect(Collectors.joining(","));
    }

    public Data meansToData() {
        List<String> varNames = getVarNames();
        List<Double> means = getMeans();

        List<DataColumn> columns = new ArrayList<>();

        for (int i = 0; i < varNames.size(); i++) {
            columns.add(new DataColumn(varNames.get(i), Arrays.asList(means.get(i))));
        }

        return new Data(columns);
    }

    public List<String> getVarNames() {
        return rows.stream().map(InputMetaDataRow::varName).collect(Collectors.toList());
    }

    public List<Double> getMinima() {
        return rows.stream().map(InputMetaDataRow::minimum).collect(Collectors.toList());
    }

    public List<Double> getMaxima() {
        return rows.stream().map(InputMetaDataRow::maximum).collect(Collectors.toList());
    }

    public List<Double> getMeans() {
        return rows.stream().map(InputMetaDataRow::mean).collect(Collectors.toList());
    }

    public List<String> getScaling() {
        return rows.stream().map(InputMetaDataRow::scaling).collect(Collectors.toList());
    }

    public List<InputMetaDataRow> getRows() {
        return rows;
    }

    public void setRows(List<InputMetaDataRow> rows) {
        this.rows = rows;
    }

}
