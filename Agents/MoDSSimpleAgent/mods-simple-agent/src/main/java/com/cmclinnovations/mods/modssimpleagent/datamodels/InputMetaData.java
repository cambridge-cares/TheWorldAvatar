package com.cmclinnovations.mods.modssimpleagent.datamodels;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import com.cmclinnovations.mods.modssimpleagent.MoDSBackend;
<<<<<<< HEAD
=======
import com.cmclinnovations.mods.modssimpleagent.FileGenerator.FileGenerationException;
import com.cmclinnovations.mods.modssimpleagent.simulations.Simulation;
>>>>>>> parent of 4fae184ea3 (Merge branch 'main' of https://github.com/cambridge-cares/TheWorldAvatar into main)
import com.cmclinnovations.mods.modssimpleagent.utils.ListUtils;
import com.fasterxml.jackson.databind.MappingIterator;
import com.fasterxml.jackson.databind.ObjectWriter;
import com.fasterxml.jackson.dataformat.csv.CsvMapper;
import com.fasterxml.jackson.dataformat.csv.CsvSchema;

public class InputMetaData {
    private List<InputMetaDataRow> rows;
<<<<<<< HEAD
    private static final CsvMapper CSV_MAPPER = new CsvMapper();
    private static final CsvSchema CSV_SCHEMA = CSV_MAPPER.typedSchemaFor(InputMetaDataRow.class).withHeader();
=======

    private static final String[] columnNames = { "variable_name", "minimum", "maximum", "mean","scaling" };
>>>>>>> parent of 4fae184ea3 (Merge branch 'main' of https://github.com/cambridge-cares/TheWorldAvatar into main)

    public static final String DEFAULT_INPUT_INFO_FILE_NAME = "inputMetaData.csv";
    private static final String DEFAULT_SCALING = "linear";

    public InputMetaData(List<InputMetaDataRow> rows) {
        this.rows = rows;
    }

    public InputMetaData(List<String> varNames, List<Double> minima, List<Double> maxima, List<Double> means,
            List<String> scalings) {
        List<InputMetaDataRow> dataInfoRows = new ArrayList<>();
        for (int i = 0; i < varNames.size(); i++) {
            dataInfoRows.add(
                    new InputMetaDataRow(varNames.get(i), minima.get(i), maxima.get(i), means.get(i), scalings.get(i)));
        }
        this.rows = dataInfoRows;
    }

    public static InputMetaData createInputMetaData(Path inputMetaDataPath) throws IOException {
        if (!Files.exists(inputMetaDataPath)) {
            throw new IOException("Input metadata file '" + inputMetaDataPath + "' not found.");
        }
        MappingIterator<InputMetaDataRow> rowIter = CSV_MAPPER.readerWithTypedSchemaFor(InputMetaDataRow.class)
                .with(CSV_SCHEMA).readValues(inputMetaDataPath.toFile());

        return new InputMetaData(rowIter.readAll());
    }

<<<<<<< HEAD
    public static InputMetaData createInputMetaData(Request request, MoDSBackend modsBackend) throws IOException {
        Data inputs = request.inputs();

        if (request.getSurrogateToLoad() != null) {
            return createInputMetaData(modsBackend.getSurrogateDirectory().resolve(DEFAULT_INPUT_INFO_FILE_NAME));
        } else if (inputs != null) {
            return createInputMetaData(inputs);
=======
    public static InputMetaData createInputMetaData(Request request, MoDSBackend modsBackend, Algorithm algorithm) throws IOException {
        Data inputs = request.getInputs();

        List<String> varNames = new ArrayList<>();
        List<Double> minima = new ArrayList<>();
        List<Double> maxima = new ArrayList<>();
        List<Double> means = new ArrayList<>();
        List<String> scaling = new ArrayList<>();

        if (algorithm.getSurrogateToLoad() != null) {

            Path path = Simulation.getSurrogateDirectory(modsBackend)
                    .resolve(DEFAULT_INPUT_INFO_FILE_NAME);

            if (!Files.exists(path)) {
                throw new IOException("Input Info '" + path + "' file not found in algorithm directory.");
            }

            try (BufferedReader br = new BufferedReader(
                    new FileReader(path.toString()))) {
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
                throw new ResponseStatusException(
                        HttpStatus.NO_CONTENT,
                        "Failed to read data info load file.", ex);
            }
        } else if (inputs != null) {
            varNames = algorithm.getVariables().stream().map(Variable::getName)
                    .collect(Collectors.toList());

            minima = ListUtils.filterAndSort(inputs.getMinimums().getColumns(), varNames,
                    DataColumn::getName, column -> column.getValues().get(0)).stream().collect(Collectors.toList());

            maxima = ListUtils.filterAndSort(inputs.getMaximums().getColumns(), varNames,
                    DataColumn::getName, column -> column.getValues().get(0)).stream().collect(Collectors.toList());

            means = ListUtils.filterAndSort(inputs.getAverages().getColumns(), varNames,
                    DataColumn::getName, column -> column.getValues().get(0)).stream().collect(Collectors.toList());

            scaling = Collections.nCopies(varNames.size(), "linear");
>>>>>>> parent of 4fae184ea3 (Merge branch 'main' of https://github.com/cambridge-cares/TheWorldAvatar into main)
        } else {
            throw new IOException("No input data or load location provided.");
        }

        return new InputMetaData(varNames, minima, maxima, means, scaling);
    }

<<<<<<< HEAD
    private static InputMetaData createInputMetaData(Data inputs) {
        List<String> varNames = inputs.getHeaders().stream().collect(Collectors.toList());

        List<Double> minima = ListUtils.filterAndSort(inputs.getMinimums().getColumns(), varNames,
                DataColumn::getName, column -> column.getValues().get(0)).stream().collect(Collectors.toList());

        List<Double> maxima = ListUtils.filterAndSort(inputs.getMaximums().getColumns(), varNames,
                DataColumn::getName, column -> column.getValues().get(0)).stream().collect(Collectors.toList());

        List<Double> means = ListUtils.filterAndSort(inputs.getAverages().getColumns(), varNames,
                DataColumn::getName, column -> column.getValues().get(0)).stream().collect(Collectors.toList());

        List<String> scalings = Collections.nCopies(varNames.size(), DEFAULT_SCALING);

        return new InputMetaData(varNames, minima, maxima, means, scalings);
=======
    public InputMetaData(List<String> varNames, List<Double> minima, List<Double> maxima, List<Double> means, List<String> scaling) {
        List<InputMetaDataRow> dataInfoRows = new ArrayList<>();
        for (int i = 0; i < varNames.size(); i++) {
            dataInfoRows.add(new InputMetaDataRow(varNames.get(i), minima.get(i), maxima.get(i), means.get(i), scaling.get(i)));
        }
        this.rows = dataInfoRows;
>>>>>>> parent of 4fae184ea3 (Merge branch 'main' of https://github.com/cambridge-cares/TheWorldAvatar into main)
    }

    public void writeToCSV(Path path) throws FileGenerationException {
        ObjectWriter writer = CSV_MAPPER.writerFor(InputMetaDataRow.class).with(CSV_SCHEMA);
        try {
            writer.writeValues(path.toFile()).writeAll(getRows());
        } catch (IOException ex) {
            throw new FileGenerationException("Failed to write input metadata to a csv file.", ex);
        }
    }

    public Data getMeansAsData() {
        List<String> varNames = getVarNames();
        List<Double> means = getMeans();

        List<DataColumn> columns = new ArrayList<>();

        for(int i=0; i<varNames.size(); i++){
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

<<<<<<< HEAD
    public List<Double> getMeans() {
        return rows.stream().map(InputMetaDataRow::mean).collect(Collectors.toList());
=======
    public List<Double> getMeans(){
        return rows.stream().map(InputMetaDataRow::getMean).collect(Collectors.toList()); 
>>>>>>> parent of 4fae184ea3 (Merge branch 'main' of https://github.com/cambridge-cares/TheWorldAvatar into main)
    }

    public List<String> getScalings() {
        return rows.stream().map(InputMetaDataRow::scaling).collect(Collectors.toList());
    }

    public List<InputMetaDataRow> getRows() {
        return rows;
    }

    public void setRows(List<InputMetaDataRow> rows) {
        this.rows = rows;
    }

}
