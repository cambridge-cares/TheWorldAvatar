package com.cmclinnovations.mods.modssimpleagent;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import com.cmclinnovations.mods.modssimpleagent.datamodels.DataColumn;
import com.cmclinnovations.mods.modssimpleagent.datamodels.IDataTable;

import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVPrinter;

public class CSVDataSeparateFiles extends AbstractCSVDataFile {

    private final String fileNamePrefix;
    private final String columnNamePrefix;

    public CSVDataSeparateFiles(IDataTable data, String fileNamePrefix) {
        super(data);
        this.fileNamePrefix = fileNamePrefix;
        columnNamePrefix = "";
    }

    public CSVDataSeparateFiles(IDataTable data, String fileNamePrefix, String columnNamePrefix) {
        super(data);
        this.fileNamePrefix = fileNamePrefix;
        this.columnNamePrefix = columnNamePrefix;
    }

    @Override
    public void marshal(Path dirPath) throws FileGenerationException {
        for (DataColumn column : getData().getColumns()) {
            String header = column.getName();
            List<Double> values = column.getValues();
            Path filePath = dirPath.resolve(fileNamePrefix + header + ".csv");
            try (CSVPrinter printer = new CSVPrinter(Files.newBufferedWriter(filePath),
                    CSVFormat.RFC4180)) {
                printer.printRecord(columnNamePrefix + header);
                for (Double value : values) {
                    printer.printRecord(value);
                }
            } catch (IOException ex) {
                throw new FileGenerationException(
                        "Filed to write out a Data object to the CSV file '" + filePath + "'.",
                        ex);
            }
        }
    }

}
