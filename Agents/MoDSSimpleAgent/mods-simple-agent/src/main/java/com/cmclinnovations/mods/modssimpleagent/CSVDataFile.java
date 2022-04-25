package com.cmclinnovations.mods.modssimpleagent;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import com.cmclinnovations.mods.modssimpleagent.datamodels.IDataTable;

import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVPrinter;

public class CSVDataFile extends AbstractCSVDataFile {

    public CSVDataFile(IDataTable data) {
        super(data);
    }

    @Override
    public void marshal(Path filePath) throws FileGenerationException {
        try (CSVPrinter printer = new CSVPrinter(Files.newBufferedWriter(filePath), CSVFormat.RFC4180)) {
            printer.printRecord(getHeaders());
            printer.printRecords(getRows());
        } catch (IOException ex) {
            throw new FileGenerationException("Filed to write out a Data object to the CSV file '" + filePath + "'.",
                    ex);
        }
    }

}
