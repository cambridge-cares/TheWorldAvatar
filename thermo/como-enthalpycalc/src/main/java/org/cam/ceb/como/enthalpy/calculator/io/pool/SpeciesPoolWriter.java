/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.io.pool;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import org.apache.commons.io.FileUtils;
import org.apache.log4j.Logger;
import org.cam.ceb.como.tools.file.writer.StringListWriter;

/**
 *
 * @author pb556
 */
public class SpeciesPoolWriter extends CSVWriter {
    
    private Logger logger = Logger.getLogger(getClass());

    public SpeciesPoolWriter() {
        super();
    }

    public SpeciesPoolWriter(String path) {
        super(path);
    }

    public SpeciesPoolWriter(File file) {
        super(file);
    }
    
    @Override
    public void write() {
        if (file == null) {
            logger.error("CSV file cannot be written. No path is defined.", new IOException("No path is defined."));
        }
        BufferedWriter out = null;
        try {
            out = new BufferedWriter(new FileWriter(file));
            au.com.bytecode.opencsv.CSVWriter writer = new au.com.bytecode.opencsv.CSVWriter(out);
            writer.writeAll(content);
            out.close();
        } catch (IOException ex) {
            logger.error("CSV file cannot be written.", ex);
        } finally {
            try {
                out.close();
            } catch (IOException ex) {
                logger.error("CSV file cannot be written.", ex);
            }
        }
    }

    @Override
    public void append() {
        if (file == null) {
            logger.error("CSV file cannot be written. No path is defined.", new IOException("No path is defined."));
        }
        BufferedWriter out = null;
        try {
            // lazy version
            List<String> prevData = null;
            if (file.exists()) {
                prevData = FileUtils.readLines(file);
            }
            out = new BufferedWriter(new FileWriter(file));
            au.com.bytecode.opencsv.CSVWriter writer = new au.com.bytecode.opencsv.CSVWriter(out);
            writer.writeAll(content);
            out.close();
            List<String> newData = FileUtils.readLines(file);
            ArrayList<String> allData = new ArrayList<String>();
            if (prevData != null) {
                for (String line : prevData) {
                    allData.add(line);
                }
            }
            for (String line : newData) {
                allData.add(line);
            }
            //FileUtils.deleteQuietly(csv);
            StringListWriter strWriter = new StringListWriter();
            strWriter.setOverwrite(true);
            strWriter.setPath(file.getAbsolutePath());
            strWriter.setContent(allData);
            strWriter.write();
        } catch (Exception ex) {
            logger.error("CSV file cannot be written.", ex);
        } finally {
            try {
                out.close();
            } catch (IOException ex) {
                logger.error("CSV file cannot be written.", ex);
            }
        }
    }
}
