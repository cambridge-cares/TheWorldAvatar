/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.pool;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import org.apache.commons.io.FileUtils;
import org.apache.log4j.Logger;
import uk.ac.cam.ceb.como.tools.file.writer.StringListWriter;

/**
 *
 * @author pb556
 */
public class SpeciesPoolWriter extends CSVWriter {
    
    private Logger logger = Logger.getLogger(getClass());

    public SpeciesPoolWriter() {
        super();
    }

    public SpeciesPoolWriter(String path) throws Exception {
        super(path);
    }

    public SpeciesPoolWriter(File file) throws Exception {
        super(file);
    }
    
    @Override
    public void write() {
        if (f == null) {
            logger.error("CSV file cannot be written. No path is defined.", new IOException("No path is defined."));
        }
        BufferedWriter out = null;
        try {
            out = new BufferedWriter(new FileWriter(f));
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
        if (f == null) {
            logger.error("CSV file cannot be written. No path is defined.", new IOException("No path is defined."));
        }
        BufferedWriter out = null;
        try {
            // lazy version
            List<String> prevData = null;
            if (f.exists()) {
                prevData = FileUtils.readLines(f);
            }
            out = new BufferedWriter(new FileWriter(f));
            au.com.bytecode.opencsv.CSVWriter writer = new au.com.bytecode.opencsv.CSVWriter(out);
            writer.writeAll(content);
            out.close();
            List<String> newData = FileUtils.readLines(f);
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
            strWriter.overwrite(true);
            strWriter.set(f.getAbsolutePath());
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

    @Override
    public void clear() throws Exception {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public void insert(int line) throws Exception {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public void insert(int pos, int line) throws Exception {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public void delete() throws Exception {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public void delete(int pos) throws Exception {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public void delete(int start, int end) throws Exception {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
}
