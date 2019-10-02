/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.io.results;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import org.apache.log4j.Logger;
import org.cam.ceb.como.enthalpy.calculator.io.Writer;
import org.cam.ceb.como.tools.file.writer.StringListWriter;

/**
 *
 * @author pb556
 */
public class ResultsWriter extends Writer {

    protected boolean overwrite = false;
    protected ResultsDataList data = new ResultsDataList();
    private Logger logger = Logger.getLogger(getClass());
    
    public ResultsWriter() {
        super();
    }

    public ResultsWriter(String path) {
        super(path);
    }

    public ResultsWriter(File file) {
        super(file);
    }

    public void set(ResultsDataList data) {
        this.data = data;
    }

    public void set(ResultsData data) {
        this.data = new ResultsDataList();
        this.data.add(data);
    }
    
    public void overwrite(boolean flag) {
        overwrite = flag;
    }
    
    @Override
    public void write() {
        if (file == null) {
            logger.error("File cannot be read. No path is defined.", new IOException("No path is defined."));
        }
        try {
            getWriter().write();
        } catch (Exception ex) {
            logger.error("File " + file.getAbsolutePath() + " could not be written!", ex);
        }
    }

    @Override
    public void append() {
        if (file == null) {
            logger.error("File cannot be read. No path is defined.", new IOException("No path is defined."));
        }
        try {
            getWriter().append();
        } catch (Exception ex) {
            logger.error("File " + file.getAbsolutePath() + " could not be written!", ex);
        }
    }
    
    protected StringListWriter getWriter() {
        StringListWriter writer = new StringListWriter();
        writer.setContent(getContent());
        writer.setPath(file.getAbsolutePath());
        writer.setOverwrite(overwrite);
        return writer;
    }
    
    protected ArrayList<String> getContent() {
        ArrayList<String> list = new ArrayList<String>();
        for (ResultsData r : data) {
            if (r != null) {
                list.add(r.toString());
            }
        }
        return list;
    }
    
}
