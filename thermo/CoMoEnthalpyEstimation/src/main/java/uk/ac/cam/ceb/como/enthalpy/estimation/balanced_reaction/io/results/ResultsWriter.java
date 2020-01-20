/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.results;

import com.cmclinnovations.io.file.writer.FileWriter;
import com.cmclinnovations.io.writer.ExtendedWriterIntf;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import org.apache.log4j.Logger;
import uk.ac.cam.ceb.como.tools.file.writer.StringListWriter;

/**
 *
 * @author pb556
 */
public class ResultsWriter extends FileWriter<ResultsDataList> implements ExtendedWriterIntf<ResultsDataList> {

    protected boolean overwrite = false;
    protected ResultsDataList data = new ResultsDataList();
    private Logger logger = Logger.getLogger(getClass());
    
    public ResultsWriter() {
        super();
    }

    public ResultsWriter(String path) throws Exception {
        set(path);
    }

    public ResultsWriter(File file) throws Exception {
        set(file);
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
        if (f == null) {
            logger.error("File cannot be read. No path is defined.", new IOException("No path is defined."));
        }
        try {
            getWriter().write();
        } catch (Exception ex) {
            logger.error("File " + f.getAbsolutePath() + " could not be written!", ex);
        }
    }

    @Override
    public void append() {
        if (f == null) {
            logger.error("File cannot be read. No path is defined.", new IOException("No path is defined."));
        }
        try {
            getWriter().append();
        } catch (Exception ex) {
            logger.error("File " + f.getAbsolutePath() + " could not be written!", ex);
        }
    }
    
    protected StringListWriter getWriter() throws Exception {
        StringListWriter writer = new StringListWriter();
        writer.setContent(getListContent());
        writer.set(f.getAbsolutePath());
        writer.overwrite(overwrite);
        return writer;
    }
    
    protected ArrayList<String> getListContent() {
        ArrayList<String> list = new ArrayList<>();
        for (ResultsData r : data) {
            if (r != null) {
                list.add(r.toString());
            }
        }
        return list;
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
