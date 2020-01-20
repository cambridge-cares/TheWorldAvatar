/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.reactions;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

import org.apache.log4j.Logger;

import com.cmclinnovations.io.file.writer.FileWriter;
import com.cmclinnovations.io.writer.ExtendedWriterIntf;

import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.Reaction;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.reaction.ReactionList;
import uk.ac.cam.ceb.como.tools.file.writer.StringListWriter;

/**
 *
 * @author pb556
 */
public class ReactionListWriter extends FileWriter<ReactionList> implements ExtendedWriterIntf<ReactionList> {

    // 0.235294x"278-06-8" 1.0x"74-82-8" <-> 0.294118x"1069-53-0"
    protected ReactionList reactions = null;
    protected boolean overwrite = false;
    private Logger logger = Logger.getLogger(getClass());

    public ReactionListWriter() {
        super();
    }

    public ReactionListWriter(String path) throws Exception {
        set(path);
    }

    public ReactionListWriter(File file) throws Exception {
        set(file);
    }

    public void set(ReactionList reactions) {
        this.reactions = reactions;
    }

    public void set(Reaction reaction) {
        reactions = new ReactionList();
        reactions.add(reaction);
    }
    
    @Override
    public void overwrite(boolean flag) {
        overwrite = flag;
    }

    @Override
    public void write() {
        if (f == null) {
            logger.error("File cannot be written. No path is defined.", new IOException("No path is defined."));
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
            logger.error("File cannot be written. No path is defined.", new IOException("No path is defined."));
        }
        try {
            getWriter().append();
        } catch (Exception ex) {
            logger.error("File " + f.getAbsolutePath() + " could not be written!", ex);
        }
    }
    
    protected StringListWriter getWriter() throws Exception {
        StringListWriter writer = new StringListWriter();
        writer.setContent(getLineContent());
        writer.set(f.getAbsolutePath());
        writer.overwrite(overwrite);
        return writer;
    }
    
    protected ArrayList<String> getLineContent() {
        ArrayList<String> list = new ArrayList<>();
        for (Reaction r : reactions) {
            if (r.getReactants().size() >= 1 && r.getProducts().size() >= 1) {
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
