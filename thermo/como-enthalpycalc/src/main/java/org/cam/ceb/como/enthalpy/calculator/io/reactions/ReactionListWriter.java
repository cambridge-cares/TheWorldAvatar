/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.enthalpy.calculator.io.reactions;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Map;
import org.apache.log4j.Logger;
import org.cam.ceb.como.enthalpy.calculator.species.Species;
import org.cam.ceb.como.enthalpy.calculator.io.Writer;
import org.cam.ceb.como.enthalpy.calculator.reaction.Reaction;
import org.cam.ceb.como.enthalpy.calculator.reaction.ReactionList;
import org.cam.ceb.como.tools.file.writer.StringListWriter;

/**
 *
 * @author pb556
 */
public class ReactionListWriter extends Writer {

    // 0.235294x"278-06-8" 1.0x"74-82-8" <-> 0.294118x"1069-53-0"
    protected ReactionList reactions = null;
    protected boolean overwrite = false;
    private Logger logger = Logger.getLogger(getClass());

    public ReactionListWriter() {
        super();
    }

    public ReactionListWriter(String path) {
        super(path);
    }

    public ReactionListWriter(File file) {
        super(file);
    }

    public void set(ReactionList reactions) {
        this.reactions = reactions;
    }

    public void set(Reaction reaction) {
        reactions = new ReactionList();
        reactions.add(reaction);
    }
    
    public void overwrite(boolean flag) {
        overwrite = flag;
    }

    @Override
    public void write() {
        if (file == null) {
            logger.error("File cannot be written. No path is defined.", new IOException("No path is defined."));
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
            logger.error("File cannot be written. No path is defined.", new IOException("No path is defined."));
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
        for (Reaction r : reactions) {
            if (r.getReactants().size() >= 1 && r.getProducts().size() >= 1) {
                list.add(r.toString());
            }
        }
        return list;
    }
}
