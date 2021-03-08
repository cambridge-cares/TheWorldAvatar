/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.tools.file.writer;

import com.cmclinnovations.io.file.writer.FileWriter;
import com.cmclinnovations.io.writer.ExtendedWriterIntf;
import java.util.List;

/**
 *
 * @author pb556
 */
public class StringListWriter extends FileWriter<List> implements ExtendedWriterIntf<List> {
    
    private StringWriter writer = new StringWriter();
    
    @Override
    public void setContent(List content) throws Exception {
        String strContent = "";
        List<String> list = (List<String>) content;
        String eol = System.getProperty("line.separator");
        for (int i = 0; i < list.size() - 1; i++) {
            strContent += list.get(i) + eol;
        }
        strContent += list.get(list.size() - 1);
        this.writer.setContent(strContent);
    }
    
    @Override
    public void write() throws Exception {
        this.writer.setEncoding(this.encoding);
        this.writer.overwrite(this.overwrite);
        this.writer.set(f);
        this.writer.write();
    }

    @Override
    public void append() throws Exception {
        this.writer.setEncoding(this.encoding);
        this.writer.overwrite(this.overwrite);
        this.writer.set(f);
        this.writer.append();
    }

    @Override
    public void insert(int line) throws Exception {
        this.writer.setEncoding(this.encoding);
        this.writer.overwrite(this.overwrite);
        this.writer.set(f);
        this.writer.insert(line);
    }

    @Override
    public void clear() throws Exception {
        this.writer.setEncoding(this.encoding);
        this.writer.overwrite(this.overwrite);
        this.writer.set(this.f);
        this.writer.clear();
    }

    @Override
    public void delete(int start, int end) throws Exception {
        this.writer.setEncoding(this.encoding);
        this.writer.overwrite(this.overwrite);
        this.writer.set(this.f);
        this.writer.delete(start, end);
    }

    @Override
    public void delete(int start) throws Exception {
        this.writer.setEncoding(this.encoding);
        this.writer.overwrite(this.overwrite);
        this.writer.set(this.f);
        this.writer.delete(start);
    }

    @Override
    public void insert(int pos, int line) throws Exception {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public void delete() throws Exception {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
    
}
