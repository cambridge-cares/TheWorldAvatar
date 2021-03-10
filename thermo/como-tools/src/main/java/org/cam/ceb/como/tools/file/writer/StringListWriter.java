/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.tools.file.writer;

import java.util.List;

/**
 *
 * @author pb556
 */
public class StringListWriter extends Writer {
    
    private StringWriter writer = new StringWriter();
    
    @Override
    public void setContent(Object content) {
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
        this.writer.setOverwrite(this.overwrite);
        this.writer.setPath(this.path);
        this.writer.write();
    }

    @Override
    public void append() throws Exception {
        this.writer.setEncoding(this.encoding);
        this.writer.setOverwrite(this.overwrite);
        this.writer.setPath(this.path);
        this.writer.append();
    }

    @Override
    public void insert(int line) throws Exception {
        this.writer.setEncoding(this.encoding);
        this.writer.setOverwrite(this.overwrite);
        this.writer.setPath(this.path);
        this.writer.insert(line);
    }

    @Override
    public void clear() throws Exception {
        this.writer.setEncoding(this.encoding);
        this.writer.setOverwrite(this.overwrite);
        this.writer.setPath(this.path);
        this.writer.clear();
    }

    @Override
    public void delete(int start, int end) throws Exception {
        this.writer.setEncoding(this.encoding);
        this.writer.setOverwrite(this.overwrite);
        this.writer.setPath(this.path);
        this.writer.delete(start, end);
    }

    @Override
    public void delete(int start) throws Exception {
        this.writer.setEncoding(this.encoding);
        this.writer.setOverwrite(this.overwrite);
        this.writer.setPath(this.path);
        this.writer.delete(start);
    }
    
}
