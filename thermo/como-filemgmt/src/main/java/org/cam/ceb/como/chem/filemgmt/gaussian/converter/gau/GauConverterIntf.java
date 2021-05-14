/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.chem.filemgmt.gaussian.converter.gau;

import org.cam.ceb.como.chem.filemgmt.parser.ChemFileParser;
import org.cam.ceb.como.chem.filemgmt.writer.ChemFileWriter;
import java.io.File;

/**
 *
 * @author pb556
 */
public interface GauConverterIntf {
    
    public void setInput(File input);
    public void setOutput(File output);

    public void setHeader(String hdr);
    public void setRestrictedHeader(String hdr);
    
    public void setParser(ChemFileParser parser);
    public void setWriter(ChemFileWriter writer);
    
    public void convert() throws Exception;
}
