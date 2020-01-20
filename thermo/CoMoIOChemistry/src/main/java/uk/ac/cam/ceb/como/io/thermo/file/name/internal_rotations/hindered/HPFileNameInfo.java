/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.io.thermo.file.name.internal_rotations.hindered;

import java.io.File;

/**
 *
 * @author pb556
 */
public interface HPFileNameInfo {
    
    public String getBase() throws Exception;
    public Double getTorsionalAngle() throws Exception;
    public String getPrefix() throws Exception;
    public String getSuffix() throws Exception;
    public String getFileExtension() throws Exception;
    public File getDirectory() throws Exception;
    
    public void setBase(String base) throws Exception;
    public void setTorsionalAngle(String angle) throws Exception;
    public void setPrefix(String prefix) throws Exception;
    public void setSuffix(String suffix) throws Exception;
    public void setFileExtension(String extension) throws Exception;
    public void setDirectory(File f) throws Exception;
}
