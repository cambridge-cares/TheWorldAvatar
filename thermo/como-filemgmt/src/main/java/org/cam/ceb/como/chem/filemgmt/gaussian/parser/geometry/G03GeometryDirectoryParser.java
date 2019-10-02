/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.chem.filemgmt.gaussian.parser.geometry;

import org.cam.ceb.como.chem.filemgmt.filter.G09FileFilter;

/**
 *
 * @author pb556
 */
public class G03GeometryDirectoryParser extends GauOutputGeometryDirectoryParser {
    
    public G03GeometryDirectoryParser() {
        this.fFilter = new G09FileFilter();
        this.parser = new G09GeometryParser();
    }
}
