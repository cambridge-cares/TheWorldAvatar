/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.chem.filemgmt.gaussian.parser.geometry;

/**
 *
 * @author pb556
 */
public class G03GeometryParser extends GauOutputGeometryParser {
    public G03GeometryParser() {    
        this.validExtensions = new String[]{".g03"};
    }
}