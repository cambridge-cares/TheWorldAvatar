/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.io.thermo.file.parser.internal_rotations.hindered;

import java.io.File;
import uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.hindered.potential.discretisation.DiscrRotation;

/**
 *
 * @author pb556
 */
public class HPDirectory {
    
    protected File directory;
    protected File reference;
    protected DiscrRotation rotation = null;
    
    public HPDirectory(File directory, File reference) {
        this.directory = directory;
        this.reference = reference;
    }
    
    public DiscrRotation getRotation() {
        return rotation;
    }
    
    public File getDirectory() {
        return directory;
    }
    
    public File getReferenceFile() {
        return reference;
    }
    
    public void setRotation(DiscrRotation rotation) {
        this.rotation = rotation;
    }
    
    public void setDirectory(File directory) {
        this.directory = directory;
    }
    
    public void setReferenceFile(File reference) {
        this.reference = reference;
    }
}
