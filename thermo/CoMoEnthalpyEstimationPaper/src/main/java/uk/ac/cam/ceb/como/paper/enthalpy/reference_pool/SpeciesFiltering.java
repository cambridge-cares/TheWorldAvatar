/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

package uk.ac.cam.ceb.como.paper.enthalpy.reference_pool;

import com.cmclinnovations.io.file.FileOperations;
import com.cmclinnovations.io.file.filter.extension.specific.G09FileFilter;
import com.cmclinnovations.io.file.filter.extension.specific.SdfFileFilter;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.util.Collection;

/**
 *
 * @author pb556
 */

public class SpeciesFiltering {
    
    private static String srcSdf = "C:\\Users\\pb556\\workspace\\methodology\\nist\\geom\\";
    private static String srcG09 = "C:\\Users\\pb556\\workspace\\methodology\\g09_reference\\";
    
    private static String destG09 = "C:\\Users\\pb556\\workspace\\methodology\\g09_reference_filtered\\";
    
    public static void main(String[] args) throws Exception {
        Collection<File> sdfFiles = FileOperations.ls(new File(srcSdf), true, new FileFilter[]{new SdfFileFilter()});
        Collection<File> g09Files = FileOperations.ls(new File(srcG09), true, new FileFilter[]{new G09FileFilter()});
        for (File g09 : g09Files) {
            boolean valid = false;
            for (File sdf : sdfFiles) {
                if (sdf.getName().replace(".sdf", "").compareTo(g09.getName().replace(".g09", "")) == 0) {
                    valid = true;
                    break;
                }
            }
            if (valid) {
                FileOperations.copy(g09, new File(destG09 + File.separator + g09.getName()));
            }
        }
    }
}