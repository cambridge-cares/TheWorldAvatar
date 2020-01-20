/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.paper.enthalpy.utils;

import com.cmclinnovations.io.file.FileOperations;
import java.io.File;
import java.util.Collection;

/**
 *
 * @author pb556
 */
public class RemoveSpecificFolder {

    private static final String src = "W:\\projects\\enthalpy-methodology\\2016-05-17\\results\\pool-filtering\\isd-only_post-processed\\enthalpy\\error_method_1\\";
    private static final String[] folderNames = {"csv-complete_enthalpy"}; // csv_enthalpy_rec-multi-ordered

    public static void main(String[] args) throws Exception {
        for (int scale = 1; scale <= 3; scale++) {
            if (new File(src + "\\scale_" + scale).exists()) {
                for (int analysis = 1; analysis <= 6; analysis++) {
                    if (new File(src + "\\scale_" + scale + "\\analysis_" + analysis).exists()) {
                        if (new File(src + "\\scale_" + scale + "\\analysis_" + analysis + "\\" + folderNames[0]).exists()) {
                            Collection<File> folderFiles = FileOperations.ls(new File(src + "\\scale_" + scale + "\\analysis_" + analysis + "\\" + folderNames[0]), true);
                            for (File f : folderFiles) {
                                f.delete();
                                        //System.out.println(f.getAbsolutePath());
                            }
                            new File(src + "\\scale_" + scale + "\\analysis_" + analysis + "\\" + folderNames[0]).delete();
                        }
                        for (int run = 0; run < 50; run++) {
                            if (new File(src + "\\scale_" + scale + "\\analysis_" + analysis + "\\run_" + run).exists()) {
                                if (new File(src + "\\scale_" + scale + "\\analysis_" + analysis + "\\run_" + run + "\\" + folderNames[0]).exists()) {
                                    Collection<File> folderFiles = FileOperations.ls(new File(src + "\\scale_" + scale + "\\analysis_" + analysis + "\\run_" + run + "\\" + folderNames[0]), true);
                                    for (File f : folderFiles) {
                                        f.delete();
                                        //System.out.println(f.getAbsolutePath());
                                    }
                                    new File(src + "\\scale_" + scale + "\\analysis_" + analysis + "\\run_" + run + "\\" + folderNames[0]).delete();
                                }
                                for (int i = 0; i < 100; i++) {
                                    System.out.println(src + "\\scale_" + scale + "\\analysis_" + analysis + "\\run_" + run + "\\" + i);
                                    if (new File(src + "\\scale_" + scale + "\\analysis_" + analysis + "\\run_" + run + "\\" + i).exists()) {
                                        if (new File(src + "\\scale_" + scale + "\\analysis_" + analysis + "\\run_" + run + "\\" + i + "\\" + folderNames[0]).exists()) {
                                            Collection<File> folderFiles = FileOperations.ls(new File(src + "\\scale_" + scale + "\\analysis_" + analysis + "\\run_" + run + "\\" + i + "\\" + folderNames[0]), true);
                                            for (File f : folderFiles) {
                                                f.delete();
                                        //System.out.println(f.getAbsolutePath());
                                            }
                                            new File(src + "\\scale_" + scale + "\\analysis_" + analysis + "\\run_" + run + "\\" + i + "\\" + folderNames[0]).delete();
                                        }
                                    } else {
                                        break;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
