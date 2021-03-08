/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.paper.enthalpy.utils;

import java.io.File;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.filefilter.DirectoryFileFilter;
import org.apache.commons.io.filefilter.RegexFileFilter;

/**
 *
 * @author pb556
 */
public class RemoveSpecificFiles {

    private static final String src = "C:\\Users\\pb556\\enthalpy-methodology\\2016-05-17\\results\\pool-filtering\\isd-only_post-processed\\calc-enthalpy-scaled\\error_method_2\\";
    private static final String[] fileNames = {"invalid-complete-distinct-reaction-list.rct", "invalid-distinct-reaction-list.rct"};

    public static void main(String[] args) throws Exception {
        for (int scale = 1; scale <= 3; scale++) {
            if (new File(src + "\\scale_" + scale).exists()) {
                for (int analysis = 1; analysis <= 6; analysis++) {
                    if (new File(src + "\\scale_" + scale + "\\analysis_" + analysis).exists()) {
                        new File(src + "\\scale_" + scale + "\\analysis_" + analysis + "\\" + fileNames[0].replace(".rct", "-and.rct").replace(".csv", "_and.csv")).delete();
                        new File(src + "\\scale_" + scale + "\\analysis_" + analysis + "\\" + fileNames[0].replace(".rct", "-or.rct").replace(".csv", "_or.csv")).delete();
                        new File(src + "\\scale_" + scale + "\\analysis_" + analysis + "\\" + fileNames[1].replace(".rct", "-and.rct").replace(".csv", "_and.csv")).delete();
                        new File(src + "\\scale_" + scale + "\\analysis_" + analysis + "\\" + fileNames[1].replace(".rct", "-or.rct").replace(".csv", "_or.csv")).delete();
                        for (int run = 0; run < 50; run++) {
                            if (new File(src + "\\scale_" + scale + "\\analysis_" + analysis + "\\run_" + run).exists()) {
                                new File(src + "\\scale_" + scale + "\\analysis_" + analysis + "\\run_" + run + "\\" + fileNames[0]).delete();
                                new File(src + "\\scale_" + scale + "\\analysis_" + analysis + "\\run_" + run + "\\" + fileNames[1]).delete();
                                for (int i = 0; i < 100; i++) {
                                    System.out.println(src + "\\scale_" + scale + "\\analysis_" + analysis + "\\run_" + run + "\\" + i);
                                    if (new File(src + "\\scale_" + scale + "\\analysis_" + analysis + "\\run_" + run + "\\" + i).exists()) {
                                        new File(src + "\\scale_" + scale + "\\analysis_" + analysis + "\\run_" + run + "\\" + i + "\\" + fileNames[0]).delete();
                                        new File(src + "\\scale_" + scale + "\\analysis_" + analysis + "\\run_" + run + "\\" + i + "\\" + fileNames[1]).delete();
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

    private static void execute(String src) {
        System.out.println("Analyse file structure...");
        Collection<File> files = FileUtils.listFiles(new File(src),
                new RegexFileFilter(".*/.rct$"),
                DirectoryFileFilter.DIRECTORY
        );
        //Collection<File> files = FileOperations.ls(new File(src), true, new String[]{"rct"});
        System.out.println("Identify files to be removed...");
        Set<File> toBeRemoved = new HashSet<>();
        for (File f : files) {
            for (String name : fileNames) {
                if (f.getName().compareTo(name) == 0) {
                    toBeRemoved.add(f);
                    break;
                }
            }
        }
        int ctr = 1;
        for (File f : toBeRemoved) {
            System.out.println("Remove file " + f.getAbsolutePath() + " (" + ctr + "/" + toBeRemoved.size() + ")");
            ctr++;
//            f.delete();
        }
    }
}
