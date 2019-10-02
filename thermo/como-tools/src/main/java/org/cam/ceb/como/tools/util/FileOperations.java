/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.tools.util;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import org.apache.commons.io.FileUtils;
import org.apache.log4j.Logger;

/**
 *
 * @author pb556
 */
public class FileOperations {

    private static Logger logger = Logger.getLogger(FileOperations.class.getName());

    public static void copy(File source, File destination) throws Exception {
        try {
            FileUtils.copyFile(source, destination);
        } catch (IOException ex) {
            logger.error("Error in copying file " + source.getAbsolutePath(), ex);
        }
    }

    public static void move(File source, File destination) throws Exception {
        try {
            FileUtils.moveFile(source, destination);
        } catch (IOException ex) {
            logger.error("Error in moving file " + source.getAbsolutePath() + " to " + destination.getAbsolutePath(), ex);
        }
    }

    public static boolean mkdir(File dir) {
        if (dir.mkdirs()) {
            return true;
        }
        logger.error("Directory " + dir.getAbsolutePath() + " could not be created!");
        return false;
    }
    
    public static Collection<File> ls(File source, boolean recursive, boolean directoriesOnly) throws IOException {
        if (directoriesOnly) {
            Collection<File> files = ls(source, recursive);
            ArrayList<File> directories = new ArrayList<File>();
            for (File f : files) {
                if (f.isDirectory()) {
                    directories.add(f);
                }
            }
            return directories;
        }
        return ls(source, recursive);
    }

    public static Collection<File> ls(File source, boolean recursive) throws IOException {
        String[] chld = source.list();
        List<File> files = new ArrayList<File>();
        if (chld == null) {
            logger.error("Specified directory " + source.getAbsolutePath() + " does not exist or is not a directory.");
            throw new IOException("Specified directory does not exist or is not a directory.");
        }
        for (int i = 0; i < chld.length; i++){
            if (new File(source.getAbsolutePath() + "\\" + chld[i]).isDirectory() && recursive) {
                files.addAll(ls(new File(source.getAbsolutePath() + "\\" + chld[i] + "\\"), recursive));
            }
            else {
                files.add(new File(source.getAbsolutePath() + "\\" + chld[i]));
            }
        }
        return files;
    }
    
    public static Collection<File> ls(File source, boolean recursive, Collection<String> extensions) throws IOException {
        String[] ext = new String[extensions.size()];
        int i = 0;
        for (String e : extensions) {
            ext[i] = e;
            i++;
        }
        return ls(source, recursive, ext);
    }
    
    public static Collection<File> ls(File source, boolean recursive, String[] extensions) throws IOException {
        Collection<File> all = ls(source, recursive);
        ArrayList<File> valid = new ArrayList<File>();
        for (File f : all) {
            for (String ext : extensions) {
                if (f.getName().endsWith(ext)) {
                    valid.add(f);
                    break;
                }
            }
        }
        return valid;
    }

//    public static void rm(File source, boolean recursive) throws Exception {
//        try {
//            FileUtil.
//        } catch (IOException ex) {
//            logger.error("Error in moving file " + source.getAbsolutePath() + " to " + destination.getAbsolutePath(), ex);
//        }
//    }
//
//    public static void rmQuietly(File source, boolean recursive) throws Exception {
//        try {
//            FileUtils.moveFile(source, destination);
//        } catch (IOException ex) {
//            logger.error("Error in moving file " + source.getAbsolutePath() + " to " + destination.getAbsolutePath(), ex);
//        }
//    }
}
