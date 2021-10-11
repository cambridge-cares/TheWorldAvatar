/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.cares.jps.base.util;

import java.io.File;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.filechooser.FileFilter;

/**
 *
 * @author pb556
 */
public class Dialogs {
    
    private static File lastAccess = new File(System.getProperty("user.home"));
    private static int defaultWidth = 400;
    
    public static void setLastAccess(File f) {
        if (f != null) {
            lastAccess = f;
        }
    }
    
    public static void setDefaultWidth(int width) {
        if (width > 0) {
            defaultWidth = width;
        }
    }

    public static synchronized File[] selectFilesDialog(File dir, FileFilter[] fileFilter) {
        JFileChooser fChooser = getFileChooser(dir, fileFilter);
        fChooser.setCurrentDirectory(lastAccess);
        fChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
        fChooser.setAcceptAllFileFilterUsed(false);
        fChooser.setMultiSelectionEnabled(true);
        if (fChooser.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) {
            File[] files = fChooser.getSelectedFiles();
            if (files != null && files.length > 0) {
                lastAccess = files[0];
            }
            return files;
        }
        return null;
    }

    public static synchronized File[] selectDirectoriesDialog(File dir, FileFilter[] fileFilter, boolean save) {
        JFileChooser fChooser = getFileChooser(dir, fileFilter);
        fChooser.setCurrentDirectory(lastAccess);
        fChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
        fChooser.setAcceptAllFileFilterUsed(false);
        fChooser.setMultiSelectionEnabled(true);
        if (fChooser.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) {
            File[] files = fChooser.getSelectedFiles();
            if (files != null && files.length > 0) {
                lastAccess = files[0];
            }
            return files;
        }
        return null;
    }

    public static synchronized File selectDirectoryDialog(File dir, FileFilter[] fileFilter, boolean save) {
        JFileChooser fChooser = getFileChooser(dir, fileFilter);
        fChooser.setCurrentDirectory(lastAccess);
        fChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
        fChooser.setAcceptAllFileFilterUsed(false);
        File file = null;
        if (save) {
            if (fChooser.showSaveDialog(null) == JFileChooser.APPROVE_OPTION) {
                file = fChooser.getSelectedFile();
            }
        } else {
            if (fChooser.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) {
                file = fChooser.getSelectedFile();
            }
        }
        if (file != null) {
            lastAccess = new File(file + File.separator);
        }
        return file;
    }

    public static synchronized File selectFileDialog(File dir, FileFilter[] fileFilter, boolean save) {
        if (dir.isFile()) {
            return selectFileDialog(dir, dir.getName(), fileFilter, save);
        }
        return selectFileDialog(dir, null, fileFilter, save);
    }
    
    public static synchronized File selectFileDialog(File dir, String name, FileFilter[] fileFilter, boolean save) {
        JFileChooser fChooser = getFileChooser(dir, fileFilter);
        fChooser.setDialogTitle("Select a TBox CSV template file.");
        fChooser.setCurrentDirectory(lastAccess);
        fChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
        fChooser.setAcceptAllFileFilterUsed(false);
        if (name != null) {
            fChooser.setSelectedFile(new File(dir.getAbsolutePath() + File.separator + name));
        }
        File file = null;
        if (save) {
            if (fChooser.showSaveDialog(null) == JFileChooser.APPROVE_OPTION) {
                file = fChooser.getSelectedFile();
            }
        } else {
            if (fChooser.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) {
                file = fChooser.getSelectedFile();
            }
        }
        if (file != null) {
            lastAccess = file;
        }
        return file;
    }

    public static synchronized File showOpenDialog(File dir, FileFilter[] fileFilter) {
        JFileChooser fChooser = getFileChooser(dir, fileFilter);
        fChooser.setCurrentDirectory(lastAccess);
        if (fChooser.showOpenDialog(null) == JFileChooser.APPROVE_OPTION) {
            File file = fChooser.getSelectedFile();
            if (file != null) {
                lastAccess = file;
            }
            return file;
        }
        return null;
    }

    public static int showYesNoConfirmDialog(String msg, String title, int msgType) {
        return JOptionPane.showConfirmDialog(null,
                "<html><p style='width: " + defaultWidth + "px;'>" + msg + "</html>", title, JOptionPane.YES_NO_OPTION, msgType);
    }

    public static synchronized int showYesNoCancelConfirmDialog(String msg, String title, int msgType) {
        return JOptionPane.showConfirmDialog(null,
                "<html><p style='width: " + defaultWidth + "px;'>" + msg + "</html>", title, JOptionPane.YES_NO_CANCEL_OPTION, msgType);
    }

    public static synchronized File showSaveDialog(FileFilter[] fileFilter, boolean checkOverwritting) {
        while (true) {
            File file = showSaveDialog(null, fileFilter);
            if (file == null) {
                return null;
            }
            if (file.exists()) {
                if (showYesNoConfirmDialog(
                        "File '" + file.getName() + "' already exists.<br>Do you want to choose another file name?",
                        "File name", JOptionPane.WARNING_MESSAGE) == JOptionPane.NO_OPTION) {
                    return file;
                }
            } else {
                return file;
            }
        }
    }

    public static synchronized File showSaveDialog(String defaultName, FileFilter[] fileFilter, boolean checkOverwritting) {
        while (true) {
            File file = showSaveDialog(defaultName, fileFilter);
            if (file == null) {
                return null;
            }
            if (file.exists()) {
                if (showYesNoConfirmDialog(
                        "File '" + file.getName() + "' already exists.<br>Do you want to choose another file name?",
                        "File name", JOptionPane.WARNING_MESSAGE) == JOptionPane.NO_OPTION) {
                    return file;
                }
            } else {
                return file;
            }
        }
    }

    public static synchronized File showSaveDialog(String defaultFileName, FileFilter[] fileFilter) {
        JFileChooser fChooser = getFileChooser(null, fileFilter);
        fChooser.setCurrentDirectory(lastAccess);
        if (defaultFileName != null) {
            fChooser.setSelectedFile(new File(defaultFileName));
        }
        if (fChooser.showSaveDialog(null) == JFileChooser.APPROVE_OPTION) {
            File file = fChooser.getSelectedFile();
            if (file != null) {
                lastAccess = file;
            }
            return file;
        }
        return null;
    }

    public static synchronized void showInfoDialog(String msg, String title) {
        JOptionPane.showMessageDialog(null,
                msg, title, JOptionPane.INFORMATION_MESSAGE);
    }

    public static synchronized void showErrorDialog(String msg, String title) {
        JOptionPane.showMessageDialog(null,
                "<html><p style='width: " + defaultWidth + "px;'>" + msg + "</html>", title, JOptionPane.ERROR_MESSAGE);
    }

    public static synchronized void showWarningDialog(String msg, String title) {
        JOptionPane.showMessageDialog(null,
                "<html><p style='width: " + defaultWidth + "px;'>" + msg + "</html>", title, JOptionPane.WARNING_MESSAGE);
    }

    public static synchronized JFileChooser getFileChooser(File dir, FileFilter[] fileFilter) {
        JFileChooser fChooser = new JFileChooser();
        if (dir != null) {
            fChooser.setCurrentDirectory(dir);
        }
        if (fileFilter != null && fileFilter.length != 0) {
            for (FileFilter fFilter : fileFilter) {
                fChooser.addChoosableFileFilter(fFilter);
            }
        }
        return fChooser;
    }
}
