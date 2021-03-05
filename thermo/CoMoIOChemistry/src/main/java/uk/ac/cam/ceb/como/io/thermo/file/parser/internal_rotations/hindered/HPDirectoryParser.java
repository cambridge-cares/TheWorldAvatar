/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.io.thermo.file.parser.internal_rotations.hindered;

import com.cmclinnovations.io.file.FileOperations;
import com.cmclinnovations.io.file.parser.DirectoryParser;
import java.io.File;
import java.io.FileFilter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

import uk.ac.cam.ceb.como.io.thermo.file.name.internal_rotations.hindered.HPFileNameInfo;
import uk.ac.cam.ceb.como.io.thermo.file.name.internal_rotations.hindered.HPFileNameInfoDefault;
import uk.ac.cam.ceb.como.io.thermo.file.name.internal_rotations.hindered.HPFileNameInfoIntf;
import uk.ac.cam.ceb.como.math.data.DataSpace;
import uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.hindered.potential.discretisation.RotationalMode;

/**
 *
 * @author pb556
 */
public abstract class HPDirectoryParser extends DirectoryParser<HPDirectory> {

    // reads a complete directory and organises the files accordingly
    protected HPDirectory directory = new HPDirectory(null, null);
    protected HPFileNameInfoIntf interpreter = new HPFileNameInfoDefault();
    protected boolean parsed = false;

    public HPDirectoryParser() {
    }
    
    // integration of a file filter which is specifically developed for the hindered rotations
    public HPDirectoryParser(HPDirectory directory, HPFileNameInfoIntf interpreter) {
        this.directory = directory;
        this.interpreter = interpreter;
    }

    public void setReference(File ref) throws Exception {
        if (!ref.exists()) {
            throw new Exception("Defined reference file '" + ref.getAbsolutePath() + "' does not exist!");
        }
        directory.setReferenceFile(ref);
    }

    public File getReference() {
        return directory.getReferenceFile();
    }

    @Override
    public void set(File path) throws Exception {
        super.set(path);
        directory.setDirectory(path);
        directoryContent.clear();
        parsed = false;
    }
    
    public void setFileNameInterpreter(HPFileNameInfoIntf interpreter) {
        this.interpreter = interpreter;
    }

//    public double getScalingFactor() {
//        return scalingFactor;
//    }
    public HPFileNameInfoIntf getFileNameInterpreter() {
        return interpreter;
    }

//    // EXCLUSION OF THE REFERENCE FILE
//    public Collection<File> getFiles() throws Exception {
//        // extract all files - no filtering
//        Collection<File> files = getFileFilter().getValidFiles(getDirectory().getDirectory());
//        ArrayList<File> selectedFiles = new ArrayList<File>();
//        for (File f : files) {
//            if (f.getAbsolutePath().equalsIgnoreCase(getDirectory().getReferenceFile().getAbsolutePath())) {
//                continue;
//            }
//            selectedFiles.add(f);
//        }
//        return selectedFiles;
//    }
    public Map<Double, File> getFilesByAngle() throws Exception {
        if (!parsed) {
            parse();
        }
        HashMap<Double, File> sorted = new HashMap<>();
        for (File f : directoryContent) {
            HPFileNameInfoIntf intprt = getFileNameInterpreter();
            intprt.interpret(f.getAbsolutePath());
            sorted.put(intprt.getTorsionalAngle(), f);
        }
        return sorted;
    }

//    public boolean createRotationObject() throws Exception {
//        try {
////            HRFileNameInterpreter intprt = getFileNameInterpreter();
////            intprt.interpret(getDirectory().getDirectory().getAbsolutePath());
////            getDirectory().setRotation(new Rotation(intprt.getBase(), getDataSpace(), getRotationalMode()));
//            getDirectory().setRotation(new Rotation(getDirectory().getDirectory().getAbsolutePath(), getDataSpace(), getRotationalMode()));
//        } catch (Exception e) {
//            return false;
//        }
//        return true;
//    }
    public File getFileByAngle(double angle) throws Exception {
        Map<Double, File> files = getFilesByAngle();
        if (files.containsKey(angle)) {
            return files.get(angle);
        }
        // floating point error
        double tolerance = 0.0001;
        for (Double val : files.keySet()) {
            if (Math.abs(val - angle) < tolerance) {
                return files.get(val);
            }
        }
        return null;
    }

    public Map<Double, File> getFileByAngleRange(double startAngle, double endAngle) throws Exception {
        Map<Double, File> files = getFilesByAngle();
        Map<Double, File> selected = new HashMap<Double, File>();
        for (Double val : files.keySet()) {
            if (val >= startAngle && val <= endAngle) {
                selected.put(val, files.get(val));
            }
        }
        return selected;
    }

//    protected Collection<File> filterByExtensions(Collection<File> files, String[] validExtensions) {
//        List<File> valid = new ArrayList<File>();
//        for (File file : files) {
//            for (String extension : validExtensions) {
//                if (file.getName().toLowerCase().trim().endsWith(extension.toLowerCase())) {
//                    valid.add(file);
//                    break;
//                }
//            }
//        }
//        return valid;
//    }
    //protected abstract double getAngle(File file) throws Exception;
    //protected abstract String getId() throws Exception;
    protected abstract DataSpace getDataSpace() throws Exception;

    protected abstract RotationalMode getRotationalMode() throws Exception;

    protected abstract RotationalMode getRotationalMode(boolean inclTorsBondAtom) throws Exception;

    @Override
    public boolean filter() throws Exception {
        if (filter != null && filter.length > 0
                && directoryContent != null && !directoryContent.isEmpty()) {
            Collection<File> allFiles = new HashSet<>(directoryContent);
            directoryContent.clear();
            for (File f : allFiles) {
                boolean accept = false;
                for (FileFilter fFilter : filter) {
                    if (fFilter.accept(f)) {
                        accept = true;
                        break;
                    }
                }
                if (accept) {
                    directoryContent.add(f);
                }
            }
            parsed = true;
            return true;
        }
        return false;
    }

    @Override
    public void clear() throws Exception {
        directory = new HPDirectory(null, null);
        interpreter = new HPFileNameInfoDefault();
    }

    @Override
    public void parse() throws Exception {
        if (directory.getDirectory() != null) {
            throw new Exception("No source directory has been defined.");
        }
        if (directory.getDirectory().exists() && directory.getDirectory().isDirectory()) {
            Collection<File> allFiles = FileOperations.ls(directory.getDirectory(), true);
            directoryContent = new ArrayList<>(allFiles);
            filter();
        } else {
            throw new Exception("Invalid source directory definition.");
        }
    }

    @Override
    public HPDirectory get() throws Exception {
        return directory;
    }

}
