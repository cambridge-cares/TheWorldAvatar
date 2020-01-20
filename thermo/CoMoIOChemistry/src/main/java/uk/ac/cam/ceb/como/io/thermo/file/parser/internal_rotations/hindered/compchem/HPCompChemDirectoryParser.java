/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.io.thermo.file.parser.internal_rotations.hindered.compchem;

import uk.ac.cam.ceb.como.thermo.calculator.rotation.internal.util.IRCompChemWrapper;
import com.cmclinnovations.io.file.parser.FileParser;
import uk.ac.cam.ceb.como.compchem.CompChem;
import java.io.File;
import java.io.FileFilter;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import org.apache.log4j.Logger;
import uk.ac.cam.ceb.como.math.conversion.AngleConversion;
import uk.ac.cam.ceb.como.math.conversion.EnergyConversion;
import uk.ac.cam.ceb.como.math.data.DataPoint;
import uk.ac.cam.ceb.como.math.data.DataSpace;
import uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.hindered.potential.discretisation.RotationalMode;
import uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.hindered.potential.discretisation.RotationalModes;
import uk.ac.cam.ceb.como.io.thermo.file.parser.internal_rotations.hindered.HPDirectoryParser;
import org.xmlcml.cml.element.CMLAtom;
import org.xmlcml.cml.element.CMLMolecule;
import uk.ac.cam.ceb.como.thermo.partition_function.rotation.internal.hindered.potential.discretisation.DiscrRotation;

/**
 *
 * @author pb556
 */
public class HPCompChemDirectoryParser extends HPDirectoryParser {

    private Logger logger = Logger.getLogger(getClass());

    @Override
    public void parse() throws Exception {
        FileParser parser = null;
        File refFile = get().getReferenceFile();
        CompChem cc = parse(refFile);
        String id = parser.getFile().getName();
        if (cc != null) {
            id = cc.getId();
        }

            // check which atoms change their position and which position did they have at the beginning
        // the initial position can be used as a reference point to identify the correct rotational mode
        // describing the properties of the rotational motion
        RotationalMode mode = getRotationalMode();

        // each mode has its own data space
        if(isDataSpaceValid()) {
            DataSpace space = getDataSpace();
            get().setRotation(new DiscrRotation(id, space, mode));
        }
    }
    
    protected FileParser getParser(File file) {
        if (fParser == null || fParser.isEmpty()) {
            logger.error("No parser defined.");
            return null;
        }
        for (FileFilter fFilter : fParser.keySet()) {
            if (fFilter.accept(file)) {
                return fParser.get(fFilter);
            }
        }
        return null;
    }
    
    protected CompChem parse(File file) throws Exception {
        FileParser parser = null;
//        File refFile = get().getReferenceFile();
        if (file == null) {
            logger.error("No file defined.");
            return null;
        }
        parser = getParser(file);
        if (parser == null) {
            logger.error("No parser defined for reference file.");
            return null;
        }
        parser.set(file);
        parser.parse();
        // required orders to limit parsing to a minimum
        try {
            CompChem cc = (CompChem) parser.get();
            return cc;
        } catch (Exception e) {
            logger.error("Invalid parser type defined.");
            return null;
        }
    }

    @Override
    protected RotationalMode getRotationalMode() throws Exception {
        return getRotationalMode(false);
    }

    @Override
    protected RotationalMode getRotationalMode(boolean inclTorsBondAtom) throws Exception {
        // USE HRCOMPCHEMWRAPPER!!!
        if (fParser == null || fParser.isEmpty()) {
            logger.error("No parser defined.");
            return new RotationalMode();
        }
        
        try {
            
            // check which atoms change their position and which position did they have at the beginning
            // the initial position can be used as a reference point to identify the correct rotational mode
            // describing the properties of the rotational motion
            Map<Double, File> files = getFilesByAngle();
            double min = Double.MAX_VALUE;
            for (Double angle : files.keySet()) {
                if (angle < min && angle != 0.0 && angle != 2.0 * Math.PI && angle != 360.0) {
                    min = angle;
                }
            }

            if (min == Double.MAX_VALUE) {
                throw new Exception("Impossible to identify the associated rotational mode based on the files contained in the directory.");
            }
            
            File minFile = files.get(min);
            if (minFile == null) {
                logger.error("No minimum value defined.");
                return new RotationalMode();
            }
            FileParser parser = getParser(minFile);
            if (parser == null) {
                logger.error("No parser defined for reference file.");
                return new RotationalMode();
            }
            parser.set(minFile);
            parser.parse();
            
            IRCompChemWrapper hrCCWRef = new IRCompChemWrapper(parse(getReference()));
            CMLMolecule molRef = hrCCWRef.getFinalMolecule();
            RotationalModes rotModes = hrCCWRef.getRotationalModes(inclTorsBondAtom); // using the reference file which needs to be a HR optimised Gaussian file!
            if (rotModes.isEmpty()) {
                return new RotationalMode();
            }
            IRCompChemWrapper hrCCW = new IRCompChemWrapper(parse(minFile));
            CMLMolecule mol = hrCCW.getFinalMolecule();

            // compare mol and molRef and identify top
            return identifyTop(rotModes, getChangedAtoms(molRef, mol));
        } catch (IndexOutOfBoundsException iooe) {
            return new RotationalMode();
        }
    }

    protected RotationalMode identifyTop(RotationalModes rotModes, List<CMLAtom> atoms) {
        for (RotationalMode m : rotModes) {
            // check tops
            if (m.getTop().getAtoms().size() == atoms.size()) {
                boolean correctMode = true;
                for (CMLAtom refAtom : m.getTop().getAtoms()) {
                    boolean identified = false;
                    for (CMLAtom atom : atoms) {
                        if (refAtom.getId().compareToIgnoreCase(atom.getId()) == 0) {
                            identified = true;
                            break;
                        }
                    }
                    if (!identified) {
                        correctMode = false;
                        break;
                    }
                }
                if (correctMode) {
                    return m;
                }
            }
        }

        return null;
    }

    protected List<CMLAtom> getChangedAtoms(CMLMolecule molRef, CMLMolecule mol) {
        ArrayList<CMLAtom> diff = new ArrayList<>();
        List<CMLAtom> refAtoms = molRef.getAtomArray().getAtoms();
        List<CMLAtom> atoms = mol.getAtomArray().getAtoms();
        for (int i = 0; i < refAtoms.size(); i++) {
            boolean identified = false;
            for (int j = 0; j < atoms.size(); j++) {
                // check id
                if (refAtoms.get(i).getId().compareToIgnoreCase(atoms.get(j).getId()) == 0) {
                    // check position
                    double[] posRef = refAtoms.get(i).getXYZ3().getArray();
                    double[] pos = atoms.get(j).getXYZ3().getArray();
                    if ((Math.abs(posRef[0] - pos[0]) < 0.0001) && (Math.abs(posRef[1] - pos[1]) < 0.0001) && (Math.abs(posRef[2] - pos[2]) < 0.0001)) {
                        identified = true;
                        break;
                    }
                    break;
                }
            }
            if (!identified) {
                diff.add(refAtoms.get(i));
            }
        }
        return diff;
    }

    // subtract GS energy
    // convert into kJ/mol
    // check if any energy is lower than the GS energy
    //      if so, throw an exception
    
    @Override
    protected DataSpace getDataSpace() throws Exception {
        DataSpace space = new DataSpace();
        Map<Double, File> files = getFilesByAngle();

        // get ground-state energy
        logger.info("Processing file " + getReference().getName() + ".");
        IRCompChemWrapper ccw = new IRCompChemWrapper(parse(getReference()));
        double gsSCF = ccw.getSCFEnergy();
//        if (gsSCF > 0) {
//            logger.error("Invalid ground-state SCF energy has been identified!", new Exception("Invalid ground-state SCF energy has been identified!"));
//        }
        DataPoint dpRef = new DataPoint(new Double[]{0.0}, EnergyConversion.HaTokJPerMol(0.0));
        dpRef.setId(getReference().getAbsolutePath());
        space.add(dpRef);
        DataPoint dpRef2 = new DataPoint(new Double[]{Math.PI * 2.0}, EnergyConversion.HaTokJPerMol(0.0));
        dpRef2.setId(getReference().getAbsolutePath());
        //space.add(dpRef2);
        for (Double angle : files.keySet()) {
            if (angle == 0 || angle == 360) {
                continue;
            }
            try {
                //DataPoint dp = new DataPoint(new Double[]{angle}, (CompChem) getParser().get());
                ccw = new IRCompChemWrapper(parse(files.get(angle)));
                double scf = ccw.getSCFEnergy();
//            if (scf > 0) {
//                logger.error("Invalid SCF energy has been identified!", new Exception("Invalid SCF energy has been identified!"));
//            }
//            if (scf < gsSCF) {
//                logger.error("A smaller SCF energy has been identified than the one from the ground state!", new Exception("Invalid ground-state geometry!"));
//            }
                //DataPoint dp = new DataPoint(new Double[]{angle}, ccw.getSCFEnergy());
                DataPoint dp = new DataPoint(new Double[]{AngleConversion.toRadians(angle)}, EnergyConversion.HaTokJPerMol(scf - gsSCF) * 1000);
                dp.setId(files.get(angle).getAbsolutePath());
                space.add(dp);
            } catch (NullPointerException npe) {
                logger.warn("Skipping file " + files.get(angle) + "(" + angle + ")!");
            }
        }
        return space;
    }

    protected boolean isDataSpaceValid() throws Exception {
        Map<Double, File> files = getFilesByAngle();

        // get ground-state energy
        IRCompChemWrapper ccw = new IRCompChemWrapper(parse(getReference()));
        double gsSCF = ccw.getSCFEnergy();
        if (gsSCF > 0) {
            logger.warn("Invalid ground-state SCF energy has been identified!");
            return false;
        }
        for (Double angle : files.keySet()) {
            if (angle == 0 || angle == 360) {
                continue;
            }
            //logger.info("Processing file " + files.get(angle).getName() + ".");
            try {
                //DataPoint dp = new DataPoint(new Double[]{angle}, (CompChem) getParser().get());
                ccw = new IRCompChemWrapper(parse(files.get(angle)));
                double scf = ccw.getSCFEnergy();
                if (scf > 0) {
                    logger.warn("Invalid SCF energy has been identified!");
                    return false;

                }
                if (scf < gsSCF) {
                    logger.warn("A smaller SCF energy has been identified than the one from the ground state!");
                    logger.info("File: " + files.get(angle).getAbsolutePath());
                    logger.info("GS-Energy = " + gsSCF);
                    logger.info("Rot-Energy = " + scf);
                    return false;
                }
            } catch (NullPointerException npe) {
                logger.warn("Problems ocurred during parsing the file " + files.get(angle) + "(" + angle + ")!");
            }
        }
        return true;
    }
    // USE REFERENCE AS ID INSTEAD
//    @Override
//    protected String getId() throws Exception {
//        return directory.getReferenceFile().getName().replace(".g09", "");
//    }
//    protected String getId(Collection<File> files) throws Exception {
//        String id = null;
//        boolean commonId = true;
//        for (File f : files) {
//            String name = f.getName().replace(".g09", "");
//            String[] items = name.split("_");
//            if (items.length <= 1) {
//                throw new Exception("No common identifier has been defined.");
//            }
//            String idBuffer = "";
//            for (int i = 0; i < items.length - 1; i++) {
//                idBuffer += items[i];
//            }
//            if (id == null) {
//                id = idBuffer;
//            } else {
//                commonId &= id.compareToIgnoreCase(idBuffer) == 0;
//            }
//        }
//        if (!commonId) {
//            throw new Exception("No common identifier has been defined.");
//        }
//        return id;
//    }
//
//    @Override
//    protected double getAngle(File file) throws Exception {
//        String name = file.getName().replace(".g09", "");
//        String[] items = name.split("_");
//        String strAngle = items[items.length - 1];
//        if (!StringUtil.isNumber(strAngle)) {
//            throw new NumberFormatException("Invalid angle information obtained.");
//        }
//        return Double.parseDouble(strAngle);
//    }
}
