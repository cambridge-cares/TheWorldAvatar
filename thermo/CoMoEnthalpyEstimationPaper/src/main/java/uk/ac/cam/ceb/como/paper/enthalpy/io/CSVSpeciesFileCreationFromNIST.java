/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package uk.ac.cam.ceb.como.paper.enthalpy.io;

import com.cmclinnovations.io.file.FileOperations;
import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import uk.ac.cam.ceb.como.compchem.CompChem;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.pool.SpeciesPoolWriter;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.io.chem.file.parser.g09.FrequencyParser;
import uk.ac.cam.ceb.como.math.conversion.EnergyConversion;
import uk.ac.cam.ceb.como.nist.info.NISTInfoReader;
import uk.ac.cam.ceb.como.nist.info.NISTSpeciesInfo;
import uk.ac.cam.ceb.como.nist.thermochemistry.NISTEnthalpy;
import uk.ac.cam.ceb.como.nist.thermochemistry.NISTSpeciesGasPhaseThermoChem;
import uk.ac.cam.ceb.como.nist.thermochemistry.NISTSpeciesGasPhaseThermoChemReader;
import uk.ac.cam.ceb.como.paper.enthalpy.utils.HfSpeciesConverter;
import uk.ac.cam.ceb.como.thermo.calculator.rotation.internal.util.IRCompChemWrapper;
import uk.ac.cam.ceb.como.tools.file.writer.StringListWriter;

/**
 *
 * @author pb556
 */

public class CSVSpeciesFileCreationFromNIST {

    private static String destMissing = "C:\\Users\\pb556\\workspace\\methodology\\energy_missing.txt";
    private static String destUnscaled = "C:\\Users\\pb556\\workspace\\methodology\\energy_unscaled_kJperMol.csv";
    private static String destScaled = "C:\\Users\\pb556\\workspace\\methodology\\energy_scaled_kJperMol.csv";
    
    private static String srcG09 = "C:\\Users\\pb556\\workspace\\methodology\\g09_reference_filtered\\";
    private static String srcHTML = "C:\\Users\\pb556\\workspace\\methodology\\nist\\html\\";
    
    
    private static String destG09 = "C:\\Users\\pb556\\workspace\\methodology\\g09_ref_pool\\";
    private static boolean copy = true;

    public static void main(String[] args) throws Exception {
        
    	// read all files
        // convert them
        // create species file
        //G09FileFilter filter = new G09FileFilter(new File(srcG09));
        //Collection<File> files = filter.getValidFiles();

    	Collection<File> files = FileOperations.ls(new File(srcG09), true);
        
        Map<Species, String> species = new HashMap<Species, String>();
        
        Map<Species, String> species_unscaled = new HashMap<Species, String>();
        
        FrequencyParser parser = new FrequencyParser();
        
        int fCtr = 1;
        
        ArrayList<String> missing = new ArrayList<String>();
        
        for (File f : files) {
            System.out.println("Processing file " + fCtr + " / " + files.size());
            fCtr++;
            parser.set(f);
            try {
                parser.parse();
                CompChem cc = (CompChem) parser.get();
                IRCompChemWrapper ccw = new IRCompChemWrapper(cc);

                String id = f.getName().replace("freq-b971_", "").replace("a-", "").replace("freq-", "").replace("fine-", "").replace(".g09", "");

                Species sUnscaled = HfSpeciesConverter.getHfSpecies(ccw.getFinalMolecule(), id); // , id, 0.9684)
                Species sScaled = HfSpeciesConverter.getHfSpecies(ccw.getFinalMolecule(), id); // , id, 0.9684)

                // check units - all in Ha here
                double scf = EnergyConversion.HaTokJPerMol(ccw.getSCFEnergy());

                double g09ZPE = EnergyConversion.HaTokJPerMol(ccw.getG09ZeroPointEnergy());
                double g09Thermal = EnergyConversion.HaTokJPerMol(ccw.getG09ThermalEnergy());

                double g09Diff = g09Thermal - g09ZPE;
                double zpe = EnergyConversion.HaTokJPerMol(HfSpeciesConverter.calculateZPE(ccw.getVibrationalNormalModes(), 1.0));
                double zpeScaled = EnergyConversion.HaTokJPerMol(HfSpeciesConverter.calculateZPE(ccw.getVibrationalNormalModes(), 0.9893));

                NISTEnthalpy enthalpy = getEnthalpyOfFormation(f, id);

                if (enthalpy != null) {
//                    sScaled.setHf(enthalpy.getValue());
//                    sScaled.setTotalEnergy(g09Thermal + scf);
//
//                    sUnscaled.setHf(enthalpy.getValue());
//                    sUnscaled.setTotalEnergy(g09Thermal + scf);

                    sScaled.setHf(enthalpy.getValue());
                    sScaled.setTotalEnergy(zpeScaled + scf + g09Diff);

                    sUnscaled.setHf(enthalpy.getValue());
                    sUnscaled.setTotalEnergy(zpe + scf + g09Diff);

//                System.out.println(zpe  + ", " + g09ZPE);
                    //System.out.println(g09Diff + ", " + (zpe + scf + g09Diff) + ", " + (scf + g09Thermal));
                    // set enthalpy of formation
                    species.put(sScaled, "#" + enthalpy.getReference());
                    species_unscaled.put(sUnscaled, "#" + enthalpy.getReference());

                    if (copy) {
                        FileOperations.copy(f, new File(destG09 + File.separator + f.getName()));
                    }

                } else {
                    missing.add(f.getAbsolutePath());
                }
            } catch (Exception e) {
                System.out.println("Could not read file " + f.getName() + "!");
            }
        }

        SpeciesPoolWriter writer = new SpeciesPoolWriter(destScaled);
        
        writer.set(species, false);
        writer.write();

        writer = new SpeciesPoolWriter(destUnscaled);
        writer.set(species_unscaled, false);
        writer.write();

        StringListWriter missingWriter = new StringListWriter();
        missingWriter.setContent(missing);
        missingWriter.overwrite(true);
        missingWriter.set(destMissing);
        missingWriter.write();
    }

    public static NISTEnthalpy getEnthalpyOfFormation(File f, String id) {
        //String id = new File(path).getName().replace("a-freq-fine-", "").replace(".g09", "");
        NISTSpeciesGasPhaseThermoChem thermo = getThermoData(id);
        NISTSpeciesInfo info = getReferenceData(id);
        if (thermo == null || info == null || info.getUrl3DSDFile() == null || info.getUrl3DSDFile().trim().isEmpty()) {
            return null;
        }
        Collection<NISTEnthalpy> enthalpies = thermo.getEnthalpies();
        double v = 0;
        NISTEnthalpy selected = null;
        int year = -1;
        for (NISTEnthalpy e : enthalpies) {
            if (e.getEnthalpyType() == NISTEnthalpy.EnthalpyType.FORMATION) {
                // select the most recent one!
                String[] items = e.getReference().split(",");
                if (items.length > 1) {
                    try {
                        // last id
                        int tempYear = Integer.parseInt(items[items.length - 1].trim());
                        if (year < tempYear) {
                            year = tempYear;
                            selected = e;
                        }
                    } catch (ClassCastException cce) {
                    }
                } else if (selected == null) {
                    selected = e;
                }
                //System.out.println(id + ": " + e.getReference() + "; " + e.getValue() + " +" + e.getPosTolerance() + "/-" + e.getNegTolerance());
            }
        }
        //System.out.println("(SELECTED) " + id + ": " + selected.getReference() + "; " + selected.getValue() + " +" + selected.getPosTolerance() + "/-" + selected.getNegTolerance());
        return selected;
    }

    public static NISTSpeciesGasPhaseThermoChem getThermoData(String id) {
        if (new File(srcHTML + "\\" + id + ".html").exists()) {
            NISTSpeciesGasPhaseThermoChemReader readerthermo = new NISTSpeciesGasPhaseThermoChemReader();
            readerthermo.setPath(srcHTML + "\\" + id + ".html");
            readerthermo.parse();
            return readerthermo.getThermoChemData();
        }
        return null;
    }

    public static NISTSpeciesInfo getReferenceData(String id) {
        if (new File(srcHTML + "\\" + id + ".html").exists()) {
            NISTInfoReader reader = new NISTInfoReader();
            reader.setPath(srcHTML + "\\" + id + ".html");
            reader.parse();
            return (NISTSpeciesInfo) reader.get();
        }
        return null;
    }
}