/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package uk.ac.cam.ceb.como.paper.enthalpy.io;

import java.io.File;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import com.cmclinnovations.io.file.FileOperations;

import uk.ac.cam.ceb.como.compchem.CompChem;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.io.pool.SpeciesPoolWriter;
import uk.ac.cam.ceb.como.enthalpy.estimation.balanced_reaction.species.Species;
import uk.ac.cam.ceb.como.io.chem.file.parser.g09.FrequencyParser;
import uk.ac.cam.ceb.como.math.conversion.EnergyConversion;
import uk.ac.cam.ceb.como.paper.enthalpy.utils.HfSpeciesConverter;
import uk.ac.cam.ceb.como.thermo.calculator.rotation.internal.util.IRCompChemWrapper;

/**
 *
 * @author pb556
 * 
 */

public class CSVSpeciesFileCreationFromG09 {

    private static String destUnscaled = "C:\\Users\\pb556\\workspace\\methodology\\diff_unscaled_kJperMol.csv";
    
    private static String destScaled = "C:\\Users\\pb556\\workspace\\methodology\\diff_scaled_kJperMols.csv";
    
    private static String srcG09 = "C:\\Users\\pb556\\workspace\\methodology\\ref\\";
    
    public static void main(String[] args) throws Exception {

        System.out.println(srcG09);
        
        System.out.println(destUnscaled);

        Collection<File> files = FileOperations.ls(new File(srcG09), true);
       
        Map<Species, String> species = new HashMap<>();
       
        Map<Species, String> species_unscaled = new HashMap<>();
        
        FrequencyParser parser = new FrequencyParser();
        
        int fCtr = 1;
        
        for (File f : files) {
        	
            double scf = 0.0;
            
            try {
                
            	System.out.println("Processing file " + fCtr + " / " + files.size());
                
                fCtr++;
                
                parser.set(f);
                parser.parse();
                
                CompChem cc = (CompChem) parser.get();
                IRCompChemWrapper ccw = new IRCompChemWrapper(cc);

                String id = f.getName().replace(".g09", "").replace("0freq-0-", "");

                Species sUnscaled = HfSpeciesConverter.getHfSpecies(ccw.getFinalMolecule(), id); // , id, 0.9684)
                Species sScaled = HfSpeciesConverter.getHfSpecies(ccw.getFinalMolecule(), id); // , id, 0.9684)

                // Check units - all in Ha here
                scf = EnergyConversion.HaTokJPerMol(ccw.getSCFEnergy());

                double g09ZPE = EnergyConversion.HaTokJPerMol(ccw.getG09ZeroPointEnergy());
                double g09Thermal = EnergyConversion.HaTokJPerMol(ccw.getG09ThermalEnergy());

                double g09Diff = g09Thermal - g09ZPE;
                double zpe = EnergyConversion.HaTokJPerMol(HfSpeciesConverter.calculateZPE(ccw.getVibrationalNormalModes(), 1.0));
                double zpeScaled = EnergyConversion.HaTokJPerMol(HfSpeciesConverter.calculateZPE(ccw.getVibrationalNormalModes(), 0.9893));

//              sScaled.setHf(0.0);
//              sScaled.setTotalEnergy(g09Thermal + scf);
//              sUnscaled.setHf(0.0);
//              sUnscaled.setTotalEnergy(g09Thermal + scf);

//              System.out.println(zpe  + ", " + g09ZPE);
                //System.out.println(g09Diff + ", " + (zpe + scf + g09Diff)  + ", " + (scf + g09Thermal));
                
                sScaled.setHf(0.0);
                sScaled.setTotalEnergy(zpeScaled + scf + g09Diff);
                sUnscaled.setHf(0.0);
                sUnscaled.setTotalEnergy(zpe + scf + g09Diff);
//                
////            System.out.println(zpe  + ", " + g09ZPE);
//              System.out.println(g09Diff + ", " + (zpe + scf + g09Diff)  + ", " + (scf + g09Thermal));
                
//              sScaled.setHf(0.0);
//              sScaled.setTotalEnergy(zpeScaled + scf);
//              sUnscaled.setHf(0.0);
//              sUnscaled.setTotalEnergy(zpe + scf);
                
                species.put(sScaled, "");                
                species_unscaled.put(sUnscaled, id);
                
            } catch (Exception e) {
                System.out.println("File " + f.getName() + " could not be read!");
                System.out.println("SCF = " + scf);
            }
        }
        
        SpeciesPoolWriter writer = new SpeciesPoolWriter(destScaled);
        writer.set(species, true);
        writer.write();

        writer = new SpeciesPoolWriter(destUnscaled);
        writer.set(species_unscaled, true);
        writer.write();
    }
}