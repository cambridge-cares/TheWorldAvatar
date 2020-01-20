/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.io.chem.filemgmt.gaussian.parser;

import com.cmclinnovations.io.file.FileOperations;
import uk.ac.cam.ceb.como.io.chem.file.parser.g09.FrequencyParser;
import uk.ac.cam.ceb.como.io.chem.file.parser.g09.GaussianParser;
import uk.ac.cam.ceb.como.io.chem.file.parser.g09.util.GaussianHelper;
import uk.ac.cam.ceb.como.compchem.CompChem;
import java.io.File;
import java.util.Collection;

import org.junit.Ignore;
import org.junit.Test;
import uk.ac.cam.ceb.como.thermo.calculator.rotation.internal.util.IRCompChemWrapper;

/**
 *
 * @author pb556
 */

public class FrequencyParserTest {

    @Test
    @Ignore
    public void parseTest() throws Exception {

    	
//      Collection<File> files = FileOperations.ls(new File("W:\\projects\\TTIP_thermo\\calculations\\dHf\\g09\\ti\\"), true);  
//    	FrequencyParser parser = new FrequencyParser();   	
//        int fCtr = 1;
//        for (File f : files) {
//            if (f.getName().contains(".g09")) {
//                System.out.println("Processing file " + fCtr + " / " + files.size());
//                fCtr++;
//                parser.set(f);                
//                parser.parse();
//                CompChem cc = (CompChem) parser.get();
//                IRCompChemWrapper ccw = new IRCompChemWrapper(cc);
//                System.out.println(ccw.getG09ThermalEnergy() + ", " + ccw.getG09ZeroPointEnergy());
//            }
//        }
    }
}