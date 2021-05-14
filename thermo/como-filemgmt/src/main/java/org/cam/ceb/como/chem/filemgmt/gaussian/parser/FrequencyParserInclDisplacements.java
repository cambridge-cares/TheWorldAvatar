/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.chem.filemgmt.gaussian.parser;

import org.cam.ceb.como.chem.filemgmt.gaussian.parser.util.StringList;
import org.cam.ceb.como.tools.parser.util.ParserHelper;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

/**
 *
 * @author pb556
 */
public class FrequencyParserInclDisplacements extends FrequencyParser {

    @Override
    protected VibrationalNormalModes parseVibrationalFrequencies(final List<String> rawFreqs) throws Exception {
        StringList sl = new StringList(rawFreqs);
        int from_line = 0;
        int pos = 0;
        HashMap<Integer, ArrayList<VibrationalNormalMode.XYZ>> dispArrayList = new HashMap<Integer, ArrayList<VibrationalNormalMode.XYZ>>();
        VibrationalNormalModes normalModes = new VibrationalNormalModes();
        normalModes.setUnits("nonSi:cm^-1", "nonSi:amu", "nonSi:A^4/amu", "nonSi:mDyne/A^-1", "nonSi:Angstroms");
        while ((pos = sl.getFirstMatchPosition(from_line, "\\s*Frequencies\\s+--.*")) > -1) {
            int atomHeaderPos = sl.getFirstMatchPosition(pos, "\\s*Atom\\s+AN.*");
            //int local_num_freqs = 0;

            List<String> infos = sl.subList(pos, atomHeaderPos);
            // parse frequency info
            boolean displacements = false;
            
            int indexCounter = 1;
            int size = 0;
            for (String line : infos) {
                if (line.contains("Frequencies")) {
                    final String freqs = line.replaceAll("^.+?--", "");
                    final List<Double> local_fs = ParserHelper.parseDoubles(ParserHelper.tokenize(freqs));
                    size = local_fs.size();
                    // number of frequencies interface each section
                    //local_num_freqs = local_fs.size();
                    normalModes.addAll(VibrationalNormalModes.Property.FREQUENCY, local_fs);
                } else if (line.contains("Red. masses")) {
                    final String rms = line.replaceAll("^.+?--", "");
                    final List<Double> local_rms = ParserHelper.parseDoubles(ParserHelper.tokenize(rms));
                    normalModes.addAll(VibrationalNormalModes.Property.REDUCED_MASS, normalModes.size() - local_rms.size(), local_rms);
                } else if (line.contains("Frc consts")) {
                    final String fcs = line.replaceAll("^.+?--", "");
                    final List<Double> local_fcs = ParserHelper.parseDoubles(ParserHelper.tokenize(fcs));
                    normalModes.addAll(VibrationalNormalModes.Property.FORCE_CONSTANT, normalModes.size() - local_fcs.size(), local_fcs);
                } else if (line.contains("IR Inten")) {
                    final String irs = line.replaceAll("^.+?--", "");
                    final List<Double> local_irs = ParserHelper.parseDoubles(ParserHelper.tokenize(irs));
                    normalModes.addAll(VibrationalNormalModes.Property.IR_INTEN, normalModes.size() - local_irs.size(), local_irs);
                } else if (line.contains("Atom\\s+AN")) {
                    displacements = true;
                    // size is used to define the number of new lists
//                    for (int i = 0; i < size; i++) {
//                        dispArrayList.put(indexCounter, new ArrayList<VibrationalNormalMode.XYZ>());
//                        indexCounter++;
//                    }
                } else if (displacements) {
//                    // extract the displacement information
//                    final List<Double> local_disp = ParserHelper.parseDoubles(ParserHelper.tokenize(line));
//                    if (local_disp.size() < 5) {
//                        displacements = false;
//                    } else {
//                        // extract the displacements and add them
//                        if ((local_disp.size() - 2) % 3 == 0) {
//                            for (int i = 0; i < size; i++) {
//                                double[] d = new double[3];
//                                VibrationalNormalMode.XYZ xyz = normalModes.get(0).new XYZ();
//                                xyz.x = local_disp.get(2 + i * 3);
//                                xyz.y = local_disp.get(2 + i * 3 + 1);
//                                xyz.z = local_disp.get(2 + i * 3 + 2);
//                                
//                                // add to the list and to the hash map!
//                                dispArrayList.get(indexCounter - size + i).add(xyz);
//                            }
//                        } else {
//                            displacements = false;
//                        }
//                    }
                } else {
                    // use a counter that counts the amount of atoms present in the molecule
                    displacements = false;
                }
            }

            // update indices
            pos = atomHeaderPos;
            from_line = atomHeaderPos;
        }
        List<Double> mode = new ArrayList<Double>();
        for (int i = 0; i < normalModes.size(); i++) {
            mode.add(i + 1.0);
//            List<VibrationalNormalMode.XYZ> p = dispArrayList.get(i + 1);
//            normalModes.get(i).setDisplacement(dispArrayList.get(i + 1));
        }
        normalModes.addAll(VibrationalNormalModes.Property.MODE, 0, mode);
        
        return normalModes;
    }
}
