/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.rotation.hindered.io;

import java.util.ArrayList;
import java.util.List;
import uk.ac.cam.ceb.como.rotation.hindered.util.SimpleAtom;
import uk.ac.cam.ceb.como.rotation.hindered.util.SimpleBond;
import uk.ac.cam.ceb.como.rotation.hindered.util.SimpleRotMode;
import uk.ac.cam.ceb.como.rotation.hindered.util.SimpleRotModes;
import uk.ac.cam.ceb.como.rotation.hindered.util.SimpleSpecies;
import uk.ac.cam.ceb.como.rotation.hindered.util.SimpleVibNormalMode;
import uk.ac.cam.ceb.como.rotation.hindered.util.SimpleVibNormalModes;

/**
 *
 * @author pb556
 */
public class MockSpecies {
    
    public static SimpleSpecies getSpecies0232() {
        List<SimpleAtom> refAtoms = new ArrayList<SimpleAtom>();
        List<SimpleBond> refBonds = new ArrayList<SimpleBond>();

        SimpleAtom a1 = new SimpleAtom("a1", "O", -2.21654, 0.9927, -0.69488);
        SimpleAtom a2 = new SimpleAtom("a2", "C", -3.0357, 1.28537, -1.81589);
        SimpleAtom a3 = new SimpleAtom("a3", "Ti", -0.88482, 0.14809, 0.13101);
        SimpleAtom a4 = new SimpleAtom("a4", "C", -2.91485, 2.77415, -2.14954);
        SimpleAtom a5 = new SimpleAtom("a5", "C", -4.47453, 0.86402, -1.50884);
        SimpleAtom a6 = new SimpleAtom("a6", "H", -2.6599, 0.69606, -2.66597);
        SimpleAtom a7 = new SimpleAtom("a7", "O", -0.03241, -0.93039, -1.05748);
        SimpleAtom a8 = new SimpleAtom("a8", "O", 0.31434, 1.33394, 0.80146);
        SimpleAtom a9 = new SimpleAtom("a9", "O", -1.58203, -0.86862, 1.46882);
        SimpleAtom a10 = new SimpleAtom("a10", "H", -3.26838, 3.38094, -1.30877);
        SimpleAtom a11 = new SimpleAtom("a11", "H", -3.51652, 3.01782, -3.032);
        SimpleAtom a12 = new SimpleAtom("a12", "H", -1.87365, 3.03836, -2.35805);
        SimpleAtom a13 = new SimpleAtom("a13", "H", -4.51839, -0.20031, -1.25978);
        SimpleAtom a14 = new SimpleAtom("a14", "H", -5.11888, 1.04546, -2.37619);
        SimpleAtom a15 = new SimpleAtom("a15", "H", -4.86333, 1.43427, -0.65844);
        SimpleAtom a16 = new SimpleAtom("a15", "H", -0.04881, -1.87995, -1.18956);
        SimpleAtom a17 = new SimpleAtom("a15", "H", 0.3122, 2.28744, 0.90063);
        SimpleAtom a18 = new SimpleAtom("a18", "H", -1.28732, -1.01106, 2.37039);
        
        refAtoms.add(a1);
        refAtoms.add(a2);
        refAtoms.add(a3);
        refAtoms.add(a4);
        refAtoms.add(a5);
        refAtoms.add(a6);
        refAtoms.add(a7);
        refAtoms.add(a8);
        refAtoms.add(a9);
        refAtoms.add(a10);
        refAtoms.add(a11);
        refAtoms.add(a12);
        refAtoms.add(a13);
        refAtoms.add(a14);
        refAtoms.add(a15);
        refAtoms.add(a16);
        refAtoms.add(a17);
        refAtoms.add(a18);
        
        refBonds.add(new SimpleBond("a1_a2", "S", a1, a2));
        refBonds.add(new SimpleBond("a1_a3", "S", a1, a3));
        refBonds.add(new SimpleBond("a2_a4", "S", a2, a4));
        refBonds.add(new SimpleBond("a2_a4", "S", a2, a5));
        refBonds.add(new SimpleBond("a2_a5", "S", a2, a6));
        refBonds.add(new SimpleBond("a3_a6", "S", a3, a7));
        refBonds.add(new SimpleBond("a3_a7", "S", a3, a8));
        refBonds.add(new SimpleBond("a3_a9", "S", a3, a9));
        refBonds.add(new SimpleBond("a4_a10", "S", a4, a10));
        refBonds.add(new SimpleBond("a4_a11", "S", a4, a11));
        refBonds.add(new SimpleBond("a4_a12", "S", a4, a12));
        refBonds.add(new SimpleBond("a5_a13", "S", a5, a13));
        refBonds.add(new SimpleBond("a5_a14", "S", a5, a14));
        refBonds.add(new SimpleBond("a5_a15", "S", a5, a15));
        refBonds.add(new SimpleBond("a7_a16", "S", a7, a16));
        refBonds.add(new SimpleBond("a8_a17", "S", a8, a17));
        refBonds.add(new SimpleBond("a9_a18", "S", a9, a18));
        
        return new SimpleSpecies("freq-hr-fine-species-02320radical-0-restricted", refAtoms, refBonds);
    }
    
    public static SimpleSpecies getSpecies0232Modified() {
        List<SimpleAtom> refAtoms = new ArrayList<SimpleAtom>();
        List<SimpleBond> refBonds = new ArrayList<SimpleBond>();

        SimpleAtom a1 = new SimpleAtom("a1", "O", -2.2, 0.9, -0.6);
        SimpleAtom a2 = new SimpleAtom("a2", "C", -3.0, 1.2, -1.8);
        SimpleAtom a3 = new SimpleAtom("a3", "Ti", -0.8, 0.1, 0.1);
        SimpleAtom a4 = new SimpleAtom("a4", "C", -2.9, 2.7, -2.1);
        SimpleAtom a5 = new SimpleAtom("a5", "C", -4.4, 0.8, -1.5);
        SimpleAtom a6 = new SimpleAtom("a6", "H", -2.6, 0.6, -2.6);
        SimpleAtom a7 = new SimpleAtom("a7", "O", 0.0, -0.9, -1.0);
        SimpleAtom a8 = new SimpleAtom("a8", "O", 0.3, 1.3, 0.8);
        SimpleAtom a9 = new SimpleAtom("a9", "O", -1.5, -0.8, 1.4);
        SimpleAtom a10 = new SimpleAtom("a10", "H", -3.2, 3.3, -1.3);
        SimpleAtom a11 = new SimpleAtom("a11", "H", -3.5, 3.0, -3.0);
        SimpleAtom a12 = new SimpleAtom("a12", "H", -1.8, 3.0, -2.3);
        SimpleAtom a13 = new SimpleAtom("a13", "H", -4.5, -0.2, -1.2);
        SimpleAtom a14 = new SimpleAtom("a14", "H", -5.1, 1.0, -2.3);
        SimpleAtom a15 = new SimpleAtom("a15", "H", -4.8, 1.4, -0.6);
        SimpleAtom a18 = new SimpleAtom("a18", "H", -1.2, -1.0, 2.3);
        
        refAtoms.add(a1);
        refAtoms.add(a2);
        refAtoms.add(a3);
        refAtoms.add(a4);
        refAtoms.add(a5);
        refAtoms.add(a6);
        refAtoms.add(a7);
        refAtoms.add(a8);
        refAtoms.add(a9);
        refAtoms.add(a10);
        refAtoms.add(a11);
        refAtoms.add(a12);
        refAtoms.add(a13);
        refAtoms.add(a14);
        refAtoms.add(a15);
        refAtoms.add(a18);
        
        refBonds.add(new SimpleBond("a1_a2", "S", a1, a2));
        refBonds.add(new SimpleBond("a1_a3", "S", a1, a3));
        refBonds.add(new SimpleBond("a2_a4", "S", a2, a4));
        refBonds.add(new SimpleBond("a2_a4", "S", a2, a5));
        refBonds.add(new SimpleBond("a2_a5", "S", a2, a6));
        refBonds.add(new SimpleBond("a3_a6", "S", a3, a7));
        refBonds.add(new SimpleBond("a3_a7", "S", a3, a8));
        refBonds.add(new SimpleBond("a3_a9", "S", a3, a9));
        refBonds.add(new SimpleBond("a4_a10", "S", a4, a10));
        refBonds.add(new SimpleBond("a4_a11", "S", a4, a11));
        refBonds.add(new SimpleBond("a5_a14", "S", a5, a14));
        refBonds.add(new SimpleBond("a5_a15", "S", a5, a15));
        refBonds.add(new SimpleBond("a9_a18", "S", a9, a18));
        
        return new SimpleSpecies("freq-hr-fine-species-0232-radical-0-restricted_diff_added", refAtoms, refBonds);
    }
    
    public static SimpleVibNormalModes getSpecies0232VibrationalNormalModes() {
        SimpleVibNormalModes modes = new SimpleVibNormalModes();
        
        modes.add(new SimpleVibNormalMode("f1", 15.572, 3.1399 , 0.028, 4.0E-4));
        modes.add(new SimpleVibNormalMode("f2", 26.147, 4.3031, 2.1489, 0.0017));
        modes.add(new SimpleVibNormalMode("f3", 42.0864, 4.8933, 3.5133, 0.0051));
        modes.add(new SimpleVibNormalMode("f4", 125.4584, 1.9487, 14.8948, 0.0181));
        modes.add(new SimpleVibNormalMode("f5", 164.1975, 2.5081, 22.8223, 0.0398));
        modes.add(new SimpleVibNormalMode("f6", 178.606, 2.7888, 6.1576, 0.0524));
        modes.add(new SimpleVibNormalMode("f7", 190.0894, 1.9574, 28.6514, 0.0417));
        modes.add(new SimpleVibNormalMode("f8", 213.8459, 2.343, 11.7351, 0.0631));
        modes.add(new SimpleVibNormalMode("f9", 219.467, 1.0997, 2.4016, 0.0312));
        modes.add(new SimpleVibNormalMode("f10", 224.1991, 1.8738, 43.1451, 0.0555));
        modes.add(new SimpleVibNormalMode("f11", 244.0796, 1.2773, 71.1936, 0.0448));
        modes.add(new SimpleVibNormalMode("f12", 261.4719, 1.2664, 110.481, 0.051));
        modes.add(new SimpleVibNormalMode("f13", 282.3035, 1.9982, 74.9603, 0.0938));
        modes.add(new SimpleVibNormalMode("f14", 322.3117, 2.6197, 28.1307, 0.1603));
        modes.add(new SimpleVibNormalMode("f15", 413.3672, 2.0411, 96.3366, 0.2055));
        modes.add(new SimpleVibNormalMode("f16", 432.885, 2.0444, 19.6767, 0.2257));
        modes.add(new SimpleVibNormalMode("f17", 443.7119, 1.552, 130.5144, 0.18));
        modes.add(new SimpleVibNormalMode("f18", 451.8867, 1.567, 81.8159, 0.1885));
        modes.add(new SimpleVibNormalMode("f19", 469.1831, 1.6737, 154.4262, 0.2171));
        modes.add(new SimpleVibNormalMode("f20", 613.6769 , 3.2171, 27.4089, 0.7138));
        modes.add(new SimpleVibNormalMode("f21", 701.8506 , 7.0192, 206.3768, 2.0372));
        modes.add(new SimpleVibNormalMode("f22", 729.63 , 7.9347, 220.8216, 2.4888));
        modes.add(new SimpleVibNormalMode("f23", 750.2117, 6.8015, 245.2923, 2.2554));
        modes.add(new SimpleVibNormalMode("f24", 862.4081 , 2.6801, 28.0254, 1.1744));
        modes.add(new SimpleVibNormalMode("f25", 923.4653, 1.2987, 0.1796, 0.6526));
        modes.add(new SimpleVibNormalMode("f26", 936.504 , 1.326, 0.2101, 0.6852));
        modes.add(new SimpleVibNormalMode("f27", 1043.1221 , 2.533, 279.0895, 1.6239));
        modes.add(new SimpleVibNormalMode("f28", 1141.6551, 2.1232, 12.8193, 1.6305));
        modes.add(new SimpleVibNormalMode("f29", 1155.6493 , 2.8031, 310.2025, 2.2057));
        modes.add(new SimpleVibNormalMode("f30", 1181.2299, 1.9851, 57.3619, 1.6319));
        modes.add(new SimpleVibNormalMode("f31", 1355.1666, 1.3345, 5.9324, 1.444));
        modes.add(new SimpleVibNormalMode("f32", 1357.3925, 1.1793, 23.4339, 1.2803));
        modes.add(new SimpleVibNormalMode("f33", 1389.7296, 1.323, 19.3592, 1.5055));
        modes.add(new SimpleVibNormalMode("f34", 1405.4699, 1.2476, 10.7332, 1.4521));
        modes.add(new SimpleVibNormalMode("f35", 1474.8222, 1.0484, 1.5558, 1.3435));
        modes.add(new SimpleVibNormalMode("f36", 1480.7955, 1.046, 0.0708, 1.3513));
        modes.add(new SimpleVibNormalMode("f37", 1487.335, 1.0474, 7.1802, 1.3651));
        modes.add(new SimpleVibNormalMode("f38", 1502.4313, 1.0495, 5.819, 1.3958));
        modes.add(new SimpleVibNormalMode("f39", 2987.3867, 1.0842, 30.0746, 5.7007));
        modes.add(new SimpleVibNormalMode("f40", 3019.3448, 1.0342, 19.1834, 5.5549));
        modes.add(new SimpleVibNormalMode("f41", 3023.8634, 1.0347, 20.3164, 5.5745));
        modes.add(new SimpleVibNormalMode("f42", 3091.6832, 1.1021, 0.3476, 6.2065));
        modes.add(new SimpleVibNormalMode("f43", 3098.7324, 1.1014, 55.4544, 6.2309));
        modes.add(new SimpleVibNormalMode("f44", 3101.0222, 1.1025, 21.9243, 6.2463));
        modes.add(new SimpleVibNormalMode("f45", 3104.8988, 1.1022, 28.3308, 6.2605));
        modes.add(new SimpleVibNormalMode("f46", 3933.7808 , 1.0669, 171.3344, 9.727));
        modes.add(new SimpleVibNormalMode("f47", 3938.871, 1.0669, 288.7865, 9.7528));
        modes.add(new SimpleVibNormalMode("f48", 3943.2299, 1.0679, 60.7271, 9.7831));
        
        return modes;
    }
    
    public static SimpleRotModes getSpecies0232RotationalModes() {
        
        SimpleVibNormalModes vibModes = getSpecies0232VibrationalNormalModes();
        SimpleSpecies species = getSpecies0232();
        
        SimpleRotModes modes = new SimpleRotModes();
        
        ArrayList<SimpleAtom> composition = new ArrayList<SimpleAtom>();
        composition.add(species.getAtoms().get(3));
        composition.add(species.getAtoms().get(4));
        composition.add(species.getAtoms().get(5));
        composition.add(species.getAtoms().get(9));
        composition.add(species.getAtoms().get(10));
        composition.add(species.getAtoms().get(11));
        composition.add(species.getAtoms().get(12));
        composition.add(species.getAtoms().get(13));
        composition.add(species.getAtoms().get(14));
        modes.add(new SimpleRotMode(16.1414, 18.1689, 1, 3, 1, vibModes.get(0), composition));
        
        composition = new ArrayList<SimpleAtom>();
        composition.add(species.getAtoms().get(9));
        composition.add(species.getAtoms().get(10));
        composition.add(species.getAtoms().get(11));
        modes.add(new SimpleRotMode(223.7377, 1.2437, 1, 3, 3, vibModes.get(8), composition));
        
        composition = new ArrayList<SimpleAtom>();
        composition.add(species.getAtoms().get(12));
        composition.add(species.getAtoms().get(13));
        composition.add(species.getAtoms().get(14));
        modes.add(new SimpleRotMode(253.2013, 1.2539, 1, 3, 3, vibModes.get(12), composition));
        
        return modes;
    }
}
