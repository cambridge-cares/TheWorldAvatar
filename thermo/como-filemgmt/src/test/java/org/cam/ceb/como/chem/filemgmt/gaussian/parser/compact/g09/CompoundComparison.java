/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.chem.filemgmt.gaussian.parser.compact.g09;

import gigadot.chom.chem.structure.BondType;
import gigadot.chom.chem.structure.Compound;
import java.util.ArrayList;

/**
 *
 * @author pb556
 */
public class CompoundComparison {

    public static boolean isCH4(Compound c) {
        boolean isCH4 = true;
        isCH4 &= (c.getAtomCount() == 5);
        isCH4 &= (c.getBondCount() == 4);
        int ctrCH = 0;
        int ctrOther = 0;
        for (int i = 0; i < c.getBondCount(); i++) {
            if ((c.getBond(i).getAtomA().getElement().symbol.equals("C") && c.getBond(i).getAtomB().getElement().symbol.equals("H") && c.getBond(i).getBondType() == BondType.SINGLE)
                    || (c.getBond(i).getAtomA().getElement().symbol.equals("H") && c.getBond(i).getAtomB().getElement().symbol.equals("C"))) {
                ctrCH++;
            } else {
                ctrOther++;
            }
        }
        isCH4 &= (ctrCH == 4);
        isCH4 &= (ctrOther == 0);

        isCH4 &= (c.getFinalSCFEnergyInHartree() == -40.5111790555);

        ArrayList<Double> vibs = new ArrayList<Double>();
        vibs.add(1335.2431);
        vibs.add(1335.2573);
        vibs.add(1335.3036);
        vibs.add(1555.2243);
        vibs.add(1555.2463);
        vibs.add(3018.5369);
        vibs.add(3131.0616);
        vibs.add(3131.1846);
        vibs.add(3131.2058);

        isCH4 &= (c.getVibrationCount() == vibs.size());
        for (int i = 0; i < vibs.size(); i++) {
            isCH4 &= (vibs.contains(c.getVibration(i).Frequency));
        }

        return isCH4;
    }

    public static boolean isC2H4(Compound c) {
        boolean isC2H4 = true;
        isC2H4 &= (c.getAtomCount() == 6);
        isC2H4 &= (c.getBondCount() == 5);
        int ctrCH = 0;
        int ctrCC = 0;
        int ctrOther = 0;
        for (int i = 0; i < c.getBondCount(); i++) {
            if ((c.getBond(i).getAtomA().getElement().symbol.equals("C") && c.getBond(i).getAtomB().getElement().symbol.equals("H") && c.getBond(i).getBondType() == BondType.SINGLE)
                    || (c.getBond(i).getAtomA().getElement().symbol.equals("H") && c.getBond(i).getAtomB().getElement().symbol.equals("C"))) {
                ctrCH++;
            } else if (c.getBond(i).getAtomA().getElement().symbol.equals("C") && c.getBond(i).getAtomB().getElement().symbol.equals("C") && c.getBond(i).getBondType() == BondType.DOUBLE) {
                ctrCC++;
            } else {
                ctrOther++;
            }
        }

        isC2H4 &= (ctrCH == 4);
        isC2H4 &= (ctrCC == 1);
        isC2H4 &= (ctrOther == 0);

        isC2H4 &= (c.getFinalSCFEnergyInHartree() == -78.5814371906);

        ArrayList<Double> vibs = new ArrayList<Double>();
        vibs.add(828.8604);
        vibs.add(968.9216);
        vibs.add(972.8753);
        vibs.add(1056.8478);
        vibs.add(1234.3780);
        vibs.add(1373.4782);
        vibs.add(1465.1495);
        vibs.add(1676.8549);
        vibs.add(3119.1325);
        vibs.add(3134.4518);
        vibs.add(3194.9118);
        vibs.add(3223.1321);

        isC2H4 &= (c.getVibrationCount() == vibs.size());
        for (int i = 0; i < vibs.size(); i++) {
            isC2H4 &= (vibs.contains(c.getVibration(i).Frequency));
        }

        return isC2H4;
    }

    public static boolean isC3H8(Compound c) {
        boolean isC3H8 = true;
        isC3H8 &= (c.getAtomCount() == 11);
        isC3H8 &= (c.getBondCount() == 10);
        int ctrCH = 0;
        int ctrCC = 0;
        int ctrOther = 0;
        for (int i = 0; i < c.getBondCount(); i++) {
            if ((c.getBond(i).getAtomA().getElement().symbol.equals("C") && c.getBond(i).getAtomB().getElement().symbol.equals("H") && c.getBond(i).getBondType() == BondType.SINGLE)
                    || (c.getBond(i).getAtomA().getElement().symbol.equals("H") && c.getBond(i).getAtomB().getElement().symbol.equals("C"))) {
                ctrCH++;
            } else if (c.getBond(i).getAtomA().getElement().symbol.equals("C") && c.getBond(i).getAtomB().getElement().symbol.equals("C") && c.getBond(i).getBondType() == BondType.SINGLE) {
                ctrCC++;
            } else {
                ctrOther++;
            }
        }

        isC3H8 &= (ctrCH == 8);
        isC3H8 &= (ctrCC == 2);
        isC3H8 &= (ctrOther == 0);

        isC3H8 &= (c.getFinalSCFEnergyInHartree() == -119.130387825);

        ArrayList<Double> vibs = new ArrayList<Double>();
        vibs.add(219.2303);
        vibs.add(268.9670);
        vibs.add(364.0668);
        vibs.add(746.7775);
        vibs.add(870.1335);
        vibs.add(905.6370);
        vibs.add(926.0845);
        vibs.add(1056.6927);
        vibs.add(1169.4119);
        vibs.add(1204.8288);
        vibs.add(1310.4402);
        vibs.add(1356.7395);
        vibs.add(1396.6648);
        vibs.add(1413.1090);
        vibs.add(1485.8370);
        vibs.add(1486.9963);
        vibs.add(1492.5551);
        vibs.add(1503.3568);
        vibs.add(1507.4176);
        vibs.add(3009.1173);
        vibs.add(3010.1424);
        vibs.add(3012.5014);
        vibs.add(3034.4677);
        vibs.add(3071.4333);
        vibs.add(3081.8885);
        vibs.add(3082.4047);
        vibs.add(3084.3253);

        isC3H8 &= (c.getVibrationCount() == vibs.size());
        for (int i = 0; i < vibs.size(); i++) {
            isC3H8 &= (vibs.contains(c.getVibration(i).Frequency));
        }

        return isC3H8;
    }
}
