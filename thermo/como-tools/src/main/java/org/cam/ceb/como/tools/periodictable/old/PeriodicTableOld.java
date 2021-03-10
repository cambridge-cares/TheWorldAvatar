package org.cam.ceb.como.tools.periodictable.old;

import java.util.ArrayList;
import java.util.List;
import org.apache.log4j.Logger;

/**
 *
 * @author pb556
 */
public class PeriodicTableOld {

    public static Logger logger = Logger.getLogger(PeriodicTableOld.class);
    private final static List<ElementOld> elements = new ArrayList<ElementOld>();

    public static enum BLOCK {

        S, D, P, F
    }
    
    public static List<ElementOld> getElements() {
        return elements;
    }

    public static ElementOld getElementByName(String name) {
        for (int i = 0; i < elements.size(); i++) {
            if (elements.get(i).name.equalsIgnoreCase(name)) {
                return elements.get(i);
            }
        }
        throw new UnsupportedOperationException("The element with the name " + name + " does not exist.");
    }
    
    public static ElementOld getElementBySymbol(String symbol) {
        for (int i = 0; i < elements.size(); i++) {
            if (elements.get(i).symbol.equalsIgnoreCase(symbol)) {
                return elements.get(i);
            }
        }
        throw new UnsupportedOperationException("The element with the symbol " + symbol + " does not exist.");
    }

    public static ElementOld getElementByAtomicNumber(int atomicNumber) {
        for (int i = 0; i < elements.size(); i++) {
            if (elements.get(i).atomicNumber == atomicNumber) {
                return elements.get(i);
            }
        }
        throw new UnsupportedOperationException("The element with the atomic number " + atomicNumber + " does not exist.");
    }
    
    public static List<ElementOld> getElementByClassification(ElementOld.CLASSIFICATION classification) {
        ArrayList<ElementOld> selectedElements = new ArrayList<ElementOld>();
        for (int i = 0; i < elements.size(); i++) {
            if (elements.get(i).getClassification() == classification) {
                selectedElements.add(elements.get(i));
            }
        }
        return selectedElements;
    }
    
    public static ElementOld getElementByPosition(int row, int col) {
        for (int i = 0; i < elements.size(); i++) {
            if (elements.get(i).row == row && elements.get(i).col == col) {
                return elements.get(i);
            }
        }
        throw new UnsupportedOperationException("The position (" + row + ", " + col + ") does not define a valid element.");
    }
    
    public static List<ElementOld> getElementByBlock(BLOCK block) {
        List<ElementOld> selectedElements = null;
        switch(block) {
            case S:
                selectedElements = getElementByPositionRange(1, 7, 1, 2);
                selectedElements.add(getElementBySymbol("He"));
                break;
            case D:
                selectedElements = getElementByPositionRange(4, 7, 4, 12);
                selectedElements.add(getElementBySymbol("Sc"));
                selectedElements.add(getElementBySymbol("Y"));
                break;
            case P:
                selectedElements = getElementByPositionRange(2, 7, 13, 18);
                break;
            case F:
                selectedElements = getElementByPositionRange(6, 7, 3, 3);
                break;
            default:
                throw new UnsupportedOperationException("The block " + block + " does not exist.");
        }

        return selectedElements;
    }
    
    public static List<ElementOld> getElementByRow(int row, BLOCK block) {
        List<ElementOld> blockElements = getElementByBlock(block);
        ArrayList<ElementOld> selectedElements = new ArrayList<ElementOld>();
        for (ElementOld e : blockElements) {
            if (e.row == row) {
                selectedElements.add(e);
            }
        }
        return selectedElements;
    }
    
    public static List<ElementOld> getElementByCol(int col, BLOCK block) {
        List<ElementOld> blockElements = getElementByBlock(block);
        ArrayList<ElementOld> selectedElements = new ArrayList<ElementOld>();
        for (ElementOld e : blockElements) {
            if (e.col == col) {
                selectedElements.add(e);
            }
        }
        return selectedElements;
    }
    
    public static List<ElementOld> getElementByRow(int row) {
        ArrayList<ElementOld> selectedElements = new ArrayList<ElementOld>();
        for (int i = 0; i < elements.size(); i++) {
            if (elements.get(i).row == row) {
                selectedElements.add(elements.get(i));
            }
        }
        return selectedElements;
    }
    
    public static List<ElementOld> getElementByCol(int col) {
        ArrayList<ElementOld> selectedElements = new ArrayList<ElementOld>();
        for (int i = 0; i < elements.size(); i++) {
            if (elements.get(i).col == col) {
                selectedElements.add(elements.get(i));
            }
        }
        return selectedElements;
    }
    
    public static List<ElementOld> getElementByAtomicNumberRange(int atomicNumberStart, int atomicNumberEnd) {
        if (atomicNumberStart > atomicNumberEnd) {
            int x = atomicNumberStart;
            atomicNumberStart = atomicNumberEnd;
            atomicNumberEnd = x;
            logger.warn("Invalid indices for atomicNumberStart and atomicNumberEnd were set and had to be switched.");
        }
        ArrayList<ElementOld> selectedElements = new ArrayList<ElementOld>();
        for (int i = 0; i < elements.size(); i++) {
            if (elements.get(i).atomicNumber >= atomicNumberStart && elements.get(i).atomicNumber <= atomicNumberEnd) {
                selectedElements.add(elements.get(i));
            }
        }
        return selectedElements;
    }
    
    public static List<ElementOld> getElementByPositionRange(int rowStart, int rowEnd, int colStart, int colEnd) {
        if (rowStart > rowEnd) {
            int x = rowStart;
            rowStart = rowEnd;
            rowEnd = x;
            logger.warn("Invalid indices for rowStart and rowEnd were set and had to be switched.");
        }
        if (colStart > colEnd) {
            int x = colStart;
            colStart = colEnd;
            colEnd = x;
            logger.warn("Invalid indices for colStart and colEnd were set and had to be switched.");
        }
        ArrayList<ElementOld> selectedElements = new ArrayList<ElementOld>();
        for (int i = 0; i < elements.size(); i++) {
            if (elements.get(i).row >= rowStart && elements.get(i).row <= rowEnd && 
                    elements.get(i).col >= colStart && elements.get(i).col <= colEnd) {
                selectedElements.add(elements.get(i));
            }
        }
        return selectedElements;
    }

    public static BLOCK getBlock(ElementOld e) {
        return BLOCK.S;
    }

    public static ElementOld getHydrogen() {

        ElementOld e = new ElementOld("H", "Hydrogen");
        e.setRow(1);
        e.setColumn(1);
        e.setAtomicNumber(1);
        e.setAtomicWeight(1.00794);
        e.setMeltingPoint(14.009985);
        e.setBoilingPoint(20.280005);
        // e.setMassNumber(1);
        e.setNumberOfNeutrons(0);
        e.setClassification(ElementOld.CLASSIFICATION.NONMETAL);
        e.setDensity(0.08988);
        return e;
    }

    public static ElementOld getHelium() {

        ElementOld e = new ElementOld("He", "Helium");
        e.setRow(1);
        e.setColumn(18);
        e.setAtomicNumber(2);
        e.setAtomicWeight(4.002602);
        e.setMeltingPoint(1.15);
        e.setBoilingPoint(4.549994);
        // e.setMassNumber(2);
        e.setNumberOfNeutrons(2);
        e.setClassification(ElementOld.CLASSIFICATION.NOBLEGAS);
        e.setDensity(0.1785);
        return e;
    }

    public static ElementOld getLithium() {

        ElementOld e = new ElementOld("Li", "Lithium");
        e.setRow(2);
        e.setColumn(1);
        e.setAtomicNumber(3);
        e.setAtomicWeight(6.941);
        e.setMeltingPoint(453.69);
        e.setBoilingPoint(1620.15);
        // e.setMassNumber(3);
        e.setNumberOfNeutrons(4);
        e.setClassification(ElementOld.CLASSIFICATION.ALKALIMETAL);
        e.setDensity(0.53);
        return e;
    }

    public static ElementOld getBeryllium() {

        ElementOld e = new ElementOld("Be", "Beryllium");
        e.setRow(2);
        e.setColumn(2);
        e.setAtomicNumber(4);
        e.setAtomicWeight(9.012182);
        e.setMeltingPoint(1551.15);
        e.setBoilingPoint(3243.15);
        // e.setMassNumber(4);
        e.setNumberOfNeutrons(5);
        e.setClassification(ElementOld.CLASSIFICATION.ALKANIEARTH);
        e.setDensity(1.8477);
        return e;
    }

    public static ElementOld getBoron() {

        ElementOld e = new ElementOld("B", "Boron");
        e.setRow(2);
        e.setColumn(13);
        e.setAtomicNumber(5);
        e.setAtomicWeight(10.811);
        e.setMeltingPoint(2573.15);
        e.setBoilingPoint(2823.15);
        // e.setMassNumber(5);
        e.setNumberOfNeutrons(6);
        e.setClassification(ElementOld.CLASSIFICATION.METALLOID);
        e.setDensity(2.34);
        return e;
    }

    public static ElementOld getCarbon() {

        ElementOld e = new ElementOld("C", "Carbon");
        e.setRow(2);
        e.setColumn(14);
        e.setAtomicNumber(6);
        e.setAtomicWeight(12.0107);
        e.setMeltingPoint(3773.15);
        e.setBoilingPoint(5100.15);
        // e.setMassNumber(6);
        e.setNumberOfNeutrons(6);
        e.setClassification(ElementOld.CLASSIFICATION.NONMETAL);
        e.setDensity(2.62);
        return e;
    }

    public static ElementOld getNitrogen() {

        ElementOld e = new ElementOld("N", "Nitrogen");
        e.setRow(2);
        e.setColumn(15);
        e.setAtomicNumber(7);
        e.setAtomicWeight(14.00674);
        e.setMeltingPoint(63.250008);
        e.setBoilingPoint(77.35);
        // e.setMassNumber(7);
        e.setNumberOfNeutrons(7);
        e.setClassification(ElementOld.CLASSIFICATION.NONMETAL);
        e.setDensity(1.2506);
        return e;
    }

    public static ElementOld getOxygen() {

        ElementOld e = new ElementOld("O", "Oxygen");
        e.setRow(2);
        e.setColumn(16);
        e.setAtomicNumber(8);
        e.setAtomicWeight(15.9994);
        e.setMeltingPoint(54.750008);
        e.setBoilingPoint(90.15);
        // e.setMassNumber(8);
        e.setNumberOfNeutrons(8);
        e.setClassification(ElementOld.CLASSIFICATION.NONMETAL);
        e.setDensity(1.429);
        return e;
    }

    public static ElementOld getFluorine() {

        ElementOld e = new ElementOld("F", "Fluorine");
        e.setRow(2);
        e.setColumn(17);
        e.setAtomicNumber(9);
        e.setAtomicWeight(18.998404);
        e.setMeltingPoint(53.530006);
        e.setBoilingPoint(85.01);
        // e.setMassNumber(9);
        e.setNumberOfNeutrons(10);
        e.setClassification(ElementOld.CLASSIFICATION.HALOGEN);
        e.setDensity(1.696);
        return e;
    }

    public static ElementOld getNeon() {

        ElementOld e = new ElementOld("Ne", "Neon");
        e.setRow(2);
        e.setColumn(18);
        e.setAtomicNumber(10);
        e.setAtomicWeight(20.1797);
        e.setMeltingPoint(24.549994);
        e.setBoilingPoint(27.049994);
        // e.setMassNumber(10);
        e.setNumberOfNeutrons(10);
        e.setClassification(ElementOld.CLASSIFICATION.NOBLEGAS);
        e.setDensity(0.901);
        return e;
    }

    public static ElementOld getSodium() {

        ElementOld e = new ElementOld("Na", "Sodium");
        e.setRow(3);
        e.setColumn(1);
        e.setAtomicNumber(11);
        e.setAtomicWeight(22.98977);
        e.setMeltingPoint(370.87);
        e.setBoilingPoint(1156);
        // e.setMassNumber(11);
        e.setNumberOfNeutrons(12);
        e.setClassification(ElementOld.CLASSIFICATION.ALKALIMETAL);
        e.setDensity(0.971);
        return e;
    }

    public static ElementOld getMagnesium() {

        ElementOld e = new ElementOld("Mg", "Magnesium");
        e.setRow(3);
        e.setColumn(2);
        e.setAtomicNumber(12);
        e.setAtomicWeight(24.305);
        e.setMeltingPoint(923.15);
        e.setBoilingPoint(1380.15);
        // e.setMassNumber(12);
        e.setNumberOfNeutrons(12);
        e.setClassification(ElementOld.CLASSIFICATION.ALKALIMETAL);
        e.setDensity(1.738);
        return e;
    }

    public static ElementOld getAluminium() {

        ElementOld e = new ElementOld("Al", "Aluminium");
        e.setRow(3);
        e.setColumn(13);
        e.setAtomicNumber(13);
        e.setAtomicWeight(26.981539);
        e.setMeltingPoint(933.52);
        e.setBoilingPoint(2740.15);
        // e.setMassNumber(13);
        e.setNumberOfNeutrons(14);
        e.setClassification(ElementOld.CLASSIFICATION.OTHERMETALS);
        e.setDensity(0.53);
        return e;
    }

    public static ElementOld getSilicon() {

        ElementOld e = new ElementOld("Si", "Silicon");
        e.setRow(3);
        e.setColumn(14);
        e.setAtomicNumber(14);
        e.setAtomicWeight(28.0855);
        e.setMeltingPoint(1683.15);
        e.setBoilingPoint(2628.15);
        // e.setMassNumber(14);
        e.setNumberOfNeutrons(14);
        e.setClassification(ElementOld.CLASSIFICATION.METALLOID);
        e.setDensity(2.329);
        return e;
    }

    public static ElementOld getPhosphorus() {

        ElementOld e = new ElementOld("P", "Phosphorus");
        e.setRow(3);
        e.setColumn(15);
        e.setAtomicNumber(15);
        e.setAtomicWeight(30.97376);
        e.setMeltingPoint(317.25);
        e.setBoilingPoint(553.15);
        // e.setMassNumber(15);
        e.setNumberOfNeutrons(16);
        e.setClassification(ElementOld.CLASSIFICATION.NONMETAL);
        e.setDensity(1.82);
        return e;
    }

    public static ElementOld getSulphur() {

        ElementOld e = new ElementOld("S", "Sulphur");
        e.setRow(3);
        e.setColumn(16);
        e.setAtomicNumber(16);
        e.setAtomicWeight(32.066);
        e.setMeltingPoint(385.95);
        e.setBoilingPoint(717.75);
        // e.setMassNumber(16);
        e.setNumberOfNeutrons(16);
        e.setClassification(ElementOld.CLASSIFICATION.NONMETAL);
        e.setDensity(2.07);
        return e;
    }

    public static ElementOld getChlorine() {

        ElementOld e = new ElementOld("Cl", "Chlorine");
        e.setRow(3);
        e.setColumn(17);
        e.setAtomicNumber(17);
        e.setAtomicWeight(35.4527);
        e.setMeltingPoint(172.17);
        e.setBoilingPoint(238.55);
        // e.setMassNumber(17);
        e.setNumberOfNeutrons(18);
        e.setClassification(ElementOld.CLASSIFICATION.HALOGEN);
        e.setDensity(3.214);
        return e;
    }

    public static ElementOld getArgon() {

        ElementOld e = new ElementOld("Ar", "Argon");
        e.setRow(3);
        e.setColumn(18);
        e.setAtomicNumber(18);
        e.setAtomicWeight(39.948);
        e.setMeltingPoint(83.85);
        e.setBoilingPoint(87.15);
        // e.setMassNumber(18);
        e.setNumberOfNeutrons(22);
        e.setClassification(ElementOld.CLASSIFICATION.NOBLEGAS);
        e.setDensity(1.784);
        return e;
    }

    public static ElementOld getPotassium() {

        ElementOld e = new ElementOld("K", "Potassium");
        e.setRow(4);
        e.setColumn(1);
        e.setAtomicNumber(19);
        e.setAtomicWeight(39.0983);
        e.setMeltingPoint(336.8);
        e.setBoilingPoint(1047.15);
        // e.setMassNumber(19);
        e.setNumberOfNeutrons(20);
        e.setClassification(ElementOld.CLASSIFICATION.ALKALIMETAL);
        e.setDensity(0.862);
        return e;
    }

    public static ElementOld getCalcium() {

        ElementOld e = new ElementOld("Ca", "Calcium");
        e.setRow(4);
        e.setColumn(2);
        e.setAtomicNumber(20);
        e.setAtomicWeight(40.078);
        e.setMeltingPoint(1112.15);
        e.setBoilingPoint(1757.15);
        // e.setMassNumber(20);
        e.setNumberOfNeutrons(20);
        e.setClassification(ElementOld.CLASSIFICATION.ALKALINEEARTH);
        e.setDensity(1.55);
        return e;
    }

    public static ElementOld getScandium() {

        ElementOld e = new ElementOld("Sc", "Scandium");
        e.setRow(4);
        e.setColumn(3);
        e.setAtomicNumber(21);
        e.setAtomicWeight(44.95591);
        e.setMeltingPoint(1812.15);
        e.setBoilingPoint(3105.15);
        // e.setMassNumber(21);
        e.setNumberOfNeutrons(24);
        e.setClassification(ElementOld.CLASSIFICATION.TRANSITIONMETAL);
        e.setDensity(2.989);
        return e;
    }

    public static ElementOld getTitanium() {

        ElementOld e = new ElementOld("Ti", "Titanium");
        e.setRow(4);
        e.setColumn(4);
        e.setAtomicNumber(22);
        e.setAtomicWeight(47.867);
        e.setMeltingPoint(1933.15);
        e.setBoilingPoint(3560.15);
        // e.setMassNumber(22);
        e.setNumberOfNeutrons(26);
        e.setClassification(ElementOld.CLASSIFICATION.TRANSITIONMETAL);
        e.setDensity(4.54);
        return e;
    }

    public static ElementOld getVanadium() {

        ElementOld e = new ElementOld("V", "Vanadium");
        e.setRow(4);
        e.setColumn(5);
        e.setAtomicNumber(23);
        e.setAtomicWeight(50.9415);
        e.setMeltingPoint(2163.15);
        e.setBoilingPoint(3653.15);
        // e.setMassNumber(23);
        e.setNumberOfNeutrons(28);
        e.setClassification(ElementOld.CLASSIFICATION.TRANSITIONMETAL);
        e.setDensity(5.8);
        return e;
    }

    public static ElementOld getChromium() {

        ElementOld e = new ElementOld("Cr", "Chromium");
        e.setRow(4);
        e.setColumn(6);
        e.setAtomicNumber(24);
        e.setAtomicWeight(51.9961);
        e.setMeltingPoint(2130.15);
        e.setBoilingPoint(2945.15);
        // e.setMassNumber(24);
        e.setNumberOfNeutrons(28);
        e.setClassification(ElementOld.CLASSIFICATION.TRANSITIONMETAL);
        e.setDensity(7.19);
        return e;
    }

    public static ElementOld getManganese() {

        ElementOld e = new ElementOld("Mn", "Manganese");
        e.setRow(4);
        e.setColumn(7);
        e.setAtomicNumber(25);
        e.setAtomicWeight(54.93805);
        e.setMeltingPoint(1518.15);
        e.setBoilingPoint(2235.15);
        // e.setMassNumber(25);
        e.setNumberOfNeutrons(30);
        e.setClassification(ElementOld.CLASSIFICATION.TRANSITIONMETAL);
        e.setDensity(7.43);
        return e;
    }

    public static ElementOld getIron() {

        ElementOld e = new ElementOld("Fe", "Iron");
        e.setRow(4);
        e.setColumn(8);
        e.setAtomicNumber(26);
        e.setAtomicWeight(55.845);
        e.setMeltingPoint(1808.15);
        e.setBoilingPoint(3023.15);
        // e.setMassNumber(26);
        e.setNumberOfNeutrons(30);
        e.setClassification(ElementOld.CLASSIFICATION.TRANSITIONMETAL);
        e.setDensity(7.86);
        return e;
    }

    public static ElementOld getCobalt() {

        ElementOld e = new ElementOld("Co", "Cobalt");
        e.setRow(4);
        e.setColumn(9);
        e.setAtomicNumber(27);
        e.setAtomicWeight(58.9332);
        e.setMeltingPoint(1768.15);
        e.setBoilingPoint(3143.15);
        // e.setMassNumber(27);
        e.setNumberOfNeutrons(32);
        e.setClassification(ElementOld.CLASSIFICATION.TRANSITIONMETAL);
        e.setDensity(8.9);
        return e;
    }

    public static ElementOld getNickel() {

        ElementOld e = new ElementOld("Ni", "Nickel");
        e.setRow(4);
        e.setColumn(10);
        e.setAtomicNumber(28);
        e.setAtomicWeight(58.6934);
        e.setMeltingPoint(1726.15);
        e.setBoilingPoint(3005.15);
        // e.setMassNumber(28);
        e.setNumberOfNeutrons(31);
        e.setClassification(ElementOld.CLASSIFICATION.TRANSITIONMETAL);
        e.setDensity(8.902);
        return e;
    }

    public static ElementOld getCopper() {

        ElementOld e = new ElementOld("Cu", "Copper");
        e.setRow(4);
        e.setColumn(11);
        e.setAtomicNumber(29);
        e.setAtomicWeight(63.546);
        e.setMeltingPoint(1356.15);
        e.setBoilingPoint(2840.15);
        // e.setMassNumber(29);
        e.setNumberOfNeutrons(35);
        e.setClassification(ElementOld.CLASSIFICATION.TRANSITIONMETAL);
        e.setDensity(8.96);
        return e;
    }

    public static ElementOld getZinc() {

        ElementOld e = new ElementOld("Zn", "Zinc");
        e.setRow(4);
        e.setColumn(12);
        e.setAtomicNumber(30);
        e.setAtomicWeight(65.39);
        e.setMeltingPoint(692.73);
        e.setBoilingPoint(1180.15);
        // e.setMassNumber(30);
        e.setNumberOfNeutrons(35);
        e.setClassification(ElementOld.CLASSIFICATION.TRANSITIONMETAL);
        e.setDensity(7.133);
        return e;
    }

    public static ElementOld getGallium() {

        ElementOld e = new ElementOld("Ga", "Gallium");
        e.setRow(4);
        e.setColumn(13);
        e.setAtomicNumber(31);
        e.setAtomicWeight(69.723);
        e.setMeltingPoint(302.93);
        e.setBoilingPoint(2676.15);
        // e.setMassNumber(31);
        e.setNumberOfNeutrons(39);
        e.setClassification(ElementOld.CLASSIFICATION.OTHERMETALS);
        e.setDensity(5.907);
        return e;
    }

    public static ElementOld getGermanium() {

        ElementOld e = new ElementOld("Ge", "Germanium");
        e.setRow(4);
        e.setColumn(14);
        e.setAtomicNumber(32);
        e.setAtomicWeight(72.61);
        e.setMeltingPoint(1210.55);
        e.setBoilingPoint(3103.15);
        // e.setMassNumber(32);
        e.setNumberOfNeutrons(41);
        e.setClassification(ElementOld.CLASSIFICATION.METALLOID);
        e.setDensity(5.323);
        return e;
    }

    public static ElementOld getArsenic() {

        ElementOld e = new ElementOld("As", "Arsenic");
        e.setRow(4);
        e.setColumn(15);
        e.setAtomicNumber(33);
        e.setAtomicWeight(74.9216);
        e.setMeltingPoint(886.15);
        e.setBoilingPoint(1090.15);
        // e.setMassNumber(33);
        e.setNumberOfNeutrons(42);
        e.setClassification(ElementOld.CLASSIFICATION.METALLOID);
        e.setDensity(5.72);
        return e;
    }

    public static ElementOld getSelenium() {

        ElementOld e = new ElementOld("Se", "Selenium");
        e.setRow(4);
        e.setColumn(16);
        e.setAtomicNumber(34);
        e.setAtomicWeight(78.96);
        e.setMeltingPoint(490.15);
        e.setBoilingPoint(958.05005);
        // e.setMassNumber(34);
        e.setNumberOfNeutrons(45);
        e.setClassification(ElementOld.CLASSIFICATION.NONMETAL);
        e.setDensity(4.79);
        return e;
    }

    public static ElementOld getBromine() {

        ElementOld e = new ElementOld("Br", "Bromine");
        e.setRow(4);
        e.setColumn(17);
        e.setAtomicNumber(35);
        e.setAtomicWeight(79.904);
        e.setMeltingPoint(265.95);
        e.setBoilingPoint(331.93);
        // e.setMassNumber(35);
        e.setNumberOfNeutrons(45);
        e.setClassification(ElementOld.CLASSIFICATION.HALOGEN);
        e.setDensity(3.119);
        return e;
    }

    public static ElementOld getKrypton() {

        ElementOld e = new ElementOld("Kr", "Krypton");
        e.setRow(4);
        e.setColumn(18);
        e.setAtomicNumber(36);
        e.setAtomicWeight(83.8);
        e.setMeltingPoint(115.950005);
        e.setBoilingPoint(119.75001);
        // e.setMassNumber(36);
        e.setNumberOfNeutrons(48);
        e.setClassification(ElementOld.CLASSIFICATION.NOBLEGAS);
        e.setDensity(3.74);
        return e;
    }

    public static ElementOld getRubidium() {

        ElementOld e = new ElementOld("Rb", "Rubidium");
        e.setRow(5);
        e.setColumn(1);
        e.setAtomicNumber(37);
        e.setAtomicWeight(85.4678);
        e.setMeltingPoint(312.04);
        e.setBoilingPoint(961.15);
        // e.setMassNumber(37);
        e.setNumberOfNeutrons(48);
        e.setClassification(ElementOld.CLASSIFICATION.ALKALIMETAL);
        e.setDensity(1.532);
        return e;
    }

    public static ElementOld getStrontium() {

        ElementOld e = new ElementOld("Sr", "Strontium");
        e.setRow(5);
        e.setColumn(2);
        e.setAtomicNumber(38);
        e.setAtomicWeight(87.62);
        e.setMeltingPoint(1042.15);
        e.setBoilingPoint(1657.15);
        // e.setMassNumber(38);
        e.setNumberOfNeutrons(50);
        e.setClassification(ElementOld.CLASSIFICATION.ALKALINEEARTH);
        e.setDensity(2.54);
        return e;
    }

    public static ElementOld getYttrium() {

        ElementOld e = new ElementOld("Y", "Yttrium");
        e.setRow(5);
        e.setColumn(3);
        e.setAtomicNumber(39);
        e.setAtomicWeight(88.90585);
        e.setMeltingPoint(1796.15);
        e.setBoilingPoint(3610.15);
        // e.setMassNumber(39);
        e.setNumberOfNeutrons(50);
        e.setClassification(ElementOld.CLASSIFICATION.TRANSITIONMETAL);
        e.setDensity(4.469);
        return e;
    }

    public static ElementOld getZirconium() {

        ElementOld e = new ElementOld("Zr", "Zirconium");
        e.setRow(5);
        e.setColumn(4);
        e.setAtomicNumber(40);
        e.setAtomicWeight(91.224);
        e.setMeltingPoint(2125.15);
        e.setBoilingPoint(4650.15);
        // e.setMassNumber(40);
        e.setNumberOfNeutrons(51);
        e.setClassification(ElementOld.CLASSIFICATION.TRANSITIONMETAL);
        e.setDensity(6.49);
        return e;
    }

    public static ElementOld getNiobium() {

        ElementOld e = new ElementOld("Nb", "Niobium");
        e.setRow(5);
        e.setColumn(5);
        e.setAtomicNumber(41);
        e.setAtomicWeight(92.90638);
        e.setMeltingPoint(2741.15);
        e.setBoilingPoint(5200.15);
        // e.setMassNumber(41);
        e.setNumberOfNeutrons(52);
        e.setClassification(ElementOld.CLASSIFICATION.TRANSITIONMETAL);
        e.setDensity(8.57);
        return e;
    }

    public static ElementOld getMolybdenum() {

        ElementOld e = new ElementOld("Mo", "Molybdenum");
        e.setRow(5);
        e.setColumn(6);
        e.setAtomicNumber(42);
        e.setAtomicWeight(95.94);
        e.setMeltingPoint(2890.15);
        e.setBoilingPoint(4885.15);
        // e.setMassNumber(42);
        e.setNumberOfNeutrons(54);
        e.setClassification(ElementOld.CLASSIFICATION.TRANSITIONMETAL);
        e.setDensity(10.20);
        return e;
    }

    public static ElementOld getTechnetium() {

        ElementOld e = new ElementOld("Tc", "Technetium");
        e.setRow(5);
        e.setColumn(7);
        e.setAtomicNumber(43);
        e.setAtomicWeight(98.0);
        e.setMeltingPoint(2473.15);
        e.setBoilingPoint(5150.15);
        // e.setMassNumber(43);
        e.setNumberOfNeutrons(55);
        e.setClassification(ElementOld.CLASSIFICATION.TRANSITIONMETAL);
        e.setDensity(11.5);
        return e;
    }

    public static ElementOld getRuthenium() {

        ElementOld e = new ElementOld("Ru", "Ruthenium");
        e.setRow(5);
        e.setColumn(8);
        e.setAtomicNumber(44);
        e.setAtomicWeight(101.07);
        e.setMeltingPoint(2523.15);
        e.setBoilingPoint(4173.15);
        // e.setMassNumber(44);
        e.setNumberOfNeutrons(57);
        e.setClassification(ElementOld.CLASSIFICATION.TRANSITIONMETAL);
        e.setDensity(12.2);
        return e;
    }

    public static ElementOld getRhodium() {

        ElementOld e = new ElementOld("Rh", "Rhodium");
        e.setRow(5);
        e.setColumn(9);
        e.setAtomicNumber(45);
        e.setAtomicWeight(102.9055);
        e.setMeltingPoint(2239.15);
        e.setBoilingPoint(4000.15);
        // e.setMassNumber(45);
        e.setNumberOfNeutrons(58);
        e.setClassification(ElementOld.CLASSIFICATION.TRANSITIONMETAL);
        e.setDensity(12.41);
        return e;
    }

    public static ElementOld getPalladium() {

        ElementOld e = new ElementOld("Pd", "Palladium");
        e.setRow(5);
        e.setColumn(10);
        e.setAtomicNumber(46);
        e.setAtomicWeight(106.42);
        e.setMeltingPoint(1825.15);
        e.setBoilingPoint(3200.15);
        // e.setMassNumber(46);
        e.setNumberOfNeutrons(60);
        e.setClassification(ElementOld.CLASSIFICATION.TRANSITIONMETAL);
        e.setDensity(12.02);
        return e;
    }

    public static ElementOld getSilver() {

        ElementOld e = new ElementOld("Ag", "Silver");
        e.setRow(5);
        e.setColumn(11);
        e.setAtomicNumber(47);
        e.setAtomicWeight(107.8682);
        e.setMeltingPoint(1235.08);
        e.setBoilingPoint(2485.15);
        // e.setMassNumber(47);
        e.setNumberOfNeutrons(61);
        e.setClassification(ElementOld.CLASSIFICATION.TRANSITIONMETAL);
        e.setDensity(10.5);
        return e;
    }

    public static ElementOld getCadmium() {

        ElementOld e = new ElementOld("Cd", "Cadmium");
        e.setRow(5);
        e.setColumn(12);
        e.setAtomicNumber(48);
        e.setAtomicWeight(112.411);
        e.setMeltingPoint(594.05);
        e.setBoilingPoint(1038.15);
        // e.setMassNumber(48);
        e.setNumberOfNeutrons(64);
        e.setClassification(ElementOld.CLASSIFICATION.TRANSITIONMETAL);
        e.setDensity(8.65);
        return e;
    }

    public static ElementOld getIndium() {

        ElementOld e = new ElementOld("In", "Indium");
        e.setRow(5);
        e.setColumn(13);
        e.setAtomicNumber(49);
        e.setAtomicWeight(114.818);
        e.setMeltingPoint(429.76);
        e.setBoilingPoint(2273.15);
        // e.setMassNumber(49);
        e.setNumberOfNeutrons(66);
        e.setClassification(ElementOld.CLASSIFICATION.OTHERMETALS);
        e.setDensity(7.31);
        return e;
    }

    public static ElementOld getTin() {

        ElementOld e = new ElementOld("Sn", "Tin");
        e.setRow(5);
        e.setColumn(14);
        e.setAtomicNumber(50);
        e.setAtomicWeight(118.71);
        e.setMeltingPoint(505.05);
        e.setBoilingPoint(2543.15);
        // e.setMassNumber(50);
        e.setNumberOfNeutrons(69);
        e.setClassification(ElementOld.CLASSIFICATION.OTHERMETALS);
        e.setDensity(7.31);
        return e;
    }

    public static ElementOld getAntimony() {

        ElementOld e = new ElementOld("Sb", "Antimony");
        e.setRow(5);
        e.setColumn(15);
        e.setAtomicNumber(51);
        e.setAtomicWeight(121.76);
        e.setMeltingPoint(903.15);
        e.setBoilingPoint(2023.15);
        // e.setMassNumber(51);
        e.setNumberOfNeutrons(71);
        e.setClassification(ElementOld.CLASSIFICATION.METALLOID);
        e.setDensity(6.684);
        return e;
    }

    public static ElementOld getTellurium() {

        ElementOld e = new ElementOld("Te", "Tellurium");
        e.setRow(5);
        e.setColumn(16);
        e.setAtomicNumber(52);
        e.setAtomicWeight(127.6);
        e.setMeltingPoint(722.65);
        e.setBoilingPoint(1262.95);
        // e.setMassNumber(52);
        e.setNumberOfNeutrons(76);
        e.setClassification(ElementOld.CLASSIFICATION.METALLOID);
        e.setDensity(6.24);
        return e;
    }

    public static ElementOld getIodine() {

        ElementOld e = new ElementOld("I", "Iodine");
        e.setRow(5);
        e.setColumn(17);
        e.setAtomicNumber(53);
        e.setAtomicWeight(126.90447);
        e.setMeltingPoint(386.65);
        e.setBoilingPoint(457.15);
        // e.setMassNumber(53);
        e.setNumberOfNeutrons(74);
        e.setClassification(ElementOld.CLASSIFICATION.HALOGEN);
        e.setDensity(4.93);
        return e;
    }

    public static ElementOld getXenon() {

        ElementOld e = new ElementOld("Xe", "Xenon");
        e.setRow(5);
        e.setColumn(18);
        e.setAtomicNumber(54);
        e.setAtomicWeight(131.29);
        e.setMeltingPoint(161.25);
        e.setBoilingPoint(165.05);
        // e.setMassNumber(54);
        e.setNumberOfNeutrons(77);
        e.setClassification(ElementOld.CLASSIFICATION.NOBLEGAS);
        e.setDensity(5.8971);
        return e;
    }

    public static ElementOld getCaesium() {

        ElementOld e = new ElementOld("Cs", "Caesium ");
        e.setRow(6);
        e.setColumn(1);
        e.setAtomicNumber(55);
        e.setAtomicWeight(132.90546);
        e.setMeltingPoint(301.65);
        e.setBoilingPoint(951.55005);
        // e.setMassNumber(55);
        e.setNumberOfNeutrons(78);
        e.setClassification(ElementOld.CLASSIFICATION.ALKALIMETAL);
        e.setDensity(1.873);
        return e;
    }

    public static ElementOld getBarium() {

        ElementOld e = new ElementOld("Ba", "Barium");
        e.setRow(6);
        e.setColumn(2);
        e.setAtomicNumber(56);
        e.setAtomicWeight(137.327);
        e.setMeltingPoint(998.15);
        e.setBoilingPoint(1413.15);
        // e.setMassNumber(56);
        e.setNumberOfNeutrons(81);
        e.setClassification(ElementOld.CLASSIFICATION.ALKALINEEARTH);
        e.setDensity(3.51);
        return e;
    }

    public static ElementOld getLanthanum() {

        ElementOld e = new ElementOld("La", "Lanthanum");
        e.setRow(6);
        e.setColumn(3);
        e.setAtomicNumber(57);
        e.setAtomicWeight(138.9055);
        e.setMeltingPoint(1193.15);
        e.setBoilingPoint(3742.15);
        // e.setMassNumber(57);
        e.setNumberOfNeutrons(82);
        e.setClassification(ElementOld.CLASSIFICATION.RAREEARTH_LANTHANIDES);
        e.setDensity(6.7);
        return e;
    }

    public static ElementOld getCerium() {

        ElementOld e = new ElementOld("Ce", "Cerium");
        e.setRow(6);
        e.setColumn(3);
        e.setAtomicNumber(58);
        e.setAtomicWeight(140.116);
        e.setMeltingPoint(1068.15);
        e.setBoilingPoint(3530.15);
        // e.setMassNumber(58);
        e.setNumberOfNeutrons(82);
        e.setClassification(ElementOld.CLASSIFICATION.RAREEARTH_LANTHANIDES);
        e.setDensity(6.773);
        return e;
    }

    public static ElementOld getPraseodymium() {

        ElementOld e = new ElementOld("Pr", "Praseodymium");
        e.setRow(6);
        e.setColumn(3);
        e.setAtomicNumber(59);
        e.setAtomicWeight(140.90765);
        e.setMeltingPoint(1208.15);
        e.setBoilingPoint(3400.15);
        // e.setMassNumber(59);
        e.setNumberOfNeutrons(82);
        e.setClassification(ElementOld.CLASSIFICATION.RAREEARTH_LANTHANIDES);
        e.setDensity(6.77);
        return e;
    }

    public static ElementOld getNeodymium() {

        ElementOld e = new ElementOld("Nd", "Neodymium");
        e.setRow(6);
        e.setColumn(3);
        e.setAtomicNumber(60);
        e.setAtomicWeight(144.24);
        e.setMeltingPoint(1283.15);
        e.setBoilingPoint(3400.15);
        // e.setMassNumber(60);
        e.setNumberOfNeutrons(84);
        e.setClassification(ElementOld.CLASSIFICATION.RAREEARTH_LANTHANIDES);
        e.setDensity(7.007);
        return e;
    }

    public static ElementOld getPromethium() {

        ElementOld e = new ElementOld("Pm", "Promethium");
        e.setRow(6);
        e.setColumn(3);
        e.setAtomicNumber(61);
        e.setAtomicWeight(145.0);
        e.setMeltingPoint(-1);
        e.setBoilingPoint(-1);
        // e.setMassNumber(61);
        e.setNumberOfNeutrons(84);
        e.setClassification(ElementOld.CLASSIFICATION.RAREEARTH_LANTHANIDES);
        e.setDensity(6.475);
        return e;
    }

    public static ElementOld getSamarium() {

        ElementOld e = new ElementOld("Sm", "Samarium");
        e.setRow(6);
        e.setColumn(3);
        e.setAtomicNumber(62);
        e.setAtomicWeight(150.36);
        e.setMeltingPoint(1345.15);
        e.setBoilingPoint(2173.15);
        // e.setMassNumber(62);
        e.setNumberOfNeutrons(88);
        e.setClassification(ElementOld.CLASSIFICATION.RAREEARTH_LANTHANIDES);
        e.setDensity(7.54);
        return e;
    }

    public static ElementOld getEuropium() {

        ElementOld e = new ElementOld("Eu", "Europium");
        e.setRow(6);
        e.setColumn(3);
        e.setAtomicNumber(63);
        e.setAtomicWeight(151.964);
        e.setMeltingPoint(1095.15);
        e.setBoilingPoint(1870.15);
        // e.setMassNumber(63);
        e.setNumberOfNeutrons(89);
        e.setClassification(ElementOld.CLASSIFICATION.RAREEARTH_LANTHANIDES);
        e.setDensity(5.259);
        return e;
    }

    public static ElementOld getGadolinium() {

        ElementOld e = new ElementOld("Gd", "Gadolinium");
        e.setRow(6);
        e.setColumn(3);
        e.setAtomicNumber(64);
        e.setAtomicWeight(157.25);
        e.setMeltingPoint(1584.15);
        e.setBoilingPoint(3506.15);
        // e.setMassNumber(64);
        e.setNumberOfNeutrons(93);
        e.setClassification(ElementOld.CLASSIFICATION.RAREEARTH_LANTHANIDES);
        e.setDensity(7.895);
        return e;
    }

    public static ElementOld getTerbium() {

        ElementOld e = new ElementOld("Tb", "Terbium");
        e.setRow(6);
        e.setColumn(3);
        e.setAtomicNumber(65);
        e.setAtomicWeight(158.92534);
        e.setMeltingPoint(1633.15);
        e.setBoilingPoint(3314.15);
        // e.setMassNumber(65);
        e.setNumberOfNeutrons(94);
        e.setClassification(ElementOld.CLASSIFICATION.RAREEARTH_LANTHANIDES);
        e.setDensity(8.27);
        return e;
    }

    public static ElementOld getDysprosium() {

        ElementOld e = new ElementOld("Dy", "Dysprosium");
        e.setRow(6);
        e.setColumn(3);
        e.setAtomicNumber(66);
        e.setAtomicWeight(162.5);
        e.setMeltingPoint(1685.15);
        e.setBoilingPoint(2835.15);
        // e.setMassNumber(66);
        e.setNumberOfNeutrons(97);
        e.setClassification(ElementOld.CLASSIFICATION.RAREEARTH_LANTHANIDES);
        e.setDensity(8.536);
        return e;
    }

    public static ElementOld getHolmium() {

        ElementOld e = new ElementOld("Ho", "Holmium");
        e.setRow(6);
        e.setColumn(3);
        e.setAtomicNumber(67);
        e.setAtomicWeight(164.93031);
        e.setMeltingPoint(1743.15);
        e.setBoilingPoint(2993.15);
        // e.setMassNumber(67);
        e.setNumberOfNeutrons(98);
        e.setClassification(ElementOld.CLASSIFICATION.RAREEARTH_LANTHANIDES);
        e.setDensity(8.54);
        return e;
    }

    public static ElementOld getErbium() {

        ElementOld e = new ElementOld("Er", "Erbium");
        e.setRow(6);
        e.setColumn(3);
        e.setAtomicNumber(68);
        e.setAtomicWeight(167.26);
        e.setMeltingPoint(1795.15);
        e.setBoilingPoint(2783.15);
        // e.setMassNumber(68);
        e.setNumberOfNeutrons(99);
        e.setClassification(ElementOld.CLASSIFICATION.RAREEARTH_LANTHANIDES);
        e.setDensity(8.795);
        return e;
    }

    public static ElementOld getThulium() {

        ElementOld e = new ElementOld("Tm", "Thulium");
        e.setRow(6);
        e.setColumn(3);
        e.setAtomicNumber(69);
        e.setAtomicWeight(168.9342);
        e.setMeltingPoint(1818.15);
        e.setBoilingPoint(2000.15);
        // e.setMassNumber(69);
        e.setNumberOfNeutrons(100);
        e.setClassification(ElementOld.CLASSIFICATION.RAREEARTH_LANTHANIDES);
        e.setDensity(9.321);
        return e;
    }

    public static ElementOld getYtterbium() {

        ElementOld e = new ElementOld("Yb", "Ytterbium");
        e.setRow(6);
        e.setColumn(3);
        e.setAtomicNumber(55);
        e.setAtomicWeight(173.04);
        e.setMeltingPoint(1097.15);
        e.setBoilingPoint(1739.15);
        // e.setMassNumber(70);
        e.setNumberOfNeutrons(103);
        e.setClassification(ElementOld.CLASSIFICATION.RAREEARTH_LANTHANIDES);
        e.setDensity(6.98);
        return e;
    }

    public static ElementOld getLutetium() {

        ElementOld e = new ElementOld("Lu", "Lutetium");
        e.setRow(6);
        e.setColumn(3);
        e.setAtomicNumber(71);
        e.setAtomicWeight(174.967);
        e.setMeltingPoint(1929.15);
        e.setBoilingPoint(3588.15);
        // e.setMassNumber(71);
        e.setNumberOfNeutrons(104);
        e.setClassification(ElementOld.CLASSIFICATION.RAREEARTH_LANTHANIDES);
        e.setDensity(9.85);
        return e;
    }

    public static ElementOld getHafnium() {

        ElementOld e = new ElementOld("Hf", "Hafnium");
        e.setRow(6);
        e.setColumn(4);
        e.setAtomicNumber(72);
        e.setAtomicWeight(178.49);
        e.setMeltingPoint(2423.15);
        e.setBoilingPoint(5673.15);
        // e.setMassNumber(72);
        e.setNumberOfNeutrons(106);
        e.setClassification(ElementOld.CLASSIFICATION.TRANSITIONMETAL);
        e.setDensity(13.2);
        return e;
    }

    public static ElementOld getTantalum() {

        ElementOld e = new ElementOld("Ta", "Tantalum");
        e.setRow(6);
        e.setColumn(5);
        e.setAtomicNumber(73);
        e.setAtomicWeight(180.9479);
        e.setMeltingPoint(3269.15);
        e.setBoilingPoint(5698.15);
        // e.setMassNumber(73);
        e.setNumberOfNeutrons(108);
        e.setClassification(ElementOld.CLASSIFICATION.TRANSITIONMETAL);
        e.setDensity(16.654);
        return e;
    }

    public static ElementOld getTungsten() {

        ElementOld e = new ElementOld("W", "Tungsten");
        e.setRow(6);
        e.setColumn(6);
        e.setAtomicNumber(74);
        e.setAtomicWeight(183.84);
        e.setMeltingPoint(3683.15);
        e.setBoilingPoint(5933.15);
        // e.setMassNumber(74);
        e.setNumberOfNeutrons(110);
        e.setClassification(ElementOld.CLASSIFICATION.TRANSITIONMETAL);
        e.setDensity(19.3);
        return e;
    }

    public static ElementOld getRhenium() {

        ElementOld e = new ElementOld("Re", "Rhenium");
        e.setRow(6);
        e.setColumn(7);
        e.setAtomicNumber(75);
        e.setAtomicWeight(186.207);
        e.setMeltingPoint(3453.15);
        e.setBoilingPoint(5900.15);
        // e.setMassNumber(75);
        e.setNumberOfNeutrons(111);
        e.setClassification(ElementOld.CLASSIFICATION.TRANSITIONMETAL);
        e.setDensity(21.02);
        return e;
    }

    public static ElementOld getOsmium() {

        ElementOld e = new ElementOld("Os", "Osmium");
        e.setRow(6);
        e.setColumn(8);
        e.setAtomicNumber(76);
        e.setAtomicWeight(190.23);
        e.setMeltingPoint(3318.15);
        e.setBoilingPoint(5300.15);
        // e.setMassNumber(76);
        e.setNumberOfNeutrons(114);
        e.setClassification(ElementOld.CLASSIFICATION.TRANSITIONMETAL);
        e.setDensity(22.4);
        return e;
    }

    public static ElementOld getIridium() {

        ElementOld e = new ElementOld("Ir", "Iridium");
        e.setRow(6);
        e.setColumn(9);
        e.setAtomicNumber(77);
        e.setAtomicWeight(192.217);
        e.setMeltingPoint(2683.15);
        e.setBoilingPoint(4800.15);
        // e.setMassNumber(77);
        e.setNumberOfNeutrons(115);
        e.setClassification(ElementOld.CLASSIFICATION.TRANSITIONMETAL);
        e.setDensity(22.5);
        return e;
    }

    public static ElementOld getPlatinum() {

        ElementOld e = new ElementOld("Pt", "Platinum");
        e.setRow(6);
        e.setColumn(10);
        e.setAtomicNumber(78);
        e.setAtomicWeight(195.078);
        e.setMeltingPoint(2045.15);
        e.setBoilingPoint(4100.15);
        // e.setMassNumber(78);
        e.setNumberOfNeutrons(117);
        e.setClassification(ElementOld.CLASSIFICATION.TRANSITIONMETAL);
        e.setDensity(21.45);
        return e;
    }

    public static ElementOld getGold() {

        ElementOld e = new ElementOld("Au", "Gold");
        e.setRow(6);
        e.setColumn(11);
        e.setAtomicNumber(79);
        e.setAtomicWeight(196.96655);
        e.setMeltingPoint(1337.5801);
        e.setBoilingPoint(3080.15);
        // e.setMassNumber(79);
        e.setNumberOfNeutrons(118);
        e.setClassification(ElementOld.CLASSIFICATION.TRANSITIONMETAL);
        e.setDensity(19.32);
        return e;
    }

    public static ElementOld getMercury() {

        ElementOld e = new ElementOld("Hg", "Mercury");
        e.setRow(6);
        e.setColumn(12);
        e.setAtomicNumber(80);
        e.setAtomicWeight(200.59);
        e.setMeltingPoint(234.28);
        e.setBoilingPoint(629.73);
        // e.setMassNumber(80);
        e.setNumberOfNeutrons(121);
        e.setClassification(ElementOld.CLASSIFICATION.TRANSITIONMETAL);
        e.setDensity(13.456);
        return e;
    }

    public static ElementOld getThallium() {

        ElementOld e = new ElementOld("Tl", "Thallium");
        e.setRow(6);
        e.setColumn(13);
        e.setAtomicNumber(81);
        e.setAtomicWeight(204.3833);
        e.setMeltingPoint(576.65);
        e.setBoilingPoint(1730.15);
        // e.setMassNumber(81);
        e.setNumberOfNeutrons(123);
        e.setClassification(ElementOld.CLASSIFICATION.OTHERMETALS);
        e.setDensity(11.85);
        return e;
    }

    public static ElementOld getLead() {

        ElementOld e = new ElementOld("Pb", "Lead");
        e.setRow(6);
        e.setColumn(14);
        e.setAtomicNumber(82);
        e.setAtomicWeight(207.2);
        e.setMeltingPoint(600.65);
        e.setBoilingPoint(2013.15);
        // e.setMassNumber(82);
        e.setNumberOfNeutrons(125);
        e.setClassification(ElementOld.CLASSIFICATION.OTHERMETALS);
        e.setDensity(11.34);
        return e;
    }

    public static ElementOld getBismuth() {

        ElementOld e = new ElementOld("Bi", "Bismuth");
        e.setRow(6);
        e.setColumn(15);
        e.setAtomicNumber(83);
        e.setAtomicWeight(208.98038);
        e.setMeltingPoint(544.45);
        e.setBoilingPoint(1833.15);
        // e.setMassNumber(83);
        e.setNumberOfNeutrons(126);
        e.setClassification(ElementOld.CLASSIFICATION.OTHERMETALS);
        e.setDensity(9.8);
        return e;
    }

    public static ElementOld getPolonium() {

        ElementOld e = new ElementOld("Po", "Polonium");
        e.setRow(6);
        e.setColumn(16);
        e.setAtomicNumber(84);
        e.setAtomicWeight(209.0);
        e.setMeltingPoint(527.15);
        e.setBoilingPoint(1235.15);
        // e.setMassNumber(84);
        e.setNumberOfNeutrons(125);
        e.setClassification(ElementOld.CLASSIFICATION.METALLOID);
        e.setDensity(9.4);
        return e;
    }

    public static ElementOld getAstatine() {

        ElementOld e = new ElementOld("At", "Astatine");
        e.setRow(6);
        e.setColumn(17);
        e.setAtomicNumber(85);
        e.setAtomicWeight(210.0);
        e.setMeltingPoint(575.15);
        e.setBoilingPoint(610.15);
        // e.setMassNumber(85);
        e.setNumberOfNeutrons(125);
        e.setClassification(ElementOld.CLASSIFICATION.HALOGEN);
        e.setDensity(-1);
        return e;
    }

    public static ElementOld getRadon() {

        ElementOld e = new ElementOld("Rn", "Radon");
        e.setRow(6);
        e.setColumn(18);
        e.setAtomicNumber(86);
        e.setAtomicWeight(220.0);
        e.setMeltingPoint(202.15);
        e.setBoilingPoint(211.35);
        // e.setMassNumber(86);
        e.setNumberOfNeutrons(136);
        e.setClassification(ElementOld.CLASSIFICATION.NOBLEGAS);
        e.setDensity(9.73);
        return e;
    }

    public static ElementOld getFrancium() {

        ElementOld e = new ElementOld("Fr", "Francium");
        e.setRow(7);
        e.setColumn(1);
        e.setAtomicNumber(87);
        e.setAtomicWeight(223.0);
        e.setMeltingPoint(300.15);
        e.setBoilingPoint(950.15);
        // e.setMassNumber(87);
        e.setNumberOfNeutrons(136);
        e.setClassification(ElementOld.CLASSIFICATION.ALKALIMETAL);
        e.setDensity(-1);
        return e;
    }

    public static ElementOld getRadium() {

        ElementOld e = new ElementOld("Ra", "Radium");
        e.setRow(7);
        e.setColumn(2);
        e.setAtomicNumber(88);
        e.setAtomicWeight(226.0);
        e.setMeltingPoint(973.15);
        e.setBoilingPoint(2010.15138);
        // e.setMassNumber(88);
        e.setNumberOfNeutrons(138);
        e.setClassification(ElementOld.CLASSIFICATION.ALKALINEEARTH);
        e.setDensity(5.0);
        return e;
    }

    public static ElementOld getActinium() {

        ElementOld e = new ElementOld("Ac", "Actinium");
        e.setRow(7);
        e.setColumn(3);
        e.setAtomicNumber(89);
        e.setAtomicWeight(227.0);
        e.setMeltingPoint(1323.15);
        e.setBoilingPoint(3473.15);
        // e.setMassNumber(89);
        e.setNumberOfNeutrons(138);
        e.setClassification(ElementOld.CLASSIFICATION.RAREEARTH_ACTINIDES);
        e.setDensity(10.07);
        return e;
    }

    public static ElementOld getThorium() {

        ElementOld e = new ElementOld("Th", "Thorium");
        e.setRow(7);
        e.setColumn(3);
        e.setAtomicNumber(90);
        e.setAtomicWeight(232.0381);
        e.setMeltingPoint(2023.15);
        e.setBoilingPoint(5063.15);
        // e.setMassNumber(90);
        e.setNumberOfNeutrons(142);
        e.setClassification(ElementOld.CLASSIFICATION.RAREEARTH_ACTINIDES);
        e.setDensity(11.72);
        return e;
    }

    public static ElementOld getProtactinium() {

        ElementOld e = new ElementOld("Pa", "Protactinium");
        e.setRow(7);
        e.setColumn(3);
        e.setAtomicNumber(91);
        e.setAtomicWeight(231.03587);
        e.setMeltingPoint(1873.15);
        e.setBoilingPoint(-1);
        // e.setMassNumber(91);
        e.setNumberOfNeutrons(140);
        e.setClassification(ElementOld.CLASSIFICATION.RAREEARTH_ACTINIDES);
        e.setDensity(15.4);
        return e;
    }

    public static ElementOld getUranium() {

        ElementOld e = new ElementOld("U", "Uranium");
        e.setRow(7);
        e.setColumn(3);
        e.setAtomicNumber(92);
        e.setAtomicWeight(238.0289);
        e.setMeltingPoint(1405.15);
        e.setBoilingPoint(4091.15);
        // e.setMassNumber(92);
        e.setNumberOfNeutrons(146);
        e.setClassification(ElementOld.CLASSIFICATION.RAREEARTH_ACTINIDES);
        e.setDensity(18.95);
        return e;
    }

    public static ElementOld getNeptunium() {

        ElementOld e = new ElementOld("Np", "Neptunium");
        e.setRow(7);
        e.setColumn(3);
        e.setAtomicNumber(93);
        e.setAtomicWeight(237.0);
        e.setMeltingPoint(913.15);
        e.setBoilingPoint(4175.15);
        // e.setMassNumber(93);
        e.setNumberOfNeutrons(144);
        e.setClassification(ElementOld.CLASSIFICATION.RAREEARTH_ACTINIDES);
        e.setDensity(20.45);
        return e;
    }

    public static ElementOld getPlutonium() {

        ElementOld e = new ElementOld("Pu", "Plutonium");
        e.setRow(7);
        e.setColumn(3);
        e.setAtomicNumber(94);
        e.setAtomicWeight(244.0);
        e.setMeltingPoint(912.65);
        e.setBoilingPoint(3508.15);
        // e.setMassNumber(94);
        e.setNumberOfNeutrons(150);
        e.setClassification(ElementOld.CLASSIFICATION.RAREEARTH_ACTINIDES);
        e.setDensity(19.84);
        return e;
    }

    public static ElementOld getAmericium() {

        ElementOld e = new ElementOld("Am", "Americium");
        e.setRow(7);
        e.setColumn(3);
        e.setAtomicNumber(95);
        e.setAtomicWeight(243.0);
        e.setMeltingPoint(1267.15);
        e.setBoilingPoint(2880.15);
        // e.setMassNumber(95);
        e.setNumberOfNeutrons(148);
        e.setClassification(ElementOld.CLASSIFICATION.RAREEARTH_ACTINIDES);
        e.setDensity(13.6);
        return e;
    }

    public static ElementOld getCurium() {

        ElementOld e = new ElementOld("Cm", "Curium");
        e.setRow(7);
        e.setColumn(3);
        e.setAtomicNumber(96);
        e.setAtomicWeight(247.0);
        e.setMeltingPoint(1613.15);
        e.setBoilingPoint(-1);
        // e.setMassNumber(96);
        e.setNumberOfNeutrons(151);
        e.setClassification(ElementOld.CLASSIFICATION.RAREEARTH_ACTINIDES);
        e.setDensity(13.511);
        return e;
    }

    public static ElementOld getBerkelium() {

        ElementOld e = new ElementOld("Bk", "Berkelium");
        e.setRow(7);
        e.setColumn(3);
        e.setAtomicNumber(97);
        e.setAtomicWeight(247.0);
        e.setMeltingPoint(-1);
        e.setBoilingPoint(-1);
        // e.setMassNumber(97);
        e.setNumberOfNeutrons(150);
        e.setClassification(ElementOld.CLASSIFICATION.RAREEARTH_ACTINIDES);
        e.setDensity(-1);
        return e;
    }

    public static ElementOld getCalifornium() {

        ElementOld e = new ElementOld("Cf", "Californium");
        e.setRow(7);
        e.setColumn(3);
        e.setAtomicNumber(98);
        e.setAtomicWeight(251.0);
        e.setMeltingPoint(-1);
        e.setBoilingPoint(-1);
        // e.setMassNumber(98);
        e.setNumberOfNeutrons(153);
        e.setClassification(ElementOld.CLASSIFICATION.RAREEARTH_ACTINIDES);
        e.setDensity(-1);
        return e;
    }

    public static ElementOld getEinsteinium() {

        ElementOld e = new ElementOld("Es", "Einsteinium");
        e.setRow(7);
        e.setColumn(3);
        e.setAtomicNumber(99);
        e.setAtomicWeight(252.0);
        e.setMeltingPoint(-1);
        e.setBoilingPoint(-1);
        // e.setMassNumber(99);
        e.setNumberOfNeutrons(153);
        e.setClassification(ElementOld.CLASSIFICATION.RAREEARTH_ACTINIDES);
        e.setDensity(-1);
        return e;
    }

    public static ElementOld getFermium() {

        ElementOld e = new ElementOld("Fm", "Fermium");
        e.setRow(7);
        e.setColumn(3);
        e.setAtomicNumber(100);
        e.setAtomicWeight(257.0);
        e.setMeltingPoint(-1);
        e.setBoilingPoint(-1);
        // e.setMassNumber(100);
        e.setNumberOfNeutrons(157);
        e.setClassification(ElementOld.CLASSIFICATION.RAREEARTH_ACTINIDES);
        e.setDensity(-1);
        return e;
    }

    public static ElementOld getMendelevium() {

        ElementOld e = new ElementOld("Md", "Mendelevium");
        e.setRow(7);
        e.setColumn(3);
        e.setAtomicNumber(101);
        e.setAtomicWeight(258.0);
        e.setMeltingPoint(-1);
        e.setBoilingPoint(-1);
        // e.setMassNumber(101);
        e.setNumberOfNeutrons(157);
        e.setClassification(ElementOld.CLASSIFICATION.RAREEARTH_ACTINIDES);
        e.setDensity(-1);
        return e;
    }

    public static ElementOld getNobelium() {

        ElementOld e = new ElementOld("No", "Nobelium");
        e.setRow(7);
        e.setColumn(3);
        e.setAtomicNumber(102);
        e.setAtomicWeight(259.0);
        e.setMeltingPoint(-1);
        e.setBoilingPoint(-1);
        // e.setMassNumber(102);
        e.setNumberOfNeutrons(157);
        e.setClassification(ElementOld.CLASSIFICATION.RAREEARTH_ACTINIDES);
        e.setDensity(-1);
        return e;
    }

    public static ElementOld getLawrencium() {

        ElementOld e = new ElementOld("Lr", "Lawrencium");
        e.setRow(7);
        e.setColumn(3);
        e.setAtomicNumber(103);
        e.setAtomicWeight(262.0);
        e.setMeltingPoint(-1);
        e.setBoilingPoint(-1);
        // e.setMassNumber(103);
        e.setNumberOfNeutrons(159);
        e.setClassification(ElementOld.CLASSIFICATION.RAREEARTH_ACTINIDES);
        e.setDensity(-1);
        return e;
    }

    public static ElementOld getRutherfordium() {

        ElementOld e = new ElementOld("Rf", "Rutherfordium");
        e.setRow(7);
        e.setColumn(4);
        e.setAtomicNumber(104);
        e.setAtomicWeight(261.0);
        e.setMeltingPoint(-1);
        e.setBoilingPoint(-1);
        // e.setMassNumber(104);
        e.setNumberOfNeutrons(157);
        e.setClassification(ElementOld.CLASSIFICATION.TRANSITIONMETAL);
        e.setDensity(-1);
        return e;
    }

    public static ElementOld getDubnium() {

        ElementOld e = new ElementOld("Db", "Dubnium");
        e.setRow(7);
        e.setColumn(5);
        e.setAtomicNumber(105);
        e.setAtomicWeight(262.0);
        e.setMeltingPoint(-1);
        e.setBoilingPoint(-1);
        // e.setMassNumber(105);
        e.setNumberOfNeutrons(157);
        e.setClassification(ElementOld.CLASSIFICATION.TRANSITIONMETAL);
        e.setDensity(-1);
        return e;
    }
    
    public static ElementOld getSeaborgium() {

        ElementOld e = new ElementOld("Sg", "Seaborgium");
        e.setRow(7);
        e.setColumn(6);
        e.setAtomicNumber(106);
        e.setAtomicWeight(263.0);
        e.setMeltingPoint(-1);
        e.setBoilingPoint(-1);
        // e.setMassNumber(106);
        e.setNumberOfNeutrons(157);
        e.setClassification(ElementOld.CLASSIFICATION.TRANSITIONMETAL);
        e.setDensity(-1);
        return e;
    }
    
    public static ElementOld getBohrium() {

        ElementOld e = new ElementOld("Bh", "Bohrium");
        e.setRow(7);
        e.setColumn(7);
        e.setAtomicNumber(107);
        e.setAtomicWeight(262.0);
        e.setMeltingPoint(-1);
        e.setBoilingPoint(-1);
        // e.setMassNumber(107);
        e.setNumberOfNeutrons(155);
        e.setClassification(ElementOld.CLASSIFICATION.TRANSITIONMETAL);
        e.setDensity(-1);
        return e;
    }
    
    public static ElementOld getHassium() {

        ElementOld e = new ElementOld("Hs", "Hassium");
        e.setRow(7);
        e.setColumn(8);
        e.setAtomicNumber(108);
        e.setAtomicWeight(265.0);
        e.setMeltingPoint(-1);
        e.setBoilingPoint(-1);
        // e.setMassNumber(108);
        e.setNumberOfNeutrons(157);
        e.setClassification(ElementOld.CLASSIFICATION.TRANSITIONMETAL);
        e.setDensity(-1);
        return e;
    }
    
    public static ElementOld getMeitnerium() {

        ElementOld e = new ElementOld("Mt", "Meitnerium");
        e.setRow(7);
        e.setColumn(9);
        e.setAtomicNumber(109);
        e.setAtomicWeight(266.0);
        e.setMeltingPoint(-1);
        e.setBoilingPoint(-1);
        // e.setMassNumber(109);
        e.setNumberOfNeutrons(157);
        e.setClassification(ElementOld.CLASSIFICATION.TRANSITIONMETAL);
        e.setDensity(-1);
        return e;
    }
    
    public static ElementOld getUnunnilium() {

        ElementOld e = new ElementOld("Uun", "Ununnilium");
        e.setRow(7);
        e.setColumn(10);
        e.setAtomicNumber(110);
        e.setAtomicWeight(269.0);
        e.setMeltingPoint(-1);
        e.setBoilingPoint(-1);
        // e.setMassNumber(110);
        e.setNumberOfNeutrons(159);
        e.setClassification(ElementOld.CLASSIFICATION.TRANSITIONMETAL);
        e.setDensity(-1);
        return e;
    }
    
    public static ElementOld getUnununium() {

        ElementOld e = new ElementOld("Uuu", "Unununium");
        e.setRow(7);
        e.setColumn(11);
        e.setAtomicNumber(111);
        e.setAtomicWeight(272.0);
        e.setMeltingPoint(-1);
        e.setBoilingPoint(-1);
        // e.setMassNumber(111);
        e.setNumberOfNeutrons(161);
        e.setClassification(ElementOld.CLASSIFICATION.TRANSITIONMETAL);
        e.setDensity(-1);
        return e;
    }
    
    public static ElementOld getUnunbium() {

        ElementOld e = new ElementOld("Uub", "Ununbium");
        e.setRow(7);
        e.setColumn(12);
        e.setAtomicNumber(112);
        e.setAtomicWeight(277.0);
        e.setMeltingPoint(-1);
        e.setBoilingPoint(-1);
        // e.setMassNumber(112);
        e.setNumberOfNeutrons(165);
        e.setClassification(ElementOld.CLASSIFICATION.TRANSITIONMETAL);
        e.setDensity(-1);
        return e;
    }

    static {
        elements.add(getHydrogen());
        elements.add(getHelium());
        
        elements.add(getLithium());
        elements.add(getBeryllium());
        elements.add(getBoron());
        elements.add(getCarbon());
        elements.add(getNitrogen());
        elements.add(getOxygen());
        elements.add(getFluorine());
        elements.add(getNeon());
        
        elements.add(getSodium());
        elements.add(getMagnesium());
        elements.add(getAluminium());
        elements.add(getSilicon());
        elements.add(getPhosphorus());
        elements.add(getSulphur());
        elements.add(getChlorine());
        elements.add(getArgon());
        
        elements.add(getPotassium());
        elements.add(getCalcium());
        elements.add(getScandium());
        elements.add(getTitanium());
        elements.add(getVanadium());
        elements.add(getChromium());
        elements.add(getManganese());
        elements.add(getIron());
        elements.add(getCobalt());
        elements.add(getNickel());
        elements.add(getCopper());
        elements.add(getZinc());
        elements.add(getGallium());
        elements.add(getGermanium());
        elements.add(getArsenic());
        elements.add(getSelenium());
        elements.add(getBromine());
        elements.add(getKrypton());
        
        elements.add(getRubidium());
        elements.add(getStrontium());
        elements.add(getYttrium());
        elements.add(getZirconium());
        elements.add(getNiobium());
        elements.add(getMolybdenum());
        elements.add(getTechnetium());
        elements.add(getRuthenium());
        elements.add(getRhodium());
        elements.add(getPalladium());
        elements.add(getSilver());
        elements.add(getCadmium());
        elements.add(getIndium());
        elements.add(getTin());
        elements.add(getAntimony());
        elements.add(getTellurium());
        elements.add(getIodine());
        elements.add(getXenon()); 
        
        elements.add(getCaesium()); 
        elements.add(getBarium()); 
        elements.add(getLanthanum()); 
        elements.add(getCerium()); 
        elements.add(getPraseodymium());
        elements.add(getNeodymium()); 
        elements.add(getPromethium()); 
        elements.add(getSamarium()); 
        elements.add(getEuropium()); 
        elements.add(getGadolinium()); 
        elements.add(getTerbium()); 
        elements.add(getDysprosium()); 
        elements.add(getHolmium()); 
        elements.add(getErbium()); 
        elements.add(getThulium()); 
        elements.add(getYtterbium()); 
        elements.add(getLutetium()); 
        elements.add(getHafnium()); 
        elements.add(getTantalum()); 
        elements.add(getTungsten()); 
        elements.add(getRhenium()); 
        elements.add(getOsmium()); 
        elements.add(getIridium());
        elements.add(getPlatinum()); 
        elements.add(getGold()); 
        elements.add(getMercury()); 
        elements.add(getThallium()); 
        elements.add(getLead()); 
        elements.add(getBismuth()); 
        elements.add(getPolonium()); 
        elements.add(getAstatine()); 
        elements.add(getRadon()); 
        
        elements.add(getFrancium()); 
        elements.add(getRadium()); 
        elements.add(getActinium()); 
        elements.add(getThorium()); 
        elements.add(getProtactinium()); 
        elements.add(getUranium()); 
        elements.add(getNeptunium()); 
        elements.add(getPlutonium()); 
        elements.add(getAmericium()); 
        elements.add(getCurium()); 
        elements.add(getBerkelium()); 
        elements.add(getCalifornium()); 
        elements.add(getEinsteinium()); 
        elements.add(getFermium()); 
        elements.add(getMendelevium()); 
        elements.add(getNobelium()); 
        elements.add(getLawrencium()); 
        elements.add(getRutherfordium()); 
        elements.add(getDubnium()); 
        elements.add(getSeaborgium()); 
        elements.add(getBohrium()); 
        elements.add(getHassium()); 
        elements.add(getMeitnerium());
        elements.add(getUnunnilium()); 
        elements.add(getUnununium()); 
        elements.add(getUnunbium()); 
    }
}
