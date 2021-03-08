/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.cam.ceb.como.tools.periodictable;

import java.util.ArrayList;
import java.util.Collection;
import org.apache.log4j.Logger;

/**
 *
 * @author pb556
 */
public class PeriodicTable {

    private final static ArrayList<Element> elements = new ArrayList<Element>();
    private static Logger logger = Logger.getLogger(PeriodicTable.class);

    public static int getNumberOfLoneElectrons(Element e, int numOfBonds) {
        return getNumberOfValenceElectrons(e) - numOfBonds;
    }
    
    public static int getNumberOfLonePairs(Element e, int numOfBonds) {
        return getNumberOfLoneElectrons(e, numOfBonds) / 2;
    }

    public static int getNumberOfValenceElectrons(Element e) {
        if (e.getGroup() < 3) {
            return e.getGroup();
        }
        if (e.getSymbol().equals("He")) {
            return 2;
        }
        if (e.getGroup() > 12) {
            return e.getGroup() - 10;
        }
        if (e.getGroup() >= 3 && e.getGroup() <= 12) {
            // sum up s + d for transition metals
            OrbitalConfiguration s = e.getElectronConfiguration().getLastOrbital(Orbital.s);
            OrbitalConfiguration d = e.getElectronConfiguration().getLastOrbital(Orbital.d);
            int valEl = 0;
            if (s != null) {
                valEl += s.getNumberOfElectrons();
            }
            if (d != null) {
                valEl += d.getNumberOfElectrons();
            }
            return valEl;
        }
        if (e.getGroup() == 0) {
            OrbitalConfiguration f = e.getElectronConfiguration().getLastOrbital(Orbital.f);
            if (f != null) {
                return f.getNumberOfElectrons();
            }
        }
        return -1;
    }

    public static int getNumberOfPossibleBonds(Element e) {
        OrbitalConfiguration o = e.getElectronConfiguration().getLastOrbital();
        return o.getOrbital().getMaxNumberOfElectrons() - o.getNumberOfElectrons();
    }

    public static Collection<Element> getElements() {
        return elements;
    }

    public static Element getElementByName(String name) {
        for (int i = 0; i < elements.size(); i++) {
            if (elements.get(i).getName().equalsIgnoreCase(name.trim())) {
                return elements.get(i);
            }
        }
        logger.error("The element with the name '" + name + "' does not exist.");
        return new Element();
    }

    public static Element getElementBySymbol(String symbol) {
        for (int i = 0; i < elements.size(); i++) {
            if (elements.get(i).getSymbol().equalsIgnoreCase(symbol.trim())) {
                return elements.get(i);
            }
        }
        logger.error("The element with the symbol '" + symbol + "' does not exist.");
        return new Element();
    }

    public static Element getElementByAtomicNumber(int atomicNumber) {
        for (int i = 0; i < elements.size(); i++) {
            if (elements.get(i).getAtomicNumber() == atomicNumber) {
                return elements.get(i);
            }
        }
        logger.error("The element with the atomic number  " + atomicNumber + " does not exist.");
        return new Element();
    }

    public static Collection<Element> getElementByClassification(ElementCategory cat) {
        ArrayList<Element> selectedElements = new ArrayList<Element>();
        for (int i = 0; i < elements.size(); i++) {
            if (elements.get(i).getElementCategory() == cat) {
                selectedElements.add(elements.get(i));
            }
        }
        return selectedElements;
    }

    public static Element getElementByPosition(int period, int group) {
        for (int i = 0; i < elements.size(); i++) {
            if (elements.get(i).getPeriod() == period && elements.get(i).getGroup() == group) {
                return elements.get(i);
            }
        }
        logger.error("The position (" + period + ", " + group + ") does not define a valid element.");
        return new Element();
    }

    public static Collection<Element> getElementByBlock(Block block) {
        ArrayList<Element> selectedElements = new ArrayList<Element>();
        for (int i = 0; i < elements.size(); i++) {
            if (elements.get(i).getBlock() == block) {
                selectedElements.add(elements.get(i));
            }
        }
        return selectedElements;
    }

    public static Collection<Element> getElementByPeriod(int period, Block block) {
        Collection<Element> blockElements = getElementByBlock(block);
        ArrayList<Element> selectedElements = new ArrayList<Element>();
        for (Element e : blockElements) {
            if (e.getPeriod() == period) {
                selectedElements.add(e);
            }
        }
        return selectedElements;
    }

    public static Collection<Element> getElementByGroup(int group, Block block) {
        Collection<Element> blockElements = getElementByBlock(block);
        ArrayList<Element> selectedElements = new ArrayList<Element>();
        for (Element e : blockElements) {
            if (e.getGroup() == group) {
                selectedElements.add(e);
            }
        }
        return selectedElements;
    }

    public static Collection<Element> getElementByPeriod(int period) {
        ArrayList<Element> selectedElements = new ArrayList<Element>();
        for (int i = 0; i < elements.size(); i++) {
            if (elements.get(i).getPeriod() == period) {
                selectedElements.add(elements.get(i));
            }
        }
        return selectedElements;
    }

    public static Collection<Element> getElementByGroup(int group) {
        ArrayList<Element> selectedElements = new ArrayList<Element>();
        for (int i = 0; i < elements.size(); i++) {
            if (elements.get(i).getGroup() == group) {
                selectedElements.add(elements.get(i));
            }
        }
        return selectedElements;
    }

    public static Collection<Element> getElementByAtomicNumberRange(int atomicNumberStart, int atomicNumberEnd) {
        if (atomicNumberStart > atomicNumberEnd) {
            int x = atomicNumberStart;
            atomicNumberStart = atomicNumberEnd;
            atomicNumberEnd = x;
            logger.warn("Invalid indices for atomicNumberStart and atomicNumberEnd were set and had to be switched.");
        }
        ArrayList<Element> selectedElements = new ArrayList<Element>();
        for (int i = 0; i < elements.size(); i++) {
            if (elements.get(i).atomicNumber >= atomicNumberStart && elements.get(i).atomicNumber <= atomicNumberEnd) {
                selectedElements.add(elements.get(i));
            }
        }
        return selectedElements;
    }

    public static Collection<Element> getElementByPositionRange(int periodStart, int periodEnd, int groupStart, int groupEnd) {
        if (periodStart > periodEnd) {
            int x = periodStart;
            periodStart = periodEnd;
            periodEnd = x;
            logger.warn("Invalid indices for rowStart and rowEnd were set and had to be switched.");
        }
        if (groupStart > groupEnd) {
            int x = groupStart;
            groupStart = groupEnd;
            groupEnd = x;
            logger.warn("Invalid indices for colStart and colEnd were set and had to be switched.");
        }
        ArrayList<Element> selectedElements = new ArrayList<Element>();
        for (int i = 0; i < elements.size(); i++) {
            if (elements.get(i).getPeriod() >= periodStart && elements.get(i).getPeriod() <= periodEnd
                    && elements.get(i).getGroup() >= groupStart && elements.get(i).getGroup() <= groupEnd) {
                selectedElements.add(elements.get(i));
            }
        }
        return selectedElements;
    }

    static {
        elements.add(PeriodicTableElements.getHydrogen());
        elements.add(PeriodicTableElements.getHelium());

        elements.add(PeriodicTableElements.getLithium());
        elements.add(PeriodicTableElements.getBeryllium());
        elements.add(PeriodicTableElements.getBoron());
        elements.add(PeriodicTableElements.getCarbon());
        elements.add(PeriodicTableElements.getNitrogen());
        elements.add(PeriodicTableElements.getOxygen());
        elements.add(PeriodicTableElements.getFluorine());
        elements.add(PeriodicTableElements.getNeon());

        elements.add(PeriodicTableElements.getSodium());
        elements.add(PeriodicTableElements.getMagnesium());
        elements.add(PeriodicTableElements.getAluminium());
        elements.add(PeriodicTableElements.getSilicon());
        elements.add(PeriodicTableElements.getPhosphorus());
        elements.add(PeriodicTableElements.getSulphur());
        elements.add(PeriodicTableElements.getChlorine());
        elements.add(PeriodicTableElements.getArgon());

        elements.add(PeriodicTableElements.getPotassium());
        elements.add(PeriodicTableElements.getCalcium());
        elements.add(PeriodicTableElements.getScandium());
        elements.add(PeriodicTableElements.getTitanium());
        elements.add(PeriodicTableElements.getVanadium());
        elements.add(PeriodicTableElements.getChromium());
        elements.add(PeriodicTableElements.getManganese());
        elements.add(PeriodicTableElements.getIron());
        elements.add(PeriodicTableElements.getCobalt());
        elements.add(PeriodicTableElements.getNickel());
        elements.add(PeriodicTableElements.getCopper());
        elements.add(PeriodicTableElements.getZinc());
        elements.add(PeriodicTableElements.getGallium());
        elements.add(PeriodicTableElements.getGermanium());
        elements.add(PeriodicTableElements.getArsenic());
        elements.add(PeriodicTableElements.getSelenium());
        elements.add(PeriodicTableElements.getBromine());
        elements.add(PeriodicTableElements.getKrypton());

        elements.add(PeriodicTableElements.getRubidium());
        elements.add(PeriodicTableElements.getStrontium());
        elements.add(PeriodicTableElements.getYttrium());
        elements.add(PeriodicTableElements.getZirconium());
        elements.add(PeriodicTableElements.getNiobium());
        elements.add(PeriodicTableElements.getMolybdenum());
        elements.add(PeriodicTableElements.getTechnetium());
        elements.add(PeriodicTableElements.getRuthenium());
        elements.add(PeriodicTableElements.getRhodium());
        elements.add(PeriodicTableElements.getPalladium());
        elements.add(PeriodicTableElements.getSilver());
        elements.add(PeriodicTableElements.getCadmium());
        elements.add(PeriodicTableElements.getIndium());
        elements.add(PeriodicTableElements.getTin());
        elements.add(PeriodicTableElements.getAntimony());
        elements.add(PeriodicTableElements.getTellurium());
        elements.add(PeriodicTableElements.getIodine());
        elements.add(PeriodicTableElements.getXenon());

        elements.add(PeriodicTableElements.getCaesium());
        elements.add(PeriodicTableElements.getBarium());
        elements.add(PeriodicTableElements.getLanthanum());
        elements.add(PeriodicTableElements.getCerium());
        elements.add(PeriodicTableElements.getPraseodymium());
        elements.add(PeriodicTableElements.getNeodymium());
        elements.add(PeriodicTableElements.getPromethium());
        elements.add(PeriodicTableElements.getSamarium());
        elements.add(PeriodicTableElements.getEuropium());
        elements.add(PeriodicTableElements.getGadolinium());
        elements.add(PeriodicTableElements.getTerbium());
        elements.add(PeriodicTableElements.getDysprosium());
        elements.add(PeriodicTableElements.getHolmium());
        elements.add(PeriodicTableElements.getErbium());
        elements.add(PeriodicTableElements.getThulium());
        elements.add(PeriodicTableElements.getYtterbium());
        elements.add(PeriodicTableElements.getLutetium());
        elements.add(PeriodicTableElements.getHafnium());
        elements.add(PeriodicTableElements.getTantalum());
        elements.add(PeriodicTableElements.getTungsten());
        elements.add(PeriodicTableElements.getRhenium());
        elements.add(PeriodicTableElements.getOsmium());
        elements.add(PeriodicTableElements.getIridium());
        elements.add(PeriodicTableElements.getPlatinum());
        elements.add(PeriodicTableElements.getGold());
        elements.add(PeriodicTableElements.getMercury());
        elements.add(PeriodicTableElements.getThallium());
        elements.add(PeriodicTableElements.getLead());
        elements.add(PeriodicTableElements.getBismuth());
        elements.add(PeriodicTableElements.getPolonium());
        elements.add(PeriodicTableElements.getAstatine());
        elements.add(PeriodicTableElements.getRadon());

        elements.add(PeriodicTableElements.getFrancium());
        elements.add(PeriodicTableElements.getRadium());
        elements.add(PeriodicTableElements.getActinium());
        elements.add(PeriodicTableElements.getThorium());
        elements.add(PeriodicTableElements.getProtactinium());
        elements.add(PeriodicTableElements.getUranium());
        elements.add(PeriodicTableElements.getNeptunium());
        elements.add(PeriodicTableElements.getPlutonium());
        elements.add(PeriodicTableElements.getAmericium());
        elements.add(PeriodicTableElements.getCurium());
        elements.add(PeriodicTableElements.getBerkelium());
        elements.add(PeriodicTableElements.getCalifornium());
        elements.add(PeriodicTableElements.getEinsteinium());
        elements.add(PeriodicTableElements.getFermium());
        elements.add(PeriodicTableElements.getMendelevium());
        elements.add(PeriodicTableElements.getNobelium());
        elements.add(PeriodicTableElements.getLawrencium());
        elements.add(PeriodicTableElements.getRutherfordium());
        elements.add(PeriodicTableElements.getDubnium());
        elements.add(PeriodicTableElements.getSeaborgium());
        elements.add(PeriodicTableElements.getBohrium());
        elements.add(PeriodicTableElements.getHassium());
        elements.add(PeriodicTableElements.getMeitnerium());
        elements.add(PeriodicTableElements.getDarmstadtium());
        elements.add(PeriodicTableElements.getRoentgenium());
        elements.add(PeriodicTableElements.getCopernicium());
        elements.add(PeriodicTableElements.getUnuntrium());
        elements.add(PeriodicTableElements.getFlerovium());
        elements.add(PeriodicTableElements.getUnunpentium());
        elements.add(PeriodicTableElements.getLivermorium());
        elements.add(PeriodicTableElements.getUnunseptium());
        elements.add(PeriodicTableElements.getUnunoctium());
    }
}
