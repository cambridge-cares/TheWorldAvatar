/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.chem.structure.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import uk.ac.cam.ceb.como.chem.periodictable.Element;
import uk.ac.cam.ceb.como.chem.structure.Atom;

/**
 *
 * @author pb556
 */
public class ModelUtils {

    /**
     * Convert List of atom objects to an empirical formula
     *
     * @param atoms
     * @return empirical formula
     */
    public static String convertToEmpiricalFormula(List<Atom> atoms) {
        List<Atom> atomList = new ArrayList<Atom>(atoms);
        Collections.sort(atomList, new ElementComparer());
        String formula = "";
        for (Atom atom : atomList) {
            formula += atom.getElement().getSymbol() + ((atom.getCount() > 1) ? atom.getCount() + "" : "");
        }
        return formula;
    }

    private static class ElementComparer implements Comparator {

        @Override
        public int compare(Object obj1, Object obj2) {
            return ((Atom) obj1).getElement().getSymbol().compareTo(((Atom) obj2).getElement().getSymbol());
        }
    }

    public static Collection<Element> getElementList(Collection<Atom> atoms) {
        List<Element> elements = new ArrayList<Element>();
        for (Iterator<Atom> it = atoms.iterator(); it.hasNext();) {
            Atom atom = it.next();
            elements.add(atom.getElement());
        }
        return elements;
    }

    public static Map<Element, Integer> getImplodedElementCountMapByElement(List<Atom> atoms) {
        Map<Element, Integer> elementCountMap = new HashMap<Element, Integer>();
        for (Atom atom : atoms) {
            Element e = atom.getElement();
            Integer count = elementCountMap.get(e);
            elementCountMap.put(e, (count != null ? count : 0) + atom.getCount());
        }
        return elementCountMap;
    }
}
