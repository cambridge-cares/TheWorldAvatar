/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.nist.info;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import uk.ac.cam.ceb.como.tools.util.StringUtil;

/**
 *
 * @author pb556
 */
public class ChemFormula {
    // name
    // composition

    private String formula = "";

    public ChemFormula(String formula) {
        this.formula = formula;
    }
    
    public String getFormula() {
        return formula;
    }
    
    public Collection<String> getElements() {
        return getComposition().keySet();
    }
    
    public boolean has(String element) {
        Map<String, Integer> comp = getComposition();
        return comp.containsKey(element);
    }
    
    public int getStoichiometry(String element) {
        Map<String, Integer> comp = getComposition();
        if (comp.containsKey(element)) {
            return comp.get(element);
        }
        return 0;
    }
    
    public Map<String, Integer> getComposition() {
        if (formula == null || formula.length() == 0) {
            return new HashMap<String, Integer>();
        }
        HashMap<String, Integer> elements = new HashMap<String, Integer>();
        String stoichiometry = "";
        String element = "";
        element += formula.charAt(0);
        for (int i = 1; i < formula.length(); i++) {
            if (StringUtil.isUpperCase(formula.charAt(i))) {
                if (stoichiometry.length() > 0) {
                    elements.put(element, Integer.parseInt(stoichiometry));
                } else {
                    elements.put(element, 1);
                }
                stoichiometry = "";
                element = "" + formula.charAt(i);
            } else if (StringUtil.isNumber(formula.charAt(i))) {
                stoichiometry += formula.charAt(i);
            } else if (StringUtil.isLowerCase(formula.charAt(i))) {
                element += formula.charAt(i);
            }
        }
        if (!element.isEmpty()) {
            if (stoichiometry.length() > 0) {
                elements.put(element, Integer.parseInt(stoichiometry));
            } else {
                elements.put(element, 1);
            }
        }
        return elements;
    }
}
