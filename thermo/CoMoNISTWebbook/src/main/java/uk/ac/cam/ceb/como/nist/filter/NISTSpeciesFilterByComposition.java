/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package uk.ac.cam.ceb.como.nist.filter;

import uk.ac.cam.ceb.como.nist.parser.NISTSpeciesList;
import uk.ac.cam.ceb.como.nist.info.NISTSpeciesId;
import java.util.HashMap;
import java.util.Map;
import org.apache.log4j.Logger;
import uk.ac.cam.ceb.como.tools.util.StringUtil;

/**
 *
 * @author pb556
 */
public class NISTSpeciesFilterByComposition {

    private Map<String, Integer> validElements = null;
    private NISTSpeciesList list = null;
    private NISTSpeciesList filteredList = null;
    private boolean complete = false;
    private static Logger logger = Logger.getLogger(NISTSpeciesFilterByComposition.class);

    public NISTSpeciesFilterByComposition(Map<String, Integer> validElements, NISTSpeciesList list, boolean complete, boolean others) {
        if (validElements == null) {
            logger.error("No set of valid elements was defined.");
            return;
        }
        if (list == null) {
            logger.error("No species list was defined.");
            return;
        }
        this.validElements = validElements;
        this.list = list;
        this.complete = complete;
    }

    public Map<String, Integer> getValidElements() {
        return validElements;
    }

    public NISTSpeciesList getList() {
        return list;
    }

    public NISTSpeciesList getFilteredList() {
        return filteredList;
    }

    public void filter() {
        // filter species
        filteredList = new NISTSpeciesList();
        NISTSpeciesId id;
        while ((id = list.next()) != null) {
            boolean validOr = false;
            boolean validAnd = true;
            boolean containsOthers = false;
            Map<String, Integer> elements = filterElements(id.getFormula());
            for (String element : elements.keySet()) {
                if (validElements.containsKey(element) && 
                        validElements.get(element) >= elements.get(element)) {
                    validOr = true;
                } else {
                    validAnd = false;
                    containsOthers = true;
                }
            }
            if ((complete && validAnd)
                    || (!complete && validOr && !containsOthers)) {
                filteredList.add(id);
            }
        }
    }

    private Map<String, Integer> filterElements(String formula) {
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
