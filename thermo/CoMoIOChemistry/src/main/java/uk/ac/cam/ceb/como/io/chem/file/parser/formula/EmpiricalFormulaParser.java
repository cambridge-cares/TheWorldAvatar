package uk.ac.cam.ceb.como.io.chem.file.parser.formula;

import java.io.IOException;
import java.io.StringReader;
import nu.xom.Attribute;
import nu.xom.Builder;
import nu.xom.Document;
import nu.xom.Element;
import nu.xom.Elements;
import nu.xom.ParsingException;
import nu.xom.ValidityException;
import org.apache.log4j.Logger;

/**
 *
 * @author pb556
 */
public class EmpiricalFormulaParser {

    private Logger logger = Logger.getLogger(getClass());

    private String replaceElementByXMLAtom(String formula) {
        return formula.replaceAll("([A-Z][a-z]{0,3}+)", "<atom element=\"$1\"/>");
    }

    private String replaceStoichiometryByXMLCount(String formula) {
        //return formula.replaceAll("\\*", "<asterisk/>").replaceAll("(\\d+)", "<stoichiometry count=\"$1\"/>");
        return formula.replaceAll("(\\d+)", "<stoichiometry count=\"$1\"/>");
    }

    private String replaceBracketByXMLGrouping(String formula) {
        return formula.replaceAll("\\(", "<formula>").replaceAll("\\)", "</formula>");
    }

    public String parse(String formula) {
        String emp_form_xml = replaceElementByXMLAtom(formula);
        emp_form_xml = replaceStoichiometryByXMLCount(emp_form_xml);
        emp_form_xml = replaceBracketByXMLGrouping(emp_form_xml);
        emp_form_xml = "<formula>" + emp_form_xml + "</formula>";
        StringReader str_reader = new StringReader(emp_form_xml);
        Document doc = null;
        Builder parser = new Builder();
        try {
            doc = parser.build(str_reader);
            Element form_elem = doc.getRootElement();
            System.out.println(form_elem.toXML());
            tidyStoichiometry(form_elem);
            System.out.println(form_elem.toXML());
            expandFormula(form_elem);
            System.out.println(form_elem.toXML());
            System.out.println(getEmpiricalFormula(form_elem));
        } catch (ValidityException ex) {
            logger.error("Cannot create a valid XML from given formula : " + formula, ex);
        } catch (ParsingException ex) {
            logger.error("Cannot parse a valid XML from given formula : " + formula, ex);
        } catch (IOException ex) {
            logger.error("IO exception for formula : " + formula, ex);
        }
        return formula;
    }

    private void tidyStoichiometry(Element elem) {
        // if formula list inside
        if (elem.getLocalName().equals("formula")) {
            // list elements
            Elements elems = elem.getChildElements();
            Element prev_elem = null;
            for (int i = 0; i < elems.size(); i++) {
                Element this_elem = elems.get(i);
                if (this_elem.getLocalName().equals("atom")) {
                    if (this_elem.getAttribute("count") == null) {
                        this_elem.addAttribute(new Attribute("count", "1"));
                    }
                } else if (this_elem.getLocalName().equals("stoichiometry")) {
                    if (prev_elem != null) {
                        int stoi = Integer.parseInt(this_elem.getAttributeValue("count"));
                        String count = prev_elem.getAttributeValue("count");
                        int n = (count == null) ? stoi : (stoi * Integer.parseInt(count));
                        prev_elem.addAttribute(new Attribute("count", Integer.toString(n)));
                    } else {
                        logger.warn("No previous element before stoichiometry.");
                    }
                    this_elem.setLocalName("deleted");
                    this_elem.removeAttribute(this_elem.getAttribute("count"));
                } else if (this_elem.getLocalName().equals("formula")) {
                    tidyStoichiometry(this_elem);
                }
                prev_elem = this_elem;
            }
        }
    }

    private void expandFormula(Element elem) {
        // if formula list inside
        if (elem.getLocalName().equals("formula")) {
            Attribute count_att = elem.getAttribute("count");
            int n_form = (count_att == null) ? 1 : Integer.parseInt(count_att.getValue());
            // list elements
            Elements elems = elem.getChildElements();
            for (int i = 0; i < elems.size(); i++) {
                Element this_elem = elems.get(i);
                String n_count = this_elem.getAttributeValue("count");
                int n_ncount = (n_count == null) ? n_form : n_form * Integer.parseInt(n_count);
                this_elem.addAttribute(new Attribute("count", Integer.toString(n_ncount)));
                if (this_elem.getLocalName().equals("atom")) {
                } else if (this_elem.getLocalName().equals("formula")) {
                    expandFormula(this_elem);
                } else {
                    this_elem.removeAttribute(this_elem.getAttribute("count"));
                }
            }
            if (count_att != null) {
                elem.removeAttribute(count_att);
            }
        }
    }

    private EmpiricalFormula getEmpiricalFormula(Element elem) {
        EmpiricalFormula emp = new EmpiricalFormula();
        if (elem.getLocalName().equals("formula")) {
            Elements elems = elem.getChildElements();
            for (int i = 0; i < elems.size(); i++) {
                Element this_elem = elems.get(i);
                if (this_elem.getLocalName().equals("atom")) {
                    int atom_count = Integer.parseInt(this_elem.getAttributeValue("count"));
                    String symbol = this_elem.getAttributeValue("element");
                    emp.add(symbol, atom_count);
                } else if (this_elem.getLocalName().equals("formula")) {
                    emp.add(getEmpiricalFormula(this_elem));
                }
            }
        }
        return emp;
    }
}
