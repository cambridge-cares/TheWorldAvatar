package uk.ac.cam.ceb.como.io.chem.file.parser.formula;

import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;

import nu.xom.Attribute;
import nu.xom.Builder;
import nu.xom.Document;
import nu.xom.Element;
import nu.xom.Elements;
import nu.xom.ParsingException;
import nu.xom.ValidityException;
import org.apache.log4j.Logger;

import uk.ac.cam.ceb.como.io.chem.file.jaxb.Atom;
import uk.ac.cam.ceb.como.io.chem.file.jaxb.AtomArray;
import uk.ac.cam.ceb.como.io.chem.file.jaxb.Formula;

import uk.ac.cam.ceb.como.io.chem.file.jaxb.Module;
import uk.ac.cam.ceb.como.io.chem.file.jaxb.Molecule;

/**
 *
 * @author pb556
 */

public class EmpiricalFormulaParser {

	Module module = new Module();
	Molecule molecule = new Molecule();
	Formula formula = new Formula();
	AtomArray atomArray = new AtomArray();

	List<Atom> listOfAtoms = new ArrayList<Atom>();

	int sum_atom = 0;	
	
	int atomSum = 0;

	String atomName ="";

	String tiSpeciesAtoms  =null;
	
	private Logger logger = Logger.getLogger(getClass());

	private String replaceElementByXMLAtom(String formula) {
		System.out.println("formula.replaceAll(\"([A-Z][a-z]{0,3}+)\", \"<atom element=\\\"$1\\\"/>\")");
		return formula.replaceAll("([A-Z][a-z]{0,3}+)", "<atom element=\"$1\"/>");
	}

	private String replaceStoichiometryByXMLCount(String formula) {
		// return formula.replaceAll("\\*", "<asterisk/>").replaceAll("(\\d+)",
		// "<stoichiometry count=\"$1\"/>");
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

	/**
	 * @author nk510
	 * @param formula
	 * @return This method is light modification of ' public String parse(String
	 *         formula)' and it calculates the number of all atoms for given
	 *         formula.
	 */
	public int getNumberOfAllAtoms(String formula) {
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
		return sum_atom;
	}
	
	/**
	 * @author nk510
	 * @param formula
	 * @return This method is light modification of 'public String parse(String
	 *         formula)' method. It calculates the number of Ti species atoms for given
	 *         formula.
	 */
	public String getTiAtomsCount(String formula) {
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
		return tiSpeciesAtoms;
	}
	

	/**
	 * @author nk510
	 * @param formula
	 * @return This method parses formula given as a string and populates data
	 *         (formula name, atoms, and number of each atom) in Molecule JAXB
	 *         object variable.
	 */

	public Molecule parseModule(String formula) {
		String emp_form_xml = replaceElementByXMLAtom(formula);
		emp_form_xml = replaceStoichiometryByXMLCount(emp_form_xml);
		emp_form_xml = replaceBracketByXMLGrouping(emp_form_xml);
		emp_form_xml = "<formula>" + emp_form_xml + "</formula>";
		System.out.println("emp_form_xml: " + emp_form_xml);
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

		return molecule;
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

	/**
	 * @author nk510
	 * @param elem
	 * @return This method gets object variable that is instance of EmpiricalFormula.  
	 */
	private EmpiricalFormula getEmpiricalFormula(Element elem) {

		String final_formula = "";

		EmpiricalFormula emp = new EmpiricalFormula();
		if (elem.getLocalName().equals("formula")) {
			Elements elems = elem.getChildElements();
			for (int i = 0; i < elems.size(); i++) {
				Element this_elem = elems.get(i);
				Atom atom = new Atom();
				if (this_elem.getLocalName().equals("atom")) {
					int atom_count = Integer.parseInt(this_elem.getAttributeValue("count"));
					String symbol = this_elem.getAttributeValue("element");

					/**
					 * @author nk510 Added information about formula in Molecule JAXB object
					 *         variable based on information given Element object variable.
					 * 
					 *         Comment written by Dr Daniel Nurkowski (dln22): Please be careful
					 *         here as sometimes you may find the following content: 'Stoichiometry
					 *         A(nr)' where: - 'A' is a single atom label (e.g. Cl,C,Si, ...) or a
					 *         set of atoms (e.g. Cl2O6) and - 'nr' is an integer number (2,3,..
					 *         etc). The 'nr' inside parentheses is not part of the stoichiometry
					 *         and should be neglected. You can see it in the case of a single
					 *         chlorine atom Gaussian log file where the stoichiometry line contains
					 *         'Cl(2)' although there is only one Cl atom.
					 * 
					 *         String object 'final_formula' is concatenation of all data about atom
					 *         ('atom' and 'atom count'). It was necessary to convert int atom_count
					 *         into double because {@author pb556} defined this variable as int. In
					 *         CompChem XML Schema (http://www.xml-cml.org/schema/schema3/) it was
					 *         required for this variable to be defined as double.
					 */

					final_formula = final_formula + symbol + " " + atom_count + " ";

					double atom_count_double = atom_count;

					sum_atom = sum_atom + atom_count;

					atom.setCount(atom_count_double);

					atom.setElementType(symbol);
					
					atomArray.getAtom().add(atom);
					
					emp.add(symbol, atom_count);
					
					if(symbol.equals("Ti")) {tiSpeciesAtoms = atom_count+"";}
					
					atomName=symbol;
					atomSum = atom_count;

				} else if (this_elem.getLocalName().equals("formula")) {
					
					emp.add(getEmpiricalFormula(this_elem));
				}
			}

			formula.setConcise(final_formula);

			molecule.getAngleOrArgOrArray().add(atomArray);

			molecule.getAngleOrArgOrArray().add(formula);
		}

		return emp;
	}
	
	/**
	 * @author nk510
	 * @param formula
	 * @return This method is light modification of ' public String parse(String
	 *         formula)' and it returns atom's name for given
	 *         formula. 
	 */
	public String getAtomName (String formula) {
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
			getEmpiricalFormula(form_elem);
		} catch (ValidityException ex) {
			logger.error("Cannot create a valid XML from given formula : " + formula, ex);
		} catch (ParsingException ex) {
			logger.error("Cannot parse a valid XML from given formula : " + formula, ex);
		} catch (IOException ex) {
			logger.error("IO exception for formula : " + formula, ex);
		}
		return atomName;
	}	
	
	/**
	 * @author nk510
	 * @param formula
	 * @return This method is light modification of ' public String parse(String
	 *         formula)' and it returns atom's number for given
	 *         formula (propositional symbol in every clause). 
	 */
	public int getAtomSum (String formula) {
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
			getEmpiricalFormula(form_elem);
		} catch (ValidityException ex) {
			logger.error("Cannot create a valid XML from given formula : " + formula, ex);
		} catch (ParsingException ex) {
			logger.error("Cannot parse a valid XML from given formula : " + formula, ex);
		} catch (IOException ex) {
			logger.error("IO exception for formula : " + formula, ex);
		}
		return atomSum;
	}	
	
}