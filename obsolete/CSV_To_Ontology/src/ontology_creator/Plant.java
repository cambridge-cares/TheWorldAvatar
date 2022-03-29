package ontology_creator;

import java.util.Scanner;
import java.util.Set;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.Reader;
import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Scanner;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.xpath.XPath;//Don't use XPath for RDF/XML
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathExpressionException;
import javax.xml.xpath.XPathFactory;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import com.hp.hpl.jena.iri.IRI;
import com.hp.hpl.jena.rdf.model.Model;
import com.hp.hpl.jena.rdf.model.ModelFactory;
import com.hp.hpl.jena.util.FileManager;
import com.hp.hpl.jena.util.FileUtils;

import edu.stanford.smi.protege.exception.OntologyLoadException;
import edu.stanford.smi.protegex.owl.ProtegeOWL;
import edu.stanford.smi.protegex.owl.jena.JenaOWLModel;
import edu.stanford.smi.protegex.owl.model.OWLDatatypeProperty;
import edu.stanford.smi.protegex.owl.model.OWLIndividual;
import edu.stanford.smi.protegex.owl.model.OWLModel;
import edu.stanford.smi.protegex.owl.model.OWLNamedClass;
import edu.stanford.smi.protegex.owl.model.OWLObjectProperty;
import edu.stanford.smi.protegex.owl.model.RDFIndividual;

public class Plant {
	
	private String plantName;
	private String chemSpecFile;
	private List<List<String>> table;
	private Map<String,String> utilityMap;
	private Map<String,String> specialStreams;
	private int utilStartNum;
	private Set<String> unitOperations;
	
	Plant(String plantName, String aswfile, String heatXfile, String chemSpecFile, String specialStreamsFile, int utilStartNum) {
		this.plantName = plantName;
		this.table = Tools.createTableFromCSV(aswfile);
		this.chemSpecFile = chemSpecFile;
		this.utilityMap = Tools.csvToMap(heatXfile);
		this.specialStreams = Tools.csvToMap(specialStreamsFile);
		this.utilStartNum = utilStartNum;
		this.unitOperations = new HashSet<String>();
	}
	
	public void createMixers() {
		/**
		 * Precondition:
		 * Reads the instance variable "table".
		 * The table must contain (at least) 3 columns in this order: Value, Variable Name, Container.
		 * Table must have a header stating "Mixers".
		 * Extra columns to the right will be ignored.
		 * 
		 * Postcondition:
		 * Generates a new OWL file from a template.
		 * 
		 */
		boolean isFound = false;
		Map<String,Mixer> mixers_Map = new HashMap<String,Mixer>();
		Mixer mixer;

		for (int r = 0; r < table.size(); r++) {
			if (table.get(r).contains("Mixers")) {
				isFound = true;
				System.out.println("Mixers found.");
				r += 2;
				try {
					while (table.get(r).get(2).contains("MIX")) {
						System.out.println("Current row: " + r);
						String newinstance = table.get(r).get(2);
						System.out.println(newinstance);
						if (!mixers_Map.containsKey(newinstance)) {
							mixer = new Mixer(newinstance);
							mixers_Map.put(newinstance, mixer);
						}
						else {
							mixer = mixers_Map.get(newinstance);
						}

						if (table.get(r).get(1).equals("Product Stream")) {
							mixer.setProdStream(table.get(r).get(0));
						}

						else if (table.get(r).get(1).equals("Feed Stream")) {
							mixer.addFeedStream(table.get(r).get(0));
						}
						r++;
					} // End while
				} catch (IndexOutOfBoundsException e) {}
			} // end if ==mixers
		} // end for loop
		
		for (Mixer mix : mixers_Map.values()) {
			mix.createOWLFile();
			unitOperations.add(mix.getName());
		}
		
		if (!isFound) {
			System.out.println("Mixers not found.");
		}
	}

	public void createTees() {
		/**
		 * Precondition:
		 * Reads the instance variable "table".
		 * The table must contain (at least) 3 columns in this order: Value, Variable Name, Container.
		 * Table must have a header stating "Tees".
		 * Extra columns to the right will be ignored.
		 * 
		 * Postcondition:
		 * Generates a new OWL file from a template.
		 * 
		 */
		boolean isFound = false;
		Map<String,Tee> tees_Map = new HashMap<String,Tee>();
		Tee tee;

		for (int r = 0; r < table.size(); r++) {
			if (table.get(r).contains("Tees")) {
				isFound = true;
				System.out.println("Tees found.");
				r += 2;
				try {
					while (table.get(r).get(2).contains("TEE")) {
						System.out.println("Current row: " + r);
						String newinstance = table.get(r).get(2);
						System.out.println(newinstance);
						if (!tees_Map.containsKey(newinstance)) {
							tee = new Tee(newinstance);
							tees_Map.put(newinstance, tee);
						}
						else {
							tee = tees_Map.get(newinstance);
						}

						if (table.get(r).get(1).equals("Product Stream")) {
							tee.addProdStream(table.get(r).get(0));
						}

						else if (table.get(r).get(1).equals("Feed Stream")) {
							tee.setFeedStream(table.get(r).get(0));
						}
						r++;
					} // End while
				} catch (IndexOutOfBoundsException e) {}
			} // end if ==tees
		} // end for loop
		
		for (Tee t : tees_Map.values()) {
			t.createOWLFile();
			unitOperations.add(t.getName());
		}
		
		if (!isFound) {
			System.out.println("Tees not found.");
		}
	}

	public void createEqReactors() {
		/**
		 * Function generates a new OWL file from a template. A wrong/modified template may cause this function to fail.
		 * 
		 * Precondition:
		 * The function reads the instance variable "table" 
		 * The table columns must be in this order: Value, Variable Name, Container, Name, Reactants/Products.
		 * Table must have "Equilibrium Reactors" as one of the header(s).
		 * NO EMPTY CELLS are allowed in the table 
		 * 
		 * Postcondition:
		 * Creates OWL file for Equilibrium Reactors.
		 * 
		 */

		boolean isFound = false;
		
		Map<String,EqReactor> eqReactors_Map = new HashMap<String,EqReactor>();
		EqReactor reactor;

		for (int r = 0; r < table.size(); r++) {

			if (table.get(r).get(0).toString().equalsIgnoreCase("Equilibrium Reactors")) { // search for the header
				isFound = true;
				System.out.println("Equilibrium Reactors found.");

				r += 2;
				
				try {
					while (table.get(r).get(2).startsWith("R")) {
						System.out.println("Current row: " + r);
						String currentReactor = table.get(r).get(2);

						if (currentReactor.endsWith("_E")) {
							currentReactor = currentReactor.substring(0, currentReactor.length()-2);
						}
						System.out.println(currentReactor);
						
						if (!eqReactors_Map.containsKey(currentReactor)) {
							reactor = new EqReactor(currentReactor);
							eqReactors_Map.put(currentReactor, reactor);
						}
						else {
							reactor = eqReactors_Map.get(currentReactor);
						}
						
						if (table.get(r).get(1).equalsIgnoreCase("Vessel Temperature")) {
							reactor.setTemperature(Float.parseFloat(table.get(r).get(0)));
						}
						else if (table.get(r).get(1).equalsIgnoreCase("Vessel Pressure")) {
							reactor.setPressure(Float.parseFloat(table.get(r).get(0)));
						}
						else if (table.get(r).get(1).equalsIgnoreCase("Vessel Pressure Drop")) {
							reactor.setPressureDrop(Float.parseFloat(table.get(r).get(0)));
						}
						else if (table.get(r).get(1).equalsIgnoreCase("Vapour Product")) {
							reactor.setVapProd(table.get(r).get(0));
						}
						else if (table.get(r).get(1).equalsIgnoreCase("Liquid Product")) {
							reactor.setLiqProd(table.get(r).get(0));
						}
						else if (table.get(r).get(1).equalsIgnoreCase("Feed Stream")) {
							reactor.addFeedStream(table.get(r).get(0));
						}
						else if (table.get(r).get(1).equalsIgnoreCase("duty")) {
							reactor.setDuty(Float.parseFloat(table.get(r).get(0)));
						}
						else if (table.get(r).get(1).equals("Utility Feed")) {
							reactor.setUtilFeed(table.get(r).get(0));
						}
						else if (table.get(r).get(1).equals("Utility Product")) {
							reactor.setUtilProd(table.get(r).get(0));
						}
						else if (table.get(r).get(1).equals("Reaction Set")) {
							reactor.setRxnSet(table.get(r).get(0));
						}
						else if (table.get(r).get(1).equals("Active Reactions")) {
							String currRxn = table.get(r).get(0);
							if (!reactor.getChemRxns().containsKey(currRxn)) {
								reactor.addChemRxn(currRxn);
							}
						}
						else if (table.get(r).get(1).equals("Reactants")) {
							reactor.addReactant(table.get(r).get(3), table.get(r).get(0));
						}
						else if (table.get(r).get(1).equals("Products")) {
							reactor.addProduct(table.get(r).get(3), table.get(r).get(0));
						}
						r++;
					}
				} catch (IndexOutOfBoundsException e) {}
			} else {} // end if == Equilibrium Reactors
		} // end for loop
		
		for (EqReactor eqReactor : eqReactors_Map.values()) {
			eqReactor.createOWLFile();
			unitOperations.add(eqReactor.getName());
		}

		if (isFound == false) {
			System.out.println("Equilibrium Reactors not found.");
		} else {}
	}

	public void createPFReactors() {
		/**
		 * Function generates a new OWL file from a template. A wrong/modified template may cause this function to fail.
		 * 
		 * Precondition:
		 * The function reads the instance variable "table" 
		 * The table columns must be in this order: Value, Variable Name, Container, Name, Reactants/Products.
		 * Table must have "Plug Flow Reactors" as one of the header(s).
		 * NO EMPTY CELLS are allowed in the table 
		 * 
		 * Postcondition:
		 * Creates OWL file for Plug Flow Reactors.
		 * 
		 */
		
		Map<String,PFReactor> plugFlowReactors_Map = new HashMap<String,PFReactor>();
		boolean isFound = false;
		PFReactor reactor;
		
		for (int r = 0; r < table.size(); r++) {
			if (table.get(r).contains("Plug Flow Reactors")) { // search for the header
				isFound = true;
				System.out.println("Plug Flow Reactors found.");

				r += 2;
				
				try {
					while (table.get(r).get(2).startsWith("R")) {
						System.out.println("Current row: " + r);
						String currentReactor = table.get(r).get(2);
						
						if (currentReactor.endsWith("_E")) {
							currentReactor = currentReactor.substring(0, currentReactor.length()-2);
						}
						System.out.println(currentReactor);
						
						if (!plugFlowReactors_Map.containsKey(currentReactor)) {
							reactor = new PFReactor(currentReactor);
							plugFlowReactors_Map.put(currentReactor, reactor);
						}
						else {
							reactor = plugFlowReactors_Map.get(currentReactor);
						}
						
						if (table.get(r).get(1).equalsIgnoreCase("Pressure Drop")) {
							reactor.setPressureDrop(Float.parseFloat(table.get(r).get(0)));
						}
						else if (table.get(r).get(1).equalsIgnoreCase("Product Stream")) {
							reactor.setProdStream(table.get(r).get(0));
						}
						else if (table.get(r).get(1).equalsIgnoreCase("Feed Stream")) {
							reactor.addFeedStream(table.get(r).get(0));
						}
						else if (table.get(r).get(1).equalsIgnoreCase("Number of Tubes")) {
							reactor.setN_tubes(Integer.parseInt(table.get(r).get(0)));
						}
						else if (table.get(r).get(1).equalsIgnoreCase("Tube Length")) {
							reactor.setLength(Float.parseFloat(table.get(r).get(0)));
						}
						else if (table.get(r).get(1).equalsIgnoreCase("Tube Diameter")) {
							reactor.setDiameter(Float.parseFloat(table.get(r).get(0)));
						}
						else if (table.get(r).get(1).equals("Heat Flow")) {
							reactor.setDuty(Float.parseFloat(table.get(r).get(0)));
						}
						else if (table.get(r).get(1).equals("Utility Feed")) {
							reactor.setUtilFeed(table.get(r).get(0));
						}
						else if (table.get(r).get(1).equals("Utility Product")) {
							reactor.setUtilProd(table.get(r).get(0));
						}
						else if (table.get(r).get(1).equals("Reaction Set")) {
							reactor.setRxnSet(table.get(r).get(0));
						}
						else if (table.get(r).get(1).equals("Active Reactions")) {
							String currRxn = table.get(r).get(0);
							if (!reactor.getChemRxns().containsKey(currRxn)) {
								reactor.addChemRxn(currRxn);
							}
						}
						else if (table.get(r).get(1).equals("Reactants")) {
							reactor.addReactant(table.get(r).get(3), table.get(r).get(0));
						}
						else if (table.get(r).get(1).equals("Products")) {
							reactor.addProduct(table.get(r).get(3), table.get(r).get(0));
						}
						r++;
					}
				} catch (IndexOutOfBoundsException e) {}
			} else {} // End if==PFR
		} // End for loop
		
		for (PFReactor plugFlowReactor : plugFlowReactors_Map.values()) {
			plugFlowReactor.createOWLFile();
			unitOperations.add(plugFlowReactor.getName());
		}

		if (isFound == false) {
			System.out.println("Plug Flow Reactors not found.");
		} else {}
	}
	
	public void createTrayColumns() {
		/**
		 * Function generates a new OWL file from a template. A wrong/modified template may cause this function to fail.
		 * 
		 * Precondition:
		 * The function reads the instance variable "table" 
		 * The table columns must be in this order: Value, Variable Name, Container, Name.
		 * Table must have "Tray Columns" as one of the header(s).
		 * NO EMPTY CELLS are allowed in the table (except for the header row).
		 * 
		 * Postcondition:
		 * Creates OWL File for Tray Columns.
		 *  
		 */
		Map<String,TrayColumn> trayColumns_Map = new HashMap<String,TrayColumn>();
		TrayColumn column;
		boolean isFound = false;
	
		for (int r = 0; r < table.size(); r++) {
			if (table.get(r).contains("Tray Columns")) {
				isFound = true;
				System.out.println("Tray Columns found.");
				r += 2;
				
				try {
					while (table.get(r).get(2).startsWith("T")) {
						System.out.println("Current row: " + r);
						System.out.println(table.get(r));
						String newinstance = table.get(r).get(2);
						System.out.println(newinstance);
						
						if (!trayColumns_Map.containsKey(newinstance)) {
							column = new TrayColumn(newinstance);
							trayColumns_Map.put(newinstance, column);
						}
						else {
							column = trayColumns_Map.get(newinstance);
						}
	
						if (table.get(r).get(1).contains("Object Type")) {
							column.setColumnType(table.get(r).get(0));
						}
						else if (table.get(r).get(1).equals("Stage Numbering Method")) {
							column.setStageNumberingMethod(Integer.parseInt(table.get(r).get(0)));
						}
	
						else if (table.get(r).get(1).equals("Number of Trays")) {
							column.setN_trays(Integer.parseInt(table.get(r).get(0)));
						}
	
						else if (table.get(r).get(1).equalsIgnoreCase("top liquid feed") || table.get(r).get(1).equalsIgnoreCase("Top Liquid Inlet")) {
							String topIn = table.get(r).get(0);
							column.setTopIn(topIn);
							if (table.get(r).get(3).equalsIgnoreCase("Feed")) {
								column.setUserDefFeed(topIn);
							}
						}
	
						else if (table.get(r).get(1).equalsIgnoreCase("overhead vapour product") || table.get(r).get(1).equalsIgnoreCase("Top Vapour Outlet")) {
							column.setTopOut(table.get(r).get(0));
						}
	
						else if (table.get(r).get(1).equalsIgnoreCase("bottom vapour feed")	|| table.get(r).get(1).equalsIgnoreCase("Btm Vapour Inlet")) {
							String btmIn = table.get(r).get(0);
							column.setBtmIn(btmIn);
							if (table.get(r).get(3).equalsIgnoreCase("Feed")) {
								column.setUserDefFeed(btmIn);
							}
						}
	
						else if (table.get(r).get(1).equalsIgnoreCase("bottom liquid product") || table.get(r).get(1).equalsIgnoreCase("Btm Liquid Outlet")) {
							column.setBtmOut(table.get(r).get(0));
						}
	
						else if (table.get(r).get(1).equals("Feed Stream")) {
							int trayNo = Integer.parseInt(table.get(r).get(3).split("__")[0]);
							column.addFeedStream(trayNo, table.get(r).get(0));
						}
	
						else if (table.get(r).get(1).equals("Draw Stream")) {
							int trayNo = Integer.parseInt(table.get(r).get(3).split("__")[0]);
							column.addDrawStream(trayNo, table.get(r).get(0));
						} 
						r++;
	
					} // End while startsWith T
	
				} catch (IndexOutOfBoundsException e) {}
	
			} // End if==Tray Columns
	
		} // End for loop
		
		for (TrayColumn trayColumn : trayColumns_Map.values()) {
			trayColumn.createOWLFile();
			unitOperations.add(trayColumn.getName());
		}
		
		if (isFound == false) {
			System.out.println("Tray Columns not found.");
		} else {}
	}

	public void createReboilers() {
		/**
		 * Function generates a new OWL file from a template. A wrong/modified template may cause this function to fail.
		 * 
		 * Precondition:
		 * The function reads the instance variable "table" 
		 * The table columns must be in this order: Value, Variable Name, Container, Name.
		 * Table must have "Reboilers" as one of the header(s).
		 * NO EMPTY CELLS are allowed in the table (except for the header row).
		 * 
		 * Postcondition:
		 * Creates OWL File for Reboilers.
		 * 
		 * */
		
		Map<String,Reboiler> reboilers_Map = new HashMap<String,Reboiler>();
		Reboiler reboiler;
		boolean isFound = false;
			
		for (int r = 0; r < table.size(); r++) {
			if (table.get(r).contains("Reboilers")) {
				isFound = true;
				System.out.println("Reboilers found.");
				r += 2;
				try {
					while (table.get(r).get(2).contains("E-")) {
						String property = table.get(r).get(1);
						System.out.println("Current row: " + r);
						String newinstance = table.get(r).get(2);
						newinstance = newinstance.substring(newinstance.indexOf("E-"), newinstance.length());
						System.out.println(newinstance);
						
						if (!reboilers_Map.containsKey(newinstance)) {
							reboiler = new Reboiler(newinstance);
							reboilers_Map.put(newinstance, reboiler);
						} 
						else {
							reboiler = reboilers_Map.get(newinstance);
						}
						
						if (property.equals("Feeds")) {
							reboiler.setFeedStream(table.get(r).get(0));
						}
						else if (property.equals("Liquid Product Stream")) {
							reboiler.setLiqProdStream(table.get(r).get(0));
						} 
						else if (property.equals("Boilup Stream")) {
							reboiler.setBoilupStream(table.get(r).get(0));
						}
						else if (property.equals("Duty")) {
							reboiler.setDuty(Float.parseFloat(table.get(r).get(0)));
						}
						else if (property.equals("Tube Side Feed Stream")) {
							reboiler.setUtilFeedStream(table.get(r).get(0));
							reboiler.setUtilSide("Tube");
						}
						else if (property.equals("Tube Side Product Stream")) {
							reboiler.setUtilProdStream(table.get(r).get(0));
							reboiler.setUtilSide("Tube");
						}
						else if (property.equals("Shell Side Feed Stream")) {
							reboiler.setUtilFeedStream(table.get(r).get(0));
							reboiler.setUtilSide("Shell");
						}
						else if (property.equals("Shell Side Product Stream")) {
							reboiler.setUtilProdStream(table.get(r).get(0));
							reboiler.setUtilSide("Shell");
						}
						r++;
					} // End while statement
				} catch (IndexOutOfBoundsException e) {}
			} // End If reboiler statement
		}
		
		for (Reboiler object : reboilers_Map.values()) {
			object.createOWLFile();
			unitOperations.add(object.getName());
		}
		
		if (!isFound) {
			System.out.println("Reboilers not found.");
		}
	}

	public void createCondensers() {
		/**
		 * Function generates a new OWL file from a template. A wrong/modified template may cause this function to fail.
		 * 
		 * Precondition:
		 * The function reads the instance variable "table" 
		 * The table columns must be in this order: Value, Variable Name, Container, Name.
		 * Table must have "Condensers" as one of the header(s).
		 * NO EMPTY CELLS are allowed in the table (except for the header row).
		 * 
		 * Postcondition:
		 * Creates OWL File for Condensers.
		 * 
		 */
		Map<String,Condenser> condensers_Map = new HashMap<String,Condenser>();
		Condenser condenser;
		boolean isFound = false;
		
		String utility="";
		
		for (int r=0; r<table.size(); r++) {
			if (table.get(r).contains("Condensers")) {
				isFound = true;
				System.out.println("Condensers found.");
				r+=2;
				
				try {
					while (table.get(r).get(2).contains("E-")) {
						System.out.println("Current row: " + r);
						String property=table.get(r).get(1);
						String newinstance=table.get(r).get(2);	
						System.out.println(newinstance);
						
						if(!condensers_Map.containsKey(newinstance)) {
							condenser = new Condenser(newinstance);
							condensers_Map.put(newinstance, condenser);
						} 
						else {
							condenser = condensers_Map.get(newinstance);
						}
						
						if (property.equals("Duty")) {
							condenser.setDuty(Float.parseFloat(table.get(r).get(0)));
						} 
						else {
							utility = utilityMap.get(newinstance);
							if (utility.equalsIgnoreCase("tube")) {
								if (property.equals("Tube Side Feed Stream")) {//Later change class from Process Stream to feed or product stream
									condenser.setUtilFeedStream(table.get(r).get(0));
								}
								else if(property.equals("Tube Side Product Stream")) {
									condenser.setUtilProdStream(table.get(r).get(0));
								}
								else if(property.equals("Shell Side Feed Stream")) {
									condenser.setFeedStream(table.get(r).get(0));
								} 
								else if(property.equals("Shell Side Product Stream")) {	
									condenser.setProdStream(table.get(r).get(0));
								} else {}
							}			
							else if (utility.equalsIgnoreCase("shell")) {
								if (property.equals("Shell Side Feed Stream")) {//Later change class from Process Stream to feed or product stream
									condenser.setUtilFeedStream(table.get(r).get(0));
								}			
								else if(property.equals("Shell Side Product Stream")) {
									condenser.setUtilProdStream(table.get(r).get(0));
								}
								else if(property.equals("Tube Side Feed Stream")) {
									condenser.setFeedStream(table.get(r).get(0));
								}
								else if(property.equals("Tube Side Product Stream")) {	
									condenser.setProdStream(table.get(r).get(0));
								}
							} else {}
						}	
						r++;				
					}
				} catch (IndexOutOfBoundsException e) {}
			}//End If condenser statement				 	
		} // End for loop
		
		for (Condenser object : condensers_Map.values()) {
			object.createOWLFile();
			unitOperations.add(object.getName());
		}
		
		if (!isFound) {
			System.out.println("Condensers not found.");
		}
	}

	public void createHeatExchangers() {
		/**
		 * Function generates a new OWL file from a template. A wrong/modified template may cause this function to fail.
		 * 
		 * Precondition:
		 * The function reads the instance variable "table" 
		 * The table columns must be in this order: Value, Variable Name, Container, Name.
		 * Table must have "Process Heat Exchangers" as one of the header(s).
		 * NO EMPTY CELLS are allowed in the table (except for the header row).
		 * 
		 * Postcondition:
		 * Creates OWL File for Process Heat Exchangers.
		 * 
		 */
		Map<String,HeatExchanger> processHE_Map = new HashMap<String,HeatExchanger>();
		boolean isFound = false;
		HeatExchanger heatX;
		
		for (int r=0; r<table.size(); r++) {
			
			if (table.get(r).contains("Heat Exchangers")) {
				isFound = true;
				System.out.println("Heat Exchangers found.");
				r+=2;
				
				try {
					while (table.get(r).get(2).contains("E-")) {
						String property = table.get(r).get(1);
						System.out.println("Current row: " + r);
						String newinstance=table.get(r).get(2);	
						System.out.println(newinstance);
						
						if (!processHE_Map.containsKey(newinstance)) {
							heatX = new HeatExchanger(newinstance);
							heatX.setUtilStartNum(utilStartNum);
							processHE_Map.put(newinstance, heatX);
						}
						else {
							heatX = processHE_Map.get(newinstance);
						}
						
						if (property.equals("Tube Side Feed Stream")) {
							heatX.setTubeFeed(table.get(r).get(0));
						}			
						else if (property.equals("Tube Side Product Stream")) {
							heatX.setTubeProd(table.get(r).get(0));
						}
						else if (property.equals("Shell Side Feed Stream")) {
							heatX.setShellFeed(table.get(r).get(0));
						}
						else if (property.equals("Shell Side Product Stream")) {	
							heatX.setShellProd(table.get(r).get(0));
						} 
						else if (property.equals("Duty")) {
							heatX.setDuty(Float.parseFloat(table.get(r).get(0)));
						}
						else if(property.equals("Tube Side Delta T")) {
							heatX.setTubeDeltaT(Float.parseFloat(table.get(r).get(0)));						
						}
						else if(property.equals("Vapour Fraction Change in Tube")) {
							heatX.setTubeDeltaVF(Float.parseFloat(table.get(r).get(0)));
						}
						else if(property.equals("Vapour Fraction Change in Shell")) {
							heatX.setShellDeltaVF(Float.parseFloat(table.get(r).get(0)));
						}
						heatX.setUtilSide(utilityMap.get(newinstance));
						r++;
					}
				} catch (IndexOutOfBoundsException e) {}
			}//End If Process Heat Exchanger statement
		}
		
		for (HeatExchanger object : processHE_Map.values()) {
			object.createOWLFile();
			unitOperations.add(object.getName());
		}
		
		if (!isFound) {
			System.out.println("Process Heat Exchangers not found.");
		}
	}

	public void createPumps() {
		/**
		 * Function generates a new OWL file from a template. A wrong/modified template may cause this function to fail.
		 * 
		 * Precondition:
		 * The function reads the instance variable "table" 
		 * The table columns must be in this order: Value, Variable Name, Container, Name.
		 * Table must have "Pumps" as one of the header(s).
		 * NO EMPTY CELLS are allowed in the table (except for the header row).
		 * 
		 * Postcondition:
		 * Creates OWL File for Pumps.
		 * 
		 */
		Map<String,Pump> pumps_Map = new HashMap<String,Pump>();
		Pump pump;
		
		boolean isFound = false;

		for (int r = 0; r < table.size(); r++) {
			if (table.get(r).contains("Pumps")) {
				isFound = true;
				System.out.println("Pumps found");
				r += 2;
				
				try {
					while (table.get(r).get(2).contains("P-")) {
						System.out.println("Current row: " + r);
						String newinstance = table.get(r).get(2);
						String property = table.get(r).get(1);
						System.out.println(newinstance);
						
						if (!pumps_Map.containsKey(newinstance)) {
							pump = new Pump(newinstance);
							pumps_Map.put(newinstance, pump);
						}
						else {
							pump = pumps_Map.get(newinstance);
						}
						
						if (property.equals("Feed Stream")) {
							pump.setFeedStream(table.get(r).get(0));
						}

						else if (property.equals("Product Stream")) {
							pump.setProdStream(table.get(r).get(0));
						}
						
						else if (property.contains("Total Head")) {
							pump.setPumpHead(Float.parseFloat(table.get(r).get(0)));
						}

						else if (property.equals("Power")) {
							pump.setPower(Float.parseFloat(table.get(r).get(0)));
						}

						else if (property.equals("Product Pressure")) {
							pump.setOutPressure(Float.parseFloat(table.get(r).get(0)));
						} else {}
						
						r++;
					}
				} catch (IndexOutOfBoundsException e) {}
			} // end if ==Pumps
		} // End for loop
		
		for (Pump p : pumps_Map.values()) {
			p.createOWLFile();
			unitOperations.add(p.getName());
		}
		
		if (!isFound) {
			System.out.println("Pumps not found.");
		}
	}

	public void createCompressors() {
		/**
		 * Function generates a new OWL file from a template. A wrong/modified template may cause this function to fail.
		 * 
		 * Precondition:
		 * The function reads the instance variable "table" 
		 * The table columns must be in this order: Value, Variable Name, Container, Name.
		 * Table must have "Compressors" as one of the header(s).
		 * NO EMPTY CELLS are allowed in the table (except for the header row).
		 * 
		 * Postcondition:
		 * Creates OWL File for Process Heat Compressors.
		 * 
		 */
		Map<String,Compressor> compressors_Map = new HashMap<String,Compressor>();
		Compressor compressor;
		boolean isFound = false;

		for (int r = 0; r < table.size(); r++) {
			if (table.get(r).contains("Compressors")) {
				isFound = true;
				System.out.println("Compressors found.");
				r += 2;
				
				try {
					while (table.get(r).get(2).contains("C-")) {
						System.out.println("Current row: " + r);
						String newinstance = table.get(r).get(2);
						String property = table.get(r).get(1);

						if (!compressors_Map.containsKey(newinstance)) {
							compressor =  new Compressor(newinstance);
							compressors_Map.put(newinstance, compressor);
						}
						else {
							compressor = compressors_Map.get(newinstance);
						}
						
						if (property.equals("Feed Stream")) {// Replace process stream names for each instance.
							compressor.setFeedStream(table.get(r).get(0));
						}

						else if (property.equals("Product Stream")) {
							compressor.setProdStream(table.get(r).get(0));
						}
						else if (property.equals("Power")) {
							compressor.setPower(Float.parseFloat(table.get(r).get(0)));
						}

						else if (property.equals("Product Pressure")) {
							compressor.setOutPressure(Float.parseFloat(table.get(r).get(0)));
						}
						r++;
					}
				} catch (IndexOutOfBoundsException e) {}			
			} // end if ==Compressors
		} // end for loop
		
		for (Compressor c : compressors_Map.values()) {
			c.createOWLFile(plantName);
			unitOperations.add(c.getName());
		}
		
		if (!isFound) {
			System.out.println("Compressors not found.");
		}
	}

	public void createSeparators() {
		/**
		 * Function generates a new OWL file from a template. A wrong/modified template may cause this function to fail.
		 * 
		 * Precondition:
		 * The function reads the instance variable "table" 
		 * The table columns must be in this order: Value, Variable Name, Container, Name.
		 * Table must have "Separators" as one of the header(s).
		 * NO EMPTY CELLS are allowed in the table (except for the header row).
		 * 
		 * Postcondition:
		 * Creates OWL File for Separators.
		 * 
		 */
		
		Map<String,Separator> seps_Map = new HashMap<String,Separator>();
		Separator sep;
		boolean isFound = false;

		for (int r = 0; r < table.size(); r++) {
			if (table.get(r).contains("Separators")) {
				isFound = true;
				System.out.println("Separator vessels found.");
				r += 2;

				try {
					while (table.get(r).get(2).startsWith("V-")) {
						System.out.println("Current row: " + r);
						String newinstance = table.get(r).get(2);
						System.out.println(newinstance);
						
						if (!seps_Map.containsKey(newinstance)) {
							sep = new Separator(newinstance);
							seps_Map.put(newinstance, sep);
						}
						else {
							sep = seps_Map.get(newinstance);
						}

						if (table.get(r).get(1).equals("Feed Stream")) {
							sep.setFeedStream(table.get(r).get(0));
						} else if (table.get(r).get(1).equals("Liquid Product")) {
							sep.setLiqProdStream(table.get(r).get(0));
						} else if (table.get(r).get(1).equals("Vapour Product")) {
							sep.setVapProdStream(table.get(r).get(0));
						} else if (table.get(r).get(1).equalsIgnoreCase("DUTY")) {
							sep.setDuty(Float.parseFloat(table.get(r).get(0)));
						} else if (table.get(r).get(1).equals("Vessel Temperature")) {
							sep.setTemp(Float.parseFloat(table.get(r).get(0)));
						} else if (table.get(r).get(1).equals("Vessel Pressure Drop")) {
							sep.setPressDrop(Float.parseFloat(table.get(r).get(0)));
						} else {}
						r++;
					} // End while startsWith V
				} catch (IndexOutOfBoundsException e) {}
			} else {} // End if==Separators
		} // End for loop
		
		for (Separator s : seps_Map.values()) {
			s.createOWLFile();
			unitOperations.add(s.getName());
		}

		if (isFound == false) {
			System.out.println("Separator vessels not found.");
		} else {}
	}

	public void createValves() {
		/**
		 * Function generates a new OWL file from a template. A wrong/modified template may cause this function to fail.
		 * 
		 * Precondition:
		 * The function reads the instance variable "table" 
		 * The table columns must be in this order: Value, Variable Name, Container, Name.
		 * Table must have "Valves" as one of the header(s).
		 * NO EMPTY CELLS are allowed in the table (except for the header row).
		 * 
		 * Postcondition:
		 * Creates OWL File for Valves.
		 * 
		 */
		Valve valve;
		Map<String,Valve> valves_Map = new HashMap<String,Valve>();
		boolean isFound = false;

		for (int r = 0; r < table.size(); r++) {

			if (table.get(r).contains("Valves")) {

				isFound = true;
				System.out.println("Valves found.");
				r += 2;

				try {
					while (table.get(r).get(2).startsWith("VLV")) {
						System.out.println("Current row: " + r);
						String newinstance = table.get(r).get(2);
						System.out.println(newinstance);
						
						if (!valves_Map.containsKey(newinstance)) {
							valve = new Valve(newinstance);
							valves_Map.put(newinstance, valve);
						}
						else {
							valve = valves_Map.get(newinstance);
						}

						if (table.get(r).get(1).equals("Feed Stream")) {
							valve.setFeedStream(table.get(r).get(0));
						} else if (table.get(r).get(1).equals("Product Stream")) {
							valve.setProdStream(table.get(r).get(0));
						} else if (table.get(r).get(1).equals("Pressure Drop") || table.get(r).get(1).equals("PDROP")) {
							valve.setPressDrop(Float.parseFloat(table.get(r).get(0)));
						} else if (table.get(r).get(1).equals("Product Pressure")) {
							valve.setOutPressure(Float.parseFloat(table.get(r).get(0)));
						} else {}
						r++;
					} // End while startsWith VLV
				} catch (IndexOutOfBoundsException e) {}
			} else {} // End if==Valves
		} // End for loop
		
		for (Valve v : valves_Map.values()) {
			v.createOWLFile();
			unitOperations.add(v.getName());
		}

		if (isFound == false) {
			System.out.println("Valves not found.");
		} else {}
	}

	public void createStreamFile() {//Order matters [Vapor fraction, temperature/pressure/molar/mass flow, phase components mole fracs], only works for two phases [Liquid and Vapor]
		/**
		 * Function generates a new OWL file from a template. A wrong/modified template may cause this function to fail.
		 * 
		 * Precondition:
		 * The function reads the instance variable "table" 
		 * The table columns must be in this order: Value, Variable Name, Container, Name.
		 * Table must have "Streams" as one of the header(s).
		 * NO EMPTY CELLS are allowed in the table (except for the header row).
		 * 
		 * Postcondition:
		 * Creates OWL File for Streams.
		 * 
		 */
		Map<String,Stream> streamFile_Map = new HashMap<String,Stream>();
		Stream stream;
		String streamNumber;
		boolean isFound = false;
		boolean endOfStreamSection=false;
		
		List<List<String>> chemSpec = Tools.createTableFromTabDelimited(chemSpecFile);
		List<String> fullName = new ArrayList<String>();
		List<String> hysysAbbr = new ArrayList<String>();
		List<Float> molWeight = new ArrayList<Float>();
		List<String> molForm = new ArrayList<String>();
		for (int row=1; row<chemSpec.size(); row++) {
			fullName.add(chemSpec.get(row).get(0));
			hysysAbbr.add(chemSpec.get(row).get(1));
			molWeight.add(Float.parseFloat(chemSpec.get(row).get(2)));
			molForm.add(chemSpec.get(row).get(3));
		}
		
		for (int r=0; r<table.size(); r++) {
			endOfStreamSection=false;
			if (table.get(r).contains("Streams")) {
				r+=2;
				System.out.println("Streams found.");
				isFound = true;
				try {
					while (endOfStreamSection==false) {
						System.out.println("Current row: " + r);
						String nameCell=table.get(r).get(3);
						int index=nameCell.indexOf(".");
						System.out.println(index);
						if (index==-1) {
							endOfStreamSection=true;
							continue;
						}
						
						streamNumber = Tools.splitByDots(table.get(r).get(3)).get(0);
						if(streamNumber.contains("'") || streamNumber.contains("x")) { // Skips over streams with prime or x
							r++;
							continue;
						}
						
						String newinstance = "Pipe_" + streamNumber;
						String property = nameCell.substring(index+1,nameCell.length());
						System.out.println(newinstance);
						System.out.println(property);
						
						if (!streamFile_Map.containsKey(newinstance)) {
							stream = new Stream(streamNumber);
							System.out.println("new Stream created: " + stream.getName());
							streamFile_Map.put(newinstance, stream);
							stream.addCompArray(fullName);
							stream.addHysysName(hysysAbbr);
							stream.addMolWeight(molWeight);
							stream.addMolFormula(molForm);
						}
						else {
							stream = streamFile_Map.get(newinstance);
						}
						
						if (!property.contains("Phase")) {
							if(property.equals("Molar Flow")){
								stream.setMolarFlow(Float.parseFloat(table.get(r).get(0)));
							}
							else if (property.equals("Mass Flow")) {
								stream.setMassFlow(Float.parseFloat(table.get(r).get(0)));
							}		
							else if(property.equals("Vapour Fraction")) {
								stream.setVapFrac(Float.parseFloat(table.get(r).get(0)));
							}
							else if(property.equals("Temperature")) {
								stream.setTemperature(Float.parseFloat(table.get(r).get(0)));
							}
							else if(property.equals("Pressure")) {
								stream.setPressure(Float.parseFloat(table.get(r).get(0)));
							}
						} // End if doesnot contain phase
						else {
							if (Tools.splitByDots(nameCell).get(2).equals("Liquid Phase") && !table.get(r).get(0).equals("0")) {
								stream.addLiqPhaseMoleFrac(Tools.splitByDots(nameCell).get(3), Float.parseFloat(table.get(r).get(0)));
							}
							else if (Tools.splitByDots(nameCell).get(2).equals("Second Liquid Phase") && !table.get(r).get(0).equals("0")) {
								stream.addLiq2PhaseMoleFrac(Tools.splitByDots(nameCell).get(3), Float.parseFloat(table.get(r).get(0)));
							}
							else if (Tools.splitByDots(nameCell).get(2).equals("Aqueous Phase") && !table.get(r).get(0).equals("0")) {
								stream.addAqPhaseMoleFrac(Tools.splitByDots(nameCell).get(3), Float.parseFloat(table.get(r).get(0)));
							}
							else if (Tools.splitByDots(nameCell).get(2).equals("Vapour Phase") && !table.get(r).get(0).equals("0")) {
								stream.addGasPhaseMoleFrac(Tools.splitByDots(nameCell).get(3), Float.parseFloat(table.get(r).get(0)));
							}
						}
						r++;
					} // end while
				} catch (IndexOutOfBoundsException e) {}
			} // End if == streams
		} // End for loop
		
		for (Stream pipe : streamFile_Map.values()) {
			pipe.createOWLFile();
			unitOperations.add("Pipe_" + pipe.getName());			
			String pipeFileName = "Pipe_" + pipe.getName() + ".owl";
			if (!pipe.getName().contains("x")) {
				int streamNum = Integer.parseInt(pipe.getName());
				if (streamNum >= utilStartNum && Tools.containsString("ProcessStream_", pipeFileName)) {
					Tools.replaceString("ProcessStream_", "UtilityStream_", pipeFileName);
					Tools.replaceString("process.owl#ProcessStream", "process.owl#Utility", pipeFileName);
				}
				else if (streamNum < utilStartNum && Tools.containsString("UtilityStream_", pipeFileName)) {
					Tools.replaceString("UtilityStream_", "ProcessStream_", pipeFileName);
					Tools.replaceString("process.owl#Utility", "process.owl#ProcessStream", pipeFileName);
				}
			}
		}
		
		if (!isFound) {
			System.out.println("Streams not found.");
		}
	}

	public void createTopNode() {
		/**
		 * Function generates a new OWL file from a template. A wrong/modified template may cause this function to fail.
		 * 
		 * Precondition:
		 * The function reads the instance variable "unitOperations"
		 * Assumes that unitOperations contains all the names of the unit operations in the plant
		 * 
		 * Postcondition:
		 * Creates OWL File for the top node.
		 * 
		 */
		
		String templateFile = "OWL Templates/Plant_Template.owl";
		String newFileName = plantName + ".owl";
		
		Tools.replaceString("BiodieselPlant-2", plantName, templateFile, newFileName);
		
		JenaOWLModel owlModel = Tools.callJena(newFileName);
		
		OWLIndividual Plant = owlModel.getOWLIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + plantName + ".owl#" + plantName);
		
		OWLObjectProperty hasSubsystem = owlModel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasSubsystem");
		OWLDatatypeProperty hasIRI = owlModel.getOWLDatatypeProperty("http://www.theworldavatar.com/ontology/ontoeip/ecoindustrialpark/EcoIndustrialPark.owl#hasIRI");
					
		for (String unitName : unitOperations) {
			if (unitName.startsWith("E-")) {
				OWLNamedClass ShellTubeApparatusClass = owlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant_equipment/apparatus.owl#ShellTubeApparatus");
				
				RDFIndividual HeatExchanger = ShellTubeApparatusClass.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + plantName + ".owl#" + unitName);
				
				Plant.addPropertyValue(hasSubsystem, HeatExchanger);
				HeatExchanger.addPropertyValue(hasIRI, "http://www.jparksimulator.com/kb/sgp/jurongisland/" + unitName + ".owl#" + unitName);
			}
			else if (unitName.startsWith("C-")) {
				OWLNamedClass CompressorClass = owlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant_equipment/machine.owl#Compressor");
				
				RDFIndividual Compressor = CompressorClass.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + plantName + ".owl#" + unitName);
				
				Plant.addPropertyValue(hasSubsystem, Compressor);
				Compressor.addPropertyValue(hasIRI, "http://www.jparksimulator.com/kb/sgp/jurongisland/" + unitName + ".owl#" + unitName);
			}
			else if (unitName.startsWith("P-")) {
				OWLNamedClass PumpClass = owlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant_equipment/machine.owl#Pump");
				
				RDFIndividual Pump = PumpClass.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + plantName + ".owl#" + unitName);
				
				Plant.addPropertyValue(hasSubsystem, Pump);
				Pump.addPropertyValue(hasIRI, "http://www.jparksimulator.com/kb/sgp/jurongisland/" + unitName + ".owl#" + unitName);
			}
			else if (unitName.startsWith("R-") || unitName.startsWith("V-")) {
				OWLNamedClass VesselClass = owlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant_equipment/apparatus.owl#Vessel");
				
				RDFIndividual Vessel = VesselClass.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + plantName + ".owl#" + unitName);
				
				Plant.addPropertyValue(hasSubsystem, Vessel);
				Vessel.addPropertyValue(hasIRI, "http://www.jparksimulator.com/kb/sgp/jurongisland/" + unitName + ".owl#" + unitName);
			}
			else if (unitName.startsWith("T-")) {
				OWLNamedClass TrayColumnClass = owlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant_equipment/apparatus.owl#TrayColumn");
				
				RDFIndividual TrayColumn = TrayColumnClass.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + plantName + ".owl#" + unitName);
				
				Plant.addPropertyValue(hasSubsystem, TrayColumn);
				TrayColumn.addPropertyValue(hasIRI, "http://www.jparksimulator.com/kb/sgp/jurongisland/" + unitName + ".owl#" + unitName);
			}
			else if (unitName.startsWith("MIX-") || unitName.startsWith("TEE-")) {
				OWLNamedClass ApparatusClass = owlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#Apparatus");
				
				RDFIndividual Apparatus = ApparatusClass.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + plantName + ".owl#" + unitName);
				
				Plant.addPropertyValue(hasSubsystem, Apparatus);
				Apparatus.addPropertyValue(hasIRI, "http://www.jparksimulator.com/kb/sgp/jurongisland/" + unitName + ".owl#" + unitName);
			}
			else if (unitName.startsWith("VLV-")) {
				OWLNamedClass ValveClass = owlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant_equipment/machine.owl#Valve");
				
				RDFIndividual Valve = ValveClass.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + plantName + ".owl#" + unitName);
				
				Plant.addPropertyValue(hasSubsystem, Valve);
				Valve.addPropertyValue(hasIRI, "http://www.jparksimulator.com/kb/sgp/jurongisland/" + unitName + ".owl#" + unitName);
			}
			else if (unitName.startsWith("Pipe_")) {
				OWLNamedClass PipeClass = owlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#Pipe");
				
				RDFIndividual Pipe = PipeClass.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + plantName + ".owl#" + unitName);
				
				Plant.addPropertyValue(hasSubsystem, Pipe);
				Pipe.addPropertyValue(hasIRI, "http://www.jparksimulator.com/kb/sgp/jurongisland/" + unitName + ".owl#" + unitName);
			}
			else {}
		} // End for loop
		Tools.saveJena(owlModel, newFileName);
	}

	private void editSpecialPipeStream(String streamNumber, String streamType) {
		/** 
		 * Helper function to change ProcessStream into special stream
		 * 
		 * Precondition:
		 * Assumes that the function is only to be used on Pipe OWL files
		 * 
		 * Postcondition:
		 * Changed the stream class and stream name in the Pipe OWL file
		 */
		if (streamType.equalsIgnoreCase("feed")) {
			Tools.replaceString("ProcessStream_" + streamNumber, "FeedStream_" + streamNumber, "Pipe_" + streamNumber + ".owl");
			Tools.replaceString("process.owl#ProcessStream", "process.owl#RawMaterial", "Pipe_" + streamNumber + ".owl");
		}
		else if (streamType.equalsIgnoreCase("waste")) {
			Tools.replaceString("ProcessStream_" + streamNumber, "WasteStream_" + streamNumber, "Pipe_" + streamNumber + ".owl");
			Tools.replaceString("process.owl#ProcessStream", "process.owl#WasteProduct", "Pipe_" + streamNumber + ".owl");
		}
		else if (streamType.equalsIgnoreCase("product")) {
			Tools.replaceString("ProcessStream_" + streamNumber, "ProductStream_" + streamNumber, "Pipe_" + streamNumber + ".owl");
			Tools.replaceString("process.owl#ProcessStream", "process.owl#EndProduct", "Pipe_" + streamNumber + ".owl");
		} 
		else {}
	}
	
	public void editSpecialStreams() {
		/** 
		 * Function to change Process Streams into special streams according to specifications
		 * 
		 * Precondition:
		 * Assumes that all Pipe OWL files have been created
		 * Assumes that all OWL files for unit operations have been created
		 * 
		 * Postcondition:
		 * Changed the Process Streams into special streams, both in Pipe OWL files and unit ops OWL files.
		 * 
		 */
		for (String streamNumber : specialStreams.keySet()) {
			editSpecialPipeStream(streamNumber, specialStreams.get(streamNumber));
			for (String unit : unitOperations) {
				if (!unit.startsWith("Pipe_") && Tools.containsString("ProcessStream_" + streamNumber, unit + ".owl")) {
					JenaOWLModel owlModel = Tools.callJena(unit + ".owl");
					
					OWLIndividual ProcessStream = owlModel.getOWLIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + unit + ".owl#" + "ProcessStream_" + streamNumber);
					
					OWLNamedClass RawMaterialClass = owlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_function/process.owl#RawMaterial");
					OWLNamedClass EndProductClass = owlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_function/process.owl#EndProduct");
					OWLNamedClass WasteProductClass = owlModel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_function/process.owl#WasteProduct");
					
					if (specialStreams.get(streamNumber).equalsIgnoreCase("feed")) {
						ProcessStream.setRDFType(RawMaterialClass);
					}
					else if (specialStreams.get(streamNumber).equalsIgnoreCase("waste")) {
						ProcessStream.setRDFType(WasteProductClass);
					}
					else if (specialStreams.get(streamNumber).equalsIgnoreCase("product")) {
						ProcessStream.setRDFType(EndProductClass);
					} 
					else {}
					
					Tools.saveJena(owlModel, unit + ".owl");
					
					if (specialStreams.get(streamNumber).equalsIgnoreCase("feed")) {
						Tools.replaceString("ProcessStream_" + streamNumber, "FeedStream_" + streamNumber, unit + ".owl");
					}
					else if (specialStreams.get(streamNumber).equalsIgnoreCase("waste")) {
						Tools.replaceString("ProcessStream_" + streamNumber, "WasteStream_" + streamNumber, unit + ".owl");
					}
					else if (specialStreams.get(streamNumber).equalsIgnoreCase("product")) {
						Tools.replaceString("ProcessStream_" + streamNumber, "ProductStream_" + streamNumber, unit + ".owl");
					} 
					else {}
				}
			}
		}
	}

	public void createOntology() {
		createStreamFile();
		
		createHeatExchangers();
		createReboilers();
		createMixers();
		createTees();
		createEqReactors();
		createPFReactors();
		createTrayColumns();
		createPumps();
		createCompressors();
		createSeparators();
		createValves();
		
		editSpecialStreams();
		createTopNode();
		
//		Tools.writeListToTXT(unitOperations, plantName + "-UnitOperations.txt");

	}
}
