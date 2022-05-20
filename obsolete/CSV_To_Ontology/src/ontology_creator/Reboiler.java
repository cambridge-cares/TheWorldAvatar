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

public class Reboiler {
	private String name;
	private String feedStream;
	private String liqProdStream;
	private String boilupStream;
	private Float duty;
	private String utilFeedStream;
	private String utilProdStream;
	private String utilSide;
	private int n_nozzle = 4;
	private String filename;
	
	
	Reboiler(String name) {
		this.name = name;
		this.filename = name + ".owl";
	}

	public String getName() {
		return name;
	}

	public String getFeedStream() {
		return feedStream;
	}

	public String getLiqProdStream() {
		return liqProdStream;
	}

	public String getBoilupStream() {
		return boilupStream;
	}

	public Float getDuty() {
		return duty;
	}

	public String getUtilFeedStream() {
		return utilFeedStream;
	}

	public String getUtilProdStream() {
		return utilProdStream;
	}

	public String getUtilSide() {
		return utilSide;
	}
	
	public int getN_nozzle() {
		return n_nozzle;
	}

	public void setName(String name) {
		this.name = name;
	}

	public void setFeedStream(String feedStream) {
		this.feedStream = feedStream;
	}

	public void setLiqProdStream(String liqProdStream) {
		this.liqProdStream = liqProdStream;
	}

	public void setBoilupStream(String boilupStream) {
		this.boilupStream = boilupStream;
	}

	public void setDuty(Float duty) {
		this.duty = duty;
	}

	public void setUtilFeedStream(String utilFeedStream) {
		this.utilFeedStream = utilFeedStream;
	}

	public void setUtilProdStream(String utilProdStream) {
		this.utilProdStream = utilProdStream;
	}

	public void setUtilSide(String utilSide) {
		this.utilSide = utilSide;
	}
	
	public void addN_nozzle() { 
		this.n_nozzle++;
	}
	
	public void createNewOutputStream(JenaOWLModel owlmodel, String streamNumber) {
		addN_nozzle();
		OWLNamedClass NozzleClass = owlmodel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#Nozzle");
		OWLNamedClass ProcessStreamClass = owlmodel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_function/process.owl#ProcessStream");
		OWLNamedClass PipeClass = owlmodel.getOWLNamedClass("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#Pipe");
		
		OWLIndividual ExchangerInstance = owlmodel.getOWLIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + filename + "#" + getName());// new instance
		OWLIndividual HeatingProcess = owlmodel.getOWLIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + filename + "#Vaporizing_ProcessStream_" + getFeedStream());
		
		OWLObjectProperty isDirectlyConnectedTo = owlmodel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#isDirectlyConnectedTo");
		OWLObjectProperty hasConnector = owlmodel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_realization/plant.owl#hasConnector");
		OWLObjectProperty hasOutput = owlmodel.getOWLObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/network_system.owl#hasOutput");
		
		RDFIndividual Nozzle = NozzleClass.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + filename + "#" + getName() + "_N_" + getN_nozzle());
		RDFIndividual ProcessStream = ProcessStreamClass.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + filename + "#ProcessStream_" + streamNumber);
		RDFIndividual Pipe = PipeClass.createRDFIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/" + filename + "#Pipe_" + streamNumber);
		
		ExchangerInstance.addPropertyValue(hasConnector, Nozzle);
		HeatingProcess.addPropertyValue(hasOutput, ProcessStream);
		Nozzle.addPropertyValue(isDirectlyConnectedTo, Pipe);
		Pipe.addPropertyValue(isDirectlyConnectedTo, Nozzle);
	}
	
	public void inputHeatDuty(JenaOWLModel owlmodel) {
		OWLIndividual HeatDuty = owlmodel.getOWLIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"	+ filename + "#ValueOfHeatDutyOf" + getName());
		OWLDatatypeProperty numericalvalue = owlmodel.getOWLDatatypeProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");
		HeatDuty.setPropertyValue(numericalvalue, getDuty());
	}
	
	public void createOWLFile() {
		Tools.replaceString("Cooling_", "Vaporizing_", "OWL Templates/E-340001_Template.owl", filename);
		Tools.replaceString("http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_function/process.owl#TemperatureChange", "http://www.theworldavatar.com/ontology/ontocape/chemical_process_system/CPS_function/process.owl#PhaseChange", filename);
		Tools.replaceString("E-340001", getName(), filename);
		
		Tools.replaceString("_340008", "_" + getFeedStream(), filename);
		Tools.replaceString("_340009", "_" + getLiqProdStream(), filename);
		Tools.replaceString("_340031", "_" + getUtilFeedStream(), filename);
		Tools.replaceString("_340032", "_" + getUtilProdStream(), filename);
		
		JenaOWLModel owlmodel = Tools.callJena(filename);
		inputHeatDuty(owlmodel);
		createNewOutputStream(owlmodel, getBoilupStream());
		Tools.createIRI(owlmodel);
		Tools.saveJena(owlmodel, filename);
	}
}
