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
import java.util.Scanner;
import java.util.Map;
import java.util.Optional;

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


/** the main function is to replace the coordinate in the template with thee one in csv file*/
public class Test {
	
	//just to switch the coordinate from the template to the real value written in csv
	public static void addCoordinates(String coordinateFile) {
		List<List<String>> table = Tools.csvToList(coordinateFile);
		for (int r=0; r<table.size(); r++) {
			if (table.get(r).contains("Unit Operation")) {
				r++;
				try {
					while (!table.get(r).isEmpty()) {
						String owlFile = table.get(r).get(0) + ".owl";
						Tools.replaceString("2.71828", table.get(r).get(1), owlFile);
						Tools.replaceString("3.1415926", table.get(r).get(2), owlFile);
						r++;
					}
				}
				catch (IndexOutOfBoundsException e) {}
			}
		}
	}

	public static void main(String[] args) {
//		List<List<String>> table = Tools.createTableFromCSV("CSV Files/VBA Attempt.csv");
//		Tools.viewTable(table,4235,4242);
		
		addCoordinates("CSV Files/cumene/Cumene_Plant_GPS.csv");
		System.out.print ("success");
		
	}


}
