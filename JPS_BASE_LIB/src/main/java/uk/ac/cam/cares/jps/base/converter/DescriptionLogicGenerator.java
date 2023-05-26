package uk.ac.cam.cares.jps.base.converter;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.stream.Collectors;

import org.semanticweb.owlapi.apibinding.OWLManager;
import org.semanticweb.owlapi.dlsyntax.renderer.DLSyntaxObjectRenderer;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLEntity;
import org.semanticweb.owlapi.model.OWLOntology;
import org.semanticweb.owlapi.model.OWLOntologyCreationException;
import org.semanticweb.owlapi.model.OWLOntologyManager;
import org.semanticweb.owlapi.util.ShortFormProvider;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;

/**
 * This class generates the description logic syntax from the ontology and save it to
 * a .tex file for LaTeX rendering.
 * 
 * @author Jiaru Bai (jb2197@cam.ac.uk)
 * 
 */
public class DescriptionLogicGenerator {
    public static String NS = "ontology.namespaces.properties";
    public static Map<String, String> ns = new HashMap<String, String>();
    public static ShortFormProvider shortFormProvider = new ShortFormProvider() {
        @Override
        public String getShortForm(OWLEntity owlEntity) {
            return owlEntity.getIRI().getIRIString();
        }
    };

    public static void main(String[] args) {
        try {
            if (args.length != 3) {
                System.out.println(
                    "Usage:\n\tjava -cp jps-base-lib.jar uk.ac.cam.cares.jps.base.converter.DescriptionLogicGenerator --url <ontologyURL> <texFilePath>" +
                    "\nor:" +
                    "\n\tjava -cp jps-base-lib.jar uk.ac.cam.cares.jps.base.converter.DescriptionLogicGenerator --file <ontologyFilePath> <texFilePath>"
                );
                System.exit(0);
            }
            if (args[0].equals("--url")) {
                generateDL(args[1], args[2]);
            } else if (args[0].equals("--file")) {
                generateDL(new File(args[1]), args[2]);
            } else {
                System.out.println(
                    "Usage:\n\tjava -cp jps-base-lib.jar uk.ac.cam.cares.jps.base.converter.DescriptionLogicGenerator --url <ontologyURL> <texFilePath>" +
                    "\nor:" +
                    "\n\tjava -cp jps-base-lib.jar uk.ac.cam.cares.jps.base.converter.DescriptionLogicGenerator --file <ontologyFilePath> <texFilePath>"
                );
                System.exit(0);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Generate the description logic syntax from the ontology URL and save it to the
     * texFilePath
     * 
     * @param ontologyURL
     * @param texFilePath
     * @throws OWLOntologyCreationException
     * @throws IOException
     */
    public static void generateDL(String ontologyURL, String texFilePath) throws OWLOntologyCreationException, IOException {
        OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
        OWLOntology ontology = manager.loadOntologyFromOntologyDocument(IRI.create(ontologyURL));
        generateDL(ontology, texFilePath);
    }

    /**
     * Generate the description logic syntax from the ontology file and save it to
     * the texFilePath
     * 
     * @param ontologyFile
     * @param texFilePath
     * @throws OWLOntologyCreationException
     * @throws IOException
     */
    public static void generateDL(File ontologyFile, String texFilePath) throws OWLOntologyCreationException, IOException {
        OWLOntologyManager manager = OWLManager.createOWLOntologyManager();
        OWLOntology ontology = manager.loadOntologyFromOntologyDocument(ontologyFile);
        generateDL(ontology, texFilePath);
    }

    /**
     * Generate the description logic syntax from the ontology object and save it to the
     * texFilePath
     * 
     * @param ontology
     * @param texFilePath
     * @throws IOException
     */
    public static void generateDL(OWLOntology ontology, String texFilePath) throws IOException {
        DLSyntaxObjectRenderer renderer = new DLSyntaxObjectRenderer();
        renderer.setShortFormProvider(shortFormProvider);
        String dlSyntax = renderer.render(ontology);
        try (BufferedWriter writer = new BufferedWriter(new FileWriter(texFilePath))) {
            writer.write(convertToLatex(dlSyntax));
        } catch (Exception e) {
            throw new JPSRuntimeException(e);
        }
    }

    /**
     * Load the namespaces from the properties file
     */
    public static void initNamespaces() {
        if (ns.isEmpty()) {
            Properties props = new Properties();
            try {
                props.load(DescriptionLogicGenerator.class.getClassLoader().getResourceAsStream(NS));
                ns = props.entrySet().stream().collect(
                    Collectors.toMap(
                        e -> String.valueOf(e.getValue()),
                        e -> String.valueOf(e.getKey()),
                        (prev, next) -> next, HashMap::new
                        )
                    );
            } catch (IOException e) {
                throw new JPSRuntimeException(e);
            }
        }
    }

    /**
     * Convert the description logic syntax to latex format
     * 
     * @param dlSyntax
     * @return
     */
    public static String convertToLatex(String dlSyntax) {
        initNamespaces();
        String latexFormat = "% add \\usepackage{seqsplit} to your preamble \n\\seqsplit{" + dlSyntax
        .replace("⊓", "$\\sqcap$")
        .replace("⊔", "$\\sqcup$")
        .replace("¬", "$\\neg$")
        .replace("≡", "$\\equiv$")
        .replace(">", "$>$")
        .replace("<", "$<$")
        .replace("=", "$=$")
        .replace("⊑", "$\\sqsubseteq$")
        .replace("⊒", "$\\sqsupseteq$")
        .replace("⊤", "$\\top$")
        .replace("⊥", "$\\bot$")
        .replace("∀", "$\\forall$")
        .replace("∃", "$\\exists$")
        .replace("→", "$\\rightarrow$")
        .replace("⇒", "$\\Rightarrow$")
        .replace("⇔", "$\\Leftrightarrow$")
        .replace("⁻", "{$^\\text{--}$}")
        .replace(" ", "~")
        .replace("\n", "} \\\\\n\\seqsplit{")
        .replace("≤", "$\\leq$")
        .replace("≥", "$\\geq$") + "}";
        latexFormat = latexFormat.replace("\\seqsplit{}", "");
        // iterate over the namespaces and replace the namespace with the prefix
        for (Map.Entry<String, String> entry : ns.entrySet()) {
            latexFormat = latexFormat.replace(entry.getKey(), entry.getValue() + ":");
        }
        return latexFormat.replace("_", "\\_");
    }
}
