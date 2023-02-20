package uk.ac.cam.cares.jps.powsys.nuclear.test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.InputStreamReader;
import java.io.Reader;
import org.apache.jena.ontology.DatatypeProperty;
import org.apache.jena.ontology.Individual;
import org.apache.jena.ontology.ObjectProperty;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.rdf.model.ModelFactory;
import org.junit.BeforeClass;
import org.junit.Test;

import uk.ac.cam.cares.jps.powsys.nuclear.LandlotsKB;
import uk.ac.cam.cares.jps.powsys.util.Util;

public class TestLandlotsKB {
    static LandlotsKB kb;
    DatatypeProperty numval = null;
    ObjectProperty hasProperty;
    ObjectProperty hasValue;
    ObjectProperty hascoordinatesystem;
    ObjectProperty hasx;
    ObjectProperty hasy;
    ObjectProperty hasdistance;
    ObjectProperty hasunit;
    ObjectProperty hasarea;
    ObjectProperty hassurfacegeometry;

    @BeforeClass
    public static void setupBeforeClass() {
        kb = new LandlotsKB();
    }

    private void initProperty(OntModel jenaOwlModel) {
        numval = jenaOwlModel.getDatatypeProperty(
                "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");
        hasProperty = jenaOwlModel.getObjectProperty(
                "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasProperty");
        hasValue = jenaOwlModel
                .getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue");
        hascoordinatesystem = jenaOwlModel.getObjectProperty(
                "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasGISCoordinateSystem");
        hasx = jenaOwlModel.getObjectProperty(
                "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasProjectedCoordinate_x");
        hasy = jenaOwlModel.getObjectProperty(
                "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasProjectedCoordinate_y");
        hasdistance = jenaOwlModel.getObjectProperty(
                "http://www.theworldavatar.com/ontology/ontoland/OntoLand.owl#hasDistanceToClosestWaterSources");
        hasunit = jenaOwlModel.getObjectProperty(
                "http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasUnitOfMeasure");
        hasarea = jenaOwlModel.getObjectProperty(
                "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/geometry/geometry.owl#has_area");
        hassurfacegeometry = jenaOwlModel.getObjectProperty(
                "http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/geometry/geometry.owl#hasSurfaceGeometry");
    }

    @Test
    public void testDoConversion() throws Exception {
        String baseURL = Util.getResourceDir(this);
        String filePath = baseURL + "/LotsTemplate.owl"; // the empty owl file
        FileInputStream inFile = new FileInputStream(filePath);
        Reader in = new InputStreamReader(inFile, "UTF-8");

        OntModel jenaOwlModel = ModelFactory.createOntologyModel();
        jenaOwlModel.read(in, null);
        kb.initOWLClasses(jenaOwlModel);

        kb.doConversion(jenaOwlModel);

        initProperty(jenaOwlModel);

        String csvFile = Util.getResourceDir(this) + "/Landlots.csv";
        String line = "";
        String cvsSplitBy = ",";
        int linereader = 0;
        BufferedReader br = new BufferedReader(new FileReader(csvFile));
        while ((line = br.readLine()) != null) {
            if (linereader == 0) {
                System.out.println("skipped because it's header");
            } else {
                String[] iri = line.split(cvsSplitBy);
                Individual lotsid = jenaOwlModel.getIndividual("http://www.jparksimulator.com/kb/sgp/jurongisland/"
                        + LandlotsKB.filename + ".owl#Lots-ID" + iri[0]);
                assertNotNull(lotsid);

                assertEquals(Double.parseDouble(iri[2]),
                        lotsid.getProperty(hascoordinatesystem).getResource()
                                .getProperty(hasx).getResource()
                                .getProperty(hasValue).getResource()
                                .getProperty(numval).getDouble(),
                        0.001);

                assertEquals(Double.parseDouble(iri[1]),
                        lotsid.getProperty(hascoordinatesystem).getResource()
                                .getProperty(hasy).getResource()
                                .getProperty(hasValue).getResource()
                                .getProperty(numval).getDouble(),
                        0.001);

                assertEquals(Double.parseDouble(iri[3]),
                        lotsid.getProperty(hassurfacegeometry).getResource()
                                .getProperty(hasarea).getResource()
                                .getProperty(hasValue).getResource()
                                .getProperty(numval).getDouble(),
                        0.001);

                assertEquals(Double.parseDouble(iri[4]),
                        lotsid.getProperty(hasdistance).getResource()
                                .getProperty(hasValue).getResource()
                                .getProperty(numval).getDouble(),
                        0.001);
            }
            linereader++;
        }

        br.close();
    }

}
