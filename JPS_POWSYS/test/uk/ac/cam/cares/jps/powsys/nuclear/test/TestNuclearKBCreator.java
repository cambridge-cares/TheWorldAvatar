package uk.ac.cam.cares.jps.powsys.nuclear.test;
import org.apache.jena.ontology.DatatypeProperty;
import org.apache.jena.ontology.Individual;
import org.apache.jena.ontology.ObjectProperty;
import org.apache.jena.ontology.OntClass;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.rdf.model.ModelFactory;
import org.junit.Test;
import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.powsys.nuclear.NuclearGenType;
import uk.ac.cam.cares.jps.powsys.nuclear.NuclearKBCreator;
import uk.ac.cam.cares.jps.powsys.util.Util;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.UnsupportedEncodingException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;


public class TestNuclearKBCreator {
    private final NuclearKBCreator creator = new NuclearKBCreator();

    private static class NuclearOntModelContainer {
        private final OntModel jenaOwlModel;
        private final String iriPrefix;
        private final String name;

        public NuclearOntModelContainer(OntModel jenaOwlModel, String iriPrefix, String name) {
            this.jenaOwlModel = jenaOwlModel;
            this.iriPrefix = iriPrefix;
            this.name = name;
        }

        public Individual get() {
            return jenaOwlModel.getIndividual(iriPrefix + name + ".owl#" + name);
        }

        public Individual getDegree() {
            return jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#degree");
        }

        public Individual getMW() {
            return jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#MW");
        }

        public Individual getLength() {
            return jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/physical_dimension/physical_dimension.owl#length");
        }

        public Individual getTph() {
            return jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#ton_per_hr");
        }

        public Individual getXAxis() {
            return jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#x-axis");
        }

        public Individual getYAxis() {
            return jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#y-axis");
        }

        public Individual getNuclearGeneration() {
            return jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#NuclearGeneration");
        }

        public Individual getNuclear() {
            return jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Nuclear");
        }

        private String formatIri(String resourcePrefix) {
            return String.format("%s%s.owl#%s%s", iriPrefix, name, resourcePrefix, name);
        }
        public Individual getPg() {
            return jenaOwlModel.getIndividual(formatIri("Pg_"));
        }

        public Individual getPgGen() {
            return jenaOwlModel.getIndividual(formatIri("V_Pg_"));
        }

        public Individual getModel() {
            return jenaOwlModel.getIndividual(formatIri("ModelOf"));
        }

        public Individual getCoordinate() {
            return jenaOwlModel.getIndividual(formatIri("CoordinateSystem_of_" ));
        }

        public Individual getXCoordinate() {
            return jenaOwlModel.getIndividual(formatIri("x_coordinate_of_"));
        }

        public Individual getXCoordinateValue() {
            return jenaOwlModel.getIndividual(formatIri("v_x_coordinate_of_"));
        }

        public Individual getYCoordinate() {
            return jenaOwlModel.getIndividual(formatIri("y_coordinate_of_"));
        }

        public Individual getYCoordinateValue() {
            return jenaOwlModel.getIndividual(formatIri("v_y_coordinate_of_"));
        }

        public Individual getDesEmission() {
            return jenaOwlModel.getIndividual(formatIri("Design_CO2_Emission_"));
        }

        public Individual getVDesEmission() {
            return jenaOwlModel.getIndividual(formatIri("V_Design_CO2_Emission_"));
        }

        public Individual getActEmission() {
            return jenaOwlModel.getIndividual(formatIri("Actual_CO2_Emission_"));
        }

        public Individual getVActEmission() {
            return jenaOwlModel.getIndividual(formatIri("V_Actual_CO2_Emission_"));
        }

        public Individual getCapa() {
            return jenaOwlModel.getIndividual(formatIri("capa_of_"));
        }

        public Individual getCapaValue() {
            return jenaOwlModel.getIndividual(formatIri("v_capa_of_"));
        }

        public Individual getCo2Emission() {
            return jenaOwlModel.getIndividual(formatIri("CO2Emission_of_"));
        }

        public Individual getVCo2Emission() {
            return jenaOwlModel.getIndividual(formatIri("v_CO2Emission_of_"));
        }

        public ObjectProperty getHasDimension() {
            return jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasDimension");
        }

        public ObjectProperty getReferTo() {
            return jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl#refersToAxis");
        }

        public ObjectProperty getHasCoordinateSystem() {
            return jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasGISCoordinateSystem");
        }

        public ObjectProperty getHasX() {
            return jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasProjectedCoordinate_x");
        }

        public ObjectProperty getHasY() {
            return jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasProjectedCoordinate_y");
        }

        public ObjectProperty getHasValue() {
            return jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue");
        }

        public ObjectProperty getHasUnit() {
            return jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasUnitOfMeasure");
        }

        public ObjectProperty getIsSubsystemOf() {
            return jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#isSubsystemOf");
        }

        public ObjectProperty getRealizes() {
            return jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#realizes");
        }

        public ObjectProperty getIsModeledBy() {
            return jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#isModeledBy");
        }

        public ObjectProperty getHasModelVariable() {
            return jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable");
        }

        public ObjectProperty getHasEmission() {
            return jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#hasEmission");
        }


        public ObjectProperty getDesignCapacity() {
            return jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_realization.owl#designCapacity");
        }

        public ObjectProperty getConsumePrimaryFuel() {
            return jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#consumesPrimaryFuel");
        }

        public DatatypeProperty getNumVal() {
            return jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");
        }
    }

    private Integer parseIntNullable(String s) {
        try {
            return Integer.parseInt(s);
        } catch (NumberFormatException e) {
            return null;
        }
    }

    @Test
    public void testStartConversion() throws IOException {
        String path = Util.getResourceDir(this);

        Map<String, OntModel> mapIri2Model = creator.startConversion(path);

        String iriPrefix =  QueryBroker.getIriPrefix() + "/nuclearpowerplants/";
        List<String> xs = Arrays.asList("103.719167", "103.698217", "103.698900", "103.683850");
        List<String> ys = Arrays.asList("1.270333", "1.263367", "1.253383", "1.270117");
        List<Double> totalCapacities = Arrays.asList(450.0, 450.0, 1125.0, 1125.0);
        for (Map.Entry<String, OntModel> entry : mapIri2Model.entrySet()) {
            String iri = entry.getKey();
            OntModel jenaOwlModel = entry.getValue();

            assertTrue(iri.startsWith(iriPrefix));
            assertTrue(iri.contains(".owl#"));

            String name = iri.substring(iriPrefix.length(), iri.indexOf(".owl#"));
            if (name.startsWith("NucGenerator_")) {
                Integer plantNum = parseIntNullable(name.substring("NucGenerator_".length(),
                        name.indexOf('_', "NucGenerator_".length())));
                assertNotNull(
                        String.format("generatorName %s is not of the form \"NucGenerator_\" + plantNum + \"_\" + type + reactorNum", name),
                        plantNum);
                assertTrue("plantNum lies outside the expected range 1..4", plantNum >= 1 && plantNum <= 4);

                Individual plant = getPlant(plantNum, iriPrefix);

                assertGeneratorModel(jenaOwlModel, iriPrefix, name, plant, xs.get(plantNum - 1), ys.get(plantNum - 1), 225);
            } else { // must be NucPP_i
                assertTrue(String.format("plantName %s is not of the form \"NucPP_\" + i", name), name.startsWith("NucPP_"));

                Integer plantNum = parseIntNullable(name.substring("NucPP_".length()));
                assertNotNull(String.format("plantName %s is not of the form \"NucPP_\" + i", name), plantNum);
                assertTrue("plantNum lies outside the expected range 1..4", plantNum >= 1 && plantNum <= 4);

                assertEquals("plantIri is not of the form iriPrefix + plantName + \".owl#\" + plantName", iri.substring(iriPrefix.length() + name.length() + ".owl#".length()), name);

                assertPlantModel(jenaOwlModel, iriPrefix, name, xs.get(plantNum - 1), ys.get(plantNum - 1),
                        totalCapacities.get(plantNum - 1));
            }
        }
    }

    @Test
    public void testUnquote() {
        String s = "\"abc\"";
        assertEquals("abc", NuclearKBCreator.unquote(s));
    }

    private ArrayList<NuclearGenType> getSampleGeneratorType() {
        NuclearGenType genType0 = new NuclearGenType("t1");
        genType0.setcapacity(50);
        NuclearGenType genType1 = new NuclearGenType("t2");
        genType1.setcapacity(225);

        ArrayList<NuclearGenType> result = new ArrayList<>();
        result.add(genType0);
        result.add(genType1);
        return result;
    }
    @Test
    public void testExtractInfoForGen() {
        ArrayList<NuclearGenType> expected = getSampleGeneratorType();

        String csvFile = Paths.get(Util.getResourceDir(this), "parameters_req.csv").toString();
        ArrayList<NuclearGenType> actual = creator.extractInformationForGen(csvFile, "0", "3");

        assertEquals(expected, actual);
    }

    private OntModel getPlantGeneratorTemplate() throws FileNotFoundException {
        String plantGeneratorTemplateFile = Paths.get(Util.getResourceDir(this), "/plantgeneratortemplate.owl").toString();
        FileInputStream inFile = new FileInputStream(plantGeneratorTemplateFile);
        Reader in = new InputStreamReader(inFile, StandardCharsets.UTF_8);
        OntModel jenaOwlModel = ModelFactory.createOntologyModel();
        jenaOwlModel.read(in, null);
        return jenaOwlModel;
    }
    private Individual getPlant(int plantNum, String iriPrefix) throws FileNotFoundException {
        OntModel jenaOwlModel = getPlantGeneratorTemplate();
        String plantName = "NucPP_" + plantNum;
        OntClass nuclearPowerPlantClass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#NuclearPlant");
        return nuclearPowerPlantClass.createIndividual(iriPrefix + plantName + ".owl#" + plantName);
    }

    private void assertGeneratorModel(OntModel generatorModel, String iriPrefix, String generatorName, Individual plant,
                                      String x, String y, double capacity) {
        NuclearOntModelContainer container = new NuclearOntModelContainer(generatorModel, iriPrefix, generatorName);

        Individual generator = container.get();
        Individual model = container.getModel();
        Individual genCoordinate = container.getCoordinate();
        Individual nuclearGeneration = container.getNuclearGeneration();
        Individual PgGen = container.getPg();
        Individual desEmission = container.getDesEmission();
        Individual actEmission = container.getActEmission();
        Individual vDesEmission = container.getVDesEmission();
        Individual vActEmission = container.getVActEmission();
        Individual tph = container.getTph();
        Individual xAxis = container.getXAxis();
        Individual yAxis = container.getYAxis();
        Individual xGenCoordinate = container.getXCoordinate();
        Individual xGenCoordinateValue = container.getXCoordinateValue();
        Individual yGenCoordinate = container.getYCoordinate();
        Individual yGenCoordinateValue = container.getYCoordinateValue();
        Individual length = container.getLength();
        Individual degree = container.getDegree();
        Individual PgGenValue = container.getPgGen();
        Individual MW = container.getMW();

        assertNotNull(generator);
        assertNotNull(model);
        assertNotNull(genCoordinate);
        assertNotNull(nuclearGeneration);
        assertNotNull(PgGen);
        assertNotNull(desEmission);
        assertNotNull(actEmission);
        assertNotNull(vDesEmission);
        assertNotNull(vActEmission);
        assertNotNull(tph);
        assertNotNull(xAxis);
        assertNotNull(yAxis);
        assertNotNull(xGenCoordinate);
        assertNotNull(xGenCoordinateValue);
        assertNotNull(yGenCoordinate);
        assertNotNull(yGenCoordinate);
        assertNotNull(length);
        assertNotNull(degree);
        assertNotNull(PgGenValue);
        assertNotNull(MW);

        ObjectProperty isModeledBy = container.getIsModeledBy();
        ObjectProperty isSubsystemOf = container.getIsSubsystemOf();
        ObjectProperty hasCoordinateSystem = container.getHasCoordinateSystem();
        ObjectProperty realizes = container.getRealizes();
        ObjectProperty hasModelVariable = container.getHasModelVariable();
        ObjectProperty hasEmission = container.getHasEmission();
        ObjectProperty hasValue = container.getHasValue();
        ObjectProperty hasUnit = container.getHasUnit();
        ObjectProperty hasX = container.getHasX();
        ObjectProperty hasY = container.getHasY();
        ObjectProperty referTo = container.getReferTo();
        ObjectProperty hasDimension = container.getHasDimension();

        assertNotNull(isModeledBy);
        assertNotNull(isSubsystemOf);
        assertNotNull(hasCoordinateSystem);
        assertNotNull(realizes);
        assertNotNull(hasModelVariable);
        assertNotNull(hasEmission);
        assertNotNull(hasValue);
        assertNotNull(hasUnit);
        assertNotNull(hasX);
        assertNotNull(hasY);
        assertNotNull(referTo);
        assertNotNull(hasDimension);

        DatatypeProperty numVal = container.getNumVal();
        assertNotNull(numVal);

        assertTrue(generator.hasProperty(isModeledBy, model));
        assertTrue(generator.hasProperty(isSubsystemOf, plant));
        assertTrue(generator.hasProperty(hasCoordinateSystem, genCoordinate));
        assertTrue(generator.hasProperty(realizes, nuclearGeneration));

        assertTrue(model.hasProperty(hasModelVariable, PgGen));

        assertTrue(nuclearGeneration.hasProperty(hasEmission, desEmission));
        assertTrue(nuclearGeneration.hasProperty(hasEmission, actEmission));

        assertTrue(desEmission.hasProperty(hasValue, vDesEmission));
        assertTrue(vDesEmission.hasProperty(numVal, generatorModel.createTypedLiteral(0.0)));
        assertTrue(vDesEmission.hasProperty(hasUnit, tph));

        assertTrue(actEmission.hasProperty(hasValue, vActEmission));
        assertTrue(vActEmission.hasProperty(numVal, generatorModel.createTypedLiteral(0.0)));
        assertTrue(vActEmission.hasProperty(hasUnit, tph));

        assertTrue(genCoordinate.hasProperty(hasX, xGenCoordinate));
        assertTrue(xGenCoordinate.hasProperty(hasValue, xGenCoordinateValue));
        assertTrue(xGenCoordinate.hasProperty(referTo, xAxis));
        assertTrue(xGenCoordinate.hasProperty(hasDimension, length));
        assertTrue(xGenCoordinateValue.hasProperty(numVal, generatorModel.createTypedLiteral(x)));
        assertTrue(xGenCoordinateValue.hasProperty(hasUnit, degree));

        assertTrue(genCoordinate.hasProperty(hasY, yGenCoordinate));
        assertTrue(yGenCoordinate.hasProperty(hasValue, yGenCoordinateValue));
        assertTrue(yGenCoordinate.hasProperty(referTo, yAxis));
        assertTrue(yGenCoordinate.hasProperty(hasDimension, length));
        assertTrue(yGenCoordinateValue.hasProperty(numVal, generatorModel.createTypedLiteral(y)));
        assertTrue(yGenCoordinateValue.hasProperty(hasUnit, degree));

        assertTrue(PgGen.hasProperty(hasValue, PgGenValue));
        assertTrue(PgGenValue.hasProperty(numVal, generatorModel.createTypedLiteral(capacity)));
        assertTrue(PgGenValue.hasProperty(hasUnit, MW));
    }
    @Test
    public void testDoConversionReactor() throws UnsupportedEncodingException, FileNotFoundException {
        // arrange
        String iriPrefix = QueryBroker.getIriPrefix() + "/nuclearpowerplants/";
        int plantNum = 1;
        int reactorNum = 0;
        String generatorName = "NucGenerator_" + plantNum + "_A" + reactorNum;
        String x = "1.270333";
        String y = "103.719167";
        double capacity = 200.000000;
        Individual plant = getPlant(plantNum, iriPrefix);

        // act
        OntModel generatorModel = creator.doConversionreactor(iriPrefix, generatorName, x, y, capacity, plant);

        // assert
        assertGeneratorModel(generatorModel, iriPrefix, generatorName, plant, x, y, capacity);
    }

    private void assertPlantModel(OntModel plantModel, String iriPrefix, String plantName, String x, String y,
                                  double totalCapacity) {
        NuclearOntModelContainer container = new NuclearOntModelContainer(plantModel, iriPrefix, plantName);

        Individual plant = container.get();
        Individual plantCoordinate = container.getCoordinate();
        Individual xCoordinate = container.getXCoordinate();
        Individual yCoordinate = container.getYCoordinate();
        Individual xCoordinateValue = container.getXCoordinateValue();
        Individual yCoordinateValue = container.getYCoordinateValue();
        Individual xAxis = container.getXAxis();
        Individual yAxis = container.getYAxis();
        Individual length = container.getLength();
        Individual degree = container.getDegree();
        Individual capa = container.getCapa();
        Individual capaValue = container.getCapaValue();
        Individual MW = container.getMW();
        Individual nuclearGeneration = container.getNuclearGeneration();
        Individual nuclear = container.getNuclear();
        Individual co2Emission = container.getCo2Emission();
        Individual vCo2Emission = container.getVCo2Emission();
        Individual tph = container.getTph();

        assertNotNull(plant);
        assertNotNull(plantCoordinate);
        assertNotNull(xCoordinate);
        assertNotNull(yCoordinate);
        assertNotNull(xCoordinateValue);
        assertNotNull(yCoordinateValue);
        assertNotNull(xAxis);
        assertNotNull(yAxis);
        assertNotNull(length);
        assertNotNull(degree);
        assertNotNull(capa);
        assertNotNull(capaValue);
        assertNotNull(MW);
        assertNotNull(nuclearGeneration);
        assertNotNull(nuclear);
        assertNotNull(co2Emission);
        assertNotNull(tph);

        ObjectProperty hasCoordinateSystem = container.getHasCoordinateSystem();
        ObjectProperty hasX = container.getHasX();
        ObjectProperty hasY = container.getHasY();
        ObjectProperty hasValue = container.getHasValue();
        ObjectProperty referTo = container.getReferTo();
        ObjectProperty hasDimension = container.getHasDimension();
        ObjectProperty hasUnit = container.getHasUnit();
        ObjectProperty designCapacity = container.getDesignCapacity();
        ObjectProperty realizes = container.getRealizes();
        ObjectProperty consumePrimaryFuel = container.getConsumePrimaryFuel();
        ObjectProperty hasEmission = container.getHasEmission();

        assertNotNull(hasCoordinateSystem);
        assertNotNull(hasX);
        assertNotNull(hasValue);
        assertNotNull(referTo);
        assertNotNull(hasDimension);
        assertNotNull(hasUnit);
        assertNotNull(designCapacity);
        assertNotNull(realizes);
        assertNotNull(consumePrimaryFuel);
        assertNotNull(hasEmission);

        DatatypeProperty numVal = container.getNumVal();
        assertNotNull(numVal);

        assertTrue(plant.hasProperty(hasCoordinateSystem, plantCoordinate));

        assertTrue(plantCoordinate.hasProperty(hasX, xCoordinate));
        assertTrue(xCoordinate.hasProperty(hasValue, xCoordinateValue));
        assertTrue(xCoordinate.hasProperty(referTo, xAxis));
        assertTrue(xCoordinate.hasProperty(hasDimension, length));
        assertTrue(xCoordinateValue.hasProperty(numVal, plantModel.createTypedLiteral(x)));
        assertTrue(xCoordinateValue.hasProperty(hasUnit, degree));

        assertTrue(plantCoordinate.hasProperty(hasY, yCoordinate));
        assertTrue(yCoordinate.hasProperty(hasValue, yCoordinateValue));
        assertTrue(yCoordinate.hasProperty(referTo, yAxis));
        assertTrue(yCoordinate.hasProperty(hasDimension, length));
        assertTrue(yCoordinateValue.hasProperty(numVal, plantModel.createTypedLiteral(y)));
        assertTrue(yCoordinateValue.hasProperty(hasUnit, degree));

        assertTrue(plant.hasProperty(designCapacity, capa));
        assertTrue(capa.hasProperty(hasValue, capaValue));
        assertTrue(capaValue.hasProperty(numVal, plantModel.createTypedLiteral(totalCapacity)));
        assertTrue(capaValue.hasProperty(hasUnit, MW));

        assertTrue(plant.hasProperty(realizes, nuclearGeneration));
        assertTrue(nuclearGeneration.hasProperty(consumePrimaryFuel, nuclear));
        assertTrue(nuclearGeneration.hasProperty(hasEmission, co2Emission));
        assertTrue(co2Emission.hasProperty(hasValue, vCo2Emission));
        assertTrue(vCo2Emission.hasProperty(numVal, plantModel.createTypedLiteral((0.0))));
        assertTrue(vCo2Emission.hasProperty(hasUnit, tph));
    }
    @Test
    public void testDoConversion() throws IOException {
        // arrange
        Map<String, OntModel> mapIri2Model = new HashMap<>();
        OntModel jenaOwlModel = getPlantGeneratorTemplate();
        String iriPrefix = QueryBroker.getIriPrefix() + "/nuclearpowerplants/";
        int plantNum = 1;
        String plantName = "NucPP_" + plantNum;
        int numReactorA = 2; // t1
        int numReactorB = 0; // t2
        String x = "1.270333";
        String y = "103.719167";
        ArrayList<NuclearGenType> generatorType = getSampleGeneratorType();

        // act
        creator.initOWLClasses(jenaOwlModel);
        creator.doConversion(mapIri2Model, jenaOwlModel, iriPrefix, plantName, numReactorA, numReactorB, x, y,
                generatorType, plantNum);

        // assert
        assertPlantModel(jenaOwlModel, iriPrefix, plantName, x, y, 100.0);

        Individual plant = getPlant(plantNum, iriPrefix);
        for (int i = 0; i < numReactorA; i++) {
            String generatorName = "NucGenerator_" + plantNum + "_A" + i;
            String generatorIri = iriPrefix + generatorName + ".owl#" + generatorName;
            OntModel generatorModel = mapIri2Model.get(generatorIri);

            assertNotNull(generatorModel);
            assertGeneratorModel(generatorModel, iriPrefix, generatorName, plant, x, y, 50);
        }
    }
}
