package uk.ac.cam.cares.jps.powsys.nuclear;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.jena.ontology.DatatypeProperty;
import org.apache.jena.ontology.Individual;
import org.apache.jena.ontology.ObjectProperty;
import org.apache.jena.ontology.OntClass;
import org.apache.jena.ontology.OntModel;
import org.apache.jena.rdf.model.ModelFactory;

import uk.ac.cam.cares.jps.base.query.QueryBroker;
import uk.ac.cam.cares.jps.base.util.MatrixConverter;
import uk.ac.cam.cares.jps.powsys.util.Util;

public class NuclearKBCreator {

    String plantname=null;

    static Individual nuclear;

    static Individual m;
    static Individual degree;
    static Individual MW;
    static Individual length;
    static Individual tph;

    static Individual xaxis;
    static Individual yaxis;

    public static final String GAMS_OUTPUT_FILENAME = "results.csv";

    private OntClass nuclearpowerplantclass = null;
    private OntClass CO2_emissionclass = null;
    private OntClass coordinateclass = null;
    private OntClass coordinatesystemclass = null;
    private OntClass valueclass = null;
    private OntClass scalarvalueclass = null;

    private OntClass designcapacityclass = null;
    //private OntClass generatedactivepowerclass=null;
    private OntClass nucleargeneratorclass = null;
    private OntClass Pgclass = null;
    private OntClass modelclass = null;


    private ObjectProperty hasdimension = null;
    private ObjectProperty referto = null;
    private ObjectProperty hascoordinatesystem = null;
    private ObjectProperty hasx = null;
    private ObjectProperty hasy = null;
    private ObjectProperty hasvalue = null;
    private ObjectProperty hasunit = null;
    private ObjectProperty hasaddress = null;
    private ObjectProperty isownedby = null;
    private ObjectProperty isSubsystemOf= null;
    private ObjectProperty designcapacity = null;
    private ObjectProperty hasyearofbuilt = null;
    private ObjectProperty realizes = null;
    private ObjectProperty isModeledby = null;
    private ObjectProperty hasModelVariable = null;

    private ObjectProperty consumesprimaryfuel = null;
    private ObjectProperty hasEmission = null;
    private ObjectProperty hascosts = null;
    private ObjectProperty hasannualgeneration = null;
    private ObjectProperty usesgenerationtechnology = null;

    private ObjectProperty hasSubsystem = null;
    //private ObjectProperty hasActivepowergenerated=null;

    private DatatypeProperty numval = null;
    private DatatypeProperty hasname = null;
    private BufferedReader br;

    public void initOWLClasses(OntModel jenaOwlModel) {
        nuclearpowerplantclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#NuclearPlant");
        nucleargeneratorclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontopowsys/PowSysRealization.owl#NuclearGenerator");
        coordinateclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#StraightCoordinate");
        coordinatesystemclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#ProjectedCoordinateSystem");
        valueclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl#CoordinateValue");
        scalarvalueclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#ScalarValue");
        designcapacityclass = jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_realization.owl#DesignCapacity");
        //generatedactivepowerclass=jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#GeneratedActivePower");
        Pgclass=jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#Pg");
        modelclass=jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontopowsys/model/PowerSystemModel.owl#PowerFlowModelAgent");
        CO2_emissionclass=jenaOwlModel.getOntClass("http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#CO2_emission");

        consumesprimaryfuel = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#consumesPrimaryFuel");
        hasdimension = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasDimension");
        referto = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/coordinate_system.owl#refersToAxis");
        hascoordinatesystem = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasGISCoordinateSystem");
        hasx = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasProjectedCoordinate_x");
        hasy = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time_extended.owl#hasProjectedCoordinate_y");
        hasvalue = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasValue");
        hasunit = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasUnitOfMeasure");
        designcapacity = jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_realization.owl#designCapacity");
        //hasActivepowergenerated=jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontopowsys/PowSysBehavior.owl#hasActivePowerGenerated");
        realizes=jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/technical_system.owl#realizes");
        hasSubsystem=jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#hasSubsystem");
        isModeledby=jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#isModeledBy");
        isSubsystemOf=jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#isSubsystemOf");
        hasModelVariable=jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontocape/model/mathematical_model.owl#hasModelVariable");
        hasEmission=jenaOwlModel.getObjectProperty("http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#hasEmission");

        numval = jenaOwlModel.getDatatypeProperty("http://www.theworldavatar.com/ontology/ontocape/upper_level/system.owl#numericalValue");
        nuclear = jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#Nuclear");
        MW=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#MW");
        degree=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#degree");
        length=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/physical_dimension/physical_dimension.owl#length");
        xaxis=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#x-axis");
        yaxis=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/space_and_time/space_and_time.owl#y-axis");
        tph=jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#ton_per_hr");
    }

    public Map<String, OntModel> startConversion(String baseUrl) throws NumberFormatException, IOException {

        Map<String, OntModel> mapIri2Model = new HashMap<String, OntModel>();

        //String iriprefix="http://www.theworldavatar.com/kbs/sgp/jurongisland/nuclearpowerplants/";
        String iriprefix = QueryBroker.getIriPrefix() + "/nuclearpowerplants/";

        ArrayList<NuclearGenType> generatortype=extractInformationForGen(baseUrl + "/parameters_req.csv", "0","3");

        String csvfileoutput = baseUrl + "/" + GAMS_OUTPUT_FILENAME;
//	   	IriMapper map2=new IriMapper();
//	    List<IriMapping> original=map2.deserialize(csvfileoutput);

        //reading from output file and put that to owl file
        String csv = new QueryBroker().readFileLocal(csvfileoutput);
        List<String[]> simulationResult = MatrixConverter.fromCsvToArray(csv);

        for (int i=1; i<simulationResult.size(); i++) {
            String[] dataWithQuotes = simulationResult.get(i);

            String[] data = new String[dataWithQuotes.length];
            for (int j=0; j<dataWithQuotes.length; j++) {
                data[j] = unquote(dataWithQuotes[j]);
            }

            String resourceDir = Util.getResourceDir(this);
            String filePath = resourceDir + "/plantgeneratortemplate.owl"; // the empty owl file

            FileInputStream inFile = new FileInputStream(filePath);
            Reader in = new InputStreamReader(inFile, "UTF-8");

            OntModel jenaOwlModel = ModelFactory.createOntologyModel();
            jenaOwlModel.read(in, null);

            initOWLClasses(jenaOwlModel);

            //assume 1 line is 1 nuclear power plant and 1 nuclear powerplant is a plant with uniform type of reactor in 1 area
            String plantName = "NucPP_" + i;
            if(data[1].equals("t1")) {
                doConversion(mapIri2Model, jenaOwlModel, iriprefix, plantName, Integer.valueOf(data[2]),0, data[4],data[5],generatortype, i); // plant,iriprefix,nreactora,nreactorb,x,y
            }
            else if(data[1].equals("t2")) {
                doConversion(mapIri2Model, jenaOwlModel, iriprefix, plantName, 0,Integer.valueOf(data[2]), data[4],data[5],generatortype, i); // plant,iriprefix,nreactora,nreactorb,x,y
            }

            String plantIri = iriprefix + plantName + ".owl#" + plantName;
            mapIri2Model.put(plantIri, jenaOwlModel);
        }

        return mapIri2Model;
    }

    public static String unquote(String s) {
        String result = s;
        if (result.startsWith("\"")) {
            result = result.substring(1);
        }
        if (result.endsWith("\"")) {
            result = result.substring(0, result.length()-1);
        }
        return result;
    }
	
	
	/*public ArrayList<LandlotObjectType> extractInformationForLots(String csvfileinputlandlot, String indexinput1,String indexinput2,String indexinput3) throws NumberFormatException, IOException {
		String line = "";
        String cvsSplitBy = ",";
        int linereader=0;
        ArrayList<LandlotObjectType> lotlisted= new ArrayList<LandlotObjectType>();
        br = new BufferedReader(new FileReader(csvfileinputlandlot));
			while ((line = br.readLine()) != null) {
				if(linereader==0) {
	        		System.out.println("skipped because it's header");
	        	}
				else {
				String[] data = line.split(cvsSplitBy);
				LandlotObjectType lots= new LandlotObjectType(data[Integer.valueOf(indexinput1)]); //should be 0
				lots.setx(Double.valueOf(data[Integer.valueOf(indexinput2)])); //should be 2
				lots.sety(Double.valueOf(data[Integer.valueOf(indexinput3)]));//should be 1
				lotlisted.add(lots);
				}
				linereader++;
				
			}
			System.out.println("lots info are captured");
			return lotlisted;
	}*/

    public ArrayList<NuclearGenType> extractInformationForGen(String csvfileinputparam, String indexinput1,
                                                              String indexinput2)
            throws NumberFormatException {
        /**some documentation:
         *Co= capital cost of single unit reactor (million $)
         *UAo= minimum area of single unit reactor (m2)
         *Fo= capacity of single unit reactor (MW)
         *Q= cooling water needed of single unit reactor (m3/h)
         *---------------------------------------------------------------
         *L= project life span (yr)
         *Ds= discount rate
         *alpha= discount to loss factor (/mMW/yr)
         *ro= neighborhood radius for 1MW plant (m)
         *FP= probability of reactor failure
         *Hu= value of human life ($)
         */

        ArrayList<NuclearGenType> nucleargeneratorlisted = new ArrayList<NuclearGenType>();

        String csv = new QueryBroker().readFileLocal(csvfileinputparam);
        List<String[]> genTypes = MatrixConverter.fromCsvToArray(csv);
        for (int i=1; i<genTypes.size(); i++) {
            String[] data = genTypes.get(i);
            NuclearGenType nuclear= new NuclearGenType(data[Integer.valueOf(indexinput1)]);//should be 0
            nuclear.setcapacity(Double.valueOf(data[Integer.valueOf(indexinput2)]));//should be 3
            nucleargeneratorlisted.add(nuclear);
        }

        System.out.println("generators info are captured");
        return nucleargeneratorlisted;
    }

    public OntModel doConversionreactor(String iriprefix, String generatorname, String xnumval, String ynumval,
                                        double capacity, Individual plant)
            throws FileNotFoundException, UnsupportedEncodingException {

        String resourceDir = Util.getResourceDir(this);
        String filePath = resourceDir + "/plantgeneratortemplate.owl"; // the empty owl file
        FileInputStream inFile = new FileInputStream(filePath);
        Reader in = new InputStreamReader(inFile, "UTF-8");

        OntModel jenaOwlModel2 = ModelFactory.createOntologyModel();
        jenaOwlModel2.read(in, null);

        initOWLClasses(jenaOwlModel2);
        Individual powergeneration = jenaOwlModel2.getIndividual("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#NuclearGeneration");
        Individual Pggen = Pgclass.createIndividual(iriprefix + generatorname + ".owl#Pg_"+generatorname);
        Individual Pggenvalue = scalarvalueclass.createIndividual(iriprefix + generatorname + ".owl#V_Pg_"+generatorname);

        Individual generator=nucleargeneratorclass.createIndividual(iriprefix + generatorname + ".owl#"+generatorname);
        Individual model=modelclass.createIndividual(iriprefix + generatorname + ".owl#ModelOf"+generatorname);
        Individual gencoordinate = coordinatesystemclass.createIndividual(iriprefix + generatorname + ".owl#CoordinateSystem_of_"+generatorname);
        Individual xgencoordinate = coordinateclass.createIndividual(iriprefix + generatorname + ".owl#x_coordinate_of_"+generatorname);
        Individual ygencoordinate = coordinateclass.createIndividual(iriprefix + generatorname+ ".owl#y_coordinate_of_"+generatorname);
        Individual xgencoordinatevalue = valueclass.createIndividual(iriprefix + generatorname + ".owl#v_x_coordinate_of_"+generatorname);
        Individual ygencoordinatevalue = valueclass.createIndividual(iriprefix + generatorname + ".owl#v_y_coordinate_of_"+generatorname);

        generator.addProperty(isModeledby, model);
        generator.addProperty(isSubsystemOf, plant);
        model.addProperty(hasModelVariable, Pggen);
        generator.addProperty(hascoordinatesystem, gencoordinate);
        generator.addProperty(realizes, powergeneration);
        OntClass emissionclass = jenaOwlModel2.getOntClass("http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#Actual_CO2_Emission");
        OntClass designemissionclass = jenaOwlModel2.getOntClass("http://www.theworldavatar.com/ontology/ontoeip/system_aspects/system_performance.owl#CO2_emission");
        Individual desemission = designemissionclass.createIndividual(iriprefix + generatorname + ".owl#Design_CO2_Emission_"+generatorname);
        Individual vdesemission = scalarvalueclass.createIndividual(iriprefix + generatorname + ".owl#V_Design_CO2_Emission_"+generatorname);
        Individual actemission = emissionclass.createIndividual(iriprefix + generatorname + ".owl#Actual_CO2_Emission_"+generatorname);
        Individual vactemission = scalarvalueclass.createIndividual(iriprefix + generatorname + ".owl#V_Actual_CO2_Emission_"+generatorname);
        powergeneration.addProperty(hasEmission,desemission);
        desemission.addProperty(hasvalue,vdesemission);
        vdesemission.setPropertyValue(numval, jenaOwlModel2.createTypedLiteral(new Double(0.0)));
        Individual tph = jenaOwlModel2.getIndividual("http://www.theworldavatar.com/ontology/ontocape/supporting_concepts/SI_unit/derived_SI_units.owl#ton_per_hr");
        vdesemission.addProperty(hasunit,tph);
        powergeneration.addProperty(hasEmission,actemission);
        actemission.addProperty(hasvalue,vactemission);
        vactemission.setPropertyValue(numval, jenaOwlModel2.createTypedLiteral(new Double(0.0)));
        vactemission.addProperty(hasunit,tph);

        gencoordinate.addProperty(hasx, xgencoordinate);
        xgencoordinate.addProperty(hasvalue, xgencoordinatevalue);
        xgencoordinate.addProperty(referto, xaxis);
        xgencoordinate.addProperty(hasdimension, length);
        xgencoordinatevalue.addProperty(numval, jenaOwlModel2.createTypedLiteral(xnumval));
        xgencoordinatevalue.addProperty(hasunit, degree);

        gencoordinate.addProperty(hasy, ygencoordinate);
        ygencoordinate.addProperty(hasvalue, ygencoordinatevalue);
        ygencoordinate.addProperty(referto, yaxis);
        ygencoordinate.addProperty(hasdimension, length);
        ygencoordinatevalue.addProperty(numval, jenaOwlModel2.createTypedLiteral(ynumval));
        ygencoordinatevalue.addProperty(hasunit, degree);

        Pggen.addProperty(hasvalue, Pggenvalue);
        Pggenvalue.setPropertyValue(numval, jenaOwlModel2.createTypedLiteral(new Double(capacity)));
        Pggenvalue.addProperty(hasunit, MW);

        return jenaOwlModel2;
    }

    public void doConversion(Map<String, OntModel> mapIri2Model, OntModel jenaOwlModel, String iriprefix,
                             String plantname, int numberofreactorA, int numberofreactorB, String xnumval,
                             String ynumval, ArrayList<NuclearGenType> generatortype, int plantnumber)
            throws NumberFormatException, IOException {


        double capacityA=0.0;
        double capacityB=0.0;
        int difftype=generatortype.size();
        for(int b=0;b<difftype;b++) {
            if(numberofreactorA>0||numberofreactorB>0){
                if(generatortype.get(b).getnucleargen().contentEquals("t1")) {
                    capacityA=generatortype.get(b).getcapacity();
                }
                else if(generatortype.get(b).getnucleargen().contentEquals("t2")){
                    capacityB=generatortype.get(b).getcapacity();
                }
            }

        }

        double totalcapacityA=numberofreactorA*capacityA;
        double totalcapacityB=numberofreactorB*capacityB;
        double totalcapacity=totalcapacityA+totalcapacityB;

        Individual plant = nuclearpowerplantclass.createIndividual(iriprefix + plantname + ".owl#"+plantname);

        Individual plantcoordinate = coordinatesystemclass.createIndividual(iriprefix + plantname + ".owl#CoordinateSystem_of_"+plantname);
        Individual xcoordinate = coordinateclass.createIndividual(iriprefix + plantname + ".owl#x_coordinate_of_"+plantname);
        Individual ycoordinate = coordinateclass.createIndividual(iriprefix + plantname + ".owl#y_coordinate_of_"+plantname);
        Individual xcoordinatevalue = valueclass.createIndividual(iriprefix + plantname + ".owl#v_x_coordinate_of_"+plantname);
        Individual ycoordinatevalue = valueclass.createIndividual(iriprefix + plantname + ".owl#v_y_coordinate_of_"+plantname);

        Individual capa = designcapacityclass.createIndividual(iriprefix + plantname + ".owl#capa_of_"+plantname);
        Individual capavalue = scalarvalueclass.createIndividual(iriprefix + plantname + ".owl#v_capa_of_"+plantname);

        Individual powergeneration = jenaOwlModel.getIndividual("http://www.theworldavatar.com/ontology/ontoeip/powerplants/PowerPlant.owl#NuclearGeneration");

        Individual co2emission = CO2_emissionclass.createIndividual(iriprefix + plantname + ".owl#CO2Emission_of_"+plantname);
        Individual vco2emission = scalarvalueclass.createIndividual(iriprefix + plantname + ".owl#v_CO2Emission_of_"+plantname);

        plant.addProperty(hascoordinatesystem, plantcoordinate);

        plantcoordinate.addProperty(hasx, xcoordinate);
        xcoordinate.addProperty(hasvalue, xcoordinatevalue);
        xcoordinate.addProperty(referto, xaxis);
        xcoordinate.addProperty(hasdimension, length);
        xcoordinatevalue.addProperty(numval, jenaOwlModel.createTypedLiteral(xnumval));
        xcoordinatevalue.addProperty(hasunit, degree);

        plantcoordinate.addProperty(hasy, ycoordinate);
        ycoordinate.addProperty(hasvalue, ycoordinatevalue);
        ycoordinate.addProperty(referto, yaxis);
        ycoordinate.addProperty(hasdimension, length);
        ycoordinatevalue.addProperty(numval, jenaOwlModel.createTypedLiteral(ynumval));
        ycoordinatevalue.addProperty(hasunit, degree);

        plant.addProperty(designcapacity, capa);
        capa.addProperty(hasvalue, capavalue);
        capavalue.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double (totalcapacity)));
        capavalue.addProperty(hasunit, MW);

        plant.addProperty(realizes, powergeneration);
        powergeneration.addProperty(consumesprimaryfuel, nuclear);
        powergeneration.addProperty(hasEmission, co2emission);
        co2emission.addProperty(hasvalue, vco2emission);
        vco2emission.setPropertyValue(numval, jenaOwlModel.createTypedLiteral(new Double (0.0)));
        vco2emission.addProperty(hasunit, tph);

        for(int f=0; f<numberofreactorA;f++){
            String generatorname="NucGenerator_" + plantnumber + "_A" + f;
            String reactorIri = iriprefix + generatorname + ".owl#" + generatorname;


            OntModel reactorModel = doConversionreactor(iriprefix, generatorname, xnumval, ynumval, capacityA,plant);
            mapIri2Model.put(reactorIri, reactorModel);

            Individual generator=nucleargeneratorclass.createIndividual(reactorIri);
            plant.addProperty(hasSubsystem, generator);
        }

        for(int f=0; f<numberofreactorB;f++){
            String generatorname="NucGenerator_" + plantnumber + "_B" + f;
            String reactorIri = iriprefix + generatorname + ".owl#" + generatorname;


            OntModel reactorModel = doConversionreactor(iriprefix, generatorname, xnumval, ynumval, capacityB,plant);
            mapIri2Model.put(reactorIri, reactorModel);
            Individual generator=nucleargeneratorclass.createIndividual(reactorIri);
            plant.addProperty(hasSubsystem, generator);
        }
    }
}
