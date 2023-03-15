package uk.ac.cam.cares.jps.agent.historicalntuenergy;
import com.bigdata.service.IService;
import org.json.JSONArray;
import org.json.JSONObject;
import org.jooq.exception.DataAccessException;
import uk.ac.cam.cares.jps.base.util.JSONKeyToIRIMapper;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeries;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesClient;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesSparql;

import java.text.SimpleDateFormat;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.time.*;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.stream.Collectors;
import java.util.zip.GZIPInputStream;

import javax.lang.model.util.ElementScanner6;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;

import org.json.JSONArray;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import org.eclipse.rdf4j.sparqlbuilder.core.query.DeleteDataQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.InsertDataQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;



import org.eclipse.rdf4j.query.*;
import org.eclipse.rdf4j.query.Update;
import org.eclipse.rdf4j.repository.*;
import org.eclipse.rdf4j.repository.http.*;
import org.eclipse.rdf4j.model.*;
import org.eclipse.rdf4j.model.impl.*;
import org.eclipse.rdf4j.sparqlbuilder.core.*;
import org.eclipse.rdf4j.sparqlbuilder.core.query.*;

public class HistoricalQueryBuilder {

    private static final Logger LOGGER = LogManager.getLogger(HistoricalNTUEnergyAgentLauncher.class);

    /**
     * Prefixes
     */
    private static final Prefix PREFIX_OM = SparqlBuilder.prefix(iri("http://www.ontology-of-units-of-measure.org/resource/om-2/"));
    private static final Prefix PREFIX_EX = SparqlBuilder.prefix(iri("https://www.theworldavatar.com/kg/ontobuiltenv/"));
    private static final Prefix PREFIX_POWSYS = SparqlBuilder.prefix(iri("https://www.theworldavatar.com/kg/ontopowsys/"));
    //private static final Prefix PREFIX_UOM = SparqlBuilder.prefix(iri("https://www.theworldavatar.com/kg/ontouom/"));

    public static final String generatedIRIPrefix = "https://www.theworldavatar.com/kg/ontotimeseries/" + "ntuenergy";

    public String queryEndpoint;
    public String updateEndpoint;

    /**
     * Classes
     */
    public static final String activePower = "https://www.theworldavatar.com/kg/ontopowsys/" + "#ActivePower";
    public static final String reactivePower = "https://www.theworldavatar.com/kg/ontopowsys/" + "#ReactivePower";


    /**
     * Relationships
     */
    private static final Iri hasValue = PREFIX_OM.iri("hasValue");
    private static final Iri hasUnit = PREFIX_OM.iri("hasUnit");
    private static final Iri hasPowerReading = PREFIX_POWSYS.iri("hasPowerProperty");
    private static final Iri hasPropertyUsage = PREFIX_EX.iri("hasPropertyUsage");
    private static final Iri hasAddress = PREFIX_EX.iri("hasAddress");


    /**
     * Individuals
     */
    private static final Iri kilowatt = PREFIX_OM.iri("killowatt");
    private static final Iri KVAR = PREFIX_OM.iri("kVAR");
    private static final Iri Measure = PREFIX_OM.iri("Measure");
    private static final Iri ActivePower = PREFIX_POWSYS.iri("ActivePower");
    private static final Iri ReactivePower = PREFIX_POWSYS.iri("ReactivePower");

    RemoteStoreClient kbClient;
    public String agentProperties;
    public String clientProperties;
    public JSONObject readings;

    private List<JSONKeyToIRIMapper> mappings;

    public HistoricalQueryBuilder(String agentProp, String clientProp) throws IOException
    {
        agentProperties = agentProp;
        clientProperties = clientProp;

        loadconfigs(clientProperties);
        //readings endpoints from client.properties

        loadproperties(agentProperties);

        kbClient = new RemoteStoreClient();
        kbClient.setUpdateEndpoint(updateEndpoint);
        kbClient.setQueryEndpoint(queryEndpoint);
    }

    public void loadconfigs(String filepath) throws IOException
    {
        File file = new File(filepath);
        if(!file.exists())
        {
            throw new FileNotFoundException("There was no file found in the path");
        }

        try(InputStream input = new FileInputStream(file))
        {
            Properties prop = new Properties();
            prop.load(input);

            if(prop.containsKey("sparql.query.endpoint"))
            {
                queryEndpoint = prop.getProperty("sparql.query.endpoint");
            }
            else
            {
                throw new IOException("The file is missing: \"sparql.query.endpoint=<queryEndpoint>\"");
            }

            if(prop.containsKey("sparql.update.endpoint"))
            {
                updateEndpoint = prop.getProperty("sparql.update.endpoint");
            }
            else
            {
                throw new IOException("The file is missing: \"sparql.update.endpoint=<updateEndpoint>\"");
            }
        }
    }

    public void loadproperties(String propfile) throws IOException
    {
        try(InputStream input = new FileInputStream(propfile))
        {
            Properties prop = new Properties();
            prop.load(input);
            String mappingfolder;
            try
            {
                mappingfolder = System.getenv(prop.getProperty("ntuenergy.mappingfolder"));
            }
            catch(NullPointerException e)
            {
                throw new IOException("The key ntuenergy.mappingfolder cannot be found");
            }
            if(mappingfolder == null)
            {
                throw new InvalidPropertiesFormatException("The properties file does not contain the key ntuenergy.mappingfolder with a path to the folder containing the required JSON key to IRI Mappings");
            }
            mappings = new ArrayList<>();
            File folder = new File(mappingfolder);
            File[] mappingFiles = folder.listFiles();

            if(mappingFiles.length == 0)
            {
                throw new IOException("No files in folder");
            }
            else
            {
                for(File mappingFile : mappingFiles)
                {
                    JSONKeyToIRIMapper mapper = new JSONKeyToIRIMapper(generatedIRIPrefix, mappingFile.getAbsolutePath());
                    mappings.add(mapper);
                    mapper.saveToFile(mappingFile.getAbsolutePath());
                }
            }
        }
    }
    public void instantiateTriples(){
        List<String> iris;

        for (JSONKeyToIRIMapper mapping : mappings){
            iris = mapping.getAllIRIs();
            for (String iri:iris){

                TriplePattern triple = iri(iri).isA(Measure);
                InsertDataQuery insertion = Queries.INSERT_DATA(triple);
                insertion.prefix(PREFIX_OM);
                kbClient.executeUpdate(insertion.getQueryString());

                if(iri.contains("KW")){

                    LOGGER.info(iri);
                    TriplePattern omHasUnit = iri(iri).has(hasUnit, kilowatt);
                    InsertDataQuery insert = Queries.INSERT_DATA(omHasUnit);
                    insert.prefix(PREFIX_OM);
                    LOGGER.info("update query: "+ insert.getQueryString());
                    kbClient.executeUpdate(insert.getQueryString());
                    LOGGER.info("Added hasUnit for    " + iri);

                    TriplePattern APHasValue = iri(activePower).has(hasValue, Measure);
                    InsertDataQuery insert2 = Queries.INSERT_DATA(APHasValue);
                    insert2.prefix(PREFIX_OM);
                    LOGGER.info("!!!KM update: "+ insert2.getQueryString());
                    kbClient.executeUpdate(insert2.getQueryString());

                    TriplePattern typeAP = iri(activePower).isA(ActivePower);
                    InsertDataQuery insert3 = Queries.INSERT_DATA(typeAP);
                    insert3.prefix(PREFIX_POWSYS);
                    kbClient.executeUpdate(insert3.getQueryString());


                }
                else if(iri.contains("KVAR")){
                    LOGGER.info(iri);
                    TriplePattern omHasUnit = iri(iri).has(hasUnit, KVAR);
                    InsertDataQuery insert = Queries.INSERT_DATA(omHasUnit);
                    insert.prefix(PREFIX_OM);
                    kbClient.executeUpdate(insert.getQueryString());

                    TriplePattern RPHasValue = iri(reactivePower).has(hasValue, Measure);
                    InsertDataQuery insert2 = Queries.INSERT_DATA(RPHasValue);
                    insert2.prefix(PREFIX_OM);
                    kbClient.executeUpdate(insert2.getQueryString());

                    TriplePattern typeRP = iri(reactivePower).isA(ReactivePower);
                    InsertDataQuery insert3 = Queries.INSERT_DATA(typeRP);
                    insert3.prefix(PREFIX_POWSYS);
                    kbClient.executeUpdate(insert3.getQueryString());
                }
            }
        }


    }



}
