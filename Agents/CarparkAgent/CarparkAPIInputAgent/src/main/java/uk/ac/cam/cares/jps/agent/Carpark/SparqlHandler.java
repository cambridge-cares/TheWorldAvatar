package uk.ac.cam.cares.jps.agent.Carpark;
import org.json.JSONArray;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.util.JSONKeyToIRIMapper;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesSparql;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.*;
import me.xdrop.fuzzywuzzy.FuzzySearch;

import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import java.util.List;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import org.eclipse.rdf4j.sparqlbuilder.core.query.InsertDataQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;

public class SparqlHandler
{
    public static final Logger Log = LogManager.getLogger(CarparkAgent.class);

    public String queryEndpoint;
    public String updateEndpoint;
    public String sparqlUsername;
    public String sparqlPassword;

    RemoteStoreClient kbClient;

    /**
     * Namespaces for ontologies
     */

    public static final String OntoCarpark = "https://www.theworldavatar.com/kg/ontocarpark/";
    public static final String RDFS_NS = "http://www.w3.org/2000/01/rdf-schema#";

    
    
	/**
     * Prefixes
     */ 

    private static final Prefix PREFIX_ONTOCARPARK = SparqlBuilder.prefix("ontoCarpark", iri(OntoCarpark));
    public static final String generatedIRIPrefix = TimeSeriesSparql.TIMESERIES_NAMESPACE + "Carpark";
    private static final Prefix PREFIX_RDFS = SparqlBuilder.prefix("rdfs", iri(RDFS_NS));

    
    
	/**
     * Relationships
    */ 


	private static final Iri hasAgency = PREFIX_ONTOCARPARK.iri("hasAgency");
	private static final Iri hasID = PREFIX_ONTOCARPARK.iri("hasID");
	private static final Iri hasLots = PREFIX_ONTOCARPARK.iri("hasLots");
	private static final Iri hasLotType = PREFIX_ONTOCARPARK.iri("hasLotType");
	private static final Iri hasLocation = PREFIX_ONTOCARPARK.iri("hasLocation");
	private static final Iri hasLatitude = PREFIX_ONTOCARPARK.iri("hasLatitude");
    private static final Iri hasLongitude = PREFIX_ONTOCARPARK.iri("hasLongitude");
    private static final Iri hasTimeSeries = PREFIX_ONTOCARPARK.iri("hasTimeSeries");
    private static final Iri hasWeekdayRates = PREFIX_ONTOCARPARK.iri("hasWeekdayRates");
    private static final Iri hasSaturdayRates = PREFIX_ONTOCARPARK.iri("hasSaturdayRates");
    private static final Iri hasSundayAndPHRates = PREFIX_ONTOCARPARK.iri("hasSundayAndPHRates");
    private static final Iri label = PREFIX_RDFS.iri("label");

    /**
     * Classes
    */

    private static final Iri AvailableLots = PREFIX_ONTOCARPARK.iri("AvailableLots");
    private static final Iri Agency = PREFIX_ONTOCARPARK.iri("Agency");
    private static final Iri WeekdayRates = PREFIX_ONTOCARPARK.iri("WeekdayRates");
    private static final Iri SaturdayRates = PREFIX_ONTOCARPARK.iri("SaturdayRates");
    private static final Iri SundayAndPHRates = PREFIX_ONTOCARPARK.iri("SundayAndPHRates");
    private static final Iri ID = PREFIX_ONTOCARPARK.iri("CarParkID");
    private static final Iri Carpark = PREFIX_ONTOCARPARK.iri("Carpark");
    private static final Iri Location = PREFIX_ONTOCARPARK.iri("Location");
    private static final Iri Longitude = PREFIX_ONTOCARPARK.iri("Longitude");
    private static final Iri Latitude = PREFIX_ONTOCARPARK.iri("Latitude");
    private static final Iri LotType = PREFIX_ONTOCARPARK.iri("LotType");
    private static final Iri Cars = PREFIX_ONTOCARPARK.iri("Cars");
    private static final Iri Motorcycles = PREFIX_ONTOCARPARK.iri("Motorcycles");
    private static final Iri HeavyVehicles = PREFIX_ONTOCARPARK.iri("HeavyVehicles");
     


    public String agentProperties;
    public String clientProperties;
    public JSONObject readings;
    public JSONObject priceReadings;

    private List<JSONKeyToIRIMapper> mappings;

    public SparqlHandler(String agentProp, String clientProp) throws IOException
    {
        agentProperties = agentProp;
        clientProperties = clientProp;


        loadconfigs(clientProperties);
        //readings endpoints from client.properties

        loadproperties(agentProperties);
        
        kbClient = new RemoteStoreClient();

        kbClient.setUpdateEndpoint(updateEndpoint);
        kbClient.setQueryEndpoint(queryEndpoint);
        kbClient.setUser(sparqlUsername);
        kbClient.setPassword(sparqlPassword);


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
                mappingfolder = System.getenv(prop.getProperty("Carpark.mappingfolder"));
            }
            catch(NullPointerException e)
            {
                throw new IOException("The key Carpark.mappingfolder cannot be found");
            }

            if(mappingfolder == null)
            {
                throw new InvalidPropertiesFormatException("The properties file does not contain the key Carpark.mappingfolder with a path to the folder containing the required JSON key to IRI Mappings");
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
            if (prop.containsKey("sparql.username")) {
                this.sparqlUsername = prop.getProperty("sparql.username");
            }
            if (prop.containsKey("sparql.password")) {
                this.sparqlPassword = prop.getProperty("sparql.password");
            }
        }
    }

    public void instantiateIfNotInstantiated(JSONObject carparkReadings, JSONObject prices)
    {
        readings = carparkReadings;
        priceReadings = prices;

        List<String> iris;

        JSONArray carparkRates = priceReadings.getJSONObject("result").getJSONArray("records");

        for(JSONKeyToIRIMapper mapping : mappings)
        {
            iris = mapping.getAllIRIs();
           
           for(String iri:iris)
           if(!iri.contains("Carpark_time_"))
           {
            {
                String r=null;
                Variable c = SparqlBuilder.var("c");
                SelectQuery q = Queries.SELECT();
    
                //Create a triplePattern
    
                TriplePattern qP = iri(iri).isA(c);
                q.prefix(PREFIX_ONTOCARPARK).select(c).where(qP);
                kbClient.setQuery(q.getQueryString());
    
    
                try
                {
                    JSONArray qR = kbClient.executeQuery();
    
                    if(qR.isEmpty())
                    {
                        TriplePattern P1 = iri(iri).isA(AvailableLots);
                        InsertDataQuery insert = Queries.INSERT_DATA(P1);
                        insert.prefix(PREFIX_ONTOCARPARK);
                        kbClient.executeUpdate(insert.getQueryString());
                    }
                    else
                    {
                        continue;
                    }
    
                }
                catch(Exception e)
                {
                    throw new JPSRuntimeException("Could not check for an rdf:type for the Data IRI");
                }
    
                StringTokenizer st = new StringTokenizer(iri,"_");
                for(int i=0;i<2;i++)
                st.nextToken();
                String CarparkID,lotType;
    
                CarparkID = st.nextToken();
                lotType = st.nextToken();

                
    
    
                final String lotTypeIri = OntoCarpark + "Carpark_LotType_" + UUID.randomUUID();
    
                if(lotType.equalsIgnoreCase("C"))
                {
                    TriplePattern updatePattern = iri(lotTypeIri).isA(Cars);
                    InsertDataQuery insert2 = Queries.INSERT_DATA(updatePattern);
                    insert2.prefix(PREFIX_ONTOCARPARK);
    
                    kbClient.executeUpdate(insert2.getQueryString());
                }
                else if(lotType.equalsIgnoreCase("H"))
                {
                    TriplePattern updatePattern = iri(lotTypeIri).isA(HeavyVehicles);
                    InsertDataQuery insert2 = Queries.INSERT_DATA(updatePattern);
                    insert2.prefix(PREFIX_ONTOCARPARK);
    
                    kbClient.executeUpdate(insert2.getQueryString());
                }
                else
                {
                    TriplePattern updatePattern = iri(lotTypeIri).isA(Motorcycles);
                    InsertDataQuery insert2 = Queries.INSERT_DATA(updatePattern);
                    insert2.prefix(PREFIX_ONTOCARPARK);
    
                    kbClient.executeUpdate(insert2.getQueryString());
                }
    
                //Creating IRI to check for multiple LotType
    
                String result = null;
                Variable carparkIRI = SparqlBuilder.var("carparkIRI");
    
                SelectQuery query = Queries.SELECT();
                TriplePattern queryPattern = carparkIRI.has(hasID,CarparkID);
                query.prefix(PREFIX_ONTOCARPARK).select(carparkIRI).where(queryPattern);
    
                kbClient.setQuery(query.getQueryString());
    
                try
                {
                    JSONArray queryResult = kbClient.executeQuery();
                    TriplePattern pattern1;
    
                    if(!queryResult.isEmpty())
                    {
                        result = kbClient.executeQuery().getJSONObject(0).getString("carparkIRI");
                        pattern1 = iri(result).has(hasLotType,iri(lotTypeIri));
                    }
                    else
                    {
                        //if queryresult is empty 
                        result = OntoCarpark + "Carpark_" + UUID.randomUUID();
                        pattern1 = iri(result).has(hasLotType,iri(lotTypeIri));
                    }
                    InsertDataQuery insert3 = Queries.INSERT_DATA(pattern1);
                    insert3.prefix(PREFIX_ONTOCARPARK);
                    kbClient.executeUpdate(insert3.getQueryString());
    
                    TriplePattern pattern8 = iri(result).isA(Carpark);
                    InsertDataQuery insert10 = Queries.INSERT_DATA(pattern8);
                    insert10.prefix(PREFIX_ONTOCARPARK);
                    kbClient.executeUpdate(insert10.getQueryString());

                    //TriplePattern for CarparkID
    
                    TriplePattern pattern7 = iri(result).has(hasID, CarparkID);
                    InsertDataQuery insert9 = Queries.INSERT_DATA(pattern7);
                    insert9.prefix(PREFIX_ONTOCARPARK);
                    kbClient.executeUpdate(insert9.getQueryString());
    
    
                    //TriplePattern to link LotType IRI to data Iri
    
                    TriplePattern updatePattern = iri(lotTypeIri).has(hasLots,iri(iri));
                    InsertDataQuery insertUpdate = Queries.INSERT_DATA(updatePattern);
                    insertUpdate.prefix(PREFIX_ONTOCARPARK);
                    kbClient.executeUpdate(insertUpdate.getQueryString());
                }
                catch(Exception e)
                {
                    throw new JPSRuntimeException("Unable to execute query: " + query.getQueryString());
                }
    
                String result1 = null;
                Variable carparkLocationIRI = SparqlBuilder.var("carparkLocationIRI");
    
                SelectQuery query1 = Queries.SELECT();
                TriplePattern queryPattern1 =  iri(result).has(hasLocation, carparkLocationIRI);
    
    
                query1.prefix(PREFIX_ONTOCARPARK).select(carparkLocationIRI).where(queryPattern1);
                kbClient.setQuery(query1.getQueryString());
    
                try
                {
                    JSONArray queryResult1 = kbClient.executeQuery();
                   
                    if(!queryResult1.isEmpty())
                    {
                        //Carpark IRI and its following relations have already been instantiated. Loop proceeds to the next IRI immediately as it doesnt enter the else condition
                    }
                    else
                    {
                        String loc="";
                        String agency = "";
                        String Devlabel="";

                        try
                        {
                            String build2 = OntoCarpark + "CarparkLocation_" + UUID.randomUUID();
    
                            JSONArray jsArr;
                            jsArr = readings.getJSONArray("value"); 

                            for(int i=0; i<jsArr.length();i++)
                            {
                                JSONObject currentObject = jsArr.getJSONObject(i);
                                String ID = currentObject.getString("CarParkID");
                                //Check for the correct iD and then store the Location and Agency
                                if(ID.equals(CarparkID))
                                {
                                    loc = currentObject.getString("Location");
                                    agency = currentObject.getString("Agency");
                                    Devlabel = currentObject.getString("Development");
                                    //Storing the values in String variables

                                    //Extracting out the Lat and Longitude and converting it into a Double 
                                    StringTokenizer str = new StringTokenizer(loc, " ");
                                    Double lat = Double.parseDouble(str.nextToken());
                                    Double lon = Double.parseDouble(str.nextToken());
                                    
                                    TriplePattern pattern2 = iri(result).has(hasLocation, iri(build2));
                                    InsertDataQuery insert4 = Queries.INSERT_DATA(pattern2);
                                    insert4.prefix(PREFIX_ONTOCARPARK);
                                    kbClient.executeUpdate(insert4.getQueryString());
    
                                    //Obtaining Location
                                    TriplePattern pattern3 = iri(build2).isA(Location);
                                    InsertDataQuery insert5 = Queries.INSERT_DATA(pattern3);
                                    insert5.prefix(PREFIX_ONTOCARPARK);
                                    kbClient.executeUpdate(insert5.getQueryString());

                                    //TriplePattern for Latitude
    
                                    TriplePattern pattern4 = iri(build2).has(hasLatitude,lat);
                                    InsertDataQuery insert6 = Queries.INSERT_DATA(pattern4);
                                    insert6.prefix(PREFIX_ONTOCARPARK);
                                    kbClient.executeUpdate(insert6.getQueryString());
    
                                    //TriplePattern for Longitude
                                    TriplePattern pattern5 = iri(build2).has(hasLongitude,lon);
                                    InsertDataQuery insert7 = Queries.INSERT_DATA(pattern5);
                                    insert7.prefix(PREFIX_ONTOCARPARK);
                                    kbClient.executeUpdate(insert7.getQueryString());
    
                                    //TriplePattern for Agency
                                    TriplePattern pattern6 = iri(result).has(hasAgency,agency);
                                    InsertDataQuery insert8 = Queries.INSERT_DATA(pattern6);
                                    insert8.prefix(PREFIX_ONTOCARPARK);
                                    kbClient.executeUpdate(insert8.getQueryString());

                                    //TriplePattern for Label(Development)

                                    TriplePattern pattern8 = iri(result).has(label,Devlabel);
                                    InsertDataQuery insert10 = Queries.INSERT_DATA(pattern8);
                                    insert10.prefix(PREFIX_RDFS);
                                    kbClient.executeUpdate(insert10.getQueryString());
                                }
                            }

                        }
                        catch (Exception e) 
                        {
                           throw new JPSRuntimeException("Readings can not be empty!", e);
                        }   


                        //FuzzyMatching for the carpark Prices

                        String saturdayRate="Carpark prices unavailable",weekday="Carpark prices unavailable",sundayAndPHRates="Carpark prices unavailable";
                        int check=0;

                        for(int i=0;i<carparkRates.length();i++)
                        {
                            JSONObject currentCarpark = carparkRates.getJSONObject(i);
                            String currentName = currentCarpark.getString("carpark");

                            //is currentCarpark same as the Devlabel of the IRI
                            if(!Devlabel.equals("") && FuzzySearch.tokenSetRatio(currentName.toLowerCase(),Devlabel.toLowerCase())>90 && FuzzySearch.partialRatio(currentName.toLowerCase(),Devlabel.toLowerCase())>75 && FuzzySearch.tokenSortRatio(currentName.toLowerCase(),Devlabel.toLowerCase())>83)
                            //if(!Devlabel.equals("") && FuzzySearch.tokenSetRatio(currentName,Devlabel.toLowerCase(Locale.ROOT))>52 && FuzzySearch.weightedRatio(currentName,Devlabel.toLowerCase(Locale.ROOT))>75 && FuzzySearch.partialRatio(currentName,Devlabel.toLowerCase(Locale.ROOT))>59 && FuzzySearch.tokenSortRatio(currentName,Devlabel.toLowerCase(Locale.ROOT))>52)
                            {
                                check=1;

                                saturdayRate =  currentCarpark.getString("saturday_rate");
                                sundayAndPHRates = currentCarpark.getString("sunday_publicholiday_rate");
                                String weekday1 = currentCarpark.getString("weekdays_rate_1");
                                String weekday2 = currentCarpark.getString("weekdays_rate_2");
                                
                                if(!(weekday2.equals("-")) && !(weekday2.equals(weekday1)))
                                {
                                    weekday = weekday1+";"+weekday2;
                                }
                                else
                                {
                                    weekday = weekday1;
                                }

                                if(sundayAndPHRates.equals("Same as Saturday"))
                                {
                                    sundayAndPHRates = saturdayRate;
                                }

                                if(sundayAndPHRates.equals("Same as wkdays") || sundayAndPHRates.equals("Same as weekdays"))
                                {
                                    sundayAndPHRates = weekday;
                                }

                                if(saturdayRate.equals("Same as wkdays") || saturdayRate.equals("Same as weekdays"))
                                {
                                    saturdayRate = weekday;
                                }
                            }
                        if(check==1)
                        i=carparkRates.length();
                    }

                        TriplePattern pattern9 = iri(result).has(hasWeekdayRates,weekday);
                        InsertDataQuery insert11 = Queries.INSERT_DATA(pattern9);
                        insert11.prefix(PREFIX_ONTOCARPARK);
                        kbClient.executeUpdate(insert11.getQueryString());

                        TriplePattern pattern10 = iri(result).has(hasSaturdayRates,saturdayRate);
                        InsertDataQuery insert12 = Queries.INSERT_DATA(pattern10);
                        insert12.prefix(PREFIX_ONTOCARPARK);
                        kbClient.executeUpdate(insert12.getQueryString());

                        TriplePattern pattern11 = iri(result).has(hasSundayAndPHRates,sundayAndPHRates);
                        InsertDataQuery insert13 = Queries.INSERT_DATA(pattern11);
                        insert13.prefix(PREFIX_ONTOCARPARK);
                        kbClient.executeUpdate(insert13.getQueryString());


                        //Looping through for subsequent IRIs
                    }
    
    
                }
                catch (Exception e)
                {
                    throw new JPSRuntimeException("Unable to execute query: " +  query1.getQueryString(), e);
                }
    
            }
    
           }
           
        }
    }

}
