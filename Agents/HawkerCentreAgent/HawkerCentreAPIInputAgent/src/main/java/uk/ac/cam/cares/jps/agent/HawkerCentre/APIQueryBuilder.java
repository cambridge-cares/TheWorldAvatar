package uk.ac.cam.cares.jps.agent.HawkerCentre;


import org.json.JSONArray;
import org.json.JSONObject;
import uk.ac.cam.cares.jps.base.timeseries.TimeSeriesSparql;
import me.xdrop.fuzzywuzzy.FuzzySearch;
import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.*;
import org.eclipse.rdf4j.sparqlbuilder.core.Prefix;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.TriplePattern;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;

import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;
import java.util.List;
import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;

import org.eclipse.rdf4j.sparqlbuilder.core.query.InsertDataQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;


public class APIQueryBuilder
{
    

    public String queryEndpoint;
    public String updateEndpoint;

    RemoteStoreClient kbClient;

    /**
     * Namespaces for ontologies
     */

    public static final String OntoHawker = "https://www.theworldavatar.com/kg/ontohawkercentre/";
    public static final String RDFS_NS = "http://www.w3.org/2000/01/rdf-schema#";

    
    
	/**
     * Prefixes
     */ 

    private static final Prefix PREFIX_ONTOHAWKER = SparqlBuilder.prefix("ontoHawker", iri(OntoHawker));
    private static final Prefix PREFIX_RDFS = SparqlBuilder.prefix("rdfs", iri(RDFS_NS));

    
    
	/**
     * Relationships
    */ 

    private static final Iri hasAddress = PREFIX_ONTOHAWKER.iri("hasAddress");
    private static final Iri hasAverageGrading = PREFIX_ONTOHAWKER.iri("hasAverageGrading");
    private static final Iri hasStall = PREFIX_ONTOHAWKER.iri("hasStall");
    private static final Iri hasType = PREFIX_ONTOHAWKER.iri("hasType");
    private static final Iri hasOwner = PREFIX_ONTOHAWKER.iri("hasOwner");
    private static final Iri label = PREFIX_RDFS.iri("label");  
    private static final Iri hasTotalStalls = PREFIX_ONTOHAWKER.iri("hasTotalStalls");
    private static final Iri hasFoodStalls = PREFIX_ONTOHAWKER.iri("hasFoodStalls");
    private static final Iri hasMarketProduceStalls = PREFIX_ONTOHAWKER.iri("hasMarketProduceStalls");
    private static final Iri hasID = PREFIX_ONTOHAWKER.iri("hasID");
    private static final Iri hasStallAddress = PREFIX_ONTOHAWKER.iri("hasStallAddress");
    private static final Iri hasLicenseNumber = PREFIX_ONTOHAWKER.iri("hasLicenseNumber");
    private static final Iri hasLicenseeName = PREFIX_ONTOHAWKER.iri("hasLicenseeName");
    private static final Iri hasStallGrading = PREFIX_ONTOHAWKER.iri("hasStallGrading");
    
    

    

    /**
     * Classes
    */
    private static final Iri HawkerCentre = PREFIX_ONTOHAWKER.iri("HawkerCentre");
    private static final Iri Stall = PREFIX_ONTOHAWKER.iri("Stall");


    public String clientProperties;

    public JSONObject hawkerReadings;
    public JSONObject stallsReadings;

    public APIQueryBuilder(String clientProp) throws IOException
    {
        clientProperties = clientProp;
        loadconfigs(clientProperties);
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


    public void instantiateIfNotInstantiated(JSONObject hawker, JSONObject stalls)
    {
        
        hawkerReadings = hawker;
        stallsReadings = stalls;

        List<String> iris;

        JSONArray hawkerArray = hawkerReadings.getJSONObject("result").getJSONArray("records");

        for(int j=0;j<hawkerArray.length();j++)
        {
            JSONObject curHawkerCentre = hawkerArray.getJSONObject(j);
            String hawkerName = curHawkerCentre.getString("name_of_centre");

            String result=null;
            Variable hawkerIRI =SparqlBuilder.var("hawkerIRI");
            SelectQuery q = Queries.SELECT();

            TriplePattern qp = hawkerIRI.isA(HawkerCentre).andHas(label,hawkerName);
            q.prefix(PREFIX_ONTOHAWKER, PREFIX_RDFS).select(hawkerIRI).where(qp);
            kbClient.setQuery(q.getQueryString());

            try
            {
                JSONArray queryResult = kbClient.executeQuery();
                  
                if(!queryResult.isEmpty())
                {
                    result = kbClient.executeQuery().getJSONObject(0).getString("hawkerIRI");
                }
                else
                {
                    //if queryresult is empty 
                    result = OntoHawker + hawkerName.replace(" ", "_") + "_" + UUID.randomUUID();
                    TriplePattern pattern = iri(result).isA(HawkerCentre);
                    InsertDataQuery insert1 = Queries.INSERT_DATA(pattern);
                    insert1.prefix(PREFIX_ONTOHAWKER);
                    kbClient.executeUpdate(insert1.getQueryString());
        
                    TriplePattern namePattern = iri(result).has(label,hawkerName);
                    InsertDataQuery insertname = Queries.INSERT_DATA(namePattern);
                    insertname.prefix(PREFIX_ONTOHAWKER, PREFIX_RDFS);
                    kbClient.executeUpdate(insertname.getQueryString());
        
                    String hawkerType = curHawkerCentre.getString("type_of_centre");
                    String hawkerOwner = curHawkerCentre.getString("owner");
        
                    TriplePattern updateType = iri(result).has(hasType,hawkerType);
                    InsertDataQuery insertType = Queries.INSERT_DATA(updateType);
                    insertType.prefix(PREFIX_ONTOHAWKER);
                    kbClient.executeUpdate(insertType.getQueryString());
        
                    TriplePattern updateOwner = iri(result).has(hasOwner,hawkerOwner);
                    InsertDataQuery insertOwner = Queries.INSERT_DATA(updateOwner);
                    insertOwner.prefix(PREFIX_ONTOHAWKER);
                    kbClient.executeUpdate(insertOwner.getQueryString());
        
                    String add = curHawkerCentre.getString("location_of_centre");
                    String numberofStalls = curHawkerCentre.getString("no_of_stalls");
                    String foodStalls = curHawkerCentre.getString("no_of_cooked_food_stalls");
                    String marketProduceStalls = curHawkerCentre.getString("no_of_mkt_produce_stalls");
        
                    //TriplePattern for address
                    TriplePattern pattern4 = iri(result).has(hasAddress,add);
                    InsertDataQuery insert4 = Queries.INSERT_DATA(pattern4);
                    insert4.prefix(PREFIX_ONTOHAWKER);
                    kbClient.executeUpdate(insert4.getQueryString());
        
                    //TriplePattern for total number of stalls
                    TriplePattern pattern5 = iri(result).has(hasTotalStalls,Integer.parseInt(numberofStalls));
                    InsertDataQuery insert5 = Queries.INSERT_DATA(pattern5);
                    insert5.prefix(PREFIX_ONTOHAWKER);
                    kbClient.executeUpdate(insert5.getQueryString());
        
                    //TriplePattern for total food stalls
                    TriplePattern patternFoodStalls = iri(result).has(hasFoodStalls,Integer.parseInt(foodStalls));
                    InsertDataQuery insertFoodStalls = Queries.INSERT_DATA(patternFoodStalls);
                    insertFoodStalls.prefix(PREFIX_ONTOHAWKER);
                    kbClient.executeUpdate(insertFoodStalls.getQueryString());
        
        
                    //TriplePattern for total market produce stalls
                    TriplePattern patternmarket = iri(result).has(hasMarketProduceStalls,Integer.parseInt(marketProduceStalls));
                    InsertDataQuery insertMarketStalls = Queries.INSERT_DATA(patternmarket);
                    insertMarketStalls.prefix(PREFIX_ONTOHAWKER);
                    kbClient.executeUpdate(insertMarketStalls.getQueryString());
                }

            }
            catch(Exception e)
            {
                throw new JPSRuntimeException("Unable to execute query: " + q.getQueryString());
            }

            //FuzzyMatching for the Avergae Grading of a Hawker Centre;
            int totalScore=0, count=0;
            
            try
            {
                JSONArray jsArr = stallsReadings.getJSONObject("result").getJSONArray("records");
                for(int i=0; i<jsArr.length();i++)
                {
                    JSONObject currentStall = jsArr.getJSONObject(i);
                    String current = currentStall.getString("premises_address");

                    if (hawkerName.contains("/")) {
                        if(FuzzySearch.tokenSetRatio(current.split("Stall")[0].replace(" ST ", " STREET ").replace(" DR ", " DRIVE ").replace(" LOR ", " LORONG ").toLowerCase(),hawkerName.toLowerCase())>87)
                    {
                        count++;
                        String score = currentStall.getString("grade");
                        if(score == "A")
                        totalScore += 90;
                        else if(score == "B")
                        totalScore += 80;
                        else
                        totalScore += 70;
                    

                        String stallRes=null;
                        Variable stallIRI = SparqlBuilder.var("stallIRI");
                        SelectQuery q1 = Queries.SELECT();

                        int stallID = currentStall.getInt("_id");
                        TriplePattern qp1 = stallIRI.isA(Stall).andHas(hasID,stallID);
                        q1.prefix(PREFIX_ONTOHAWKER).select(stallIRI).where(qp1);
                        kbClient.setQuery(q1.getQueryString());

                        try
                        {
                            JSONArray queryResult1 = kbClient.executeQuery();
                            if(!queryResult1.isEmpty())
                            {
                                stallRes = kbClient.executeQuery().getJSONObject(0).getString("stallIRI");
                            }
                            else
                            {
                                stallRes = OntoHawker + "stall_" + stallID + "_" + UUID.randomUUID();
                            }
                        }
                        catch(Exception e)
                        {
                            throw new JPSRuntimeException("Unable to execute query: " + q1.getQueryString());
                        }
                        TriplePattern patternUpdate = iri(stallRes).isA(Stall);
                        InsertDataQuery insertUpdate1 = Queries.INSERT_DATA(patternUpdate);
                        insertUpdate1.prefix(PREFIX_ONTOHAWKER);
                        kbClient.executeUpdate(insertUpdate1.getQueryString());

                        //TriplePattern to instantiate this stall as part of the current HawkerCentre

                        TriplePattern patternStall = iri(result).has(hasStall,iri(stallRes));
                        InsertDataQuery insertStall = Queries.INSERT_DATA(patternStall);
                        insertStall.prefix(PREFIX_ONTOHAWKER);
                        kbClient.executeUpdate(insertStall.getQueryString());

                        String licName = currentStall.getString("licensee_name");
                        String licNo = currentStall.getString("licence_number");
                        String grade = currentStall.getString("grade");
                        String stallAddress = currentStall.getString("premises_address");

                        TriplePattern patternLicName = iri(stallRes).has(hasLicenseeName,licName);
                        InsertDataQuery insertLicName = Queries.INSERT_DATA(patternLicName);
                        insertLicName.prefix(PREFIX_ONTOHAWKER);
                        kbClient.executeUpdate(insertLicName.getQueryString());

                        TriplePattern patternLicNo = iri(stallRes).has(hasLicenseNumber,licNo);
                        InsertDataQuery insertLicNo = Queries.INSERT_DATA(patternLicNo);
                        insertLicNo.prefix(PREFIX_ONTOHAWKER);
                        kbClient.executeUpdate(insertLicNo.getQueryString());

                        TriplePattern patterngrade = iri(stallRes).has(hasStallGrading,grade);
                        InsertDataQuery insertGrade = Queries.INSERT_DATA(patterngrade);
                        insertGrade.prefix(PREFIX_ONTOHAWKER);
                        kbClient.executeUpdate(insertGrade.getQueryString());

                        TriplePattern patternAddress = iri(stallRes).has(hasStallAddress,stallAddress);
                        InsertDataQuery insertAddress = Queries.INSERT_DATA(patternAddress);
                        insertAddress.prefix(PREFIX_ONTOHAWKER);
                        kbClient.executeUpdate(insertAddress.getQueryString());


                    }
                    } else if (hawkerName.contains(" & ")) {
                        if(FuzzySearch.tokenSetRatio(current.split("Stall")[0].replace(" ST ", " STREET ").replace(" DR ", " DRIVE ").replace(" LOR ", " LORONG ").toLowerCase(),hawkerName.toLowerCase())>90)
                    {
                        count++;
                        String score = currentStall.getString("grade");
                        if(score == "A")
                        totalScore += 90;
                        else if(score == "B")
                        totalScore += 80;
                        else
                        totalScore += 70;
                    

                        String stallRes=null;
                        Variable stallIRI = SparqlBuilder.var("stallIRI");
                        SelectQuery q1 = Queries.SELECT();

                        int stallID = currentStall.getInt("_id");
                        TriplePattern qp1 = stallIRI.isA(Stall).andHas(hasID,stallID);
                        q1.prefix(PREFIX_ONTOHAWKER).select(stallIRI).where(qp1);
                        kbClient.setQuery(q1.getQueryString());

                        try
                        {
                            JSONArray queryResult1 = kbClient.executeQuery();
                            if(!queryResult1.isEmpty())
                            {
                                stallRes = kbClient.executeQuery().getJSONObject(0).getString("stallIRI");
                            }
                            else
                            {
                                stallRes = OntoHawker + "stall_" + stallID + "_" + UUID.randomUUID();
                            }
                        }
                        catch(Exception e)
                        {
                            throw new JPSRuntimeException("Unable to execute query: " + q1.getQueryString());
                        }
                        TriplePattern patternUpdate = iri(stallRes).isA(Stall);
                        InsertDataQuery insertUpdate1 = Queries.INSERT_DATA(patternUpdate);
                        insertUpdate1.prefix(PREFIX_ONTOHAWKER);
                        kbClient.executeUpdate(insertUpdate1.getQueryString());

                        //TriplePattern to instantiate this stall as part of the current HawkerCentre

                        TriplePattern patternStall = iri(result).has(hasStall,iri(stallRes));
                        InsertDataQuery insertStall = Queries.INSERT_DATA(patternStall);
                        insertStall.prefix(PREFIX_ONTOHAWKER);
                        kbClient.executeUpdate(insertStall.getQueryString());

                        String licName = currentStall.getString("licensee_name");
                        String licNo = currentStall.getString("licence_number");
                        String grade = currentStall.getString("grade");
                        String stallAddress = currentStall.getString("premises_address");

                        TriplePattern patternLicName = iri(stallRes).has(hasLicenseeName,licName);
                        InsertDataQuery insertLicName = Queries.INSERT_DATA(patternLicName);
                        insertLicName.prefix(PREFIX_ONTOHAWKER);
                        kbClient.executeUpdate(insertLicName.getQueryString());

                        TriplePattern patternLicNo = iri(stallRes).has(hasLicenseNumber,licNo);
                        InsertDataQuery insertLicNo = Queries.INSERT_DATA(patternLicNo);
                        insertLicNo.prefix(PREFIX_ONTOHAWKER);
                        kbClient.executeUpdate(insertLicNo.getQueryString());

                        TriplePattern patterngrade = iri(stallRes).has(hasStallGrading,grade);
                        InsertDataQuery insertGrade = Queries.INSERT_DATA(patterngrade);
                        insertGrade.prefix(PREFIX_ONTOHAWKER);
                        kbClient.executeUpdate(insertGrade.getQueryString());

                        TriplePattern patternAddress = iri(stallRes).has(hasStallAddress,stallAddress);
                        InsertDataQuery insertAddress = Queries.INSERT_DATA(patternAddress);
                        insertAddress.prefix(PREFIX_ONTOHAWKER);
                        kbClient.executeUpdate(insertAddress.getQueryString());
                    }
                } else if(FuzzySearch.tokenSetRatio(current.split("Stall")[0].replace(" ST ", " STREET ").replace(" DR ", " DRIVE ").replace(" LOR ", " LORONG ").toLowerCase(),hawkerName.toLowerCase())>99)
                    {
                        count++;
                        String score = currentStall.getString("grade");
                        if(score == "A")
                        totalScore += 90;
                        else if(score == "B")
                        totalScore += 80;
                        else
                        totalScore += 70;
                    

                        String stallRes=null;
                        Variable stallIRI = SparqlBuilder.var("stallIRI");
                        SelectQuery q1 = Queries.SELECT();

                        int stallID = currentStall.getInt("_id");
                        TriplePattern qp1 = stallIRI.isA(Stall).andHas(hasID,stallID);
                        q1.prefix(PREFIX_ONTOHAWKER).select(stallIRI).where(qp1);
                        kbClient.setQuery(q1.getQueryString());

                        try
                        {
                            JSONArray queryResult1 = kbClient.executeQuery();
                            if(!queryResult1.isEmpty())
                            {
                                stallRes = kbClient.executeQuery().getJSONObject(0).getString("stallIRI");
                            }
                            else
                            {
                                stallRes = OntoHawker + "stall_" + stallID + "_" + UUID.randomUUID();
                            }
                        }
                        catch(Exception e)
                        {
                            throw new JPSRuntimeException("Unable to execute query: " + q1.getQueryString());
                        }
                        TriplePattern patternUpdate = iri(stallRes).isA(Stall);
                        InsertDataQuery insertUpdate1 = Queries.INSERT_DATA(patternUpdate);
                        insertUpdate1.prefix(PREFIX_ONTOHAWKER);
                        kbClient.executeUpdate(insertUpdate1.getQueryString());

                        //TriplePattern to instantiate this stall as part of the current HawkerCentre

                        TriplePattern patternStall = iri(result).has(hasStall,iri(stallRes));
                        InsertDataQuery insertStall = Queries.INSERT_DATA(patternStall);
                        insertStall.prefix(PREFIX_ONTOHAWKER);
                        kbClient.executeUpdate(insertStall.getQueryString());

                        String licName = currentStall.getString("licensee_name");
                        String licNo = currentStall.getString("licence_number");
                        String grade = currentStall.getString("grade");
                        String stallAddress = currentStall.getString("premises_address");

                        TriplePattern patternLicName = iri(stallRes).has(hasLicenseeName,licName);
                        InsertDataQuery insertLicName = Queries.INSERT_DATA(patternLicName);
                        insertLicName.prefix(PREFIX_ONTOHAWKER);
                        kbClient.executeUpdate(insertLicName.getQueryString());

                        TriplePattern patternLicNo = iri(stallRes).has(hasLicenseNumber,licNo);
                        InsertDataQuery insertLicNo = Queries.INSERT_DATA(patternLicNo);
                        insertLicNo.prefix(PREFIX_ONTOHAWKER);
                        kbClient.executeUpdate(insertLicNo.getQueryString());

                        TriplePattern patterngrade = iri(stallRes).has(hasStallGrading,grade);
                        InsertDataQuery insertGrade = Queries.INSERT_DATA(patterngrade);
                        insertGrade.prefix(PREFIX_ONTOHAWKER);
                        kbClient.executeUpdate(insertGrade.getQueryString());

                        TriplePattern patternAddress = iri(stallRes).has(hasStallAddress,stallAddress);
                        InsertDataQuery insertAddress = Queries.INSERT_DATA(patternAddress);
                        insertAddress.prefix(PREFIX_ONTOHAWKER);
                        kbClient.executeUpdate(insertAddress.getQueryString());


                    }

                }

            }
            catch(Exception e)
            {
                throw new JPSRuntimeException("Readings can not be empty!", e);
            }
            String grading;
            if(count!=0)
            {
                int avgScore = totalScore/count;
               
                if(avgScore>85)
                grading="A";
                else if(avgScore<75)
                grading="C";
                else
                grading="C";
            }
            else
            grading="No gradings available";

            TriplePattern updateGrading = iri(result).has(hasAverageGrading,grading);
            InsertDataQuery insertGrading = Queries.INSERT_DATA(updateGrading);
            insertGrading.prefix(PREFIX_ONTOHAWKER);
            kbClient.executeUpdate(insertGrading.getQueryString());

            
        }
    }
}
