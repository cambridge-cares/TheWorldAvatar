package uk.ac.cam.cares.jps.agent.assetmanager;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.eclipse.rdf4j.model.vocabulary.RDFS;
import org.eclipse.rdf4j.model.vocabulary.XSD;
import org.eclipse.rdf4j.sparqlbuilder.core.SparqlBuilder;
import org.eclipse.rdf4j.sparqlbuilder.core.Variable;
import org.eclipse.rdf4j.sparqlbuilder.core.query.Queries;
import org.eclipse.rdf4j.sparqlbuilder.core.query.SelectQuery;
import org.eclipse.rdf4j.sparqlbuilder.core.query.ModifyQuery;
import org.eclipse.rdf4j.sparqlbuilder.graphpattern.GraphPatterns;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Iri;
import org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf;
import org.jooq.DeleteQuery;
import org.json.JSONArray;
import org.json.JSONObject;

import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.iri;
import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.literalOf;
import static org.eclipse.rdf4j.sparqlbuilder.rdf.Rdf.objects;

import uk.ac.cam.cares.jps.base.exception.JPSRuntimeException;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

import static uk.ac.cam.cares.jps.agent.assetmanager.ClassAndProperties.*;
import static uk.ac.cam.cares.jps.agent.assetmanager.QueryUtil.*;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.UUID;

public class AssetDelete {
    private RemoteStoreClient storeClientAsset, storeClientOffice, storeClientPurchDoc, storeClientLab;
    String DelLogFileLoc;

    /**
     * Logger for reporting info/errors.
     */
    private static final Logger LOGGER = LogManager.getLogger(AssetManagerAgent.class);

    public AssetDelete (RemoteStoreClient clientAsset, RemoteStoreClient clientOffice, RemoteStoreClient clientPurchDoc, RemoteStoreClient clientLab, String deleteLogFile) {
        storeClientAsset = clientAsset;
        storeClientOffice = clientOffice;
        storeClientPurchDoc = clientPurchDoc;
        storeClientLab = clientLab;

        DelLogFileLoc = deleteLogFile;
    }

    public void deleteMaintenanceData (JSONObject maintenanceData){
        ModifyQuery query = Queries.DELETE();
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG_FORMAL, Pref_FIBO_ORG_ORGS, Pref_BOT, 
            Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE,
            Pref_TIME
        );
        //MandatoryIRI
        Iri deviceIRI = iri(maintenanceData.getString("deviceIRI"));
        Iri maintenanceScheduleIRI = iri(maintenanceData.getString("maintenanceScheduleIRI"));
        Iri maintenanceTaskIRI = iri(maintenanceData.getString("maintenanceTaskIRI"));
        Iri performerIRI = iri(maintenanceData.getString("performerIRI"));
        
        query.delete(deviceIRI.has(hasMaintenanceSchedule, maintenanceScheduleIRI));
        query.delete(maintenanceScheduleIRI.has(hasTask, maintenanceTaskIRI));
        query.delete(maintenanceTaskIRI.has(isPerformedBy, performerIRI));
        query.delete(maintenanceScheduleIRI.isA(MaintenanceSchedule));
        query.delete(maintenanceTaskIRI.isA(MaintenanceTask));

        //OptionalIRI and literals
        Iri lastServiceIRI, nextServiceIRI, intervalIRI, durationIRI;
        String lastServiceTime, nextServiceTime, durationMonth, durationYear;
        if (maintenanceData.has("lastServiceIRI")){
            lastServiceIRI = iri(maintenanceData.getString("lastServiceIRI"));
            lastServiceTime = maintenanceData.getString("lastServiceTime");
            query.delete(
                maintenanceTaskIRI.has(performedAt, lastServiceIRI),
                lastServiceIRI.isA(Instant),
                //lastServiceIRI.has(inXSDDateTimeStamp, lastServiceTime),
                lastServiceIRI.has(inXSDDateTimeStamp, Rdf.literalOfType(lastServiceTime, XSD.DATE))
            );
        }

        if (maintenanceData.has("nextServiceIRI")){
            nextServiceIRI = iri(maintenanceData.getString("nextServiceIRI"));
            nextServiceTime = maintenanceData.getString("nextServiceTime");
            query.delete(
                maintenanceTaskIRI.has(scheduledFor, nextServiceIRI),
                nextServiceIRI.isA(Instant),
                //nextServiceIRI.has(inXSDDateTimeStamp, nextServiceTime)
                nextServiceIRI.has(inXSDDateTimeStamp, Rdf.literalOfType(nextServiceTime, XSD.DATE))
            );
        }

        if (maintenanceData.has("intervalIRI")){
            intervalIRI = iri(maintenanceData.getString("intervalIRI"));
            durationIRI = iri(maintenanceData.getString("durationIRI"));
            durationMonth = maintenanceData.getString("durationMonth");
            durationYear = maintenanceData.getString("durationYear");
            query.delete(
                maintenanceTaskIRI.has(hasInterval, intervalIRI),
                intervalIRI.has(hasDurationDescription, durationIRI),
                //durationIRI.has(months, Rdf.literalOf(durationMonth)).andHas(years, Rdf.literalOf(durationYear)),
                durationIRI.has(SparqlBuilder.var("timePred"), SparqlBuilder.var("timeDuration")),
                intervalIRI.isA(Interval),
                durationIRI.isA(DurationDescription)
            );
            //Does not recognise the literal somehow
            query.where(durationIRI.has(SparqlBuilder.var("timePred"), SparqlBuilder.var("timeDuration")));
        }
        
        

        LOGGER.debug("Maintenance delete query:: " + query.getQueryString());
        storeClientAsset.executeUpdate(query.getQueryString());

    }

    public void deleteByLiteral (String literal, String predicate, RemoteStoreClient storeClient){
        deleteByLiteral (literal, iri(predicate), storeClient);
    }

    public void deleteByLiteral (String literal, Iri predicate, RemoteStoreClient storeClient){
        ModifyQuery query = Queries.DELETE();
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG_FORMAL, Pref_FIBO_ORG_ORGS, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        Variable subject = SparqlBuilder.var("Subject");

        try {
            query.delete(subject.has(predicate, literal));
            query.where(subject.has(predicate, literal));

            storeClient.executeUpdate(query.getQueryString());
        } catch (Exception e) {
            LOGGER.error("Failed to delete literal: "+ literal , e);
            throw new JPSRuntimeException("Failed to delete literal: "+ literal , e);
        }
        
    }

    public void deleteBySubject(String objectIRI, String predicate, RemoteStoreClient storeClient){
        deleteBySubject(iri(objectIRI), iri(predicate), storeClient);
    }

    public void deleteBySubject(Iri objectIRI, Iri predicate, RemoteStoreClient storeClient){
        ModifyQuery query = Queries.DELETE();
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG_FORMAL, Pref_FIBO_ORG_ORGS, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        Variable subject = SparqlBuilder.var("Subject");

        try {
            query.delete(subject.has(predicate, objectIRI));
            query.where(subject.has(predicate, objectIRI));

            storeClient.executeUpdate(query.getQueryString());
        } catch (Exception e) {
            LOGGER.error("Failed to delete literal: "+ objectIRI , e);
            throw new JPSRuntimeException("Failed to delete literal: "+ objectIRI , e);
        }

    }

    public JSONArray deleteByAssetData(JSONObject assetData) {
        JSONArray message = new JSONArray();
        assetData.put("itemType", "https://purl.org/p2p-o/item#Item");
        String[] keyArrayAsset = {
             "deviceTypeIRI","label","personIRI","serialNum",
            "modelNumber","priceMeasureIRI", "price", "manufacturerURL", "SpecSheetIRI",
            "ManualIRI", "SupplierOrgIRI","ManufacturerIRI","workspaceIRI", "storage",
            "itemComment","itemType","ID"
        };

        String[] keyArrayDevice = {
            "deviceTypeIRI",
            "roomIRI",
            "storageIRI",
            "Location"
        };

        String[] keyArrayPurchDoc = {
            "itemName",
            "itemType",
            "itemComment",
            "ServiceCategoryIRI",
            "InvoiceIRI",
            "DeliveryOrderIRI",
            "PurchaseOrderIRI",
            "PriceDetailsIRI",
            "SupplierOrgIRI",
            "ManufacturerIRI"
        };
        

        //Get itemIRI, assetIRI
        String deviceIRI = assetData.getString("deviceIRI");
        String itemIRI = assetData.getString("itemIRI");

        //Delete asset endpoint
        deleteNamespace(iri(itemIRI), storeClientAsset, assetData, keyArrayAsset);
        deleteNamespace(iri(deviceIRI), storeClientAsset, assetData, keyArrayAsset);
        //Delete purchase doc endpoint
        deleteNamespace(iri(itemIRI), storeClientPurchDoc, assetData, keyArrayPurchDoc);
        //Delete device endpoint
        //TODO add check here? So only need to call once?
        deleteNamespace(iri(deviceIRI), storeClientOffice, assetData, keyArrayDevice);
        deleteNamespace(iri(deviceIRI), storeClientLab, assetData, keyArrayDevice);


        return message;
    }

    private void deleteNamespace (Iri startIRI, RemoteStoreClient storeClient, JSONObject assetData, String[] keyArray){
        for (String key : keyArray){
            String target = assetData.getString(key).replace("\\", "\\\\"); //To prevent lexical error in the SPARQL query
            //if (!target.isBlank()){
                deletePathTo(startIRI, target, storeClient);
            //}
        }
    }

    public void deletePathTo (Iri start, String target, RemoteStoreClient storeClient) {
        ModifyQuery query = Queries.DELETE();
        SelectQuery loggingQuery = Queries.SELECT();
        query.prefix(Pref_DEV, Pref_LAB, Pref_SYS, Pref_INMA, Pref_ASSET, Pref_EPE, Pref_BIM, Pref_SAREF,
            Pref_OM, Pref_FIBO_AAP, Pref_FIBO_ORG_FORMAL, Pref_FIBO_ORG_ORGS, Pref_BOT, Pref_P2P_ITEM, Pref_P2P_DOCLINE, Pref_P2P_INVOICE
        );
        //Variable start = SparqlBuilder.var("start");
        Variable subject = SparqlBuilder.var("subject");
        Variable predicate = SparqlBuilder.var("predicate");
        Variable object = SparqlBuilder.var("object");
        Variable toReplace  = SparqlBuilder.var("toReplace");
        String BothWayWildcard = "((<>|!<>)|^(<>|!<>))*"; //https://stackoverflow.com/questions/30916040/sparql-is-there-any-path-between-two-nodes
        String OneWayWildCard = "(<>|!<>)*"; //Using both-ways retrieve the whole graph apparently

        loggingQuery.select(subject, predicate, object);
        loggingQuery.where(start.has(toReplace, subject));
        loggingQuery.where(subject.has(predicate, object));
        if (isIRI(target)){
            loggingQuery.where(GraphPatterns.union(
                object.has(toReplace, iri(target)),
                object.has(toReplace, target)
            ));
        }
        else{
            loggingQuery.where(object.has(toReplace, target));
        }
        

        query.delete(subject.has(predicate, object));
        query.where(start.has(toReplace, subject));
        query.where(subject.has(predicate, object));
        if (isIRI(target)){
            query.where(GraphPatterns.union(
                object.has(toReplace, iri(target)),
                object.has(toReplace, target)
            ));
        }
        else{
            query.where(object.has(toReplace, target));
        }

        String loggingQueryString = loggingQuery.getQueryString().replace("?toReplace", OneWayWildCard);
        String queryString = query.getQueryString().replace("?toReplace", OneWayWildCard);
        LOGGER.info("LOGGINGQUERY::"+loggingQueryString);
        saveDeleteLog(storeClient.executeQuery(loggingQueryString), storeClient.getUpdateEndpoint());
        LOGGER.info("DELETE query :: " + queryString);
        //ACTIVATE ONLY WHEN READY FOR PROD
        //AS WE ALL KNOW WHAT WILL HAPPEN WHEN THIS THING FAILS
        storeClient.executeUpdate(queryString);

        //For when the target is the object
        if (isIRI(target)){
            query = Queries.DELETE();
            loggingQuery = Queries.SELECT();

            loggingQuery.select(subject, predicate, object);
            loggingQuery.where(object.has(toReplace, start));
            loggingQuery.where(subject.has(predicate, object));
            loggingQuery.where(iri(target).has(toReplace, subject));

            query.delete(subject.has(predicate, object));
            query.where(object.has(toReplace, start));
            query.where(subject.has(predicate, object));
            query.where(iri(target).has(toReplace, subject));
            loggingQueryString = loggingQuery.getQueryString().replace("?toReplace", OneWayWildCard);
            queryString = query.getQueryString().replace("?toReplace", OneWayWildCard);
            LOGGER.info("LOGGINGQUERY::"+loggingQueryString);
            saveDeleteLog(storeClient.executeQuery(loggingQueryString), storeClient.getUpdateEndpoint());
            LOGGER.info("DELETE query :: " + queryString);
            //ACTIVATE ONLY WHEN READY FOR PROD
            //AS WE ALL KNOW WHAT WILL HAPPEN WHEN THIS THING FAILS
            storeClient.executeUpdate(queryString);
        }

    }

    private void saveDeleteLog (JSONArray reqRes, String endpoint){
        if (reqRes.length() > 1){
            //Create hash -- UUID?
            String logID = UUID.randomUUID().toString();
            //Log hash with deleted query, time of exe, and update endpoint
            try {
                File logFile = new File(DelLogFileLoc);
                if(!logFile.exists()) {
                    if (logFile.createNewFile()) {
                    LOGGER.info("File created: " + logFile.getName());
                    }
                }
                BufferedWriter bw = new BufferedWriter(new FileWriter(logFile, true));

                String line = logID + "::" + endpoint + "::" + reqRes+"\n";

                bw.write(line);
                bw.close();

            } catch (IOException e) {
                LOGGER.error("Failed to save delete log");
                //throw new JPSRuntimeException("Failed to save delete log. Cancelling delete for safety");
            }
        }
        
    }

    
}
