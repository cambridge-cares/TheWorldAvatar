package uk.ac.cam.cares.jps.agent.cea.tasks;

import java.lang.reflect.Field;
import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.stream.Collectors;

import org.json.JSONArray;

import uk.ac.cam.cares.jps.agent.cea.data.CEAConstants;
import uk.ac.cam.cares.jps.agent.cea.data.CEAOutputData;
import uk.ac.cam.cares.jps.agent.cea.utils.AnnualValueHelper;
import uk.ac.cam.cares.jps.agent.cea.utils.TimeSeriesHelper;
import uk.ac.cam.cares.jps.agent.cea.utils.datahandler.DataManager;
import uk.ac.cam.cares.jps.base.query.RemoteRDBStoreClient;
import uk.ac.cam.cares.jps.base.query.RemoteStoreClient;

public class CEAOutputUpdater {

    private RemoteRDBStoreClient rdbStoreClient;
    private RemoteStoreClient storeClient;
    private String tsDb;
    private String ceaRoute;

    public CEAOutputUpdater(RemoteStoreClient storeClient, RemoteRDBStoreClient rdbStoreClient, String tsDb, String ceaRoute) {
        this.storeClient = storeClient;
        this.rdbStoreClient = rdbStoreClient;
        this.tsDb = tsDb;
        this.ceaRoute = ceaRoute;
    }

    /**
     * Update triplestore and relational database after CEA simulation
     * 
     * @param uriArray   List of building URI
     * @param times      List of time of outputs
     * @param timeSeries List of list of list of time series (building, variable,
     *                   value)
     * @param scalars    Hash map of scalar outputs (variable name, list of values)
     */
    public void updateCEA(JSONArray uriArray, List<OffsetDateTime> times, List<List<List<?>>> timeSeries,
            LinkedHashMap<String, List<Double>> scalars) {
        TimeSeriesHelper tsHelper = new TimeSeriesHelper(storeClient, rdbStoreClient, tsDb);

        List<String> uninitialisedBuilding = DataManager.bulkCheckBuildingInitialised(uriArray, ceaRoute);
        DataManager.bulkInitialiseBuilding(uninitialisedBuilding, ceaRoute);

        for (int i = 0; i < uriArray.length(); i++) {
            LinkedHashMap<String, String> tsIris = new LinkedHashMap<>();
            LinkedHashMap<String, String> scalarIris = new LinkedHashMap<>();

            String uri = uriArray.getString(i);

            if (!DataManager.checkDataInitialised(uri, tsIris, scalarIris, ceaRoute)) {
                tsHelper.createTimeSeries(tsIris);
                DataManager.initialiseData(i, scalars, uri, tsIris, scalarIris, ceaRoute);
            } else {
                DataManager.updateScalars(ceaRoute, scalarIris, scalars, i);
            }
            tsHelper.addDataToTimeSeries(timeSeries.get(i), times, tsIris);
            AnnualValueHelper.instantiateAnnual(timeSeries.get(i), tsIris, ceaRoute);
        }
    }

    public void updateCEA(CEAOutputData result) {
        JSONArray uriArray = new JSONArray(result.iris);
        List<OffsetDateTime> times = result.times.stream().map(OffsetDateTime::parse).collect(Collectors.toList());

        Class<?> clazz = result.getClass();

        LinkedHashMap<String, List<Double>> scalars = new LinkedHashMap<>();
        for (String scalar : CEAConstants.SCALARS) {
            try {
                Field scalarField = clazz.getDeclaredField(scalar);
                scalarField.setAccessible(true);
                List<Double> scalarValue = (List<Double>) scalarField.get(result);
                scalars.put(scalar, scalarValue);
            } catch (NoSuchFieldException | SecurityException | IllegalArgumentException | IllegalAccessException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }

        List<List<List<?>>> timeSeries = new ArrayList<>();
        for (int i = 0; i < uriArray.length(); i++) {
            List<List<?>> iriList = new ArrayList<>();
            for (String ts : CEAConstants.TIME_SERIES) {
                try {
                    Field tsField = clazz.getDeclaredField(ts);
                    tsField.setAccessible(true);
                    List<List<Double>> tsValue = (List<List<Double>>) tsField.get(result);
                    iriList.add(tsValue.get(i));
                    
                } catch (NoSuchFieldException | SecurityException | IllegalArgumentException | IllegalAccessException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
            }
            timeSeries.add(iriList);
        }

        updateCEA(uriArray, times, timeSeries, scalars);

    }
}
