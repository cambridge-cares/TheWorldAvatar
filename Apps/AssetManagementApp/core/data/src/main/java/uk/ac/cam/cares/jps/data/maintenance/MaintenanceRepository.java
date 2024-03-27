package uk.ac.cam.cares.jps.data.maintenance;

import uk.ac.cam.cares.jps.data.RepositoryCallback;
import uk.ac.cam.cares.jps.model.Maintenance;
import uk.ac.cam.cares.jps.network.maintenance.MaintenanceNetworkSource;

public class MaintenanceRepository {
    MaintenanceNetworkSource networkSource;

    public MaintenanceRepository(MaintenanceNetworkSource networkSource) {
        this.networkSource = networkSource;
    }

    public void addMaintenance(Maintenance maintenance, RepositoryCallback<Boolean> callback) {
        networkSource.addMaintenance(maintenance, response -> callback.onSuccess(true), callback::onFailure);
    }
}
