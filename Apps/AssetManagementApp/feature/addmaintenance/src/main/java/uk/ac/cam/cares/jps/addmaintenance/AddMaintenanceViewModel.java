package uk.ac.cam.cares.jps.addmaintenance;

import static uk.ac.cam.cares.jps.utils.AssetInfoConstant.INVENTORY_ID;

import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

import org.apache.log4j.BasicConfigurator;
import org.apache.log4j.Logger;

import java.time.LocalDate;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import uk.ac.cam.cares.jps.data.RepositoryCallback;
import uk.ac.cam.cares.jps.data.maintenance.MaintenanceRepository;
import uk.ac.cam.cares.jps.model.AssetInfo;
import uk.ac.cam.cares.jps.model.Maintenance;

@HiltViewModel
public class AddMaintenanceViewModel extends ViewModel {
    Maintenance maintenance = new Maintenance();

    MutableLiveData<Boolean> isServiceDateMissing = new MutableLiveData<>();
    MutableLiveData<Boolean> isTimeTravel = new MutableLiveData<>();
    MutableLiveData<Boolean> isServiceProviderMissing = new MutableLiveData<>();
    MutableLiveData<Boolean> isMaintenanceUpdated = new MutableLiveData<>();

    private MaintenanceRepository repository;
    private Logger LOGGER = Logger.getLogger(AddMaintenanceViewModel.class);

    @Inject
    AddMaintenanceViewModel(MaintenanceRepository repository) {
        BasicConfigurator.configure();
        this.repository = repository;
    }

    public void addMaintenance() {
        boolean hasInputError = false;
        if (maintenance.getLastServiceDate().isEmpty() && maintenance.getNextServiceDate().isEmpty()) {
            isServiceDateMissing.setValue(true);
            hasInputError = true;
        }

        if (maintenance.getServiceProvider().isEmpty()) {
            isServiceProviderMissing.setValue(true);
            hasInputError = true;
        }

        if (!maintenance.getLastServiceDate().isEmpty() && !maintenance.getNextServiceDate().isEmpty()) {
            LocalDate lastServiceDate = LocalDate.parse(maintenance.getLastServiceDate());
            LocalDate nextServiceDate = LocalDate.parse(maintenance.getNextServiceDate());

            if (nextServiceDate.isBefore(lastServiceDate)) {
                isTimeTravel.setValue(true);
                hasInputError = true;
            }
        }

        if (hasInputError) {
            return;
        }

        repository.addMaintenance(maintenance, new RepositoryCallback<Boolean>() {
            @Override
            public void onSuccess(Boolean result) {
                isMaintenanceUpdated.setValue(true);
            }

            @Override
            public void onFailure(Throwable error) {
                isMaintenanceUpdated.setValue(false);
            }
        });
    }

    public void initMaintenanceInfoFromAssetInfo(AssetInfo assetInfo) {
        maintenance.setId(assetInfo.getProperty(INVENTORY_ID));
        // todo: add the rest field, need to fix keys in constant as well
    }
}
