package uk.ac.cam.cares.jps.data;

import android.content.Context;

import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;

import java.util.List;
import java.util.Map;

import uk.ac.cam.cares.jps.login.LoginRepository;
import uk.ac.cam.cares.jps.login.User;
import uk.ac.cam.cares.jps.model.YearMonthCompositeKey;
import uk.ac.cam.cares.jps.network.DatesWithTrajectoryNetworkSource;
import uk.ac.cam.cares.jps.utils.RepositoryCallback;

public class DatesWithTrajectoryRepository {
    private final LoginRepository loginRepository;
    private final Context context;
    private final DatesWithTrajectoryNetworkSource datesWithTrajectoryNetworkSource;
    private final Logger LOGGER = LogManager.getLogger(DatesWithTrajectoryRepository.class);

    public DatesWithTrajectoryRepository(DatesWithTrajectoryNetworkSource datesWithTrajectoryNetworkSource,
                                         LoginRepository loginRepository,
                                         Context context) {
        this.datesWithTrajectoryNetworkSource = datesWithTrajectoryNetworkSource;
        this.loginRepository = loginRepository;
        this.context = context;
    }

    public void getDatesWithTrajectory(String timezone, RepositoryCallback<Map<YearMonthCompositeKey, List<Integer>>> callback) {
        loginRepository.getUserInfo(new RepositoryCallback<>() {
            @Override
            public void onSuccess(User result) {
                datesWithTrajectoryNetworkSource.getDates(result.getId(), timezone,
                        callback::onSuccess,
                        volleyError -> LOGGER.error("error when retrieving dates, not sure how to handle yet"));
            }

            @Override
            public void onFailure(Throwable error) {
                callback.onFailure(error);
            }
        });
    }
}
