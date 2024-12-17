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

/**
 * A repository that manages dates information for trajectory.
 */
public class DatesWithTrajectoryRepository {
    private final LoginRepository loginRepository;
    private final Context context;
    private final DatesWithTrajectoryNetworkSource datesWithTrajectoryNetworkSource;
    private final Logger LOGGER = LogManager.getLogger(DatesWithTrajectoryRepository.class);

    /**
     * Constructor of the class. Instantiation of this class is done by dependency injection.
     * @param datesWithTrajectoryNetworkSource Network source managed by this class, which is responsible for date related request.
     * @param loginRepository Repository responsible for user login related functions.
     * @param context Context of the app.
     */
    public DatesWithTrajectoryRepository(DatesWithTrajectoryNetworkSource datesWithTrajectoryNetworkSource,
                                         LoginRepository loginRepository,
                                         Context context) {
        this.datesWithTrajectoryNetworkSource = datesWithTrajectoryNetworkSource;
        this.loginRepository = loginRepository;
        this.context = context;
    }

    /**
     * Get dates that have trajectory data for the logged in user
     * @param timezone The phone's current timezone
     * @param callback Callback to notify UI level components when responses are returned from server
     */
    public void getDatesWithTrajectory(String timezone, RepositoryCallback<Map<YearMonthCompositeKey, List<Integer>>> callback) {
        loginRepository.getAccessToken(new RepositoryCallback<>() {
            @Override
            public void onSuccess(String result) {
                datesWithTrajectoryNetworkSource.getDates(result, timezone,
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
