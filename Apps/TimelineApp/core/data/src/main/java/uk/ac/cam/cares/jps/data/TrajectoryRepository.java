package uk.ac.cam.cares.jps.data;

import android.content.Context;

import uk.ac.cam.cares.jps.login.AccountException;
import uk.ac.cam.cares.jps.login.LoginRepository;
import uk.ac.cam.cares.jps.login.User;
import uk.ac.cam.cares.jps.network.TrajectoryNetworkSource;
import uk.ac.cam.cares.jps.utils.RepositoryCallback;

/**
 * A repository class that gathers information for trajectory related requests and call the lower level network source.
 */
public class TrajectoryRepository {
    private TrajectoryNetworkSource trajectoryNetworkSource;
    private LoginRepository loginRepository;
    private Context context;

    /**
     * Constructor of the class. Instantiation of this class is done by dependency injection.
     * @param trajectoryNetworkSource Network source managed by this class, which is responsible for trajectory related request.
     * @param loginRepository Repository responsible for user login related functions.
     * @param context Context of the app.
     */
    public TrajectoryRepository(TrajectoryNetworkSource trajectoryNetworkSource,
                                LoginRepository loginRepository,
                                Context context) {
        this.trajectoryNetworkSource = trajectoryNetworkSource;
        this.loginRepository = loginRepository;
        this.context = context;
    }

    /**
     * Get trajectory of the logged in user on the chosen date
     * @param lowerbound lower bound of the chosen date in millisecond
     * @param upperbound upper bound of the chosen date in millisecond
     * @param callback Callback to notify UI level components when responses are returned from server
     */
    public void getTrajectory(long lowerbound, long upperbound, RepositoryCallback<String> callback) {
        loginRepository.getAccessToken(new RepositoryCallback<>() {
            @Override
            public void onSuccess(String accessToken) {
                trajectoryNetworkSource.getTrajectory(accessToken, lowerbound, upperbound,
                        callback::onSuccess,
                        error -> callback.onFailure(new Throwable("Failed to get trajectory", error)));
            }

            @Override
            public void onFailure(Throwable error) {
                callback.onFailure(error);
            }
        });

    }
}
