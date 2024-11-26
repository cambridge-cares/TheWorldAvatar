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
     * @param date Date chosen for which trajectory to show
     * @param callback Callback to notify UI level components when responses are returned from server
     */
    public void getTrajectory(String date, RepositoryCallback<String> callback) {
        loginRepository.getUserInfo(new RepositoryCallback<>() {
            @Override
            public void onSuccess(User result) {
                trajectoryNetworkSource.getTrajectory(result.getId(), date,
                        callback::onSuccess,
                        volleyError -> {
                            if (volleyError.getMessage() != null &&
                                    volleyError.getMessage().equals(context.getString(uk.ac.cam.cares.jps.utils.R.string.trajectoryagent_no_phone_id_on_the_user))) {
                                callback.onFailure(new AccountException(context.getString(uk.ac.cam.cares.jps.utils.R.string.trajectoryagent_no_phone_id_on_the_user)));
                                return;
                            }

                            callback.onFailure(new Throwable("Failed to get trajectory"));
                        });
            }

            @Override
            public void onFailure(Throwable error) {
                callback.onFailure(error);
            }
        });

    }
}
