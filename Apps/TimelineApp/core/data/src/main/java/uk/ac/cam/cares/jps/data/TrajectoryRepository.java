package uk.ac.cam.cares.jps.data;

import android.content.Context;

import uk.ac.cam.cares.jps.login.AccountException;
import uk.ac.cam.cares.jps.login.LoginRepository;
import uk.ac.cam.cares.jps.login.User;
import uk.ac.cam.cares.jps.network.TrajectoryNetworkSource;
import uk.ac.cam.cares.jps.utils.RepositoryCallback;

public class TrajectoryRepository {
    private TrajectoryNetworkSource trajectoryNetworkSource;
    private LoginRepository loginRepository;
    private Context context;

    public TrajectoryRepository(TrajectoryNetworkSource trajectoryNetworkSource,
                                LoginRepository loginRepository,
                                Context context) {
        this.trajectoryNetworkSource = trajectoryNetworkSource;
        this.loginRepository = loginRepository;
        this.context = context;
    }

    public void getTrajectory(RepositoryCallback<String> callback) {
        loginRepository.getUserInfo(new RepositoryCallback<>() {
            @Override
            public void onSuccess(User result) {
                trajectoryNetworkSource.getTrajectory(result.getId(),
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
