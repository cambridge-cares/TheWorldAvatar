package uk.ac.cam.cares.jps.data;

import uk.ac.cam.cares.jps.login.LoginRepository;
import uk.ac.cam.cares.jps.login.User;
import uk.ac.cam.cares.jps.network.TrajectoryNetworkSource;

public class TrajectoryRepository{
    private TrajectoryNetworkSource trajectoryNetworkSource;
    private LoginRepository loginRepository;

    public TrajectoryRepository(TrajectoryNetworkSource trajectoryNetworkSource, LoginRepository loginRepository) {
        this.trajectoryNetworkSource = trajectoryNetworkSource;
        this.loginRepository = loginRepository;
    }

    public void getTrajectory(RepositoryCallback<String> callback) {
        loginRepository.getUserInfo(new uk.ac.cam.cares.jps.login.RepositoryCallback<>() {
            @Override
            public void onSuccess(User result) {
                trajectoryNetworkSource.getTrajectory(result.getId(), callback::onSuccess, volleyError -> callback.onFailure(new Throwable("Failed to get trajectory")));
            }

            @Override
            public void onFailure(Throwable error) {
                callback.onFailure(error);
            }
        });

    }
}
