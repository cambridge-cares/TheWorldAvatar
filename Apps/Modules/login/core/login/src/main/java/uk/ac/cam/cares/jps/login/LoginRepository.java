package uk.ac.cam.cares.jps.login;

import android.content.Intent;

import androidx.activity.result.ActivityResultLauncher;
import androidx.fragment.app.Fragment;

import com.google.android.material.dialog.MaterialAlertDialogBuilder;

import kotlin.Pair;
import uk.ac.cam.cares.jps.utils.RepositoryCallback;

public interface LoginRepository {
    void initAuth(RepositoryCallback<Boolean> callback);
    void doAuth(RepositoryCallback<Intent> callback);
    void processAuthorizationResponse(Intent data, RepositoryCallback<Boolean> repositoryCallback);
    void getUserInfo(RepositoryCallback<User> repositoryCallback);
    Intent getLogOutIntent();
    ActivityResultLauncher<Intent> getLogoutLauncher(Fragment fragment, RepositoryCallback<Pair<Boolean, String>> callback);
    MaterialAlertDialogBuilder getSessionExpiredDialog(Fragment fragment);
}
