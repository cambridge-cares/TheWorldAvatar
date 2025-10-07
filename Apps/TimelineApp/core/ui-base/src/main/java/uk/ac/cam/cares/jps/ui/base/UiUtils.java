package uk.ac.cam.cares.jps.ui.base;

import android.content.Context;

import com.google.android.material.dialog.MaterialAlertDialogBuilder;

/**
 * A class for common shared UI functions
 */
public class UiUtils {
    /**
     * Show a dialog for not implemented feature
     * @param context
     */
    public static void showNotImplementedDialog(Context context) {
        new MaterialAlertDialogBuilder(context)
                .setMessage(uk.ac.cam.cares.jps.ui.base.R.string.not_yet_implemented)
                .setPositiveButton(uk.ac.cam.cares.jps.ui.base.R.string.ok, (dialogInterface, i) -> {})
                .show();
    }
}
