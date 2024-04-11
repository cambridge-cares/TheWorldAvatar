package uk.ac.cam.cares.jps.timeline.ui.bottomsheet;

import static android.view.View.inflate;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.appcompat.widget.LinearLayoutCompat;

import uk.ac.cam.cares.jps.timelinemap.R;

public class ErrorBottomSheet extends BottomSheet{

    private View.OnClickListener retryConnectionAndRetrieveTrajectory;

    public enum ErrorType {
        CONNECTION_ERROR,
        TRAJECTORY_ERROR
    }

    // todo: not very ideal to get the viewModel in this way
    public ErrorBottomSheet(@NonNull Context context,
                            View.OnClickListener retryConnectionAndRetrieveTrajectory) {
        super(context);
        this.retryConnectionAndRetrieveTrajectory = retryConnectionAndRetrieveTrajectory;
    }

    @Override
    void init(Context context) {
        bottomSheet = (LinearLayoutCompat) LayoutInflater.from(context).inflate(R.layout.bottom_sheet_widget_error, null);
    }

    public void setErrorMessage(ErrorType errorType) {
        switch (errorType) {
            case CONNECTION_ERROR -> {
                ((ImageView) bottomSheet.findViewById(R.id.error_icon)).setImageResource(R.drawable.outline_cancel_24);
                ((TextView) bottomSheet.findViewById(R.id.error_tv)).setText(R.string.no_connection_please_check_internet_connection_and_try_again);
                bottomSheet.findViewById(R.id.try_again_button).setOnClickListener(retryConnectionAndRetrieveTrajectory);
            }
            case TRAJECTORY_ERROR -> {
                ((ImageView) bottomSheet.findViewById(R.id.error_icon)).setImageResource(R.drawable.baseline_error_outline_24);
                ((TextView) bottomSheet.findViewById(R.id.error_tv)).setText(R.string.encountered_error);
                bottomSheet.findViewById(R.id.try_again_button).setOnClickListener(retryConnectionAndRetrieveTrajectory);
            }
        }

    }
}
