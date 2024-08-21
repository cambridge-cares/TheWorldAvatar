package uk.ac.cam.cares.jps.timeline.ui.bottomsheet;

import static uk.ac.cam.cares.jps.timeline.ui.bottomsheet.ErrorBottomSheet.ErrorType.ACCOUNT_ERROR;
import static uk.ac.cam.cares.jps.timeline.ui.bottomsheet.ErrorBottomSheet.ErrorType.CONNECTION_ERROR;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.appcompat.widget.LinearLayoutCompat;

import java.util.HashMap;
import java.util.Map;

import uk.ac.cam.cares.jps.timeline.viewmodel.ConnectionViewModel;
import uk.ac.cam.cares.jps.timeline.viewmodel.UserPhoneViewModel;
import uk.ac.cam.cares.jps.timelinemap.R;

public class ErrorBottomSheet extends BottomSheet{

    private final Map<ErrorType, View.OnClickListener> errorHandler;

    public enum ErrorType {
        CONNECTION_ERROR,
        TRAJECTORY_ERROR,
        ACCOUNT_ERROR
    }

    /**
     * Constructor
     * @param context fragment context
     */
    public ErrorBottomSheet(@NonNull Context context,
                            ConnectionViewModel connectionViewModel,
                            UserPhoneViewModel userPhoneViewModel) {
        super(context);
        this.errorHandler = getErrorTypeOnClickListenerMap(connectionViewModel, userPhoneViewModel);
    }

    /**
     * Inflate ui for the bottomSheet layout object
     * @param context fragment context
     */
    @Override
    void init(Context context) {
        bottomSheet = (LinearLayoutCompat) LayoutInflater.from(context).inflate(R.layout.bottom_sheet_widget_error, null);
    }

    /**
     * Configure the bottom sheet message and retry operation based on the error type
     * @param errorType
     */
    public void setErrorType(ErrorType errorType) {
        switch (errorType) {
            case CONNECTION_ERROR -> {
                ((ImageView) bottomSheet.findViewById(R.id.error_icon)).setImageResource(R.drawable.outline_cancel_24);
                ((TextView) bottomSheet.findViewById(R.id.error_tv)).setText(R.string.no_connection_please_check_internet_connection_and_try_again);
                bottomSheet.findViewById(R.id.try_again_button).setOnClickListener(errorHandler.get(CONNECTION_ERROR));
            }
            case TRAJECTORY_ERROR -> {
                ((ImageView) bottomSheet.findViewById(R.id.error_icon)).setImageResource(R.drawable.baseline_error_outline_24);
                ((TextView) bottomSheet.findViewById(R.id.error_tv)).setText(R.string.encountered_error);
                bottomSheet.findViewById(R.id.try_again_button).setOnClickListener(errorHandler.get(CONNECTION_ERROR));
            }
            case ACCOUNT_ERROR -> {
                ((ImageView) bottomSheet.findViewById(R.id.error_icon)).setImageResource(R.drawable.baseline_error_outline_24);
                ((TextView) bottomSheet.findViewById(R.id.error_tv)).setText(R.string.encountered_error);
                bottomSheet.findViewById(R.id.try_again_button).setOnClickListener(errorHandler.get(ACCOUNT_ERROR));
            }
        }

    }

    private Map<ErrorBottomSheet.ErrorType, View.OnClickListener> getErrorTypeOnClickListenerMap(ConnectionViewModel connectionViewModel,
                                                                                                 UserPhoneViewModel userPhoneViewModel) {
        View.OnClickListener retryConnectionAndRetrieveTrajectory = view -> connectionViewModel.checkNetworkConnection();
        View.OnClickListener registerUserToPhone = view -> userPhoneViewModel.registerPhoneToUser();

        Map<ErrorBottomSheet.ErrorType, View.OnClickListener> errorHandler = new HashMap<>();
        errorHandler.put(ErrorBottomSheet.ErrorType.CONNECTION_ERROR, retryConnectionAndRetrieveTrajectory);
        errorHandler.put(ErrorBottomSheet.ErrorType.ACCOUNT_ERROR, registerUserToPhone);
        return errorHandler;
    }
}
