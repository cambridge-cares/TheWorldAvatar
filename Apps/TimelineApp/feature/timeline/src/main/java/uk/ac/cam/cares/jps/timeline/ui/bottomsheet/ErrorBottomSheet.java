package uk.ac.cam.cares.jps.timeline.ui.bottomsheet;

import static android.view.View.inflate;

import static uk.ac.cam.cares.jps.timeline.ui.bottomsheet.ErrorBottomSheet.ErrorType.ACCOUNT_ERROR;
import static uk.ac.cam.cares.jps.timeline.ui.bottomsheet.ErrorBottomSheet.ErrorType.CONNECTION_ERROR;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.appcompat.widget.LinearLayoutCompat;

import java.util.Map;

import uk.ac.cam.cares.jps.timelinemap.R;

public class ErrorBottomSheet extends BottomSheet{

    private Map<ErrorType, View.OnClickListener> errorHandler;

    public enum ErrorType {
        CONNECTION_ERROR,
        TRAJECTORY_ERROR,
        ACCOUNT_ERROR
    }

    // todo: not very ideal to get the viewModel in this way
    public ErrorBottomSheet(@NonNull Context context,
                            Map<ErrorType, View.OnClickListener> errorHandler) {
        super(context);
        this.errorHandler = errorHandler;
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
                bottomSheet.findViewById(R.id.try_again_button).setOnClickListener(errorHandler.get(CONNECTION_ERROR));
            }
            case TRAJECTORY_ERROR -> {
                ((ImageView) bottomSheet.findViewById(R.id.error_icon)).setImageResource(R.drawable.baseline_error_outline_24);
                ((TextView) bottomSheet.findViewById(R.id.error_tv)).setText(R.string.encountered_error);
                // same way to handle trajectory error with connection error
                bottomSheet.findViewById(R.id.try_again_button).setOnClickListener(errorHandler.get(CONNECTION_ERROR));
            }
            case ACCOUNT_ERROR -> {
                ((ImageView) bottomSheet.findViewById(R.id.error_icon)).setImageResource(R.drawable.baseline_error_outline_24);
                ((TextView) bottomSheet.findViewById(R.id.error_tv)).setText(R.string.encountered_error);
                // todo: trigger user phone registration
                bottomSheet.findViewById(R.id.try_again_button).setOnClickListener(errorHandler.get(ACCOUNT_ERROR));
            }
        }

    }
}
