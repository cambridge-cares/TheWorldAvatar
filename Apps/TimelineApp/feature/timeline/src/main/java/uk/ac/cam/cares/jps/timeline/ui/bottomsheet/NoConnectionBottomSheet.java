package uk.ac.cam.cares.jps.timeline.ui.bottomsheet;

import static android.view.View.inflate;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;

import uk.ac.cam.cares.jps.timeline.ConnectionViewModel;
import uk.ac.cam.cares.jps.timelinemap.R;

public class NoConnectionBottomSheet extends BottomSheet{
    ConnectionViewModel connectionViewModel;
    public NoConnectionBottomSheet(@NonNull Context context, ViewGroup parent, ConnectionViewModel connectionViewModel) {
        super(context, parent);
        this.connectionViewModel = connectionViewModel;
    }

    @Override
    void init(Context context, ViewGroup parent) {
        bottomSheet = inflate(context, R.layout.bottom_sheet_widget_not_connected, parent).findViewById(R.id.bottom_sheet);
        bottomSheet.findViewById(R.id.try_again_button).setOnClickListener(view -> {
            connectionViewModel.checkNetworkConnection();
        });
    }
}
