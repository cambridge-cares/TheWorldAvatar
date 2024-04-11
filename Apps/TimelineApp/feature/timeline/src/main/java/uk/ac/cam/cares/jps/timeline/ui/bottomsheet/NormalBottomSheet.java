package uk.ac.cam.cares.jps.timeline.ui.bottomsheet;

import static android.view.View.inflate;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.appcompat.widget.LinearLayoutCompat;

import uk.ac.cam.cares.jps.timelinemap.R;

public class NormalBottomSheet extends BottomSheet {


    public NormalBottomSheet(@NonNull Context context) {
        super(context);
    }

    @Override
    void init(Context context) {
        bottomSheet = (LinearLayoutCompat) LayoutInflater.from(context).inflate(R.layout.bottom_sheet_widget, null);
    }
}
