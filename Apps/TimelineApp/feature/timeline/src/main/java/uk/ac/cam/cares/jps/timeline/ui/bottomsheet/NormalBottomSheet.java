

package uk.ac.cam.cares.jps.timeline.ui.bottomsheet;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.TextView;

import androidx.appcompat.widget.LinearLayoutCompat;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import java.util.List;

import uk.ac.cam.cares.jps.model.UniqueSessions;
import uk.ac.cam.cares.jps.timeline.ui.adapter.UniqueSessionsAdapter;
import uk.ac.cam.cares.jps.timelinemap.R;

/**
 * Bottom sheet to be shown when there is no error
 */
public class NormalBottomSheet extends BottomSheet {

    private RecyclerView recyclerView;
    private UniqueSessionsAdapter adapter;

    public NormalBottomSheet(Context context) {
        super(context);
        init(context);
    }

    @Override
    void init(Context context) {
        bottomSheet = (LinearLayoutCompat) LayoutInflater.from(context).inflate(R.layout.bottom_sheet_widget, null);

        recyclerView = bottomSheet.findViewById(R.id.recycler_view);

        adapter = new UniqueSessionsAdapter();
        recyclerView.setAdapter(adapter);
        recyclerView.setLayoutManager(new LinearLayoutManager(context));
    }

    public void showFetchingAnimation(boolean isFetching) {
        if (isFetching) {
            adapter = new UniqueSessionsAdapter();
            recyclerView.setAdapter(adapter);
            
            getBottomSheet().findViewById(R.id.progress_linear).setVisibility(View.VISIBLE);
            getBottomSheet().findViewById(R.id.trajectory_info_tv).setVisibility(View.GONE);
            recyclerView.setVisibility(View.GONE);
            
        
        } else {
            getBottomSheet().findViewById(R.id.progress_linear).setVisibility(View.GONE);
            recyclerView.setVisibility(View.VISIBLE);
        }
    }

    public void updateUniqueSessionsList(List<UniqueSessions> uniqueSessionsList) {
        if (uniqueSessionsList != null && !uniqueSessionsList.isEmpty()) {
            adapter.setUniqueSessionsList(uniqueSessionsList);
            adapter.notifyDataSetChanged();
            
            TextView trajectoryTextView = getBottomSheet().findViewById(R.id.trajectory_info_tv);
            if (trajectoryTextView != null) {
                trajectoryTextView.setVisibility(View.GONE);
            }
        } else {
            showEmptyState(); 
        }
    }

    private void showEmptyState() {
        TextView trajectoryTextView = getBottomSheet().findViewById(R.id.trajectory_info_tv);
        if (trajectoryTextView != null) {
            trajectoryTextView.setText(uk.ac.cam.cares.jps.utils.R.string.trajectoryagent_no_trajectory_found);
            trajectoryTextView.setVisibility(View.VISIBLE); 
        }
    }
}
