package uk.ac.cam.cares.jps.mailbox;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import java.util.ArrayList;
import java.util.List;

import uk.ac.cam.cares.jps.data.mail.Mail;

public class MailboxAdapter extends RecyclerView.Adapter<MailboxAdapter.ViewHolder>{

    List<Mail> mails = new ArrayList<>();
    Context context;

    public void updateMails(List<Mail> mails) {
        this.mails = mails;
        notifyDataSetChanged();
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(parent.getContext())
                .inflate(R.layout.mail_row_item, parent, false);
        context = parent.getContext();

        return new ViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {
        Mail currentMail = mails.get(position);
        holder.time.setText(currentMail.getDate());
        holder.title.setText(currentMail.getTitle());
        holder.content.setText(currentMail.getContent());

        switch (currentMail.getType()) {
            case ASSET:
                holder.icon.setImageResource(R.drawable.outline_inventory_2_24);
                break;
            case MAINTENANCE:
                holder.icon.setImageResource(R.drawable.outline_handyman_24);
                break;
            default:
                break;
        }
    }

    @Override
    public int getItemCount() {
        return mails.size();
    }

    public static class ViewHolder extends RecyclerView.ViewHolder {

        ImageView icon;
        TextView title;
        TextView content;
        TextView time;

        public ViewHolder(@NonNull View itemView) {
            super(itemView);
            icon = itemView.findViewById(R.id.leading_icon);
            title = itemView.findViewById(R.id.title_tv);
            content = itemView.findViewById(R.id.content_tv);
            time = itemView.findViewById(R.id.time_tv);
        }
    }

}
