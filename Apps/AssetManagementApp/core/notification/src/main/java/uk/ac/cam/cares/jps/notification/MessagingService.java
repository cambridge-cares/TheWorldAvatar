package uk.ac.cam.cares.jps.notification;

import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.content.Context;
import android.media.RingtoneManager;
import android.net.Uri;
import android.os.Build;

import androidx.annotation.NonNull;
import androidx.core.app.NotificationCompat;

import com.google.firebase.messaging.FirebaseMessagingService;
import com.google.firebase.messaging.RemoteMessage;

import org.apache.log4j.Logger;

public class MessagingService extends FirebaseMessagingService {
    private static final Logger LOGGER = Logger.getLogger(MessagingService.class);

    @Override
    public void onNewToken(@NonNull String token) {
        super.onNewToken(token);
        LOGGER.info("Refreshed token: " + token);
        getSharedPreferences("firebase", MODE_PRIVATE).edit().putString("firebaseId", token).apply();
    }

    @Override
    public void onMessageReceived(@NonNull RemoteMessage message) {
        super.onMessageReceived(message);
        LOGGER.info("From: " + message.getFrom());

        if (message.getNotification() != null) {
            LOGGER.info("Message ID: " + message.getNotification().getBody());
            if (message.getNotification().getBody() != null) {
                sendNotification(message.getNotification().getBody(), message.getNotification().getTitle());
            }
        }

    }

    private void sendNotification(String notificationBody, String notificationTitle) {
        // todo: get mail content from mail repository in the intent activity with the mail id if user clicked on the notification
        // todo: should direct the user to mail box fragment
//        Intent intent = new Intent(this, MainActivity.class);
//        intent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
//        PendingIntent pendingIntent = PendingIntent.getActivity(this, 0 /* Request code */, intent,
//                PendingIntent.FLAG_IMMUTABLE);

        String channelId = getString(R.string.default_notification_channel_id);
        Uri defaultSoundUri = RingtoneManager.getDefaultUri(RingtoneManager.TYPE_NOTIFICATION);
        NotificationCompat.Builder notificationBuilder =
                new NotificationCompat.Builder(this, channelId)
                        .setSmallIcon(uk.ac.cam.cares.jps.ui.R.mipmap.ic_launcher)
                        .setContentTitle(notificationTitle)
                        .setContentText(notificationBody)
                        .setAutoCancel(true)
                        .setSound(defaultSoundUri);
//                        .setContentIntent(pendingIntent);

        NotificationManager notificationManager =
                (NotificationManager) getSystemService(Context.NOTIFICATION_SERVICE);

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            NotificationChannel channel = new NotificationChannel(channelId,
                    getString(R.string.default_notification_channel_name),
                    NotificationManager.IMPORTANCE_HIGH);
            notificationManager.createNotificationChannel(channel);
        }

        notificationManager.notify(0 /* ID of notification */, notificationBuilder.build());
    }
}
