package uk.ac.cam.cares.jps.data;

import android.os.Handler;

import com.android.volley.Response;

import java.util.ArrayList;
import java.util.List;
import org.apache.log4j.Logger;

import javax.inject.Inject;

import uk.ac.cam.cares.jps.network.mail.MailNetworkSource;

public class MailRepository {

    private static Logger LOGGER = Logger.getLogger(MailRepository.class);
    MailNetworkSource networkSource;

    @Inject
    public MailRepository(MailNetworkSource networkSource) {
        this.networkSource = networkSource;
    }

    public void getMailList(Response.Listener<List<Mail>> onSuccess, Response.ErrorListener onError) {
        Handler handler = new Handler();
        handler.postDelayed((Runnable) () -> {
            LOGGER.info("creating test mail data");
            List<Mail> testMail = new ArrayList<>();
            testMail.add(new Mail("Test: Maintenance Update", "This is a test maintenance update message", "4 Aug", false, Mail.MailType.MAINTENANCE));
            testMail.add(new Mail("Test: Asset Update", "This is a test asset update message", "4 Aug", true, Mail.MailType.ASSET));
            testMail.add(new Mail("Test: Maintenance Update", "This is a test maintenance update message", "3 Aug", true, Mail.MailType.MAINTENANCE));
            onSuccess.onResponse(testMail);

            handler.removeCallbacksAndMessages(null);
        }, 2000);
    }
}
