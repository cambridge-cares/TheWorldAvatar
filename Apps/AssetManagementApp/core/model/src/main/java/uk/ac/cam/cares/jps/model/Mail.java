package uk.ac.cam.cares.jps.model;

public class Mail {
    public enum MailType {
        MAINTENANCE,
        ASSET
    }

    String title;
    String content;
    String date;  //todo: need to check the format and any potential formatting
    boolean isRead;
    MailType type;

    public Mail(String title, String content, String date, boolean isRead, MailType type) {
        this.title = title;
        this.content = content;
        this.date = date;
        this.isRead = isRead;
        this.type = type;
    }

    public String getTitle() {
        return title;
    }

    public String getContent() {
        return content;
    }

    public String getDate() {
        return date;
    }

    public boolean isRead() {
        return isRead;
    }

    public MailType getType() {
        return type;
    }
}
