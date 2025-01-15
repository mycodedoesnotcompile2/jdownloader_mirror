package jd.controlling.packagecontroller;

public interface AbstractNodeNotifier {
    public static enum NOTIFY {
        STRUCTURE_CHANGE,
        PROPERTY_CHANGE
    }

    void nodeUpdated(AbstractNode source, NOTIFY notify, Object param);

    boolean hasNotificationListener();
}
