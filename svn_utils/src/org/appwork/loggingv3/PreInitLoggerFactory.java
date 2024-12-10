package org.appwork.loggingv3;

import java.net.URL;
import java.util.ArrayList;

import org.appwork.loggingv3.simple.Formatter;
import org.appwork.loggingv3.simple.LogRecord2;
import org.appwork.loggingv3.simple.SimpleLoggerFactory;
import org.appwork.loggingv3.simple.SinkProvider;
import org.appwork.loggingv3.simple.sink.Sink;

public class PreInitLoggerFactory extends SimpleLoggerFactory {
    private final ArrayList<LogRecord2> cached;
    private volatile SinkProvider       follower;

    @Override
    public PreInitLoggerFactory initDefaults() {
        addSink(new Sink() {
            private final boolean ide;
            {
                ide = isIDE();
            }

            @Override
            public void publish(final LogRecord2 record) {
                final SinkProvider fol = follower;
                if (fol != null) {
                    fol.publish(record);
                    return;
                }
                synchronized (cached) {
                    cached.add(record);
                }
                if (ide) {
                    System.out.println("PreInitIDEOut>" + record.timestamp + " - " + record.message);
                }
            }

            @Override
            public Sink setFormatter(Formatter formatter) {
                return null;
            }
        });
        return this;
    }

    /**
     * @return
     */
    private boolean isIDE() {
        ClassLoader cl = PreInitLoggerFactory.class.getClassLoader();
        if (cl == null) {
            // no custom classloader so far -> in Jar
            return false;
        }
        URL url = cl.getResource(PreInitLoggerFactory.class.getName().replace(".", "/") + ".class");
        if (url != null && url.toExternalForm().startsWith("file:/")) {
            return true;
        } else {
            return false;
        }
    }

    public PreInitLoggerFactory() {
        cached = new ArrayList<LogRecord2>();
    }

    /**
     * @see org.appwork.loggingv3.simple.SimpleLoggerFactory#setSuccessor(org.appwork.loggingv3.LogV3Factory)
     */
    @Override
    public void setSuccessor(LogV3Factory newFactory) {
        super.setSuccessor(newFactory);
        if (newFactory instanceof SinkProvider) {
            this.follower = (SinkProvider) newFactory;
            // forward cached entries
            synchronized (cached) {
                for (final LogRecord2 c : cached) {
                    follower.publish(c);
                }
            }
        }
    }
}
