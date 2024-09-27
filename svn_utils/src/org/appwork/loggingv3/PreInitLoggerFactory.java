package org.appwork.loggingv3;

import java.util.ArrayList;

import org.appwork.loggingv3.simple.LogRecord2;
import org.appwork.loggingv3.simple.SimpleLoggerFactory;
import org.appwork.loggingv3.simple.sink.AbstractSink;

public class PreInitLoggerFactory extends SimpleLoggerFactory {
    private final ArrayList<LogRecord2> cached;

    public ArrayList<LogRecord2> getCached() {
        synchronized (cached) {
            return new ArrayList<LogRecord2>(cached);
        }
    }

    @Override
    public PreInitLoggerFactory initDefaults() {
        addSink(new AbstractSink() {
            @Override
            public void publish(final LogRecord2 record) {
                synchronized (cached) {
                    cached.add(record);
                }
            }
        });
        return this;
    }

    public PreInitLoggerFactory() {
        cached = new ArrayList<LogRecord2>();
    }
}
