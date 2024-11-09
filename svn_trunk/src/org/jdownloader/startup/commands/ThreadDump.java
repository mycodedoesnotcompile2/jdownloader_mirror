package org.jdownloader.startup.commands;

import org.appwork.utils.Application;
import org.appwork.utils.formatter.SizeFormatter;

public class ThreadDump extends AbstractStartupCommand {
    public ThreadDump() {
        super("threaddump");
    }

    @Override
    public void run(String command, String... parameters) {
        try {
            final java.lang.management.MemoryUsage memory = java.lang.management.ManagementFactory.getMemoryMXBean().getHeapMemoryUsage();
            if (memory != null) {
                final StringBuilder sb = new StringBuilder();
                sb.append("HeapUsed:" + SizeFormatter.formatBytes(memory.getUsed()));
                sb.append("|HeapComitted:" + SizeFormatter.formatBytes(memory.getCommitted()));
                sb.append("|HeapMax:" + SizeFormatter.formatBytes(memory.getMax()));
                sb.append("\r\n");
                logger.severe(sb.toString());
            }
        } catch (final Throwable e) {
            logger.log(e);
        }
        logger.severe(Application.getThreadDump());
        logger.flush();
    }

    @Override
    public String getDescription() {
        return "prints thread dump";
    }
}
