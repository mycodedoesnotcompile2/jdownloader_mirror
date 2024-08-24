/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2015, AppWork GmbH <e-mail@appwork.org>
 *         Schwabacher Straße 117
 *         90763 Fürth
 *         Germany
 * === Preamble ===
 *     This license establishes the terms under which the [The Product] Source Code & Binary files may be used, copied, modified, distributed, and/or redistributed.
 *     The intent is that the AppWork GmbH is able to provide  their utilities library for free to non-commercial projects whereas commercial usage is only permitted after obtaining a commercial license.
 *     These terms apply to all files that have the [The Product] License header (IN the file), a <filename>.license or <filename>.info (like mylib.jar.info) file that contains a reference to this license.
 *
 * === 3rd Party Licences ===
 *     Some parts of the [The Product] use or reference 3rd party libraries and classes. These parts may have different licensing conditions. Please check the *.license and *.info files of included libraries
 *     to ensure that they are compatible to your use-case. Further more, some *.java have their own license. In this case, they have their license terms in the java file header.
 *
 * === Definition: Commercial Usage ===
 *     If anybody or any organization is generating income (directly or indirectly) by using [The Product] or if there's any commercial interest or aspect in what you are doing, we consider this as a commercial usage.
 *     If your use-case is neither strictly private nor strictly educational, it is commercial. If you are unsure whether your use-case is commercial or not, consider it as commercial or contact as.
 * === Dual Licensing ===
 * === Commercial Usage ===
 *     If you want to use [The Product] in a commercial way (see definition above), you have to obtain a paid license from AppWork GmbH.
 *     Contact AppWork for further details: e-mail@appwork.org
 * === Non-Commercial Usage ===
 *     If there is no commercial usage (see definition above), you may use [The Product] under the terms of the
 *     "GNU Affero General Public License" (http://www.gnu.org/licenses/agpl-3.0.en.html).
 *
 *     If the AGPL does not fit your needs, please contact us. We'll find a solution.
 * ====================================================================================================================================================
 * ==================================================================================================================================================== */
package org.appwork.utils.singleapp.test;

import java.io.IOException;
import java.net.InetAddress;
import java.net.Socket;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import org.appwork.exceptions.WTFException;
import org.appwork.loggingv3.LogV3;
import org.appwork.testframework.AWTest;
import org.appwork.utils.Application;
import org.appwork.utils.Exceptions;
import org.appwork.utils.Time;
import org.appwork.utils.UniqueAlltimeID;
import org.appwork.utils.net.httpconnection.HTTPConnectionUtils;
import org.appwork.utils.net.httpconnection.HTTPConnectionUtils.IPVERSION;
import org.appwork.utils.singleapp.AnotherInstanceRunningException;
import org.appwork.utils.singleapp.ExceptionInRunningInstance;
import org.appwork.utils.singleapp.FailedToSendResponseException;
import org.appwork.utils.singleapp.IncommingMessageListener;
import org.appwork.utils.singleapp.Response;
import org.appwork.utils.singleapp.ResponseListener;
import org.appwork.utils.singleapp.ResponseSender;
import org.appwork.utils.singleapp.SingleAppInstance;
import org.appwork.utils.singleapp.SingleAppInstance.ErrorReadingResponseException;
import org.appwork.utils.singleapp.SingleAppInstance.InvalidResponseID;
import org.appwork.utils.singleapp.UncheckableInstanceException;

/**
 * @author daniel
 * @date May 29, 2019
 *
 */
public class TestSingleInstance extends AWTest {
    /**
     *
     */
    private static final String  EXCPTION_DUE_TO_CONNECTION_CLOSE                 = "EXCPTION_DUE_TO_CONNECTION_CLOSE";
    /**
     *
     */
    private static final String  EXCEPTION_MISSING_DONE_RESPONSE_SERVER_SHUT_DOWN = "Exception:Missing DONE Response. Server shut down?";
    /**
     *
     */
    public static final String   EXCEPTION_IN_RUNNING_INSTANCE                    = "exception in running instance";
    /**
     *
     */
    private static final String  FAILED_SO_SEND_RESPONSE                          = "FAILED_SO_SEND_RESPONSE";
    /**
     *
     */
    private static final int     EXTRA_SHORT_READTIMEOUT                          = 2000;
    /**
     *
     */
    private static final int     SECONDS                                          = 15;
    protected volatile Throwable exception;

    public static void main(String[] args) throws Exception {
        run();
    }

    /*
     * (non-Javadoc)
     *
     * @see org.appwork.testframework.TestInterface#runTest()
     */
    @Override
    public void runTest() throws Exception {
        Application.setApplication(".test");
        try {
            LOCALHOST = HTTPConnectionUtils.getLoopback(IPVERSION.IPV4_IPV6)[0];
            // LOCALHOST = InetAddress.getAllByName("192.168.2.123")[0];
            for (int i = 0; i < 1; i++) {
                System.out.println("Test:" + i);
                testInvalidClientID();
                testByeBye();
                testExceptionOnRunningInstanceSide();
                testCallbackInterruptedServer();
                testCallbackInterruptedClient();
                testCallback();
                testWithoutResponse();
                testWithResponse();
            }
        } catch (Exception e) {
            throw e;
        } catch (Throwable e) {
            throw new WTFException(e);
        }
    }

    private IncommingMessageListener mayNotReceiveMessages = new IncommingMessageListener() {
                                                               @Override
                                                               public void onIncommingMessage(ResponseSender callback, String[] message) {
                                                                   exception = new Exception("this should not happen");
                                                               }
                                                           };
    private ResponseListener         mayNotGetResponses    = new ResponseListener() {
                                                               @Override
                                                               public void onReceivedResponse(Response r) {
                                                                   exception = new Exception("Received Response - this should not happen");
                                                               }

                                                               @Override
                                                               public void onConnected(String[] message) {
                                                               }
                                                           };

    public void testExceptionOnRunningInstanceSide() throws Throwable {
        if (exception != null) {
            throw new Exception(exception);
        } else {
            exception = null;
        }
        final ConcurrentHashMap<String, Boolean> triggerToClear = new ConcurrentHashMap<String, Boolean>();
        triggerToClear.put(EXCEPTION_IN_RUNNING_INSTANCE, Boolean.TRUE);
        IncommingMessageListener listener = new IncommingMessageListener() {
            @Override
            public void onIncommingMessage(ResponseSender sender, String[] message) {
                throw new WTFException("Ups");
            }
        };
        final ExtSingleAppInstance server = new ExtSingleAppInstance("test", listener);
        try {
            server.setForwardMessageDirectIfNoOtherInstanceIsFound(false);
            server.start(mayNotGetResponses, "");
            try {
                final SingleAppInstance client = new ExtSingleAppInstance("test", mayNotReceiveMessages);
                try {
                    client.start(null, "");
                    throw new Exception("Should not reach this part");
                } catch (ExceptionInRunningInstance e) {
                    // good
                    triggerToClear.remove(EXCEPTION_IN_RUNNING_INSTANCE);
                } catch (InterruptedException e) {
                    throw new Exception("Should not reach this part", e);
                } catch (AnotherInstanceRunningException e) {
                    throw new Exception("Should not reach this part", e);
                }
                if (exception != null) {
                    throw new Exception(exception);
                }
            } catch (Exception e) {
                exception = e;
            }
            while (clientThreads.size() > 0) {
                final Thread clientThread = clientThreads.remove(0);
                clientThread.join();
            }
            if (exception != null) {
                throw new Exception(exception);
            } else if (triggerToClear.size() > 0) {
                throw new Exception("Triggers left: Not all code points reached: " + triggerToClear);
            }
        } finally {
            final Thread thread = server.exit(true);
            if (thread != null) {
                thread.join();
            }
        }
    }

    private static InetAddress                 LOCALHOST     = null;
    private final CopyOnWriteArrayList<Thread> clientThreads = new CopyOnWriteArrayList<Thread>();

    public class ExtSingleAppInstance extends SingleAppInstance {
        /**
         * @param appID
         * @param listenr
         */
        public ExtSingleAppInstance(String appID, IncommingMessageListener listenr) {
            super(appID, listenr);
            // TODO Auto-generated constructor stub
        }

        /*
         * (non-Javadoc)
         *
         * @see org.appwork.utils.singleapp.SingleAppInstance#getLocalHost()
         */
        @Override
        protected InetAddress getLocalHost() throws UnknownHostException {
            return LOCALHOST;
        }

        /*
         * (non-Javadoc)
         *
         * @see org.appwork.utils.singleapp.SingleAppInstance#onNewIncommingConnection(java.net.Socket, java.lang.Thread)
         */
        @Override
        protected void onNewIncommingConnection(Socket client, Thread thread) {
            clientThreads.add(thread);
        }

        /*
         * (non-Javadoc)
         *
         * @see org.appwork.utils.singleapp.SingleAppInstance#onUncaughtExceptionDuringHandlingIncommingConnections(java.lang.Throwable)
         */
        @Override
        protected void onUncaughtExceptionDuringHandlingIncommingConnections(Throwable e) {
            exception = e;
        }
    }

    public void testCallbackInterruptedServer() throws AnotherInstanceRunningException, UncheckableInstanceException, Throwable, IOException {
        if (exception != null) {
            throw new Exception(exception);
        } else {
            exception = null;
        }
        final ConcurrentHashMap<String, Boolean> triggerToClear = new ConcurrentHashMap<String, Boolean>();
        triggerToClear.put(EXCEPTION_MISSING_DONE_RESPONSE_SERVER_SHUT_DOWN, Boolean.TRUE);
        final AtomicReference<ExtSingleAppInstance> server = new AtomicReference<TestSingleInstance.ExtSingleAppInstance>();
        IncommingMessageListener listener = new IncommingMessageListener() {
            @Override
            public void onIncommingMessage(ResponseSender sender, String[] message) {
                // try {
                // server.get().exit(true).join();
                // } catch (InterruptedException ignore) {
                // }
            }
        };
        final ExtSingleAppInstance serverI = new ExtSingleAppInstance("test", listener) {
            // @Override
            // protected boolean closeConnection(ClientConnection client) throws IOException {
            // return true;
            // }
            /**
             * @see org.appwork.utils.singleapp.SingleAppInstance#sendDone(org.appwork.utils.singleapp.SingleAppInstance.ClientConnection)
             */
            @Override
            protected void sendDone(ClientConnection client) throws IOException {
                // do not send done.this should trigger an error
            }
            // /*
            // * (non-Javadoc)
            // *
            // * @see
            // org.appwork.utils.singleapp.SingleAppInstance#onUncaughtExceptionDuringHandlingIncommingConnections(java.lang.Throwable)
            // */
            // @Override
            // protected void onUncaughtExceptionDuringHandlingIncommingConnections(Throwable e) {
            // // should not happen because closeAllConnections will close socket but after that IOExceptions no longer should land here
            // triggerToClear.put(EXCPTION_DUE_TO_CONNECTION_CLOSE, Boolean.TRUE);
            // }
        };
        try {
            server.set(serverI);
            serverI.setForwardMessageDirectIfNoOtherInstanceIsFound(false);
            serverI.start(mayNotGetResponses, "ThisIsMyMessage");
            try {
                final SingleAppInstance client = new ExtSingleAppInstance("test", mayNotReceiveMessages);
                try {
                    client.start(null, "ThisIsMyMessage2");
                    throw new Exception("Should not reach this part");
                } catch (ErrorReadingResponseException e) {
                    triggerToClear.remove(EXCEPTION_MISSING_DONE_RESPONSE_SERVER_SHUT_DOWN);
                } catch (InterruptedException e) {
                    throw new Exception("Should not reach this part");
                } catch (AnotherInstanceRunningException e) {
                    throw new Exception("Should not reach this part");
                }
                if (exception != null) {
                    throw new Exception(exception);
                }
            } catch (Exception e) {
                exception = e;
            }
            while (clientThreads.size() > 0) {
                final Thread clientThread = clientThreads.remove(0);
                clientThread.join();
            }
            if (exception != null) {
                throw new Exception(exception);
            } else if (triggerToClear.size() > 0) {
                throw new Exception("Triggers left: Not all code points reached: " + triggerToClear);
            }
        } finally {
            final Thread thread = serverI.exit(true);
            if (thread != null) {
                thread.join();
            }
        }
    }

    public void testCallbackInterruptedClient() throws AnotherInstanceRunningException, UncheckableInstanceException, Throwable, IOException {
        if (exception != null) {
            throw new Exception(exception);
        } else {
            exception = null;
        }
        final HashSet<String> received = new HashSet<String>();
        final HashSet<String> sent = new HashSet<String>();
        final Object LOCK = new Object();
        final ConcurrentHashMap<String, Boolean> triggerToClear = new ConcurrentHashMap<String, Boolean>();
        triggerToClear.put(FAILED_SO_SEND_RESPONSE, Boolean.TRUE);
        final SingleAppInstance server = new ExtSingleAppInstance("test", new IncommingMessageListener() {
            @Override
            public void onIncommingMessage(ResponseSender sender, String[] message) {
                for (String s : message) {
                    synchronized (LOCK) {
                        received.add(s);
                    }
                }
                for (int i = 0; i < SECONDS; i++) {
                    try {
                        sender.sendResponse(new Response("TestResponse", "Received " + i + ":" + Arrays.asList(message).toString()));
                    } catch (FailedToSendResponseException e1) {
                        triggerToClear.remove(FAILED_SO_SEND_RESPONSE);
                        return;
                    }
                    try {
                        Thread.sleep(1000);
                    } catch (InterruptedException e) {
                        // TODO Auto-generated catch block
                        e.printStackTrace();
                    }
                }
                exception = new Exception("We should not reach this codepoint");
            }
        }) {
            /*
             * (non-Javadoc)
             *
             * @see org.appwork.utils.singleapp.test.TestSingleInstance.ExtSingleAppInstance#
             * onUncaughtExceptionDuringHandlingIncommingConnections(java.lang.Throwable)
             */
            @Override
            protected void onUncaughtExceptionDuringHandlingIncommingConnections(Throwable e) {
            }

            protected int getReadtimeoutForReadingResponses() {
                return EXTRA_SHORT_READTIMEOUT;
            };
        };
        try {
            server.setForwardMessageDirectIfNoOtherInstanceIsFound(false);
            server.start(mayNotGetResponses, "");
            long started = Time.systemIndependentCurrentJVMTimeMillis();
            try {
                final SingleAppInstance client = new ExtSingleAppInstance("test", mayNotReceiveMessages);
                final ArrayList<Response> responsesViaCallback = new ArrayList<Response>();
                final String withStart = "ViaStart" + UniqueAlltimeID.next();
                try {
                    synchronized (LOCK) {
                        sent.add(withStart);
                        LogV3.info("Sent: " + sent + " (" + withStart + ")");
                    }
                    new Thread() {
                        private Thread mainThread;
                        {
                            mainThread = Thread.currentThread();
                        }

                        @Override
                        public void run() {
                            try {
                                Thread.sleep(4000);
                            } catch (InterruptedException e) {
                                // TODO Auto-generated catch block
                                e.printStackTrace();
                            }
                            System.out.println("Interrupt Client");
                            mainThread.interrupt();
                        }
                    }.start();
                    client.start(new ResponseListener() {
                        @Override
                        public void onConnected(String[] message) {
                        }

                        @Override
                        public void onReceivedResponse(Response r) {
                            LogV3.info("Client " + Thread.currentThread().getName() + ":" + r);
                            responsesViaCallback.add(r);
                        }
                    }, withStart);
                    throw new Exception("Should not reach this part");
                } catch (InterruptedException e) {
                    // ok
                    client.exit();
                    System.out.println("Caught Interrupt.Good!");
                } catch (AnotherInstanceRunningException e) {
                    throw new Exception("Should not reach this part");
                }
                if (exception != null) {
                    throw new Exception(exception);
                }
            } catch (Exception e) {
                exception = e;
            }
            if (exception != null) {
                throw new Exception(exception);
            }
            long dura = Time.systemIndependentCurrentJVMTimeMillis() - started;
            if (dura < 4000 || dura > 4500) {
                throw new Exception("unexpected run duration");
            }
            Thread.sleep(2000);
            if (triggerToClear.size() > 0) {
                throw new Exception("Triggers left: Not all code points reached: " + triggerToClear);
            }
            synchronized (LOCK) {
                assertThat(received).equalsDeep(sent);
                assertTrue(received.size() > 0);
            }
            if (exception != null) {
                throw new Exception(exception);
            }
        } finally {
            final Thread thread = server.exit(true);
            if (thread != null) {
                thread.join();
            }
        }
    }

    public void testCallback() throws AnotherInstanceRunningException, UncheckableInstanceException, Throwable, IOException {
        if (exception != null) {
            throw new Exception(exception);
        } else {
            exception = null;
        }
        final HashSet<String> received = new HashSet<String>();
        final HashSet<String> sent = new HashSet<String>();
        final HashSet<String> responses = new HashSet<String>();
        final Object LOCK = new Object();
        final SingleAppInstance server = new ExtSingleAppInstance("test", new IncommingMessageListener() {
            @Override
            public void onIncommingMessage(ResponseSender sender, String[] message) {
                for (String s : message) {
                    System.out.println("Received " + s);
                    synchronized (LOCK) {
                        received.add(s);
                    }
                }
                try {
                    Thread.sleep(EXTRA_SHORT_READTIMEOUT * 2);
                } catch (InterruptedException e1) {
                    // TODO Auto-generated catch block
                    e1.printStackTrace();
                }
                for (int i = 0; i < SECONDS; i++) {
                    try {
                        sender.sendResponse(new Response("TestResponse", "Received " + i + ":" + Arrays.asList(message).toString()));
                    } catch (FailedToSendResponseException e1) {
                        exception = e1;
                        throw new WTFException(e1);
                    }
                    try {
                        Thread.sleep(1000);
                    } catch (InterruptedException e) {
                        // TODO Auto-generated catch block
                        e.printStackTrace();
                    }
                }
            }
        }) {
            protected int getReadtimeoutForReadingResponses() {
                return EXTRA_SHORT_READTIMEOUT;
            };
        };
        try {
            server.setForwardMessageDirectIfNoOtherInstanceIsFound(false);
            server.start(new ResponseListener() {
                @Override
                public void onReceivedResponse(Response r) {
                    exception = new Exception("Received Response 1 - this should not happen");
                }

                @Override
                public void onConnected(String[] message) {
                    exception = new Exception("Received Response 1 - this should not happen");
                }
            }, "");
            ArrayList<Thread> threads = new ArrayList<Thread>();
            long started = Time.systemIndependentCurrentJVMTimeMillis();
            for (int t = 0; t < 2; t++) {
                Thread th = new Thread("Client " + t) {
                    /*
                     * (non-Javadoc)
                     *
                     * @see java.lang.Thread#run()
                     */
                    @Override
                    public void run() {
                        try {
                            final SingleAppInstance client = new ExtSingleAppInstance("test", mayNotReceiveMessages) {
                                protected int getReadtimeoutForReadingResponses() {
                                    return 2000;
                                };
                            };
                            long start = Time.systemIndependentCurrentJVMTimeMillis();
                            final ArrayList<Response> responsesViaCallback = new ArrayList<Response>();
                            final String withStart = "ViaStart" + UniqueAlltimeID.next();
                            try {
                                synchronized (LOCK) {
                                    sent.add(withStart);
                                    LogV3.info("Sent: " + sent + " (" + withStart + ")");
                                }
                                client.start(new ResponseListener() {
                                    @Override
                                    public void onConnected(String[] message) {
                                    }

                                    @Override
                                    public void onReceivedResponse(Response r) {
                                        LogV3.info("Client " + Thread.currentThread().getName() + ":" + r);
                                        responsesViaCallback.add(r);
                                    }
                                }, withStart);
                                throw new Exception("Should not reach this part");
                            } catch (AnotherInstanceRunningException e) {
                                long dura = Time.systemIndependentCurrentJVMTimeMillis() - start;
                                if (dura < (SECONDS) * 1000 + 2 * EXTRA_SHORT_READTIMEOUT || dura > (SECONDS + 1) * 1000 + 2 * EXTRA_SHORT_READTIMEOUT) {
                                    throw new Exception("Expected to take around " + (SECONDS + 2 * EXTRA_SHORT_READTIMEOUT / 1000) + "s. instead: " + dura);
                                }
                                assertTrue(responsesViaCallback.size() == SECONDS);
                            }
                            if (exception != null) {
                                throw new Exception(exception);
                            }
                        } catch (Throwable e) {
                            exception = e;
                        }
                    }
                };
                th.start();
                threads.add(th);
            }
            for (Thread th : threads) {
                th.join();
            }
            if (exception != null) {
                throw new Exception(exception);
            }
            synchronized (LOCK) {
                assertThat(received).equalsDeep(sent);
                assertTrue(received.size() > 0);
            }
            long dura = Time.systemIndependentCurrentJVMTimeMillis() - started;
            if (dura < (SECONDS) * 1000 + 2 * EXTRA_SHORT_READTIMEOUT || dura > (SECONDS + 1) * 1000 + 2 * EXTRA_SHORT_READTIMEOUT) {
                throw new Exception("Expected to take around " + (SECONDS + 2 * EXTRA_SHORT_READTIMEOUT / 1000) + "s. instead: " + dura);
            }
            if (exception != null) {
                throw new Exception(exception);
            }
        } finally {
            final Thread thread = server.exit(true);
            if (thread != null) {
                thread.join();
            }
        }
    }

    public void testWithResponse() throws AnotherInstanceRunningException, UncheckableInstanceException, Throwable, IOException {
        if (exception != null) {
            throw new Exception(exception);
        } else {
            exception = null;
        }
        final HashSet<String> received = new HashSet<String>();
        final HashSet<String> sent = new HashSet<String>();
        final HashSet<String> responses = new HashSet<String>();
        final Object LOCK = new Object();
        final SingleAppInstance server = new ExtSingleAppInstance("test", new IncommingMessageListener() {
            @Override
            public void onIncommingMessage(ResponseSender callback, String[] message) {
                for (String s : message) {
                    System.out.println("Received " + s);
                    synchronized (LOCK) {
                        received.add(s);
                    }
                }
                try {
                    callback.sendResponse(new Response("TestResponse", "Received " + Arrays.asList(message).toString()));
                } catch (FailedToSendResponseException e1) {
                    exception = e1;
                    throw new WTFException(e1);
                }
            }
        });
        try {
            server.setForwardMessageDirectIfNoOtherInstanceIsFound(false);
            server.start(mayNotGetResponses, "");
            for (int i = 0; i < 5; i++) {
                final SingleAppInstance client = new ExtSingleAppInstance("test", mayNotReceiveMessages);
                String toSend = "test" + System.currentTimeMillis();
                ResponseListener callback = new ResponseListener() {
                    @Override
                    public void onConnected(String[] message) {
                    }

                    @Override
                    public void onReceivedResponse(Response r) {
                        System.out.println("Received Response " + r);
                        synchronized (LOCK) {
                            responses.add(r.toString());
                        }
                    }
                };
                long startedToConnect = Time.systemIndependentCurrentJVMTimeMillis();
                final String withStart = "ViaStart" + System.currentTimeMillis();
                try {
                    synchronized (LOCK) {
                        sent.add(withStart);
                    }
                    client.start(callback, withStart);
                    throw new Exception("Should not reach this part");
                } catch (AnotherInstanceRunningException e) {
                    long duration = Time.systemIndependentCurrentJVMTimeMillis() - startedToConnect;
                    System.out.println("Duration to connect: " + duration);
                    assertThat(duration).isLowerThan(300);
                    long startedToSend = Time.systemIndependentCurrentJVMTimeMillis();
                    synchronized (LOCK) {
                        sent.add(toSend);
                    }
                    client.sendToRunningInstance(callback, toSend);
                    duration = Time.systemIndependentCurrentJVMTimeMillis() - startedToSend;
                    System.out.println("Duration to send: " + duration);
                    assertThat(duration).isLowerThan(300);
                }
                if (exception != null) {
                    throw new Exception(exception);
                }
            }
            synchronized (LOCK) {
                assertThat(received).equalsDeep(sent);
                assertTrue(received.size() > 0);
            }
            if (exception != null) {
                throw new Exception(exception);
            }
        } finally {
            final Thread thread = server.exit(true);
            if (thread != null) {
                thread.join();
            }
        }
    }

    public void testByeBye() throws AnotherInstanceRunningException, UncheckableInstanceException, Throwable, IOException {
        if (exception != null) {
            throw new Exception(exception);
        } else {
            exception = null;
        }
        final AtomicInteger count = new AtomicInteger();
        final SingleAppInstance server = new ExtSingleAppInstance("test", null);
        server.setListener(new IncommingMessageListener() {
            @Override
            public void onIncommingMessage(ResponseSender callback, String[] message) {
                count.incrementAndGet();
                for (String s : message) {
                    System.out.println("Received " + s);
                }
                while (server.isRunning()) {
                    try {
                        Thread.sleep(1000);
                    } catch (InterruptedException ignore) {
                    }
                }
            }
        });
        try {
            server.setForwardMessageDirectIfNoOtherInstanceIsFound(false);
            server.start(mayNotGetResponses, "");
            final List<Thread> clientThreads = new ArrayList<Thread>();
            for (int i = 0; i < 5; i++) {
                final Thread clientThread = new Thread() {
                    /*
                     * (non-Javadoc)
                     *
                     * @see java.lang.Thread#run()
                     */
                    @Override
                    public void run() {
                        final SingleAppInstance client = new ExtSingleAppInstance("test", mayNotReceiveMessages);
                        try {
                            try {
                                final String withStart = "ViaStart" + System.currentTimeMillis();
                                client.start(mayNotGetResponses, withStart);
                                throw new Exception("Should not reach this part");
                            } catch (AnotherInstanceRunningException ignore) {
                            }
                        } catch (Throwable e) {
                            exception = e;
                        }
                    }
                };
                clientThreads.add(clientThread);
                clientThread.start();
            }
            while (count.get() != clientThreads.size()) {
                Thread.sleep(100);
            }
            // lclose all withStart
            server.exit(true).join();
            for (Thread thread : clientThreads) {
                thread.join();
            }
            if (exception != null) {
                throw new Exception(exception);
            }
        } finally {
            final Thread thread = server.exit(true);
            if (thread != null) {
                thread.join();
            }
        }
    }

    public void testInvalidClientID() throws AnotherInstanceRunningException, UncheckableInstanceException, Throwable, IOException {
        if (exception != null) {
            throw new Exception(exception);
        } else {
            exception = null;
        }
        final HashSet<String> received = new HashSet<String>();
        final Object LOCK = new Object();
        final SingleAppInstance server = new ExtSingleAppInstance("test", new IncommingMessageListener() {
            @Override
            public void onIncommingMessage(ResponseSender callback, String[] message) {
                for (String s : message) {
                    System.out.println("Received " + s);
                    synchronized (LOCK) {
                        received.add(s);
                    }
                }
            }
        });
        try {
            server.setForwardMessageDirectIfNoOtherInstanceIsFound(false);
            server.start(mayNotGetResponses, "");
            for (int i = 0; i < 5; i++) {
                final SingleAppInstance client = new ExtSingleAppInstance("test", mayNotReceiveMessages) {
                    @Override
                    public String getClientID() {
                        return super.getClientID() + "Nope";
                    }
                };
                final String toSend = "test" + System.currentTimeMillis();
                try {
                    final String withStart = "ViaStart" + System.currentTimeMillis();
                    client.start(mayNotGetResponses, withStart);
                    throw new Exception("Should not reach this part");
                } catch (AnotherInstanceRunningException e) {
                    assertTrue(Exceptions.containsInstanceOf(e, InvalidResponseID.class));
                    InvalidResponseID invalidResponseIDException = null;
                    try {
                        client.sendToRunningInstance(mayNotGetResponses, toSend);
                    } catch (InvalidResponseID e2) {
                        invalidResponseIDException = e2;
                    }
                    assertTrue(invalidResponseIDException != null);
                }
            }
            synchronized (LOCK) {
                assertTrue(received.size() == 0);
            }
            if (exception != null) {
                throw new Exception(exception);
            }
        } finally {
            final Thread thread = server.exit(true);
            if (thread != null) {
                thread.join();
            }
        }
    }

    public void testWithoutResponse() throws AnotherInstanceRunningException, UncheckableInstanceException, Throwable, IOException {
        if (exception != null) {
            throw new Exception(exception);
        } else {
            exception = null;
        }
        final HashSet<String> received = new HashSet<String>();
        final HashSet<String> sent = new HashSet<String>();
        final Object LOCK = new Object();
        final SingleAppInstance server = new ExtSingleAppInstance("test", new IncommingMessageListener() {
            @Override
            public void onIncommingMessage(ResponseSender callback, String[] message) {
                for (String s : message) {
                    System.out.println("Received " + s);
                    synchronized (LOCK) {
                        received.add(s);
                    }
                }
            }
        });
        try {
            server.setForwardMessageDirectIfNoOtherInstanceIsFound(false);
            server.start(mayNotGetResponses, "");
            for (int i = 0; i < 5; i++) {
                final SingleAppInstance client = new ExtSingleAppInstance("test", mayNotReceiveMessages);
                String toSend = "test" + System.currentTimeMillis();
                long startedToConnect = Time.systemIndependentCurrentJVMTimeMillis();
                try {
                    String withStart = "ViaStart" + System.currentTimeMillis();
                    synchronized (LOCK) {
                        sent.add(withStart);
                    }
                    client.start(mayNotGetResponses, withStart);
                    throw new Exception("Should not reach this part");
                } catch (AnotherInstanceRunningException e) {
                    long duration = Time.systemIndependentCurrentJVMTimeMillis() - startedToConnect;
                    System.out.println("Duration to connect: " + duration);
                    assertThat(duration).isLowerThan(300);
                    long startedToSend = Time.systemIndependentCurrentJVMTimeMillis();
                    synchronized (LOCK) {
                        sent.add(toSend);
                    }
                    client.sendToRunningInstance(mayNotGetResponses, toSend);
                    duration = Time.systemIndependentCurrentJVMTimeMillis() - startedToSend;
                    System.out.println("Duration to send: " + duration);
                    assertThat(duration).isLowerThan(300);
                }
                if (exception != null) {
                    throw new Exception(exception);
                }
            }
            synchronized (LOCK) {
                assertThat(received).equalsDeep(sent);
                assertTrue(received.size() > 0);
            }
            if (exception != null) {
                throw new Exception(exception);
            }
        } finally {
            final Thread thread = server.exit(true);
            if (thread != null) {
                thread.join();
            }
        }
    }
}
