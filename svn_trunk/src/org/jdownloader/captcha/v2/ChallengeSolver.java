package org.jdownloader.captcha.v2;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.WeakHashMap;
import java.util.concurrent.LinkedBlockingDeque;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.regex.Pattern;

import jd.controlling.captcha.SkipException;

import org.appwork.exceptions.WTFException;
import org.appwork.utils.StringUtils;
import org.jdownloader.captcha.v2.solver.browser.AbstractBrowserChallenge;
import org.jdownloader.captcha.v2.solver.jac.SolverException;
import org.jdownloader.captcha.v2.solverjob.SolverJob;

public abstract class ChallengeSolver<T> {
    public static final ChallengeSolver EXTERN = new ChallengeSolver<Object>() {
        @Override
        public void solve(SolverJob<Object> solverJob) throws InterruptedException, SolverException, SkipException {
            throw new WTFException("Not Implemented");
        }
    };

    protected ChallengeSolver() {
    }

    public boolean setInvalid(AbstractResponse<?> response) {
        return false;
    }

    public boolean setUnused(AbstractResponse<?> response) {
        return false;
    }

    public boolean setValid(AbstractResponse<?> response) {
        return false;
    }

    protected ThreadPoolExecutor threadPool;
    private Class<T>             resultType;
    protected SolverService      service;

    /**
     *
     * @param i
     *            size of the threadpool. if i<=0 there will be no threadpool. each challenge will get a new thread in this case
     */
    @SuppressWarnings("unchecked")
    public ChallengeSolver(SolverService service, int i) {
        this.service = service;
        if (service == null) {
            this.service = (SolverService) this;
        }
        initThreadPool(i);
        Class<?> cls = this.getClass();
        while (true) {
            Type superClass = cls.getGenericSuperclass();
            if (superClass == null) {
                throw new IllegalArgumentException("Wrong Construct");
            }
            if (superClass instanceof Class) {
                cls = (Class<?>) superClass;
            } else if (superClass instanceof ParameterizedType) {
                resultType = (Class<T>) ((ParameterizedType) superClass).getActualTypeArguments()[0];
                break;
            } else {
                throw new IllegalArgumentException("Wrong Construct");
            }
        }
    }

    public ChallengeSolver(int i) {
        this(null, i);
    }

    protected final WeakHashMap<SolverJob<T>, JobRunnable<T>> map = new WeakHashMap<SolverJob<T>, JobRunnable<T>>();

    public SolverService getService() {
        return service;
    }

    /* If logins are required for a solver, return false if they are invalid (e.g. empty API key field or invalid API key format). */
    protected boolean validateLogins() {
        return true;
    }

    public boolean isEnabled() {
        return getService().getConfig().isEnabled();
    }

    public List<SolverJob<T>> listJobs() {
        synchronized (map) {
            return new ArrayList<SolverJob<T>>(map.keySet());
        }
    }

    public boolean hasJobs() {
        synchronized (map) {
            return map.size() > 0;
        }
    }

    public boolean isJobDone(SolverJob<?> job) {
        synchronized (map) {
            return !map.containsKey(job);
        }
    }

    public void enqueue(SolverJob<T> job) {
        final JobRunnable<T> jr = new JobRunnable<T>(this, job);
        synchronized (map) {
            map.put(job, jr);
            if (threadPool == null) {
                new Thread(jr, "ChallengeSolverThread").start();
            } else {
                threadPool.execute(jr);
            }
        }
    }

    protected static void checkInterruption() throws InterruptedException {
        if (Thread.interrupted()) {
            throw new InterruptedException();
        }
    }

    public void kill(SolverJob<T> job) {
        if (job == null) {
            return;
        }
        synchronized (map) {
            final JobRunnable<T> jr = map.remove(job);
            if (jr != null) {
                job.getLogger().info("Cancel " + jr);
                jr.cancel();
            } else {
                job.getLogger().info("Could not kill " + job + " in " + this);
            }
        }
    }

    public String toString() {
        return getClass().getSimpleName();
    }

    private void initThreadPool(int i) {
        if (i <= 0) {
            return;
        }
        threadPool = new ThreadPoolExecutor(i, i, 5000, TimeUnit.MILLISECONDS, new LinkedBlockingDeque<Runnable>(), new ThreadFactory() {
            public Thread newThread(final Runnable r) {
                return new Thread(r, "SolverThread:" + ChallengeSolver.this.toString());
            }
        }, new ThreadPoolExecutor.AbortPolicy());
        threadPool.allowCoreThreadTimeOut(true);
    }

    public abstract void solve(SolverJob<T> solverJob) throws InterruptedException, SolverException, SkipException;

    public Class<T> getResultType() {
        return resultType;
    }

    protected boolean isChallengeSupported(final Challenge<?> c) {
        if (c instanceof AbstractBrowserChallenge) {
            return false;
        } else {
            return true;
        }
    }

    public boolean canHandle(final Challenge<?> c) {
        if (!isChallengeSupported(c)) {
            return false;
        } else if (!getResultType().isAssignableFrom(c.getResultType())) {
            return false;
        } else if (!validateBlackWhite(c)) {
            return false;
        } else {
            return true;
        }
    }

    /**
     * returns true for whitelisted and false for blacklisted
     *
     * @param c
     * @return
     */
    public boolean validateBlackWhite(final Challenge<?> c) {
        if (!getService().getConfig().isBlackWhiteListingEnabled()) {
            /* Black/Whitelist disabled by user -> No need to check */
            return true;
        }
        final String host = c.getHost();
        Boolean result = null;
        final ArrayList<String> whitelist = getService().getConfig().getWhitelistEntries();
        if (whitelist != null && whitelist.size() > 0) {
            whitelist: for (final String s : whitelist) {
                try {
                    final Pattern pattern = Pattern.compile(s, Pattern.CASE_INSENSITIVE);
                    if (!StringUtils.equalsIgnoreCase(host, c.getTypeID())) {
                        if (pattern.matcher(host + "-" + c.getTypeID()).matches()) {
                            result = true;
                            break whitelist;
                        }
                        if (pattern.matcher(host).matches()) {
                            result = true;
                            break whitelist;
                        }
                    }
                    if (pattern.matcher(c.getTypeID()).matches()) {
                        result = true;
                        break whitelist;
                    }
                } catch (Throwable e) {
                    c.getPlugin().getLogger().log(e);
                }
            }
        }
        if (result == null) {
            final ArrayList<String> blacklist = getService().getConfig().getBlacklistEntries();
            if (blacklist != null && blacklist.size() > 0) {
                blacklist: for (final String s : blacklist) {
                    try {
                        final Pattern pattern = Pattern.compile(s, Pattern.CASE_INSENSITIVE);
                        if (!StringUtils.equalsIgnoreCase(host, c.getTypeID())) {
                            if (pattern.matcher(host + "-" + c.getTypeID()).matches()) {
                                result = false;
                                break blacklist;
                            }
                            if (pattern.matcher(host).matches()) {
                                result = false;
                                break blacklist;
                            }
                        }
                        if (pattern.matcher(c.getTypeID()).matches()) {
                            result = false;
                            break blacklist;
                        }
                    } catch (Throwable e) {
                        c.getPlugin().getLogger().log(e);
                    }
                }
            }
        }
        if (result == null) {
            return true;
        }
        if (result) {
            c.getPlugin().getLogger().info(c + " is whitelisted for " + this);
        } else {
            c.getPlugin().getLogger().info(c + " is blacklisted for " + this);
        }
        return result.booleanValue();
    }

    public long getTimeout() {
        return -1;
    }

    public int getWaitForByID(String solverID) {
        Integer obj = getWaitForMap().get(solverID);
        return obj == null ? 0 : obj.intValue();
    }

    private Map<String, Integer> waitForMap = null;

    public synchronized Map<String, Integer> getWaitForMap() {
        if (waitForMap != null) {
            return waitForMap;
        }
        Map<String, Integer> map = getService().getConfig().getWaitForMap();
        if (map == null || map.size() == 0) {
            map = getService().getWaitForOthersDefaultMap();
            getService().getConfig().setWaitForMap(map);
        }
        waitForMap = Collections.synchronizedMap(map);
        return waitForMap;
    }
}
