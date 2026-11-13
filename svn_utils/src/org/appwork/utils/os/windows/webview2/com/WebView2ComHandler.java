/**
 *
 * ====================================================================================================================================================
 *         "AppWork Utilities" License
 *         The "AppWork Utilities" will be called [The Product] from now on.
 * ====================================================================================================================================================
 *         Copyright (c) 2009-2026, AppWork GmbH <e-mail@appwork.org>
 *         Spalter Strasse 58
 *         91183 Abenberg
 *         e-mail@appwork.org
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
package org.appwork.utils.os.windows.webview2.com;

import java.util.Collections;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import org.appwork.utils.Time;

import com.sun.jna.Callback;
import com.sun.jna.CallbackReference;
import com.sun.jna.Memory;
import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.platform.win32.Guid;
import com.sun.jna.platform.win32.W32Errors;
import com.sun.jna.platform.win32.WinNT;
import com.sun.jna.ptr.PointerByReference;
import com.sun.jna.win32.StdCallLibrary.StdCallCallback;

/**
 * Minimal IUnknown + Invoke COM callback object for WebView2 completion/event handlers.
 * <p>
 * Native layout: [Pointer vtable]. An instance stays pinned via {@link #KEEP_ALIVE} as long as somebody holds a COM reference to it.
 * It starts with the reference of its creator. After the handler was handed to WebView2, the creator calls {@link #detach()}; from
 * then on WebView2 alone decides how long the handler lives, also when the caller stops waiting for it (timeout).
 */
public final class WebView2ComHandler {
    public interface QueryInterfaceCallback extends StdCallCallback {
        WinNT.HRESULT callback(Pointer thisPtr, Pointer riid, PointerByReference ppvObject);
    }

    public interface AddRefCallback extends StdCallCallback {
        int callback(Pointer thisPtr);
    }

    public interface ReleaseCallback extends StdCallCallback {
        int callback(Pointer thisPtr);
    }

    /** Event-style: Invoke(this, IUnknown* sender, IUnknown* args) */
    public interface Invoke2Callback extends StdCallCallback {
        int callback(Pointer thisPtr, Pointer a, Pointer b);
    }

    /** Completion-style: Invoke(this, HRESULT errorCode, Pointer result) */
    public interface InvokeHRCallback extends StdCallCallback {
        int callback(Pointer thisPtr, int hresult, Pointer result);
    }

    public interface Invoke2 {
        int invoke(Pointer thisPtr, Pointer a, Pointer b);
    }

    public interface InvokeHRObject {
        int invoke(Pointer thisPtr, int hresult, Pointer result);
    }

    private static final Set<WebView2ComHandler>              KEEP_ALIVE     = Collections.newSetFromMap(new ConcurrentHashMap<WebView2ComHandler, Boolean>());
    /**
     * Handlers without references. They are kept a little longer instead of being dropped at once: the last Release may still be
     * returning through the native callback stub when the count reaches 0, and that stub must not be freed under it.
     */
    private static final ConcurrentLinkedQueue<Object[]>      RETIRED        = new ConcurrentLinkedQueue<Object[]>();
    private static final long                                 RETIRED_MILLIS = TimeUnit.SECONDS.toMillis(30);
    private final AtomicBoolean                               detached       = new AtomicBoolean(false);

    private final Guid.GUID            iid;
    private final AtomicInteger        refCount = new AtomicInteger(1);
    private final Memory               objectMemory;
    private final Memory               vtableMemory;
    private final QueryInterfaceCallback cbQueryInterface;
    private final AddRefCallback       cbAddRef;
    private final ReleaseCallback      cbRelease;
    private final Callback             cbInvoke;

    private WebView2ComHandler(Guid.GUID iid, final Invoke2 invoke2, final InvokeHRObject invokeHR) {
        this.iid = iid;
        this.vtableMemory = new Memory(4L * Native.POINTER_SIZE);
        this.objectMemory = new Memory(Native.POINTER_SIZE);

        this.cbQueryInterface = new QueryInterfaceCallback() {
            public WinNT.HRESULT callback(Pointer thisPtr, Pointer riid, PointerByReference ppvObject) {
                return queryInterface(thisPtr, riid, ppvObject);
            }
        };
        this.cbAddRef = new AddRefCallback() {
            public int callback(Pointer thisPtr) {
                return refCount.incrementAndGet();
            }
        };
        this.cbRelease = new ReleaseCallback() {
            public int callback(Pointer thisPtr) {
                return releaseReference();
            }
        };
        if (invokeHR != null) {
            this.cbInvoke = new InvokeHRCallback() {
                public int callback(Pointer thisPtr, int hresult, Pointer result) {
                    return invokeHR.invoke(thisPtr, hresult, result);
                }
            };
        } else {
            this.cbInvoke = new Invoke2Callback() {
                public int callback(Pointer thisPtr, Pointer a, Pointer b) {
                    return invoke2.invoke(thisPtr, a, b);
                }
            };
        }

        this.vtableMemory.setPointer(0L * Native.POINTER_SIZE, CallbackReference.getFunctionPointer(cbQueryInterface));
        this.vtableMemory.setPointer(1L * Native.POINTER_SIZE, CallbackReference.getFunctionPointer(cbAddRef));
        this.vtableMemory.setPointer(2L * Native.POINTER_SIZE, CallbackReference.getFunctionPointer(cbRelease));
        this.vtableMemory.setPointer(3L * Native.POINTER_SIZE, CallbackReference.getFunctionPointer(cbInvoke));
        this.objectMemory.setPointer(0, this.vtableMemory);

        KEEP_ALIVE.add(this);
        purgeRetired();
    }

    private int releaseReference() {
        final int left = refCount.decrementAndGet();
        if (left <= 0) {
            retire();
        }
        return Math.max(0, left);
    }

    private void retire() {
        if (KEEP_ALIVE.remove(this)) {
            RETIRED.add(new Object[] { this, Long.valueOf(Time.systemIndependentCurrentJVMTimeMillis()) });
        }
    }

    private static void purgeRetired() {
        final long now = Time.systemIndependentCurrentJVMTimeMillis();
        Object[] oldest;
        while ((oldest = RETIRED.peek()) != null && now - ((Long) oldest[1]).longValue() > RETIRED_MILLIS) {
            RETIRED.remove(oldest);
        }
    }

    /**
     * Gives up the reference of the creator. Call it once after the handler was successfully handed to WebView2. The handler then
     * lives exactly as long as WebView2 references it.
     */
    public void detach() {
        if (detached.compareAndSet(false, true)) {
            releaseReference();
        }
    }

    public static WebView2ComHandler createHR(Guid.GUID iid, InvokeHRObject handler) {
        return new WebView2ComHandler(iid, null, handler);
    }

    public static WebView2ComHandler create(Guid.GUID iid, Invoke2 handler) {
        return new WebView2ComHandler(iid, handler, null);
    }

    public Pointer getPointer() {
        return objectMemory;
    }

    /** Unpins a handler that WebView2 never received, e.g. because the call that should hand it over failed. */
    public void releaseKeepAlive() {
        retire();
    }

    private WinNT.HRESULT queryInterface(Pointer thisPtr, Pointer riid, PointerByReference ppvObject) {
        if (ppvObject == null) {
            return new WinNT.HRESULT(W32Errors.E_POINTER);
        }
        final Guid.GUID requested = new Guid.GUID(riid);
        requested.read();
        if (WebView2Guids.IID_IUnknown.equals(requested) || iid.equals(requested)) {
            ppvObject.setValue(thisPtr);
            refCount.incrementAndGet();
            return W32Errors.S_OK;
        }
        ppvObject.setValue(Pointer.NULL);
        return new WinNT.HRESULT(W32Errors.E_NOINTERFACE);
    }
}
