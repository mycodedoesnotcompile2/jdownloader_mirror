package org.jdownloader.extensions.antistandby;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import jd.gui.swing.jdgui.views.settings.components.Checkbox;
import jd.gui.swing.jdgui.views.settings.components.MultiComboBox;

import org.appwork.loggingv3.LogV3;
import org.appwork.storage.config.ValidationException;
import org.appwork.storage.config.events.GenericConfigEventListener;
import org.appwork.storage.config.handler.BooleanKeyHandler;
import org.appwork.storage.config.handler.KeyHandler;
import org.appwork.utils.ReflectionUtils;
import org.appwork.utils.event.DefaultEvent;
import org.appwork.utils.event.DontThrowFromCurrentThreadEventSuppressor;
import org.appwork.utils.event.EventSuppressor;
import org.appwork.utils.os.CrossSystem;
import org.jdownloader.extensions.ExtensionConfigPanel;
import org.jdownloader.extensions.antistandby.translate.T;
import org.jdownloader.logging.LogController;

public class AntistandbyConfigPanel extends ExtensionConfigPanel<AntiStandbyExtension> {

    public AntistandbyConfigPanel(AntiStandbyExtension trayExtension) {
        super(trayExtension);

        final KeyHandler m = CFG_ANTISTANDBY.CONDITION;
        try {
            final Type[] types = ((ParameterizedType) m.getTypeRef().getType()).getActualTypeArguments();
            final MultiComboBox<Object> comp = new MultiComboBox<Object>(((Class) types[0]).getEnumConstants()) {
                private final GenericConfigEventListener<Set<Enum>> listener = new GenericConfigEventListener<Set<Enum>>() {
                                                                                 @Override
                                                                                 public void onConfigValidatorError(KeyHandler<Set<Enum>> keyHandler, Set<Enum> invalidValue, ValidationException validateException) {
                                                                                 }

                                                                                 @Override
                                                                                 public void onConfigValueModified(KeyHandler<Set<Enum>> keyHandler, Set<Enum> newValue) {
                                                                                     updateModel(newValue);
                                                                                 }
                                                                             };
                {
                    Set<Enum> value = (Set<Enum>) m.getValue();
                    if (value == null) {
                        value = newSetInstance(m);
                        for (Object e : ((Class) types[0]).getEnumConstants()) {
                            value.add((Enum) e);
                        }
                    }
                    m.getEventSender().addListener(listener, true);
                    updateModel(value);
                }

                @Override
                protected String getLabel(List<Object> list) {
                    return "[" + list.size() + "/" + getValues().size() + "] " + super.getLabel(list);
                }

                protected Set<Enum> newSetInstance(KeyHandler m) throws InstantiationException, IllegalAccessException {
                    Class raw = ReflectionUtils.getRaw(m.getTypeRef().getType());
                    if (raw.isInterface()) {
                        raw = HashSet.class;
                    }
                    final Set<Enum> value = (Set<Enum>) raw.newInstance();
                    return value;
                }

                protected void updateModel(final Set<Enum> newValue) {
                    if (newValue == null) {
                        setSelectedItems(((Class) types[0]).getEnumConstants());
                    } else {
                        setSelectedItems((Object[]) newValue.toArray(new Enum[0]));
                    }
                }

                @Override
                public void onChanged() {
                    super.onChanged();
                    final EventSuppressor added = new DontThrowFromCurrentThreadEventSuppressor<DefaultEvent>();
                    m.getEventSender().addEventSuppressor(added);
                    try {
                        final Set<Enum> set = newSetInstance(m);
                        for (Object e : getSelectedItems()) {
                            set.add((Enum) e);
                        }
                        m.setValue(set);
                    } catch (InstantiationException e1) {
                        LogController.CL().log(e1);
                    } catch (IllegalAccessException e1) {
                        LogController.CL().log(e1);
                    } finally {
                        m.getEventSender().removeEventSuppressor(added);
                    }
                }
            };
            addPair(T.T.mode(), null, null, comp);
        } catch (Throwable e) {
            LogV3.log(e);
        }
        if (CrossSystem.isWindows()) {
            final BooleanKeyHandler displayRequired = CFG_ANTISTANDBY.DISPLAY_REQUIRED;
            addPair(T.T.prevent_screensaver(), null, new Checkbox(displayRequired));
        }
    }

    @Override
    public void save() {
    }

    @Override
    public void updateContents() {

    }

}
