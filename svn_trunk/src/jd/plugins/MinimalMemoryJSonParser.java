package jd.plugins;

import java.util.Map;

import org.appwork.storage.JSonStorage;
import org.appwork.storage.simplejson.JSonParser;
import org.appwork.storage.simplejson.MinimalMemoryMap;

public class MinimalMemoryJSonParser extends JSonParser {
    public MinimalMemoryJSonParser(String json) {
        super(json);
    }

    @Override
    protected boolean isBigIntegerAllowed() {
        return true;
    }

    @Override
    protected Map<String, ? extends Object> createJSonObject() {
        return new MinimalMemoryMap<String, Object>();
    }

    @Override
    protected String dedupeString(String string) {
        return JSonStorage.dedupeString(string);
    }
}
