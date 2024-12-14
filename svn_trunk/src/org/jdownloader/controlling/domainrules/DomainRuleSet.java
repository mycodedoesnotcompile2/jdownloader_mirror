package org.jdownloader.controlling.domainrules;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.concurrent.atomic.AtomicInteger;

public class DomainRuleSet extends ArrayList<CompiledDomainRule> {
    private final LinkedHashMap<CompiledDomainRule, AtomicInteger> counterMap = new LinkedHashMap<CompiledDomainRule, AtomicInteger>();

    @Override
    public boolean add(CompiledDomainRule e) {
        counterMap.put(e, new AtomicInteger());
        return super.add(e);
    }

    @Override
    public boolean remove(Object o) {
        counterMap.remove(o);
        return super.remove(o);
    }

    @Override
    public CompiledDomainRule remove(int index) {
        final CompiledDomainRule ret = super.remove(index);
        counterMap.remove(ret);
        return ret;
    }

    public HashMap<CompiledDomainRule, AtomicInteger> getMap() {
        return counterMap;
    }
}
