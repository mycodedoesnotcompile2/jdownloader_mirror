package org.appwork.storage.flexijson.mapper.tests;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import org.appwork.storage.Storable;

public class ClassWithSetsListMaps implements Storable {

    private volatile Set<String> setInterface;

    public Set<String> getSetInterface() {
        return setInterface;
    }

    public void setSetInterface(Set<String> setInterface) {
        this.setInterface = setInterface;
    }

    public Map<String, String> getMapInterface() {
        return mapInterface;
    }

    public void setMapInterface(Map<String, String> mapInterface) {
        this.mapInterface = mapInterface;
    }

    public List<String> getListInterface() {
        return listInterface;
    }

    public void setListInterface(List<String> listInterface) {
        this.listInterface = listInterface;
    }

    public HashSet<String> getSet() {
        return set;
    }

    public void setSet(HashSet<String> set) {
        this.set = set;
    }

    public HashMap<String, String> getMap() {
        return map;
    }

    public void setMap(HashMap<String, String> map) {
        this.map = map;
    }

    public ArrayList<String> getList() {
        return list;
    }

    public void setList(ArrayList<String> list) {
        this.list = list;
    }

    private volatile Map<String, String>     mapInterface;
    private volatile List<String>            listInterface;

    private volatile HashSet<String>         set;
    private volatile HashMap<String, String> map;
    private volatile ArrayList<String>       list;

    public ClassWithSetsListMaps() {
    }

    /**
     * @return
     */
    public static ClassWithSetsListMaps filledInstance() {
        ClassWithSetsListMaps ret = new ClassWithSetsListMaps();
        ret.list = new ArrayList<String>();
        ret.listInterface = new ArrayList<String>();
        ret.map = new HashMap<String, String>();
        ret.mapInterface = new TreeMap<String, String>();
        ret.set = new HashSet<String>();
        ret.setInterface = new HashSet<String>();
        return ret;
    }

}
