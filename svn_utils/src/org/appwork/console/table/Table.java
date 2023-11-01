package org.appwork.console.table;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;

import org.appwork.utils.StringUtils;

public class Table<Data> {
    /**
     *
     */
    private static final String           PIPE    = "|";
    ArrayList<Column<Data>>               columns = new ArrayList<Column<Data>>();
    private List<Data>                    rows;
    private String[]                      sortIds;
    private boolean                       sortAsc;
    private HashMap<String, Column<Data>> colMap  = new HashMap<String, Column<Data>>();
    private boolean                       subheader;
    private boolean                       footer;
    private boolean                       showHeader;

    public Table(boolean subheader, boolean footer, boolean showHeader) {
        this.subheader = subheader;
        this.footer = footer;
        this.showHeader = showHeader;
    }

    public Column<Data> addColumn(Column<Data> c) {
        this.columns.add(c);
        this.colMap.put(c.header, c);
        c.index = this.columns.size() - 1;
        return c;
    }

    @Override
    public String toString() {
        if (this.sortIds != null) {
            Collections.sort(this.rows, new Comparator<Data>() {
                @Override
                public int compare(Data o1, Data o2) {
                    int ret = 0;
                    for (String s : Table.this.sortIds) {
                        if (ret != 0) {
                            return ret;
                        }
                        Column<Data> c = Table.this.colMap.get(s);
                        if (Table.this.sortAsc) {
                            ret = c.renderer.compare(o1, o2, c);
                        } else {
                            ret = c.renderer.compare(o2, o1, c);
                        }
                    }
                    return ret;
                }
            });
        }
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < this.columns.size(); i++) {
            Column<Data> c = this.columns.get(i);
            c.width = this.width(c, i);
        }
        for (int i = 0; i < this.columns.size(); i++) {
            Column<Data> c = this.columns.get(i);
            if (c.visible) {
                if (i > 0) {
                    sb.append(" " + PIPE + " ");
                } else {
                    sb.append("" + PIPE + " ");
                }
                if (this.sortIds != null && this.sortIds.length > 0 && this.sortIds[0].equals(c.header)) {
                    sb.append(StringUtils.fillPost((this.sortAsc ? "▲" : "▼") + c.header, " ", c.width));
                } else {
                    sb.append(StringUtils.fillPost(" " + c.header, " ", c.width));
                }
            }
        }
        sb.append(" " + PIPE + "\r\n");
        int totalWidth = sb.length() - 2;
        sb.append(StringUtils.fillPost("", "─", totalWidth));
        sb.append("\r\n");
        if (!this.showHeader) {
            sb.setLength(0);
            sb.insert(0, StringUtils.fillPost("", "─", totalWidth) + "\r\n");
        } else {
            sb.insert(0, StringUtils.fillPost("", "─", totalWidth) + "\r\n");
        }
        if (this.subheader) {
            for (int i = 0; i < this.columns.size(); i++) {
                Column<Data> c = this.columns.get(i);
                if (c.visible) {
                    if (i > 0) {
                        sb.append(" " + PIPE + " ");
                    } else {
                        sb.append("" + PIPE + " ");
                    }
                    if (c.right) {
                        sb.append(StringUtils.fillPre(String.valueOf(c.renderer.getSubHeader(this.rows, c)), " ", c.width));
                    } else {
                        sb.append(StringUtils.fillPost(String.valueOf(c.renderer.getSubHeader(this.rows, c)), " ", c.width));
                    }
                }
            }
            sb.append(" " + PIPE + "\r\n");
            sb.append(StringUtils.fillPost("", "=", totalWidth) + "\r\n");
        }
        int row = 0;
        for (Data d : this.rows) {
            int maxLines = 1;
            for (int i = 0; i < this.columns.size(); i++) {
                Column<Data> c = this.columns.get(i);
                maxLines = Math.max(maxLines, StringUtils.getLines(String.valueOf(c.renderer.getString(d, row, c))).length);
            }
            for (int l = 0; l < maxLines; l++) {
                for (int i = 0; i < this.columns.size(); i++) {
                    Column<Data> c = this.columns.get(i);
                    if (c.visible) {
                        String[] lines = StringUtils.getLines(String.valueOf(c.renderer.getString(d, row, c)));
                        String value = l < lines.length ? lines[l] : "";
                        if (i > 0) {
                            sb.append(" " + PIPE + " ");
                        } else {
                            sb.append("" + PIPE + " ");
                        }
                        if (c.right) {
                            sb.append(StringUtils.fillPre(value, " ", c.width));
                        } else {
                            sb.append(StringUtils.fillPost(value, " ", c.width));
                        }
                    }
                }
                sb.append(" " + PIPE + "\r\n");
            }
            row++;
        }
        sb.append(StringUtils.fillPost("", "─", totalWidth));
        if (this.footer) {
            sb.append("\r\n");
            for (int i = 0; i < this.columns.size(); i++) {
                Column<Data> c = this.columns.get(i);
                if (c.visible) {
                    if (i > 0) {
                        sb.append(" " + PIPE + " ");
                    } else {
                        sb.append("" + PIPE + " ");
                    }
                    if (c.right) {
                        sb.append(StringUtils.fillPre(String.valueOf(c.renderer.getFooter(this.rows, c)), " ", c.width));
                    } else {
                        sb.append(StringUtils.fillPost(String.valueOf(c.renderer.getFooter(this.rows, c)), " ", c.width));
                    }
                }
            }
            sb.append(" " + PIPE + "\r\n");
            sb.append(StringUtils.fillPost("", "=", totalWidth));
        }
        return sb.toString();
    }

    private int width(Column<Data> c, int cIndex) {
        int max = this.showHeader ? (c.header.length() + 1) : 0;
        if (this.footer) {
            max = Math.max(max, String.valueOf(c.renderer.getFooter(this.rows, c)).length());
        }
        if (this.subheader) {
            max = Math.max(max, String.valueOf(c.renderer.getSubHeader(this.rows, c)).length());
        }
        int i = 0;
        for (Data d : this.rows) {
            for (String line : StringUtils.getLines(String.valueOf(c.renderer.getString(d, i++, c)))) {
                max = Math.max(max, line.length());
            }
        }
        return Math.max(c.minWidth, max);
    }

    public void setData(List<Data> dataList) {
        this.rows = dataList;
    }

    public void sortOn(boolean b, String... columIds) {
        this.sortAsc = b;
        this.sortIds = columIds;
    }
}