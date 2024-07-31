package openjoe.smart.sso.server.stage.core;

import java.util.Collections;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * 分页实体
 * 注：避免ORM异构带来的RPC分页实体转换
 *
 * @author Joe
 */
public class Page<T> {

    /**
     * 返回记录列表
     */
    protected List<T> records;

    /**
     * 总记录数
     */
    protected long total;

    /**
     * 当前页
     */
    protected long current;

    /**
     * 每页记录数
     */
    protected long size;

    public Page() {
        this.records = Collections.emptyList();
        this.total = 0L;
        this.current = 1L;
        this.size = 10L;
    }

    public Page(long current, long size) {
        this(current, size, 0L);
    }

    public Page(long current, long size, long total) {
        this.current = current;
        this.size = size;
        this.total = total;
    }

    public List<T> getRecords() {
        return this.records;
    }

    public Page<T> setRecords(List<T> records) {
        this.records = records;
        return this;
    }

    public long getTotal() {
        return this.total;
    }

    public Page<T> setTotal(long total) {
        this.total = total;
        return this;
    }

    public long getSize() {
        return this.size;
    }

    public Page<T> setSize(long size) {
        this.size = size;
        return this;
    }

    public long getCurrent() {
        return this.current;
    }

    public Page<T> setCurrent(long current) {
        this.current = current;
        return this;
    }

    public long getPages() {
        if (this.getSize() == 0L) {
            return 0L;
        } else {
            long pages = this.getTotal() / this.getSize();
            if (this.getTotal() % this.getSize() != 0L) {
                ++pages;
            }

            return pages;
        }
    }

    public <R> Page<R> convert(Function<? super T, ? extends R> mapper) {
        Page<R> p = Page.of(current, size, total);
        p.setRecords(records.stream().map(mapper).collect(Collectors.toList()));
        return p;
    }

    public static <T> Page<T> of(long current, long size) {
        return of(current, size, 0L);
    }

    public static <T> Page<T> of(long current, long size, long total) {
        return new Page(current, size, total);
    }
}