package com.smart.sso.server.util;

import com.baomidou.mybatisplus.core.metadata.IPage;
import com.smart.sso.server.model.Page;
import org.springframework.util.CollectionUtils;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * 转换工具类
 * 
 * @author Joe
 * 
 */
public class ConvertUtils {
    
    /**
     * 分页对象转换
     *
     * @return
     */
    public static <T> Page<T> convert(IPage<T> page) {
        Page<T> p = Page.create(page.getCurrent(), page.getSize());
        p.setList(page.getRecords());
        p.setRowCount(page.getTotal());
        return p;
    }
    
    /**
     * List对象转换
     * 
     * @param collection 集合
     * @param function 对象转换方法
     * @return
     */
    public static <T, E> List<E> convert(Collection<T> collection, Function<? super T, ? extends E> function) {
        if (CollectionUtils.isEmpty(collection)) {
            return Collections.emptyList();
        }
        return collection.stream().map(function::apply).collect(Collectors.toList());
    }
}