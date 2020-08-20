package com.smart.mvc.util;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.springframework.util.CollectionUtils;

import com.smart.mvc.model.Page;

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
     * @param source
     *            源分页对象
     * @param function
     *            对象转换方法
     * @return
     */
    public static <T, E> Page<E> convert(Page<T> source, Function<? super T, ? extends E> function) {
        if (source == null) {
            return null;
        }
        Page<E> target = Page.create(source.getPageNo(), source.getPageSize());
        target.setRowCount(source.getRowCount());
        target.setList(convert(source.getList(), function));
        return target;
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
