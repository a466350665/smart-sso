package com.smart.mvc.util;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.smart.mvc.model.EnumItemable;
import com.smart.mvc.model.Item;

/**
 * 枚举工具类
 * 
 * @author Joe
 */
public class EnumHelper {

    public static <E extends EnumItemable<?>> E get(Class<E> enumType, Object value) {
        return Stream.of(enumType.getEnumConstants()).filter(a -> a.getValue().equals(value)).findAny().orElse(null);
    }
    
    public static <E extends EnumItemable<?>> String getLabel(Class<E> enumType, Object value) {
        return Optional.ofNullable(get(enumType, value)).map(d -> d.getLabel()).orElse(null);
    }

    public static <E extends EnumItemable<?>> E getByLabel(Class<E> enumType, String Label) {
        return Stream.of(enumType.getEnumConstants()).filter(a -> a.getLabel().equals(Label)).findAny().orElse(null);
    }

    public static <E extends EnumItemable<?>> List<Item> getItemList(Class<E> enumType) {
        return Stream.of(enumType.getEnumConstants()).map(e -> Item.create(e.getLabel(), e.getValue()))
            .collect(Collectors.toList());
    }
}
