package openjoe.smart.sso.server.util;

import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * 转换工具类
 *
 * @author Joe
 */
public class ConvertUtils {

    /**
     * List对象转换
     *
     * @param collection 集合
     * @param function   对象转换方法
     * @return
     */
    public static <T, E> List<E> convert(Collection<T> collection, Function<? super T, ? extends E> function) {
        if (CollectionUtils.isEmpty(collection)) {
            return Collections.emptyList();
        }
        return collection.stream().map(function::apply).collect(Collectors.toList());
    }

    public static List<Long> convertToIdList(final String ids) {
        return StringUtils.hasLength(ids) ? Stream.of(ids.split(",")).filter(s -> StringUtils.hasLength(s)).map(s -> Long.valueOf(s.trim()))
                .collect(Collectors.toList())
                : Collections.emptyList();
    }
}