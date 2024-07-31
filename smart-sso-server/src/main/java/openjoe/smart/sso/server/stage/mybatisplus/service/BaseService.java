package openjoe.smart.sso.server.stage.mybatisplus.service;

import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;
import openjoe.smart.sso.server.stage.core.Page;
import openjoe.smart.sso.server.stage.mybatisplus.util.PageHelper;

/**
 * MybatisPlus基础Service
 *
 * @param <T>
 */
public interface BaseService<T> extends IService<T> {

    /**
     * 提供新的分页方法，统一分页返回Entity
     *
     * @param current
     * @param size
     * @return
     */
    default Page<T> findPage(long current, long size) {
        IPage<T> t = page(new com.baomidou.mybatisplus.extension.plugins.pagination.Page<>(current, size));
        return PageHelper.convert(t);
    }

    /**
     * 提供新的分页方法，统一分页返回Entity
     *
     * @param current
     * @param size
     * @param wrapper
     * @return
     */
    default Page<T> findPage(long current, long size, Wrapper<T> wrapper) {
        IPage<T> t = page(new com.baomidou.mybatisplus.extension.plugins.pagination.Page<>(current, size), wrapper);
        return PageHelper.convert(t);
    }
}