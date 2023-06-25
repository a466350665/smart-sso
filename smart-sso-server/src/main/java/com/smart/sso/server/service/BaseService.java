package com.smart.sso.server.service;

import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;
import com.smart.sso.server.model.Page;
import com.smart.sso.server.util.ConvertUtils;

public interface BaseService<T> extends IService<T> {
    default Page<T> findPage(long page, long limit) {
        IPage<T> t = this.page(new com.baomidou.mybatisplus.extension.plugins.pagination.Page(page, limit));
        return ConvertUtils.convert(t);
    }

    default Page<T> findPage(long page, long limit, Wrapper<T> wrapper) {
        IPage<T> t = this.page(new com.baomidou.mybatisplus.extension.plugins.pagination.Page(page, limit), wrapper);
        return ConvertUtils.convert(t);
    }
}