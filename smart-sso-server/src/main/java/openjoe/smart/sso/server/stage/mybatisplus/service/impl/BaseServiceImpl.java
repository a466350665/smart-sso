package openjoe.smart.sso.server.stage.mybatisplus.service.impl;


import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import openjoe.smart.sso.server.stage.mybatisplus.service.BaseService;

/**
 * MybatisPlus基础ServiceImpl
 *
 * @param <T>
 */
public class BaseServiceImpl<M extends BaseMapper<T>, T> extends ServiceImpl<M, T> implements BaseService<T> {
}