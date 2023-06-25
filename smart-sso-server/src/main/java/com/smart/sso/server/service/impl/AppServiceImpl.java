package com.smart.sso.server.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.smart.sso.server.model.Page;
import com.smart.sso.server.service.impl.BaseServiceImpl;
import com.smart.sso.server.dao.AppDao;
import com.smart.sso.server.model.App;
import com.smart.sso.server.service.AppService;
import com.smart.sso.server.service.PermissionService;
import com.smart.sso.server.service.RolePermissionService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Collection;
import java.util.List;

@Service("appService")
public class AppServiceImpl extends BaseServiceImpl<AppDao, App> implements AppService {
	
	@Autowired
	private PermissionService permissionService;
	@Autowired
	private RolePermissionService rolePermissionService;

	@Override
    @Transactional(readOnly = false)
    public void enable(Boolean isEnable, List<Integer> idList) {
        selectByIds(idList).forEach(t -> {
            t.setIsEnable(isEnable);
            updateById(t);
        });
    }

	private List<App> selectByIds(List<Integer> idList){
		LambdaQueryWrapper<App> wrapper =  Wrappers.lambdaQuery();
		wrapper.in(App::getId, idList);
		return list(wrapper);
	}
	
	@Override
	public List<App> selectAll(Boolean isEnable) {
		LambdaQueryWrapper<App> wrapper =  Wrappers.lambdaQuery();
		wrapper.eq(App::getIsEnable, isEnable);
		return list(wrapper);
	}

	@Override
	public Page<App> selectPage(String name, Integer pageNo, Integer pageSize) {
		LambdaQueryWrapper<App> wrapper =  Wrappers.lambdaQuery();
		wrapper.like(App::getName, name);
		return findPage(pageNo, pageSize, wrapper);
	}

	@Override
	public App selectByCode(String code) {
		LambdaQueryWrapper<App> wrapper =  Wrappers.lambdaQuery();
		wrapper.eq(App::getCode, code);
		return getOne(wrapper);
	}
	
	@Override
	@Transactional(readOnly = false)
	public void deleteByIds(Collection<Integer> idList) {
		rolePermissionService.deleteByAppIds(idList);
		permissionService.deleteByAppIds(idList);
		super.removeByIds(idList);
	}
}
