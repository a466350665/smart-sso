package com.smart.sso.server.service.impl;

import java.util.Collection;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.smart.mvc.model.Condition;
import com.smart.mvc.model.Page;
import com.smart.mvc.service.impl.ServiceImpl;
import com.smart.sso.server.dao.AppDao;
import com.smart.sso.server.model.App;
import com.smart.sso.server.service.AppService;
import com.smart.sso.server.service.PermissionService;
import com.smart.sso.server.service.RolePermissionService;

@Service("appService")
public class AppServiceImpl extends ServiceImpl<AppDao, App> implements AppService {
	
	@Autowired
	private PermissionService permissionService;
	@Autowired
	private RolePermissionService rolePermissionService;

	@Override
    @Transactional(readOnly = false)
    public void enable(Boolean isEnable, List<Integer> idList) {
        selectByIds(idList).forEach(t -> {
            t.setIsEnable(isEnable);
            update(t);
        });
    }
	
	@Override
	public List<App> selectAll(Boolean isEnable) {
		return selectList(Condition.create().eq("isEnable", isEnable));
	}

	@Override
	public Page<App> selectPage(String name, Page<App> p) {
		return selectPage(Condition.create().like("name", name), p);
	}

	@Override
	public App selectByCode(String code) {
		return selectOne(Condition.create().eq("code", code));
	}
	
	@Override
	@Transactional(readOnly = false)
	public void deleteByIds(Collection<Integer> idList) {
		rolePermissionService.deleteByAppIds(idList);
		permissionService.deleteByAppIds(idList);
		super.deleteByIds(idList);
	}
}
