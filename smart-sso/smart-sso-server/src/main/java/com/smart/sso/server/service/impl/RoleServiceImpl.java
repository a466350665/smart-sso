package com.smart.sso.server.service.impl;

import java.util.Collection;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.smart.mvc.model.Condition;
import com.smart.mvc.model.Page;
import com.smart.mvc.service.impl.ServiceImpl;
import com.smart.sso.server.dao.RoleDao;
import com.smart.sso.server.model.Role;
import com.smart.sso.server.service.RolePermissionService;
import com.smart.sso.server.service.RoleService;
import com.smart.sso.server.service.UserRoleService;

@Service("roleService")
public class RoleServiceImpl extends ServiceImpl<RoleDao, Role> implements RoleService {

	@Autowired
	private UserRoleService userRoleService;
	@Autowired
	private RolePermissionService rolePermissionService;

	@Transactional
	@Override
    public void enable(Boolean isEnable, List<Integer> idList) {
        selectByIds(idList).forEach(t -> {
            t.setIsEnable(isEnable);
            update(t);
        });
    }

	@Override
	public Page<Role> selectPage(String name, Page<Role> p) {
		return selectPage(Condition.create().like("name", name), p);
	}

	@Override
	public List<Role> selectAll(Boolean isEnable) {
		return selectList(Condition.create().eq("isEnable", isEnable));
	}

	@Transactional
	@Override
	public void deleteByIds(Collection<Integer> idList) {
		userRoleService.deleteByRoleIds(idList);
		rolePermissionService.deleteByRoleIds(idList);
		deleteByIds(idList);
	}
}
