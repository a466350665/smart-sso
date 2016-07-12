package com.smart.sso.server.service.impl;

import java.util.Arrays;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.smart.ssm.service.impl.ServiceImpl;
import com.smart.sso.server.common.Permissible;
import com.smart.sso.server.dao.RolePermissionDao;
import com.smart.sso.server.model.RolePermission;
import com.smart.sso.server.service.RolePermissionService;

@Service("rolePermissionService")
public class RolePermissionServiceImpl extends ServiceImpl<RolePermissionDao, RolePermission, Integer> implements RolePermissionService {

	@Autowired
	public void setDao(RolePermissionDao dao) {
		this.dao = dao;
	}

	@Permissible
	@Transactional
	public int allocate(Integer roleId, List<RolePermission> list) {
		dao.deleteByRoleIds(Arrays.asList(roleId));
		return super.save(list);
	}

	public List<RolePermission> findByRoleId(Integer roleId) {
		return dao.findByRoleId(roleId);
	}

	public int deleteByPermissionIds(List<Integer> idList) {
		return dao.deleteByPermissionIds(idList);
	}
	
	public int deleteByRoleIds(List<Integer> idList) {
		return dao.deleteByRoleIds(idList);
	}
	
	public int deleteByAppIds(List<Integer> idList) {
		return dao.deleteByAppIds(idList);
	}
}
