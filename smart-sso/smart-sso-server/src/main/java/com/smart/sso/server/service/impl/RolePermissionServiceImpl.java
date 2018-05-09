package com.smart.sso.server.service.impl;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.annotation.Resource;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import com.smart.mvc.service.mybatis.impl.ServiceImpl;
import com.smart.sso.server.dao.RolePermissionDao;
import com.smart.sso.server.model.RolePermission;
import com.smart.sso.server.service.AppService;
import com.smart.sso.server.service.PermissionJmsService;
import com.smart.sso.server.service.RolePermissionService;
import com.smart.sso.server.service.RoleService;

@Service("rolePermissionService")
public class RolePermissionServiceImpl extends ServiceImpl<RolePermissionDao, RolePermission, Integer> implements RolePermissionService {
	
	@Resource
	private RoleService roleService;
	@Resource
	private AppService appService;
	@Resource
	private PermissionJmsService permissionJmsService;

	@Autowired
	public void setDao(RolePermissionDao dao) {
		this.dao = dao;
	}

	@Transactional
	public void allocate(Integer appId, Integer roleId, List<Integer> permissionIdList) {
		dao.deleteByAppAndRoleId(appId, roleId);

		List<RolePermission> list = new ArrayList<RolePermission>();
		Integer permissionId;
		for (Iterator<Integer> i$ = permissionIdList.iterator(); i$.hasNext(); list
				.add(new RolePermission(appId, roleId, permissionId))) {
			permissionId = i$.next();
		}
		if (!CollectionUtils.isEmpty(list)) {
			super.save(list);
		}

		// JMS通知权限变更
		permissionJmsService.send(appService.get(appId).getCode());
	}

	public List<RolePermission> findByRoleId(Integer roleId) {
		return dao.findByRoleId(roleId);
	}

	public void deleteByPermissionIds(List<Integer> idList) {
		dao.deleteByPermissionIds(idList);
	}
	
	public void deleteByRoleIds(List<Integer> idList) {
		dao.deleteByRoleIds(idList);
	}
	
	public void deleteByAppIds(List<Integer> idList) {
		dao.deleteByAppIds(idList);
	}
}
