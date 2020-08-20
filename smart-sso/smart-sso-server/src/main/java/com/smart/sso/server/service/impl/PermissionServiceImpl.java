package com.smart.sso.server.service.impl;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.smart.mvc.model.Condition;
import com.smart.mvc.service.impl.ServiceImpl;
import com.smart.sso.client.model.RpcPermission;
import com.smart.sso.server.dao.PermissionDao;
import com.smart.sso.server.model.Permission;
import com.smart.sso.server.model.RolePermission;
import com.smart.sso.server.service.PermissionService;
import com.smart.sso.server.service.RolePermissionService;

@Service("permissionService")
public class PermissionServiceImpl extends ServiceImpl<PermissionDao, Permission> implements PermissionService {

	@Autowired
	private RolePermissionService rolePermissionService;

	@Override
	public List<Permission> selectList(Integer appId, Integer roleId, Boolean isEnable) {
        List<Permission> permissionList = selectList(Condition.create().eq(appId != null, "appId", appId)
            .eq(isEnable != null && isEnable, "isEnable", isEnable));
		if (roleId != null) {
			List<RolePermission> rolePermissionList = rolePermissionService.selectByRoleId(roleId);
			for (Permission permission : permissionList) {
				for (RolePermission rp : rolePermissionList) {
					if (permission.getId().equals(rp.getPermissionId())) {
						permission.setChecked(true);
						break;
					}
				}
			}
		}
		return permissionList;
	}

	@Override
	@Transactional
	public void delete(Integer id, Integer appId) {
		List<Integer> idList = new ArrayList<>();

		List<Permission> list = selectList(appId, null, null);
		loopSubList(id, idList, list);
		idList.add(id);

		rolePermissionService.deleteByPermissionIds(idList);

		deleteByIds(idList);
	}

	// 递归方法，删除子权限
	protected void loopSubList(Integer id, List<Integer> idList, List<Permission> list) {
		for (Permission p : list) {
			if (id.equals(p.getParentId())) {
				idList.add(p.getId());
				loopSubList(p.getId(), idList, list);
			}
		}
	}

	@Override
	public void deleteByAppIds(Collection<Integer> idList) {
		deleteByCondition(Condition.create().in("appId", idList));
	}

	@Override
	public List<RpcPermission> selectListByUserId(String appCode, Integer userId) {
		return dao.selectListByUserId(appCode, userId);
	}
}
