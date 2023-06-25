package com.smart.sso.server.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.google.common.collect.Lists;
import com.smart.sso.server.service.impl.BaseServiceImpl;
import com.smart.sso.server.dao.RolePermissionDao;
import com.smart.sso.server.model.RolePermission;
import com.smart.sso.server.service.RolePermissionService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;

@Service("rolePermissionService")
public class RolePermissionServiceImpl extends BaseServiceImpl<RolePermissionDao, RolePermission> implements RolePermissionService {
	
	@Transactional
	@Override
	public void allocate(Integer appId, Integer roleId, List<Integer> permissionIdList) {
		deleteByAppIdAndRoleId(appId, roleId);

		List<RolePermission> list = Lists.newArrayList();
		Integer permissionId;
		for (Iterator<Integer> i$ = permissionIdList.iterator(); i$.hasNext(); list
				.add(createRolePermission(appId, roleId, permissionId))) {
			permissionId = i$.next();
		}
		if (!CollectionUtils.isEmpty(list)) {
			saveBatch(list);
		}
	}

	private void deleteByAppIdAndRoleId(Integer appId, Integer roleId){
		LambdaQueryWrapper<RolePermission> wrapper =  Wrappers.lambdaQuery();
		wrapper.eq(appId != null, RolePermission::getAppId, appId);
		wrapper.eq(roleId != null, RolePermission::getRoleId, roleId);
		remove(wrapper);
	}
	
	private RolePermission createRolePermission(Integer appId, Integer roleId, Integer permissionId) {
	    RolePermission r = new RolePermission();
	    r.setAppId(appId);
	    r.setRoleId(roleId);
	    r.setPermissionId(permissionId);
	    return r;
	}

	@Override
	public List<RolePermission> selectByRoleId(Integer roleId) {
		LambdaQueryWrapper<RolePermission> wrapper =  Wrappers.lambdaQuery();
		wrapper.eq(roleId != null, RolePermission::getRoleId, roleId);
		return list(wrapper);
	}

	@Override
	public void deleteByPermissionIds(List<Integer> idList) {
		LambdaQueryWrapper<RolePermission> wrapper =  Wrappers.lambdaQuery();
		wrapper.in(RolePermission::getPermissionId, idList);
		remove(wrapper);
	}
	
	@Override
	public void deleteByRoleIds(Collection<Integer> idList) {
		LambdaQueryWrapper<RolePermission> wrapper =  Wrappers.lambdaQuery();
		wrapper.in(RolePermission::getRoleId, idList);
		remove(wrapper);
	}
	
	@Override
	public void deleteByAppIds(Collection<Integer> idList) {
		LambdaQueryWrapper<RolePermission> wrapper =  Wrappers.lambdaQuery();
		wrapper.in(RolePermission::getAppId, idList);
		remove(wrapper);
	}

    @Override
    public List<Integer> findPermissionIdListByRoleId(Integer roleId) {
        return selectByRoleId(roleId).stream().map(t -> t.getPermissionId()).collect(Collectors.toList());
    }
}
