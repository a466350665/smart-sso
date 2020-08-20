package com.smart.sso.server.service.impl;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import com.google.common.collect.Lists;
import com.smart.mvc.model.Condition;
import com.smart.mvc.service.impl.ServiceImpl;
import com.smart.mvc.util.ConvertUtils;
import com.smart.sso.server.dao.RolePermissionDao;
import com.smart.sso.server.model.RolePermission;
import com.smart.sso.server.service.RolePermissionService;

@Service("rolePermissionService")
public class RolePermissionServiceImpl extends ServiceImpl<RolePermissionDao, RolePermission> implements RolePermissionService {
	
	@Transactional
	@Override
	public void allocate(Integer appId, Integer roleId, List<Integer> permissionIdList) {
		deleteByCondition(Condition.create().eq("appId", appId).eq("roleId", roleId));

		List<RolePermission> list = Lists.newArrayList();
		Integer permissionId;
		for (Iterator<Integer> i$ = permissionIdList.iterator(); i$.hasNext(); list
				.add(createRolePermission(appId, roleId, permissionId))) {
			permissionId = i$.next();
		}
		if (!CollectionUtils.isEmpty(list)) {
			super.save(list);
		}
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
		return selectList(Condition.create().eq("roleId", roleId));
	}

	@Override
	public void deleteByPermissionIds(List<Integer> idList) {
	    deleteByCondition(Condition.create().in("permissionId", idList));
	}
	
	@Override
	public void deleteByRoleIds(Collection<Integer> idList) {
		deleteByCondition(Condition.create().in("roleId", idList));
	}
	
	@Override
	public void deleteByAppIds(Collection<Integer> idList) {
		deleteByCondition(Condition.create().in("appId", idList));
	}

    @Override
    public List<Integer> findPermissionIdListByRoleId(Integer roleId) {
        return ConvertUtils.convert(selectByRoleId(roleId), t -> t.getPermissionId());
    }
}
