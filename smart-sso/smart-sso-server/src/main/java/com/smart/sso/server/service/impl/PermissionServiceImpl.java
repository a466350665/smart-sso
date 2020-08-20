package com.smart.sso.server.service.impl;

import java.util.Collection;
import java.util.Collections;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import com.google.common.collect.Lists;
import com.smart.mvc.model.Condition;
import com.smart.mvc.service.impl.ServiceImpl;
import com.smart.mvc.util.ConvertUtils;
import com.smart.sso.client.model.RpcPermission;
import com.smart.sso.server.dao.PermissionDao;
import com.smart.sso.server.dto.PermissionDto;
import com.smart.sso.server.model.Permission;
import com.smart.sso.server.service.PermissionService;
import com.smart.sso.server.service.RolePermissionService;

@Service("permissionService")
public class PermissionServiceImpl extends ServiceImpl<PermissionDao, Permission> implements PermissionService {

	@Autowired
	private RolePermissionService rolePermissionService;

	@Override
	public List<Permission> selectList(Integer appId, Integer roleId, Boolean isEnable) {
        List<Permission> permissionList = findByAppId(appId, isEnable);
        if (roleId == null) {
            return ConvertUtils.convert(permissionList, r -> convertToDto(r, Collections.emptyList()));
        }
        List<Integer> permissionIdList = rolePermissionService.findPermissionIdListByRoleId(roleId);
        return ConvertUtils.convert(permissionList, r -> convertToDto(r, permissionIdList));
	}
	
    private List<Permission> findByAppId(Integer appId, Boolean isEnable) {
        return selectList(Condition.create().eq(appId != null, "appId", appId)
                .eq(isEnable != null, "isEnable", isEnable).orderBy("sort asc, id asc"));
    }
	
	private PermissionDto convertToDto(Permission r, List<Integer> permissionIdList) {
        PermissionDto dto = new PermissionDto();
        BeanUtils.copyProperties(r, dto);
        if (CollectionUtils.isEmpty(permissionIdList)) {
            dto.setChecked(false);
        }
        else {
            dto.setChecked(permissionIdList.contains(r.getId()));
        }
        return dto;
    }

	@Override
	@Transactional
	public void delete(Integer id, Integer appId) {
		List<Integer> idList = Lists.newArrayList();

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
