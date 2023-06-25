package com.smart.sso.server.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import com.smart.sso.server.service.impl.BaseServiceImpl;
import com.smart.sso.client.dto.RpcPermissionDto;
import com.smart.sso.server.common.Tree;
import com.smart.sso.server.dao.PermissionDao;
import com.smart.sso.server.dto.PermissionDto;
import com.smart.sso.server.dto.TreeDto;
import com.smart.sso.server.model.App;
import com.smart.sso.server.model.Permission;
import com.smart.sso.server.service.AppService;
import com.smart.sso.server.service.PermissionService;
import com.smart.sso.server.service.RolePermissionService;
import com.smart.sso.server.service.UserRoleService;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

@Service("permissionService")
public class PermissionServiceImpl extends BaseServiceImpl<PermissionDao, Permission> implements PermissionService {

	@Autowired
	private RolePermissionService rolePermissionService;
	@Autowired
	private UserRoleService userRoleService;
	@Autowired
	private AppService appService;

	@Override
    public List<PermissionDto> selectTree(Integer appId, Integer roleId, Boolean isEnable) {
        List<Permission> permissionList = findByAppId(appId, isEnable);
        if (roleId == null) {
            return addRoot(Tree.build(permissionList, r -> convertToDto(r, false)));
        }
        List<Integer> permissionIdList = rolePermissionService.findPermissionIdListByRoleId(roleId);
        return addRoot(Tree.build(permissionList, r -> convertToDto(r, permissionIdList.contains(r.getId()))));
    }
	
	public List<PermissionDto> addRoot(List<TreeDto> list) {
        PermissionDto dto = new PermissionDto();
        dto.setName("根节点");
        dto.setChildren(list);
        return Lists.newArrayList(dto);
    }
	
	private PermissionDto convertToDto(Permission r, Boolean checked) {
        PermissionDto dto = new PermissionDto();
        BeanUtils.copyProperties(r, dto);
        dto.setChecked(checked);
        return dto;
    }
	
    private List<Permission> findByAppId(Integer appId, Boolean isEnable) {
		LambdaQueryWrapper<Permission> wrapper =  Wrappers.lambdaQuery();
		wrapper.eq(appId != null, Permission::getAppId, appId);
		wrapper.eq(isEnable != null, Permission::getIsEnable, isEnable);
		wrapper.orderByAsc(Permission::getSort).orderByAsc(Permission::getId);
		return list(wrapper);
    }

	@Override
	@Transactional
	public void delete(Integer id, Integer appId) {
		List<Integer> idList = Lists.newArrayList();

		List<Permission> list = findByAppId(appId, null);
		loopSubList(id, idList, list);
		idList.add(id);

		rolePermissionService.deleteByPermissionIds(idList);

		removeByIds(idList);
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
		LambdaQueryWrapper<Permission> wrapper =  Wrappers.lambdaQuery();
		wrapper.in(Permission::getAppId, idList);
		remove(wrapper);
	}

	@Override
	public List<RpcPermissionDto> selectListByUserId(String appCode, Integer userId) {
		App app = appService.selectByCode(appCode);
		if (app == null || !app.getIsEnable()) {
			return Collections.emptyList();
		}
		List<Permission> list = findByAppId(app.getId(), true);
		if (userId != null) {
			List<Integer> roleIdList = userRoleService.findRoleIdListByUserId(userId);
			if (CollectionUtils.isEmpty(roleIdList)) {
				return Collections.emptyList();
			}
			Set<Integer> permissionSet = Sets.newHashSet();
			roleIdList.forEach(roleId -> permissionSet.addAll(rolePermissionService.findPermissionIdListByRoleId(roleId)));
			if (CollectionUtils.isEmpty(permissionSet)) {
				return Collections.emptyList();
			}
			list = list.stream().filter(t -> permissionSet.contains(t.getId())).collect(Collectors.toList());
		}
		return list.stream().map(t -> {
			RpcPermissionDto dto = new RpcPermissionDto();
			BeanUtils.copyProperties(t, dto);
			return dto;
		}).collect(Collectors.toList());
	}
}
