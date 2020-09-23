package com.smart.sso.server.service.impl;

import java.util.Collection;
import java.util.List;

import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.google.common.collect.Lists;
import com.smart.mvc.model.Condition;
import com.smart.mvc.service.impl.ServiceImpl;
import com.smart.sso.client.dto.RpcPermissionDto;
import com.smart.sso.server.common.Tree;
import com.smart.sso.server.dao.PermissionDao;
import com.smart.sso.server.dto.PermissionDto;
import com.smart.sso.server.dto.TreeDto;
import com.smart.sso.server.model.Permission;
import com.smart.sso.server.service.PermissionService;
import com.smart.sso.server.service.RolePermissionService;

@Service("permissionService")
public class PermissionServiceImpl extends ServiceImpl<PermissionDao, Permission> implements PermissionService {

	@Autowired
	private RolePermissionService rolePermissionService;

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
        return selectList(Condition.create().eq(appId != null, "app_id", appId)
                .eq(isEnable != null, "is_enable", isEnable).orderBy("sort asc, id asc"));
    }

	@Override
	@Transactional
	public void delete(Integer id, Integer appId) {
		List<Integer> idList = Lists.newArrayList();

		List<Permission> list = findByAppId(appId, null);
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
		deleteByCondition(Condition.create().in("app_id", idList));
	}

	@Override
	public List<RpcPermissionDto> selectListByUserId(String appCode, Integer userId) {
		return dao.selectListByUserId(appCode, userId);
	}
}
