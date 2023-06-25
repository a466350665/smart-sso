package com.smart.sso.server.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.smart.sso.server.model.Page;
import com.smart.sso.server.service.impl.BaseServiceImpl;
import com.smart.sso.server.dao.RoleDao;
import com.smart.sso.server.dto.RoleDto;
import com.smart.sso.server.enums.TrueFalseEnum;
import com.smart.sso.server.model.Role;
import com.smart.sso.server.service.RolePermissionService;
import com.smart.sso.server.service.RoleService;
import com.smart.sso.server.service.UserRoleService;
import com.smart.sso.server.util.ConvertUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.util.Collection;
import java.util.Collections;
import java.util.List;

@Service("roleService")
public class RoleServiceImpl extends BaseServiceImpl<RoleDao, Role> implements RoleService {

	@Autowired
	private UserRoleService userRoleService;
	@Autowired
	private RolePermissionService rolePermissionService;

	@Transactional
	@Override
    public void enable(Boolean isEnable, List<Integer> idList) {
        selectByIds(idList).forEach(t -> {
            t.setIsEnable(isEnable);
            updateById(t);
        });
    }

    private List<Role> selectByIds(List<Integer> idList){
        LambdaQueryWrapper<Role> wrapper =  Wrappers.lambdaQuery();
        wrapper.in(Role::getId, idList);
        return list(wrapper);
    }

	@Override
	public Page<Role> selectPage(String name, Integer pageNo, Integer pageSize) {
        LambdaQueryWrapper<Role> wrapper =  Wrappers.lambdaQuery();
        wrapper.like(Role::getName, name);
        return findPage(pageNo, pageSize, wrapper);
	}

	@Override
	public List<Role> selectAll(Boolean isEnable) {
        LambdaQueryWrapper<Role> wrapper =  Wrappers.lambdaQuery();
        wrapper.eq(Role::getIsEnable, isEnable);
        return list(wrapper);
	}

	@Transactional
	@Override
	public void deleteByIds(Collection<Integer> idList) {
		userRoleService.deleteByRoleIds(idList);
		rolePermissionService.deleteByRoleIds(idList);
		super.removeByIds(idList);
	}

	@Override
    public List<RoleDto> getRoleList(Integer userId) {
        List<Role> list = selectAll(TrueFalseEnum.TRUE.getValue());
        if (CollectionUtils.isEmpty(list)) {
            return Collections.emptyList();
        }
        if (userId == null) {
            return ConvertUtils.convert(list, r -> convertToDto(r, Collections.emptyList()));
        }
        List<Integer> roleIdList = userRoleService.findRoleIdListByUserId(userId);
        return ConvertUtils.convert(list, r -> convertToDto(r, roleIdList));
    }

    private RoleDto convertToDto(Role r, List<Integer> roleIdList) {
        RoleDto dto = new RoleDto();
        BeanUtils.copyProperties(r, dto);
        if (!CollectionUtils.isEmpty(roleIdList)) {
            dto.setChecked(roleIdList.contains(r.getId()));
        }
        else {
            dto.setChecked(false);
        }
        return dto;
    }
}
