package openjoe.smart.sso.server.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import openjoe.smart.sso.server.stage.core.Page;
import openjoe.smart.sso.server.stage.mybatisplus.service.impl.BaseServiceImpl;
import openjoe.smart.sso.server.dto.PermissionDTO;
import openjoe.smart.sso.server.entity.Role;
import openjoe.smart.sso.server.mapper.RoleMapper;
import openjoe.smart.sso.server.service.RolePermissionService;
import openjoe.smart.sso.server.service.RoleService;
import openjoe.smart.sso.server.service.UserRoleService;
import openjoe.smart.sso.server.util.ConvertUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.util.Collection;
import java.util.Collections;
import java.util.List;

@Service("roleService")
public class RoleServiceImpl extends BaseServiceImpl<RoleMapper, Role> implements RoleService {

	@Autowired
	private UserRoleService userRoleService;
	@Autowired
	private RolePermissionService rolePermissionService;

	@Transactional
	@Override
    public void enable(Boolean isEnable, List<Long> idList) {
        selectByIds(idList).forEach(t -> {
            t.setIsEnable(isEnable);
            updateById(t);
        });
    }

    private List<Role> selectByIds(List<Long> idList){
        LambdaQueryWrapper<Role> wrapper =  Wrappers.lambdaQuery();
        wrapper.in(Role::getId, idList);
        return list(wrapper);
    }

	@Override
	public Page<Role> selectPage(String name, Long current, Long size) {
        LambdaQueryWrapper<Role> wrapper =  Wrappers.lambdaQuery();
        wrapper.like(Role::getName, name);
        return findPage(current, size, wrapper);
	}

	@Override
	public List<Role> selectAll(Boolean isEnable) {
        LambdaQueryWrapper<Role> wrapper =  Wrappers.lambdaQuery();
        wrapper.eq(Role::getIsEnable, isEnable);
        return list(wrapper);
	}

	@Transactional
	@Override
	public void deleteByIds(Collection<Long> idList) {
		userRoleService.deleteByRoleIds(idList);
		rolePermissionService.deleteByRoleIds(idList);
		super.removeByIds(idList);
	}

	@Override
    public List<PermissionDTO> getRoleList(Long userId) {
        List<Role> list = selectAll(true);
        if (CollectionUtils.isEmpty(list)) {
            return Collections.emptyList();
        }
        if (userId == null) {
            return ConvertUtils.convert(list, r -> convertToDto(r, Collections.emptyList()));
        }
        List<Long> roleIdList = userRoleService.findRoleIdListByUserId(userId);
        return ConvertUtils.convert(list, r -> convertToDto(r, roleIdList));
    }

    private PermissionDTO convertToDto(Role r, List<Long> roleIdList) {
        PermissionDTO dto = new PermissionDTO();
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
