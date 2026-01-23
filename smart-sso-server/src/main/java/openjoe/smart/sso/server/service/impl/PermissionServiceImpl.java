package openjoe.smart.sso.server.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import openjoe.smart.sso.base.entity.TokenMenu;
import openjoe.smart.sso.base.entity.TokenPermission;
import openjoe.smart.sso.server.dto.PermissionDTO;
import openjoe.smart.sso.server.dto.TreeDTO;
import openjoe.smart.sso.server.entity.Permission;
import openjoe.smart.sso.server.entity.Tree;
import openjoe.smart.sso.server.manager.PermissionManager;
import openjoe.smart.sso.server.mapper.PermissionMapper;
import openjoe.smart.sso.server.service.PermissionService;
import openjoe.smart.sso.server.service.RolePermissionService;
import openjoe.smart.sso.server.service.UserRoleService;
import openjoe.smart.stage.mybatisplus.service.impl.BaseServiceImpl;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.util.*;
import java.util.stream.Collectors;

@Service("permissionService")
public class PermissionServiceImpl extends BaseServiceImpl<PermissionMapper, Permission> implements PermissionService, PermissionManager {

	@Autowired
	private RolePermissionService rolePermissionService;
	@Autowired
	private UserRoleService userRoleService;

	@Override
    public List<PermissionDTO> selectTree(Long appId, Long roleId, Boolean isEnable) {
        List<Permission> permissionList = findByAppId(appId, isEnable);
        if (roleId == null) {
            return addRoot(Tree.build(permissionList, r -> convertToDto(r, false)));
        }

        Set<Long> permissionIdSet = rolePermissionService.findPermissionIdSetByRoleIds(Collections.singletonList(roleId));
        return addRoot(Tree.build(permissionList, r -> convertToDto(r, permissionIdSet.contains(r.getId()))));
    }
	
	public List<PermissionDTO> addRoot(List<TreeDTO> list) {
        PermissionDTO dto = new PermissionDTO();
        dto.setName("根节点");
        dto.setChildren(list);
		List<PermissionDTO> dtoList = new ArrayList<>();
		dtoList.add(dto);
        return dtoList;
    }
	
	private PermissionDTO convertToDto(Permission r, Boolean checked) {
        PermissionDTO dto = new PermissionDTO();
        BeanUtils.copyProperties(r, dto);
        dto.setChecked(checked);
        return dto;
    }
	
    private List<Permission> findByAppId(Long appId, Boolean isEnable) {
		LambdaQueryWrapper<Permission> wrapper =  Wrappers.lambdaQuery();
		wrapper.eq(appId != null, Permission::getAppId, appId);
		wrapper.eq(isEnable != null, Permission::getIsEnable, isEnable);
		wrapper.orderByDesc(Permission::getSort).orderByAsc(Permission::getId);
		return list(wrapper);
    }

	@Override
	@Transactional
	public void delete(Long id, Long appId) {
		List<Long> idList = new ArrayList<>();

		List<Permission> list = findByAppId(appId, null);
		loopSubList(id, idList, list);
		idList.add(id);

		rolePermissionService.deleteByPermissionIds(idList);

		removeByIds(idList);
	}

	// 递归方法，删除子权限
	protected void loopSubList(Long id, List<Long> idList, List<Permission> list) {
		for (Permission p : list) {
			if (id.equals(p.getParentId())) {
				idList.add(p.getId());
				loopSubList(p.getId(), idList, list);
			}
		}
	}

	@Override
	public void deleteByAppIds(Collection<Long> idList) {
		LambdaQueryWrapper<Permission> wrapper =  Wrappers.lambdaQuery();
		wrapper.in(Permission::getAppId, idList);
		remove(wrapper);
	}

	@Override
	public TokenPermission getUserPermission(Long userId, Long appId) {
		List<Permission> list = findByAppId(appId, true);
		if(CollectionUtils.isEmpty(list)){
			return new TokenPermission(Collections.emptySet(), Collections.emptySet(), Collections.emptyList());
		}
		Set<Long> permissionIdSet = getUserPermissionIdSet(userId);
		Set<String> permissionSet;
		Set<String> noPermissionSet;
		if (CollectionUtils.isEmpty(permissionIdSet)) {
			permissionSet = Collections.emptySet();
			noPermissionSet = list.stream().map(t -> t.getUrl()).collect(Collectors.toSet());
		}
		else{
			permissionSet = list.stream().filter(t -> permissionIdSet.contains(t.getId())).map(t -> t.getUrl()).collect(Collectors.toSet());
			noPermissionSet= list.stream().filter(t -> !permissionIdSet.contains(t.getId())).map(t -> t.getUrl()).collect(Collectors.toSet());
		}
		List<TokenMenu> menuList = list.stream().filter(t -> t.getIsMenu() && !noPermissionSet.contains(t.getUrl())).map(t->{
			TokenMenu dto = new TokenMenu();
			BeanUtils.copyProperties(t, dto);
			return dto;
		}).collect(Collectors.toList());
		return new TokenPermission(permissionSet, noPermissionSet, menuList);
	}

	private Set<Long> getUserPermissionIdSet(Long userId){
		List<Long> roleIdList = userRoleService.findRoleIdListByUserId(userId);
		if (CollectionUtils.isEmpty(roleIdList)) {
			return Collections.emptySet();
		}
		return rolePermissionService.findPermissionIdSetByRoleIds(roleIdList);
	}
}
