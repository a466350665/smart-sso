package openjoe.smart.sso.server.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.google.common.collect.Lists;
import com.google.common.collect.Sets;
import openjoe.smart.sso.base.entity.TokenPermission;
import openjoe.smart.sso.server.common.Tree;
import openjoe.smart.sso.server.dto.MenuDTO;
import openjoe.smart.sso.server.dto.PermissionDTO;
import openjoe.smart.sso.server.dto.TreeDTO;
import openjoe.smart.sso.server.entity.App;
import openjoe.smart.sso.server.entity.Permission;
import openjoe.smart.sso.server.mapper.PermissionDao;
import openjoe.smart.sso.server.service.AppService;
import openjoe.smart.sso.server.service.PermissionService;
import openjoe.smart.sso.server.service.RolePermissionService;
import openjoe.smart.sso.server.service.UserRoleService;
import openjoe.smart.stage.mybatisplus.service.impl.BaseServiceImpl;
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
    public List<PermissionDTO> selectTree(Long appId, Long roleId, Boolean isEnable) {
        List<Permission> permissionList = findByAppId(appId, isEnable);
        if (roleId == null) {
            return addRoot(Tree.build(permissionList, r -> convertToDto(r, false)));
        }
        List<Long> permissionIdList = rolePermissionService.findPermissionIdListByRoleId(roleId);
        return addRoot(Tree.build(permissionList, r -> convertToDto(r, permissionIdList.contains(r.getId()))));
    }
	
	public List<PermissionDTO> addRoot(List<TreeDTO> list) {
        PermissionDTO dto = new PermissionDTO();
        dto.setName("根节点");
        dto.setChildren(list);
        return Lists.newArrayList(dto);
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
		List<Long> idList = Lists.newArrayList();

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
	public TokenPermission getUserPermission(Long userId, String appKey) {
		App app = appService.selectByCode(appKey);
		if (app == null || !app.getIsEnable()) {
			return new TokenPermission(Collections.emptySet(), Collections.emptySet());
		}
		List<Permission> list = findByAppId(app.getId(), true);
		if(CollectionUtils.isEmpty(list)){
			return new TokenPermission(Collections.emptySet(), Collections.emptySet());
		}
		Set<String> allPermissionSet = list.stream().map(t -> t.getUrl()).collect(Collectors.toSet());
		List<Long> roleIdList = userRoleService.findRoleIdListByUserId(userId);
		if (CollectionUtils.isEmpty(roleIdList)) {
			return new TokenPermission(Collections.emptySet(), allPermissionSet);
		}
		Set<Long> permissionIdSet = Sets.newHashSet();
		roleIdList.forEach(roleId -> permissionIdSet.addAll(rolePermissionService.findPermissionIdListByRoleId(roleId)));
		if (CollectionUtils.isEmpty(permissionIdSet)) {
			return new TokenPermission(Collections.emptySet(), allPermissionSet);
		}
		Set<String> permissionSet = list.stream().filter(t -> permissionIdSet.contains(t.getId())).map(t -> t.getUrl()).collect(Collectors.toSet());
		Set<String> noPermissionSet= list.stream().filter(t -> !permissionIdSet.contains(t.getId())).map(t -> t.getUrl()).collect(Collectors.toSet());
		return new TokenPermission(permissionSet, noPermissionSet);
	}

	@Override
	public List<MenuDTO> getUserMenuList(Long userId, String appKey, Set<String> noPermissionSet) {
		App app = appService.selectByCode(appKey);
		if (app == null || !app.getIsEnable()) {
			return Collections.emptyList();
		}
		List<Permission> list = findByAppId(app.getId(), true);
		if(CollectionUtils.isEmpty(list)){
			return Collections.emptyList();
		}
		// 过滤菜单且排出用户没有的权限列表，反向过滤是为了避免应用不需要做权限控制时，前端出现空菜单的情况
		return list.stream().filter(t -> t.getIsMenu() && !noPermissionSet.contains(t.getUrl())).map(t->{
			MenuDTO dto = new MenuDTO();
			BeanUtils.copyProperties(t, dto);
			return dto;
		}).collect(Collectors.toList());
	}
}
