package openjoe.smart.sso.server.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.google.common.collect.Lists;
import openjoe.smart.sso.server.entity.RolePermission;
import openjoe.smart.sso.server.mapper.RolePermissionMapper;
import openjoe.smart.sso.server.service.RolePermissionService;
import openjoe.smart.stage.mybatisplus.service.impl.BaseServiceImpl;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

@Service("rolePermissionService")
public class RolePermissionServiceImpl extends BaseServiceImpl<RolePermissionMapper, RolePermission> implements RolePermissionService {
	
	@Transactional
	@Override
	public void allocate(Long appId, Long roleId, List<Long> permissionIdList) {
		deleteByAppIdAndRoleId(appId, roleId);

		List<RolePermission> list = Lists.newArrayList();
		Long permissionId;
		for (Iterator<Long> ite = permissionIdList.iterator(); ite.hasNext(); list
				.add(createRolePermission(appId, roleId, permissionId))) {
			permissionId = ite.next();
		}
		if (!CollectionUtils.isEmpty(list)) {
			saveBatch(list);
		}
	}

	private void deleteByAppIdAndRoleId(Long appId, Long roleId){
		LambdaQueryWrapper<RolePermission> wrapper =  Wrappers.lambdaQuery();
		wrapper.eq(appId != null, RolePermission::getAppId, appId);
		wrapper.eq(roleId != null, RolePermission::getRoleId, roleId);
		remove(wrapper);
	}
	
	private RolePermission createRolePermission(Long appId, Long roleId, Long permissionId) {
	    RolePermission r = new RolePermission();
	    r.setAppId(appId);
	    r.setRoleId(roleId);
	    r.setPermissionId(permissionId);
	    return r;
	}

	@Override
	public List<RolePermission> selectByRoleIds(List<Long> roleIdList) {
		LambdaQueryWrapper<RolePermission> wrapper =  Wrappers.lambdaQuery();
		wrapper.in(!CollectionUtils.isEmpty(roleIdList), RolePermission::getRoleId, roleIdList);
		return list(wrapper);
	}

	@Override
	public void deleteByPermissionIds(List<Long> idList) {
		LambdaQueryWrapper<RolePermission> wrapper =  Wrappers.lambdaQuery();
		wrapper.in(RolePermission::getPermissionId, idList);
		remove(wrapper);
	}
	
	@Override
	public void deleteByRoleIds(Collection<Long> idList) {
		LambdaQueryWrapper<RolePermission> wrapper =  Wrappers.lambdaQuery();
		wrapper.in(RolePermission::getRoleId, idList);
		remove(wrapper);
	}
	
	@Override
	public void deleteByAppIds(Collection<Long> idList) {
		LambdaQueryWrapper<RolePermission> wrapper =  Wrappers.lambdaQuery();
		wrapper.in(RolePermission::getAppId, idList);
		remove(wrapper);
	}

    @Override
    public Set<Long> findPermissionIdSetByRoleIds(List<Long> roleIdList) {
        return selectByRoleIds(roleIdList).stream().map(t -> t.getPermissionId()).collect(Collectors.toSet());
    }
}
