package com.smart.sso.server.service.impl;

import java.util.Collection;
import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.smart.mvc.model.Condition;
import com.smart.mvc.service.impl.ServiceImpl;
import com.smart.sso.server.dao.UserRoleDao;
import com.smart.sso.server.model.UserRole;
import com.smart.sso.server.service.UserRoleService;

@Service("userRoleService")
public class UserRoleServiceImpl extends ServiceImpl<UserRoleDao, UserRole> implements UserRoleService {

	@Transactional
	@Override
	public void allocate(Integer userId, List<UserRole> list) {
		deleteByCondition(Condition.create().eq("userId", userId));
		super.save(list);
	}
	
	@Override
	public UserRole selectByUserRoleId(Integer userId, Integer roleId) {
		return selectOne(Condition.create().eq("userId", userId).eq("roleId", roleId));
	}
	
	@Override
	public void deleteByRoleIds(Collection<Integer> idList) {
		deleteByCondition(Condition.create().in("roleId", idList));
	}
	
	@Override
	public void deleteByUserIds(Collection<Integer> idList) {
		deleteByCondition(Condition.create().in("userId", idList));
	}
}
